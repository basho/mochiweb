-module(mochiweb_http_tests).
-include_lib("eunit/include/eunit.hrl").

-define(R15_WORKAROUND, false).

has_acceptor_bug_test_() ->
    {setup,
     fun start_server/0,
     fun mochiweb_http:stop/1,
     fun has_acceptor_bug_tests/1}.

unexpected_msg_test_() ->
    {setup,
     fun start_server/0,
     fun mochiweb_http:stop/1,
     fun unexpected_msg_in_hdr_tests/1}.

start_server() ->
    application:start(inets),
    {ok, Pid} = mochiweb_http:start_link([{port, 0},
                                          {acceptor_pool_size,1},
                                          {loop, fun responder/1}]),
    Pid.

has_acceptor_bug_tests(Server) ->
    Port = mochiweb_socket_server:get(Server, port),
    [{"1000 should be fine even with the bug",
      ?_assertEqual(false, has_bug(Port, 1000))},
     {"10000 should trigger the bug if present",
      ?_assertEqual(?R15_WORKAROUND, has_bug(Port, 10000))}].

responder(Req) ->
    Req:respond({200,
                 [{"Content-Type", "text/html"}],
                 ["<html><body>Hello</body></html>"]}).

has_bug(Port, Len) ->
  case
    httpc:request(get, {"http://127.0.0.1:" ++ integer_to_list(Port) ++ "/",
                        [{"X-Random", lists:duplicate(Len, $a)}]}, [], [])
  of
      {error, socket_closed_remotely} ->
          true;
      {ok, {{"HTTP/1.1", 200, "OK"}, _, "<html><body>Hello</body></html>"}} ->
          false;
      {ok, {{"HTTP/1.1", 400, "Bad Request"}, _, []}} ->
          false
  end.

-define(IN_METHOD, 3). % The space after the get.
-define(IN_HEADER, 20). % The dash after user
get_req() ->
    <<"GET / HTTP/1.1\r\n"
      "User-Agent: mochiweb_http_tests/0.0\r\n"
      "Accept: */*\r\n\r\n">>.

unexpected_msg(Server, MsgAt, Msg) ->
    {_, FieldIdx} = lists:keyfind(
        acceptor_pool, 1, mochiweb_socket_server:state_fields()),
    %% Set up with a single acceptor, dig out the pid from the
    %% mochiweb_socket_server state record.
    [Acceptor] = sets:to_list(element(FieldIdx, sys:get_state(Server))),
    Port = mochiweb_socket_server:get(Server, port),
    <<Before:MsgAt/binary, After/binary>> = get_req(),
    {ok, S} = gen_tcp:connect({127,0,0,1},Port,[binary,{active,false}]),
    ok = gen_tcp:send(S, Before),
    Acceptor ! Msg,
    gen_tcp:send(S, After),
    {gen_tcp:recv(S, 0, 5000), process_info(Acceptor, messages)}.


unexpected_msg_in_hdr_tests(Server) ->
    [{"should ignore a message in the middle of the request line",
      ?_assertMatch({{ok, <<"HTTP/1.1 200 OK", _Rest/binary>>}, {messages, []}},
        	    unexpected_msg(Server, ?IN_METHOD, unexpected_msg_in_your_method))},
     {"should ignore a message in the middle of a header",
      ?_assertMatch({{ok, <<"HTTP/1.1 200 OK", _Rest/binary>>}, {messages, []}},
        	    unexpected_msg(Server, ?IN_HEADER, unexpected_msg_in_your_header))},
     {"should close on a TCP error on the request line",
      ?_assertMatch({{error, closed}, undefined},
        	    unexpected_msg(Server, ?IN_METHOD, {tcp_error, your_port_you_dont_match_on, something_terrible}))},
     {"should close on a TCP error in a header",
      ?_assertMatch({{error, closed}, undefined},
        	    unexpected_msg(Server, ?IN_HEADER, {tcp_error, your_port_you_dont_match_on, something_terrible}))}].
