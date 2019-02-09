-module(mochiweb_http_tests).
-include_lib("eunit/include/eunit.hrl").

-ifdef(gen_tcp_r15b_workaround).
-define(SHOULD_HAVE_BUG, true).
-else.
-define(SHOULD_HAVE_BUG, false).
-endif.

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

bad_headers_test_() ->
    {setup,
     fun start_server/0,
     fun mochiweb_http:stop/1,
     fun bad_headers_tests/1}.

bad_headers2_test_() ->
    {setup,
     fun start_bigbuf_server/0,
     fun mochiweb_http:stop/1,
     fun bad_headers_bigbuf_tests/1}.

start_server() ->
    application:start(inets),
    {ok, Pid} = mochiweb_http:start_link([{port, 0},
                                          {acceptor_pool_size,1},
                                          {loop, fun responder/1}]),
    Pid.

start_bigbuf_server() ->
    application:start(inets),
    {ok, Pid} = mochiweb_http:start_link([{port, 0},
                                          {recbuf, 65536},
                                          {acceptor_pool_size,1},
                                          {loop, fun responder/1}]),
    Pid.


has_acceptor_bug_tests(Server) ->
    Port = mochiweb_socket_server:get(Server, port),
    [{"1000 should be fine even with the bug",
      ?_assertEqual(false, has_bug(Port, 1000))},
     {"10000 should trigger the bug if present",
      ?_assertEqual(?SHOULD_HAVE_BUG, has_bug(Port, 10000))}].

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
      {ok, {{"HTTP/1.1", 431, "Internal Server Error"}, _, []}} ->
          false
  end.

-define(IN_METHOD, 3). % The space after the get.
-define(IN_HEADER, 20). % The dash after user
get_req() ->
    <<"GET / HTTP/1.1\r\n"
      "User-Agent: mochiweb_http_tests/0.0\r\n"
      "Accept: */*\r\n">>.

eol(R) ->
    EOL = <<"\r\n">>,
    <<R/binary, EOL/binary>>.


setup_server(Server) ->
    %% Set up with a single acceptor, dig out the pid - sensitive to change in mochiweb_socket_server
    %% state record.
    [Acceptor] = sets:to_list(element(15, sys:get_state(Server))),
    Port = mochiweb_socket_server:get(Server, port),
    {ok, S} = gen_tcp:connect({127,0,0,1},Port,[binary,{active,false}]),
    {S, Acceptor}.

unexpected_msg_send(Server, MsgAt, Msg) ->
    {S, Acceptor} = setup_server(Server),
    <<Before:MsgAt/binary, After/binary>> = eol(get_req()) ,
    ok = gen_tcp:send(S, Before),
    Acceptor ! Msg,
    gen_tcp:send(S, After),
    gen_tcp:recv(S, 0, 5000).

invalid_header_send(Server, Msg) ->
    {S, Acceptor} = setup_server(Server),
    gen_tcp:send(S, Msg),
    gen_tcp:recv(S, 0, 5000).



unexpected_msg_in_hdr_tests(Server) ->
    [{"should ignore a message in the middle of the request line",
      ?_assertMatch({ok, <<"HTTP/1.1 400 Bad Request", _Rest/binary>>},
		    unexpected_msg_send(Server, ?IN_METHOD, unexpected_msg_in_your_method))},
     {"should ignore a message in the middle of a header",
      ?_assertMatch({ok, <<"HTTP/1.1 400 Bad Request", _Rest/binary>>},
		    unexpected_msg_send(Server, ?IN_HEADER, unexpected_msg_in_your_header))},
     {"should close on a TCP error on the request line",
      ?_assertMatch({error, closed},
		    unexpected_msg_send(Server, ?IN_METHOD, {tcp_error, your_port_you_dont_match_on, something_terrible}))},
     {"should close on a TCP error in a header",
      ?_assertMatch({error, closed},
		    unexpected_msg_send(Server, ?IN_HEADER, {tcp_error, your_port_you_dont_match_on, something_terrible}))}].


build_headers(Count, Size) ->
    AllowedChars = ["a", "b", "c", "1"],
    CharFun = 
        fun(_I, Acc) ->
            C = lists:nth(random:uniform(length(AllowedChars)), AllowedChars),
            [C|Acc]
        end,
    FullString = lists:reverse(lists:foldl(CharFun, [], lists:seq(1, Size))),
    Header = list_to_binary(FullString),
    build_headers(Count, get_req(), Header).

build_headers(0, ReqAcc, _Header) ->
    eol(ReqAcc);
build_headers(Count, ReqAcc, Header) ->
    RiakHeader = <<"x-riak-index-garbage_bin: ">>,
    build_headers(Count - 1,
                    eol(<<ReqAcc/binary, RiakHeader/binary, Header/binary>>),
                    Header).


bad_headers_tests(Server) ->
    [{"too many headers returns error", 
        ?_assertMatch({ok, <<"HTTP/1.1 400 Bad Request", _Rest/binary>>},
                        invalid_header_send(Server, build_headers(1001, 4)))},
        {"small enough headers are OK",
        ?_assertMatch({ok, <<"HTTP/1.1 200 OK", _Rest/binary>>},
                        invalid_header_send(Server, build_headers(10, 100)))}].

bad_headers_bigbuf_tests(Server) ->
    [{"too many headers returns error", 
        ?_assertMatch({ok, <<"HTTP/1.1 400 Bad Request", _Rest/binary>>},
                        invalid_header_send(Server, build_headers(1001, 4)))},
        {"header too big returns error",
        ?_assertMatch({ok, <<"HTTP/1.1 200 OK", _Rest/binary>>},
                        invalid_header_send(Server, build_headers(1, 10000)))},
        {"small enough headers are OK",
        ?_assertMatch({ok, <<"HTTP/1.1 200 OK", _Rest/binary>>},
                        invalid_header_send(Server, build_headers(10, 100)))}].

