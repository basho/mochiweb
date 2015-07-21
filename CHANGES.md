Version 2.12.2 released 2015-02-21

* Close connections quietly when setopts fails with a closed socket.
  https://github.com/mochi/mochiweb/pull/152

Version 2.12.1 released 2015-02-01

* Fix active_socket accounting
  https://github.com/mochi/mochiweb/issues/149
* Added full MIT license preludes to each source file to make it
  easier for mochiweb's code to be used piecemeal
  https://github.com/mochi/mochiweb/pull/148

Version 2.12.0 released 2015-01-16

* Send "Connection: close" header when the server is going to close
  a Keep-Alive connection, usually due to unread data from the
  client
  https://github.com/mochi/mochiweb/issues/146

Version 2.11.2 released 2015-01-16

* Fix regression introduced in #147
  https://github.com/mochi/mochiweb/pull/147

Version 2.11.1 released 2015-01-16

* Accept range end position which exceededs the resource size
  https://github.com/mochi/mochiweb/pull/147

Version 2.11.0 released 2015-01-12

* Perform SSL handshake after releasing acceptor back into the pool,
  and slow accept rate when file descriptors are not available,
  to mitigate a potential DoS attack. Adds new mochiweb_socket
  functions transport_accept/1 and finish_accept/1 which should be
  used in preference to the now deprecated accept/1 function.
  https://github.com/mochi/mochiweb/issues/138

Version 2.10.1 released 2015-01-11

* Fixes issue with SSL and mochiweb_websocket. Note that
  mochiweb_websocket is still experimental and the API
  is subject to change in future versions.
  https://github.com/mochi/mochiweb/pull/144

Version 2.10.0 released 2014-12-17

* Added new `recbuf` option to mochiweb_http to allow the receive
  buffer to be configured.
  https://github.com/mochi/mochiweb/pull/134

Version 2.9.2 released 2014-10-16

* Add timeouts to SSL connect to prevent DoS by opening a connection
  and not doing anything.
  https://github.com/mochi/mochiweb/pull/140
* Prevent using ECDH cipher in R16B because it is broken
  https://github.com/mochi/mochiweb/pull/140
* For default SSL connections, remove usage of sslv3 and not-so-secure
  ciphers.
  https://github.com/mochi/mochiweb/pull/140

Version 2.9.1 released 2014-09-29

* Fix Makefile rule for building docs
  https://github.com/mochi/mochiweb/issues/135
* Minimize gen_tcp:send calls to optimize performance.
  https://github.com/mochi/mochiweb/pull/137

Version 2.9.0 released 2014-06-24

* Increased timeout in test suite for FreeBSD
  https://github.com/mochi/mochiweb/pull/121
* Updated rebar to v2.5.0 and fixed associated build issues
  https://github.com/mochi/mochiweb/issues/131

Version 2.8.0 released 2014-01-01

* Websocket support
  https://github.com/mochi/mochiweb/pull/120
* Force files named "crossdomain.xml" to have MIME type
  text/x-cross-domain-policy.
  https://github.com/mochi/mochiweb/pull/118

Version 2.7.0 released 2013-08-01

* Fix 0-length range responses
  https://github.com/mochi/mochiweb/pull/87
* Add support for all possible `erlang:decode_packet/3` responses,
  previously these would just crash.
  https://github.com/mochi/mochiweb/pull/114
* Makefile fixed to make `make test` work before `make all`
  https://github.com/mochi/mochiweb/pull/116
* Usage of the crypto module made R16B01+ compatible
  https://github.com/mochi/mochiweb/pull/115
* Build fixed for R16B01
  https://github.com/mochi/mochiweb/pull/112
* `mochiweb_socket_server:stop/1` is now a synchronous
  call instead of an asynchronous cast
* `mochiweb_html:parse_tokens/1` (and `parse/1`) will now create a
  html element to wrap documents that have a HTML5 doctype
  (`<!doctype html>`) but no html element 
  https://github.com/mochi/mochiweb/issues/110

Version 2.6.0 released 2013-04-15

* Enable R15B gen_tcp workaround only on R15B
  https://github.com/mochi/mochiweb/pull/107

Version 2.5.0 released 2013-03-04

* Replace now() with os:timestamp() in acceptor (optimization)
  https://github.com/mochi/mochiweb/pull/102
* New mochiweb_session module for managing session cookies.
  NOTE: this module is only supported on R15B02 and later!
  https://github.com/mochi/mochiweb/pull/94
* New mochiweb_base64url module for base64url encoding
  (URL and Filename safe alphabet, see RFC 4648).
* Fix rebar.config in mochiwebapp_skel to use {branch, "master"}
  https://github.com/mochi/mochiweb/issues/105
  
Version 2.4.2 released 2013-02-05

* Fixed issue in mochiweb_response introduced in v2.4.0
  https://github.com/mochi/mochiweb/pull/100

Version 2.4.1 released 2013-01-30

* Fixed issue in mochiweb_request introduced in v2.4.0
  https://github.com/mochi/mochiweb/issues/97
* Fixed issue in mochifmt_records introduced in v2.4.0
  https://github.com/mochi/mochiweb/issues/96

Version 2.4.0 released 2013-01-23

* Switch from parameterized modules to explicit tuple module calls for
  R16 compatibility (#95)
* Fix for mochiweb_acceptor crash with extra-long HTTP headers under
  R15B02 (#91)
* Fix case in handling range headers (#85)
* Handle combined Content-Length header (#88)
* Windows security fix for `safe_relative_path`, any path with a
  backslash on any platform is now considered unsafe (#92)

Version 2.3.2 released 2012-07-27

* Case insensitive match for "Connection: close" (#81)

Version 2.3.1 released 2012-03-31

* Fix edoc warnings (#63)
* Fix mochiweb_html handling of invalid charref sequences (unescaped &) (#69).
* Add a manual garbage collection between requests to avoid worst case behavior
  on keep-alive sockets.
* Fix dst cookie bug (#73)
* Removed unnecessary template_dir option, see
  https://github.com/basho/rebar/issues/203

Version 2.3.0 released 2011-10-14

* Handle ssl_closed message in mochiweb_http (#59)
* Added support for new MIME types (otf, eot, m4v, svg, svgz, ttc, ttf,
  vcf, webm, webp, woff) (#61)
* Updated mochiweb_charref to support all HTML5 entities. Note that
  if you are using this module directly, the spec has changed to return
  `[integer()]` for some entities. (#64)

Version 2.2.1 released 2011-08-31

* Removed `mochiweb_skel` module from the pre-rebar era

Version 2.2.0 released 2011-08-29

* Added new `mochiweb_http:start_link/1` and
  `mochiweb_socket_server:start_link/1` APIs to explicitly start linked
  servers. Also added `{link, false}` option to the `start/1` variants
  to explicitly start unlinked. This is in expectation that we will
  eventually change the default behavior of `start/1` to be unlinked as you
  would expect it to. See https://github.com/mochi/mochiweb/issues/58 for
  discussion.

Version 2.1.0 released 2011-08-29

* Added new `mochijson2:decode/2` with `{format, struct | proplist | eep18}`
  options for easy decoding to various proplist formats. Also added encoding
  support for eep18 style objects.
