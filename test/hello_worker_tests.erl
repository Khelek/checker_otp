-module(hello_worker_tests).
-include_lib("eunit/include/eunit.hrl").

hello_test() ->
  meck:new(hello_worker),
  meck:expect(hello_worker, hello, fun() -> "Hello!" end),

  ?assertEqual(hello_worker:hello(), "Hello!"),
  ?assert(meck:validate(hello_worker)),

  meck:unload(hello_worker).

get_hello_count_test() ->
% FIXME
  meck:new(hello_worker),
  meck:expect(hello_worker, hello, fun() -> "Hello!" end),

  hello_worker:hello(),
  hello_worker:hello(),
  {ok, {_, HelloCount }} = hello_worker:get_hello_count(),

  ?assertEqual(2, HelloCount),
  meck:unload(hello_worker).
