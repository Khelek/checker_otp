-module(checker_otp).

-export([start/0, stop/0, check/3]).

start() ->
  {ok, _} = application:ensure_all_started(?MODULE).

stop() ->
  Apps = [sync, checker_otp, poolboy, inets, ibrowse],
  [application:stop(App) || App <- Apps],
  ok.

check(URL, Limit, Timeout) ->
	{ok, Pid} = linkchecker_sup:create_child(),
  gen_server:call(Pid, {URL, Limit}, Timeout).
