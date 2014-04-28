-module(checker_otp).

-export([start/0, stop/0, check/2]).

start() ->
  {ok, _} = application:ensure_all_started(?MODULE).

stop() ->
  Apps = [sync, checker_otp, poolboy, inets],
  [application:stop(App) || App <- Apps],
  ok.

check(URL, Limit) ->
	linkchecker_worker:check(URL, Limit, self()).
