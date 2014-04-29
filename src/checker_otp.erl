-module(checker_otp).

-export([start/0, stop/0, check/2]).

start() ->
  {ok, _} = application:ensure_all_started(?MODULE).

stop() ->
  Apps = [sync, checker_otp, poolboy, inets, ibrowse],
  [application:stop(App) || App <- Apps],
  ok.

check(URL, Limit) ->
	linkchecker_sup:create_child(URL, Limit, self()).
