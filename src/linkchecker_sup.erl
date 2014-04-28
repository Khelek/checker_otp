-module(linkchecker_sup).
-behaviour(supervisor).

-export([start_link/0, create_child/3]).
-export([init/1]).

-define(CHILD(I, Type), {I, {I, start_link, []}, permanent, 5000, Type, [I]}).

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

init([]) ->
 {ok, {{simple_one_for_one, 0, 1},
       [?CHILD(linkchecker_worker, worker)]}}.

create_child(URL, Limit, Pid) ->
  supervisor:start_child(?MODULE, [URL, Limit, Pid]).
