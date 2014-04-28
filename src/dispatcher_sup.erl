-module(dispatcher_sup).
-behaviour(supervisor).

-export([start_link/0]).
-export([init/1]).

-define(CHILD(I, Type), {I, {I, start_link, []}, permanent, 5000, Type, [I]}).

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

init([]) ->
 {ok, {{simple_one_for_one, 0, 1},
       [{dispatcher, {dispatcher, start_link, []},
         temporary, brutal_kill, worker, [dispatcher]}]}}.

create_child() ->
  supervisor:start_child(?MODULE, [URL, Limit, self()]).
