-module(dispatcher).
-behaviour(gen_server).

-export([start_link/0]).
-export([init/1, handle_call/3, handle_cast/2,
         handle_info/2, terminate/2, code_change/3]).

start_link() ->
  gen_server:start_link({ local, ?MODULE}, ?MODULE, [], []).

init([Domain, Limit, Pid]) ->
  gen_server:cast(?MODULE, { check, Domain, Limit, Pid }).
  {ok, []}.

handle_call(_Message, _From, State) ->
  { reply, invalid_command, State }.

handle_cast({ check, Domain, Limit, Pid }, State) ->
  check_all(Domain, Limit, Pid),
  { noreply, State };
handle_cast(_Message, State) ->
  { noreply, State }.

handle_info(_Message, State) ->
  { noreply, State }.

terminate(_Reason, _State) ->
  ok.

code_change(_OldVersion, State, _Extra) ->
  { ok, State }.
