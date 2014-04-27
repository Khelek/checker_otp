-module(hello_worker).
-behaviour(gen_server).

-export([start_link/0, hello/0, get_hello_count/0]).
-export([init/1, handle_call/3, handle_cast/2,
         handle_info/2, terminate/2, code_change/3]).

start_link() ->
  gen_server:start_link({ local, ?MODULE}, ?MODULE, [], []).

hello() ->
  gen_server:call(?MODULE, { hello }).

get_hello_count() ->
  gen_server:call(?MODULE, { hello_counter }).

init([]) ->
  {ok, { hello_counter, 0}}.

handle_call({ hello }, _From, { hello_counter, Counter }) ->
  NewState = { hello_counter, Counter + 1 },
  { reply, { ok, "Hello!" }, NewState };

handle_call({ hello_counter }, _From, State) ->
  { reply, { ok, State }, State };

handle_call(_Message, _From, State) ->
  { reply, invalid_command, State }.

handle_cast(_Message, State) ->
  { noreply, State }.

handle_info(_Message, State) ->
  { noreply, State }.

terminate(_Reason, _State) ->
  ok.

code_change(_OldVersion, State, _Extra) ->
  { ok, State }.