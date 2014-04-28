-module(request_worker).
-behaviour(gen_server).

-export([start_link/0]).
-export([init/1, handle_call/3, handle_cast/2,
         handle_info/2, terminate/2, code_change/3]).

start_link() ->
  gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

request(Link, Pid) ->
  gen_server:cast(?MODULE, {request, Link, Pid}).

init([]) ->
  {ok, []}.

handle_call(_Message, _From, State) ->
  { reply, invalid_command, State }.

handle_cast({ request, Link, Pid }, State) ->
  {ibrowse_req_id, ReqID} = ibrowse:send_req(Link, [], get, [], [{stream_to, self()}]),
  { noreply, [ReqID | State] };
handle_cast(_Message, State) ->
  { noreply, State }.

handle_info({ibrowse_async_headers, ReqID, StatusCode, _Headers}, State) ->

  { noreply, State };
handle_info({ibrowse_async_response, ReqID, Body) ->
  { noreply, State }.
  
terminate(_Reason, _State) ->
  ok.

code_change(_OldVersion, State, _Extra) ->
  { ok, State }.
