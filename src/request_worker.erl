-module(request_worker).
-behaviour(gen_server).

-export([start_link/0, request/2]).
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

handle_cast({ request, Link, Pid }, Statuses) ->
  {ibrowse_req_id, ReqID} = ibrowse:send_req(Link, [], get, [], [{stream_to, self()}]),
  El = {ReqID, Link, Pid, []},
  { noreply, [El | Statuses] };
handle_cast(_Message, State) ->
  { noreply, State }.

%% TODO add tests to deffered links and limit
handle_info({ibrowse_async_headers, ReqID, StatusCode, _Headers}, Statuses) ->
  {value, {ReqID, Link, Pid, []}, NewStatuses} = lists:keytake(ReqID, 1, Statuses),
  { noreply, [{ReqID, Link, Pid, [StatusCode]} | Statuses] };
handle_info({ibrowse_async_response, ReqID, Body}, Statuses) ->
  {value, {ReqID, Link, Pid, [StatusCode]}, NewStatuses} = lists:keytake(ReqID, 1, Statuses),
  Pid ! {response, Link, StatusCode, Body},
  { noreply, NewStatuses }.
  
terminate(_Reason, _State) ->
  ok.

code_change(_OldVersion, State, _Extra) ->
  { ok, State }.
