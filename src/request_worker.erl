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
  Waiting = queue:new(),
  %% TODO add application env
  Limit = 50,
  {ok, {[], Limit, Waiting}}.

handle_call(_Message, _From, State) ->
  {reply, invalid_command, State}.

handle_cast({request, Link, Pid}, {Statuses, 0, Waiting}) ->
  NewWaiting = queue:in({Link, Pid}, Waiting),
  {noreply, {Statuses, 0, NewWaiting}};
handle_cast({request, Link, Pid}, {Statuses, Limit, Waiting}) ->
  {ibrowse_req_id, ReqId} = ibrowse:send_req(Link, [], get, [], [{stream_to, self()}]),
  El = {ReqId, Link, Pid, []},
  {noreply, {[El | Statuses], Limit - 1, Waiting}};
handle_cast(_Message, State) ->
  {noreply, State}.

handle_info({ibrowse_async_headers, ReqId, StatusCode, _Headers}, {Statuses, Limit, Waiting}) ->
  NewStatuses = push_status_code(ReqId, Statuses, StatusCode),
  {noreply, {NewStatuses, Limit, Waiting}};
handle_info({ibrowse_async_response, ReqId, Body}, {Statuses, Limit, Waiting}) ->
  NewStatuses = append_body(ReqId, Statuses, Body),
  {noreply, {NewStatuses, Limit, Waiting}};
handle_info({ibrowse_async_response_end, ReqId}, {Statuses, Limit, Waiting}) ->
  {value, {ReqId, Link, Pid, ResponseChunks}, StateExceptCurrent} = lists:keytake(ReqId, 1, Statuses),
  StatusCode = proplists:get_value(status_code, ResponseChunks),
  Body = proplists:get_value(body, ResponseChunks),
  Pid ! {response, Link, StatusCode, Body},
  NewWaiting = process_next_deferred_link(Waiting),
  {noreply, {StateExceptCurrent, Limit + 1, NewWaiting}};
handle_info(_Message, State) ->
  {noreply, State}.
  
terminate(_Reason, _State) ->
  ok.

code_change(_OldVersion, State, _Extra) ->
  { ok, State }.

append_body(ReqId, Statuses, NewBody) ->
  {value, {ReqId, Link, Pid, ResponseChunks}, StateExceptCurrent} = lists:keytake(ReqId, 1, Statuses),
  case lists:keytake(body, 1, ResponseChunks) of
    {value, {body, OldBody}, NewResponseChunks} ->
      %% FIXME падает по таймату flatten'a при больших body
      Body = lists:flatten([OldBody | NewBody]),
      [{ReqId, Link, Pid, [{body, Body} | NewResponseChunks]} | StateExceptCurrent];
    false ->
      [{ReqId, Link, Pid, [{body, NewBody} | ResponseChunks]} | StateExceptCurrent]
  end.

push_status_code(ReqId, Statuses, StatusCode) ->  
  {value, {ReqId, Link, Pid, ResponseChunks}, StateExceptCurrent} = lists:keytake(ReqId, 1, Statuses),
  [{ReqId, Link, Pid, [{status_code, StatusCode} | ResponseChunks]} | StateExceptCurrent].

process_next_deferred_link(Waiting) ->
  case queue:out(Waiting) of
    {{value, {NextLink, NextPid}}, NewWaiting} ->
      request(NextLink, NextPid),
      NewWaiting;
    {empty, Waiting} ->
      Waiting
  end.

