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
  Limit = 50, %% TODO add application env
  {ok, {[], Limit, Waiting}}.

handle_call(_Message, _From, State) ->
  {reply, invalid_command, State}.

handle_cast({request, Link, Pid}, {Statuses, 0, Waiting}) ->
  NewWaiting = queue:in({Link, Pid}, Waiting),
  {noreply, {Statuses, 0, NewWaiting}};
handle_cast({request, Link, Pid}, {Statuses, Limit, Waiting}) ->
  case ibrowse:send_req(Link, [], get, [], [{stream_to, self()}]) of
    {ibrowse_req_id, ReqId} ->
      El = {ReqId, Link, Pid, []},
      {noreply, {[El | Statuses], Limit - 1, Waiting}};
    {error, Reason} ->
      response_error(Pid, Link, Reason),
      {noreply, {Statuses, Limit, Waiting}}
  end;
handle_cast(_Message, State) ->
  {noreply, State}.

handle_info({ibrowse_async_headers, ReqId, StatusCode, _Headers}, {Statuses, Limit, Waiting}) ->
  NewStatuses = push_status_code(ReqId, Statuses, StatusCode),
  {noreply, {NewStatuses, Limit, Waiting}};
handle_info({ibrowse_async_response, ReqId, Body}, {Statuses, Limit, Waiting}) ->
  NewStatuses = append_body(ReqId, Statuses, Body),
  {noreply, {NewStatuses, Limit, Waiting}};
handle_info({ibrowse_async_response_end, ReqId}, {Statuses, Limit, Waiting}) ->
  {{ReqId, Link, Pid, ResponseChunks}, StateWithoutCurr} = take_by_key(ReqId, Statuses),
  [StatusCode, Body] = get_values([status_code, body], ResponseChunks),
  response(Pid, Link, StatusCode, Body),
  NewWaiting = process_next_deferred_link(Waiting),
  {noreply, {StateWithoutCurr, Limit + 1, NewWaiting}};
handle_info(_Message, State) ->
  {noreply, State}.
  
terminate(_Reason, _State) ->
  ok.

code_change(_OldVersion, State, _Extra) ->
  { ok, State }.

response(Pid, Link, StatusCode, Body) ->
  Pid ! {response, Link, StatusCode, Body}.

response_error(Pid, Link, Reason) ->
  Pid ! {response, error, Link, Reason}.

append_body(ReqId, Statuses, Body) ->
  {{ReqId, Link, Pid, ResponseChunks}, StateWithoutCurr} = take_by_key(ReqId, Statuses),
  NewResponseChunks = case lists:keytake(body, 1, ResponseChunks) of
                        {value, {body, OldBody}, ResponseChunksWithoutBody} ->
                          NewBody = lists:flatten([OldBody | Body]), %% FIXME падает по таймату flatten'a при больших body
                          [{body, NewBody} | ResponseChunksWithoutBody];
                        false ->
                          [{body, Body} | ResponseChunks]
                      end,
  [{ReqId, Link, Pid, NewResponseChunks} | StateWithoutCurr].

push_status_code(ReqId, Statuses, StatusCode) ->  
  {{ReqId, Link, Pid, ResponseChunks}, StateWithoutCurr} = take_by_key(ReqId, Statuses),
  NewResponseChunks = [{status_code, StatusCode} | ResponseChunks],
  [{ReqId, Link, Pid, NewResponseChunks} | StateWithoutCurr].

process_next_deferred_link(Waiting) ->
  case queue:out(Waiting) of
    {{value, {NextLink, NextPid}}, NewWaiting} ->
      request(NextLink, NextPid),
      NewWaiting;
    {empty, Waiting} ->
      Waiting
  end.

take_by_key(Key, List) ->
  {value, Value, ListWithoutValue} = lists:keytake(Key, 1, List),
  {Value, ListWithoutValue}.

get_values(Keys, Proplist) ->
  [proplists:get_value(K, Proplist) || K <- Keys].
