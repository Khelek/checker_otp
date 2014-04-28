-module(request_worker).
-behaviour(gen_server).

-export([start_link/0]).
-export([init/1, handle_call/3, handle_cast/2,
         handle_info/2, terminate/2, code_change/3]).

start_link() ->
  gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

request(Link, Pid) ->
  gen_server:cast(?MODULE, {request, Link, Pid}).

%% FIXME maybe free_link, or link_worker
process_next_deferred_link() ->
  gen_server:cast(?MODULE, free_link).

init([]) ->
  %% TODO add application enviroment
  Limit = 100,
  {ok, {Limit, [], []}}.

handle_call(_Message, _From, State) ->
  {reply, invalid_command, State}.

%% TODO maybe records? как то зафиксировать, чтобы не зависело от порядка
handle_cast(free_link, {Limit, RequestsState, [{Link, Pid} | Tail]}) ->
  gen_server:cast(?MODULE, {request, Link, Pid}),
  {noreply, {Limit, RequestsState, Tail}};
handle_cast({request, Link, Pid}, {0, RequestsState, DeferredLinks}) ->
  DeferredLink = {Link, Pid},
  {noreply, {0, RequestsState, [DeferredLink | DeferredLinks]}};
handle_cast({request, Link, Pid}, {Limit, RequestsState, DeferredLinks}) ->
  {ibrowse_req_id, ReqId} = ibrowse:send_req(Link, [], get, [], [{stream_to, self()}]),
  RequestState = {Pid, Link, ReqId, []},
  {noreply, {Limit - 1, [RequestState | RequestsState], DeferredLinks};
handle_cast(_Message, State) ->
  {noreply, State}.

handle_info({ibrowse_async_headers, ReqId, StatusCode, _Headers}, {Limit, RequestsState, DefLinks}) ->
  NewRequestsState = process_response(ReqId, RequestsState, {status_code, StatusCode}),
  {noreply, {Limit, NewRequestsState, DefLinks}};
handle_info({ibrowse_async_response, ReqId, Body}, State) ->
  NewRequestsState = process_response(ReqId, RequestsState, {body, Body}),
  {noreply, {Limit, NewRequestsState, DefLinks}}.
  
terminate(_Reason, _State) ->
  ok.

code_change(_OldVersion, State, _Extra) ->
  {ok, State}.

process_response(ReqId, {Limit, RequestsState, DefLinks}, {Key, Value}) ->
  % FIXME плохая идея - привязываться к порядку
  {value, {Pid, Link, ReqId, ResponseChunks}, StateExceptCurrent} = lists:keytake(ReqId, 3, RequestsState),
  NewState = case ResponseChunks of
    [{status_code, StatusCode}] ->
      Pid ! {Link, StatusCode, Value};
      process_next_deferred_link(),
      {Limit + 1, StateExceptCurrent, DeferredLinks};
    [{body, Body}] ->
      Pid ! {Link, Value, Body};
      process_next_deferred_link(),
      {Limit + 1, StateExceptCurrent, DeferredLinks};
    [] ->
      {Limit, [{Pid, Link, ReqId, [{Key, Value}]} | StateExceptCurrent], DeferredLinks}
  end.
