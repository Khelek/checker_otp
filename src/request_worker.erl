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

handle_cast({ request, Link, Pid }, {Links, Pids, Statuses}) ->
  {ibrowse_req_id, ReqID} = ibrowse:send_req(Link, [], post, [], [{stream_to, self()}]),
  erlang:display("ibowse request"),
  { noreply, {[{ReqID, Link} | Links], [{ReqID, Pid} | Pids], Statuses} };
handle_cast(_Message, State) ->
  { noreply, State }.

handle_info({ibrowse_async_headers, ReqID, StatusCode, _Headers}, {Links, Pids, StatusCodes}) ->
  erlang:display(StatusCode),
  { noreply, {Links, Pids, [{ReqID, StatusCode} | StatusCodes]} };
handle_info({ibrowse_async_response, ReqID, Body}, {Links, Pids, StatusCodes}) ->
  
  [ElLink || ElLink = {ReqID, _Link} <- Links],
  [ElStatus || ElStatus = {ReqID, StatusCode} <- StatusCodes],
  [ElPid || ElPid = {ReqID, _FromPid} <- Pids],



  linkchecker ! {response, Link, StatusCode, Body}.


  { noreply, State }.
  
terminate(_Reason, _State) ->
  ok.

code_change(_OldVersion, State, _Extra) ->
  { ok, State }.
