-module(linkchecker_worker).
-behaviour(gen_server).

-export([start_link/0]).
-export([init/1, handle_call/3, handle_cast/2,
         handle_info/2, terminate/2, code_change/3]).


start_link() ->
  gen_server:start_link(?MODULE, [], []).

init([]) ->
  {ok, []}.

handle_call({Link, Limit}, Pid, State) ->
  send_request(Link),
  case http_uri:parse(Link) of
    {ok, {http,_, Domain, _, _, _}} ->
      {noreply, {Domain, sets:new(), [], Pid, Limit - 1, 1}};
    {ok, {https, _, _, _, _, _}} -> 
      gen_server:reply(Pid, {error, not_working_with_https}),
      {stop, normal, {error, not_working_with_https}};
    Error -> 
      gen_server:reply(Pid, {error, Error}),
      {stop, normal, [Error]}
  end;
handle_call(_Message, _From, State) ->
  {reply, invalid_command, State}.

handle_cast(_Message, State) ->
  {noreply, State}.

handle_info({response, error, Link, Reason}, {_, _, Collected, ClientPid, _, _} = State) ->
  gen_server:reply(ClientPid, {error, Reason, Collected}),
  {stop, normal, State};
handle_info({response, Link, StatusCode, Body}, 
            State = {Domain, Visited, Collected, ClientPid, Limit, WaitingsCount}) ->
  NewCollected = [{StatusCode, Link} | Collected],
  Links = get_links(Body, Domain),
  {ProcessLinksCount, NewVisited, NewLimit} = process_links(Links, Visited, Limit),
  case WaitingsCount - 1 + ProcessLinksCount of
    0 -> 
      gen_server:reply(ClientPid, {ok, NewCollected}),
      {stop, normal, State};
    NewWaitingsCount ->
      {noreply, {Domain, NewVisited, NewCollected, ClientPid, NewLimit, NewWaitingsCount}}
  end;
handle_info(_Message, State) ->
  { noreply, State }.

terminate(_Reason, _State) ->
  ok.

code_change(_OldVersion, State, _Extra) ->
  { ok, State }.

send_request(Link) ->
  gen_server:cast(request_worker, {request, Link, self()}).

check_link(Link) ->
  gen_server:cast(?MODULE, { check, Link }).

process_links(Links, Visited, Limit) ->
  process_links(Links, Visited, Limit, 0).

process_links(_, Visited, 0, WaitingsCount) ->
  {WaitingsCount, Visited, 0};
process_links([], Visited, Limit, WaitingsCount) ->
  {WaitingsCount, Visited, Limit};
process_links([Current | RestLinks], Visited, Limit, WaitingsCount) ->
  case sets:is_element(Current, Visited) of
    false ->
      send_request(Current),
      VisitedWithCurr = sets:add_element(Current, Visited),
      process_links(RestLinks, VisitedWithCurr, Limit - 1, WaitingsCount + 1);
    true ->
      process_links(RestLinks, Visited, Limit, WaitingsCount)
  end.

get_links(Body, Domain) ->
  ExtractedLinks = extract_links(Body, Domain),
  [normalize_link(Domain, Link) || Link <- ExtractedLinks].

normalize_link(Domain, ResourcePath) ->
  lists:flatten(io_lib:format("http://~s/~s", [Domain, ResourcePath])).

extract_links(Body, Domain) ->
  RegExp = lists:flatten(io_lib:format("href=\"(?:http:\/\/)?(?:www\.)?(?:~s)?\/(?<href>[^\.\"]*(\.asp|\.html|\.html|\.php|))\"", [Domain])),
  case re:run(Body, RegExp, [global, {capture, [href], list}]) of
    {match, PageLinks} ->
      [Link || [Link] <- PageLinks];
    _ ->
      []
  end.
