-module(linkchecker_worker).
-behaviour(gen_server).

-export([start_link/3]).
-export([init/1, handle_call/3, handle_cast/2,
         handle_info/2, terminate/2, code_change/3]).


start_link(Link, Limit, Pid) ->
  gen_server:start_link({local, ?MODULE}, ?MODULE, [Link, Limit, Pid], []).

%%TODO обрабатывать EXIT request_worker'a, или сделать в нем обработку ошибок

%% TODO link is ONLY domain. сделать, чтобы это было не так, http_uri:parse
init([Link, Limit, Pid]) ->
  gen_server:cast(?MODULE, { check, Link }),
  case http_uri:parse(Link) of
    {ok, {http,_, Domain, _, _, _}} ->
      {ok, {Domain, [], sets:new(), Pid, Limit, 0}};
    {ok, {https, _, _, _, _, _}} -> 
      Pid ! {error, not_working_with_https},
      {stop, normal, {error, not_working_with_https}};
    Error -> 
      Pid ! Error,
      {stop, normal, [Error]}
  end.

handle_call(_Message, _From, State) ->
  { reply, invalid_command, State }.

handle_cast({ check, Link }, {Domain, ProcessedLinks, Visited, ClientPid, Limit, RequestCount}) ->
  erlang:display(["to request worker", Link]),
  cast_request_worker(Link),
  { noreply, {Domain, ProcessedLinks, Visited, ClientPid, Limit, RequestCount + 1}};
handle_cast(_Message, State) ->
  { noreply, State }.

%% FIXME может вместо большого tupla лучше подойдут рекорды
handle_info({response, Link, StatusCode, Body}, State = {Domain, ProcessedLinks, Visited, ClientPid, Limit, RequestCount}) ->
  erlang:display(["response from request worker", Link, StatusCode, Body]),
  Links = get_links(Body, Domain),
  ClientPid ! {StatusCode, Link},
  {ProcessLinks, NewVisited} = process_links(Links, Visited),
  case {ProcessLinks, RequestCount - 1} of
    {[], 0} -> 
      erlang:display(["end", Link]),
      ClientPid ! {ok, process_end},
      {stop, normal, State};
    {ProcessLinks, NewRequestCount} ->
      erlang:display(["noreply", Link]),
      { noreply, {Domain, [{StatusCode, Link} | ProcessedLinks], NewVisited, ClientPid, Limit, NewRequestCount} }
  end;
handle_info(_Message, State) ->
  { noreply, State }.

terminate(_Reason, _State) ->
  ok.

code_change(_OldVersion, State, _Extra) ->
  { ok, State }.

check_link(Link) ->
  gen_server:cast(?MODULE, { check, Link }).

cast_request_worker(Link) ->
  gen_server:cast(request_worker, {request, Link, self()}).

process_links(Links, Visited) ->
  process_links(Links, Visited, []).

process_links([], Visited, ProcessLinks) ->
  {ProcessLinks, Visited};
process_links([Current | RestLinks], Visited, ProcessLinks) ->
  {NewProcessLinks, NewVisited} = case sets:is_element(Current, Visited) of
    false ->
      check_link(Current),
      {[Current | ProcessLinks], sets:add_element(Current, Visited)};
    true ->
      {ProcessLinks, Visited}
  end,
  process_links(RestLinks, NewVisited, NewProcessLinks).

get_links(Body, Domain) ->
  ExtractedLinks = extract_links(Body, Domain),
  [normalize_link(Domain, Link) || Link <- ExtractedLinks].

normalize_link(Domain, ResourcePath) ->
  lists:flatten(io_lib:format("http://~s/~s", [Domain, ResourcePath])).

extract_links(Body, Domain) ->
  RegExp = lists:flatten(io_lib:format("href=\"(?:http:\/\/)?(?:www\.)?(?:~s)?\/(?<href>[^\"]*)?\"", [Domain])),
  case re:run(Body, RegExp, [global, {capture, [href], list}]) of
    {match, PageLinks} ->
      [Link || [Link] <- PageLinks];
    _ ->
      []
  end.
