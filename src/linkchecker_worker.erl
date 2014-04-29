-module(linkchecker_worker).
-behaviour(gen_server).

-export([start_link/3]).
-export([init/1, handle_call/3, handle_cast/2,
         handle_info/2, terminate/2, code_change/3]).


start_link(Link, Limit, Pid) ->
  gen_server:start_link({local, ?MODULE}, ?MODULE, [Link, Limit, Pid], []).

%%TODO обрабатывать EXIT request_worker'a, или сделать в нем обработку ошибок

init([Link, Limit, Pid]) ->
  cast_request_worker(Link),
  case http_uri:parse(Link) of
    {ok, {http,_, Domain, _, _, _}} ->
      {ok, {Domain, [], sets:new(), Pid, Limit - 1, 1}};
    {ok, {https, _, _, _, _, _}} -> 
      Pid ! {error, not_working_with_https},
      {stop, normal, {error, not_working_with_https}};
    Error -> 
      Pid ! Error,
      {stop, normal, [Error]}
  end.

handle_call(_Message, _From, State) ->
  { reply, invalid_command, State }.

handle_cast(_Message, State) ->
  { noreply, State }.

%% FIXME может вместо большого tupla лучше подойдут рекорды
handle_info({response, Link, StatusCode, Body}, State = {Domain, ProcessedLinks, Visited, ClientPid, Limit, RequestsCount}) ->
  ClientPid ! {StatusCode, Link},
  Links = get_links(Body, Domain),
  {ProcessLinksCount, NewVisited, NewLimit} = process_links(Links, Visited, Limit),
  case RequestsCount - 1 + ProcessLinksCount of
    0 -> 
      ClientPid ! {ok, process_end},
      {stop, normal, State};
    NewRequestsCount ->
      { noreply, {Domain, [{StatusCode, Link} | ProcessedLinks], NewVisited, ClientPid, NewLimit, NewRequestsCount} }
  end;
handle_info(_Message, State) ->
  { noreply, State }.

terminate(_Reason, _State) ->
  ok.

code_change(_OldVersion, State, _Extra) ->
  { ok, State }.

cast_request_worker(Link) ->
  gen_server:cast(request_worker, {request, Link, self()}).

check_link(Link) ->
  gen_server:cast(?MODULE, { check, Link }).

process_links(Links, Visited, Limit) ->
  process_links(Links, Visited, Limit, 0).

process_links(_, Visited, 0, RequestsCount) ->
  {RequestsCount, Visited, 0};
process_links([], Visited, Limit, RequestsCount) ->
  {RequestsCount, Visited, Limit};
process_links([Current | RestLinks], Visited, Limit, RequestsCount) ->
  case sets:is_element(Current, Visited) of
    false ->
      cast_request_worker(Current),
      process_links(RestLinks, sets:add_element(Current, Visited), Limit - 1, RequestsCount + 1);
    true ->
      process_links(RestLinks, Visited, Limit, RequestsCount)
  end.

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
