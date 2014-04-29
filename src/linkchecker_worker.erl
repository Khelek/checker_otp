-module(linkchecker_worker).
-behaviour(gen_server).

-export([start_link/3]).
-export([init/1, handle_call/3, handle_cast/2,
         handle_info/2, terminate/2, code_change/3]).

-import(httpc,[request/4]).

start_link(Domain, Limit, Pid) ->
  gen_server:start_link({ local, ?MODULE}, ?MODULE, [Domain, Limit, Pid], []).

% -----------------------

init([Domain, Limit, Pid]) ->
  gen_server:cast(?MODULE, { check, Domain, Limit, Pid }),
  {ok, []}.

handle_call(_Message, _From, State) ->
  { reply, invalid_command, State }.

handle_cast({ check, Domain, Limit, Pid }, State) ->
  gen_server:cast(request_worker, {request, Link, self()}),

  { noreply, State };
handle_cast(_Message, State) ->
  { noreply, State }.

handle_info({_Message}, State) ->
  check_all(Domain, Limit, Pid);
handle_info(_Message, State) ->
  { noreply, State }.

terminate(_Reason, _State) ->
  ok.

code_change(_OldVersion, State, _Extra) ->
  { ok, State }.

% LinkChecker Impl

check_all(URL, LinksCount, Pid) ->
  case http_uri:parse(URL) of
    {ok, {http,_, Domain, _, _, _}} ->
      Set = sets:new(),
%      spawn_link(link_checker, collect_links, [[URL], Set, Domain, LinksCount, self()]);
      collect_links([URL], Set, Domain, LinksCount, Pid);
    {ok, {https, _, _, _, _, _}} -> {error, not_working_with_https};
    Error -> Error
  end.

collect_links(Links, _, _, Limit, Pid) when Links == [] orelse Limit == 0 ->
  Pid ! {ok, proccess_end};
collect_links([Current | Rest], VisitedExceptCurrent, Domain, Limit, Pid) ->
  timer:sleep(200),
  case sets:is_element(Current, VisitedExceptCurrent) of
    false ->
      Visited = sets:add_element(Current, VisitedExceptCurrent),
      case request(Current) of
        {200, Body} ->
          Pid ! {200, Current},
          ToCheck = get_links(Body, Domain) ++ Rest,
          collect_links(ToCheck, Visited, Domain, Limit - 1, Pid);
        {Status, _Body} ->
          Pid ! {Status, Current}, 
          collect_links(Rest, Visited, Domain, Limit - 1, Pid)
      end;
    true ->
      collect_links(Rest, VisitedExceptCurrent, Domain, Limit, Pid)
  end.

request(URL) ->
  case httpc:request(get, {URL, []}, [{timeout, timer:seconds(10)}], []) of
    {ok, {{_, StatusCode, _}, _, Body}} ->
      {StatusCode, Body};
    _ -> 
      {timeout, URL}
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
