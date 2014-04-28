-module(linkchecker_worker).
-behaviour(gen_server).

-export([start_link/0, hello/0, get_hello_count/0, check/1]).
-export([init/1, handle_call/3, handle_cast/2,
         handle_info/2, terminate/2, code_change/3]).

-import(httpc,[request/4]).

start_link() ->
  gen_server:start_link({ local, ?MODULE}, ?MODULE, [], []).


% -----------------------
check(Domain) ->
  sync:go(),
  gen_server:cast(?MODULE, { check, Domain }).

hello() ->
  gen_server:call(?MODULE, { hello }).

get_hello_count() ->
  gen_server:call(?MODULE, { hello_counter }).

init([]) ->
  {ok, { hello_counter, 0}}.

handle_call({ check, Domain }, _From, State) ->
  Responses = check_all(Domain),
  { reply, { ok, Responses }, State };
handle_call({ hello }, _From, { hello_counter, Counter }) ->
  NewState = { hello_counter, Counter + 1 },
  { reply, { ok, "Hello!" }, NewState };
handle_call({ hello_counter }, _From, State) ->
  { reply, { ok, State }, State };
handle_call(_Message, _From, State) ->
  { reply, invalid_command, State }.

handle_cast(_Message, State) ->
  { noreply, State }.

handle_info(_Message, State) ->
  { noreply, State }.

terminate(_Reason, _State) ->
  ok.

code_change(_OldVersion, State, _Extra) ->
  { ok, State }.

% LinkChecker Impl

check_all(Domain) ->
  inets:start(),
%  Domain = "nox73.ru",
  URL = normalize_link(Domain, ""),
  Set = sets:add_element(Domain, sets:new()),
  io:format("StartSet: ~p~n", [Set]),
  collect_failed_links(Set, [URL], Domain, 125, [], Pid).  % Pid - FIXME

collect_failed_links(_, _, _, 0, Responses, Pid) ->
  io:format("Responses: ~p~n", [Responses]),
  Responses;
collect_failed_links(_, [], _, _, Responses, Pid) ->
  Responses;
collect_failed_links(VisitedLinksPrev, [FirstLink | RestLinks], DomainName, Limit, Responses, Pid) ->
  timer:sleep(200),
  io:format("TestLink: ~p~n", [FirstLink]),


  case sets:is_element(FirstLink, VisitedLinksPrev) of
    true ->
      io:format("Yet exist Link: ~p~n", [FirstLink]);
    false ->
      io:format("New       Link: ~p~n", [FirstLink])
  end,

  VisitedLinks = sets:add_element(FirstLink, VisitedLinksPrev),
  case get_response(FirstLink) of
    {200, Body} ->
      Pid ! {200, FirstLink},
      io:format("Limit = ~p  |  CorrectLink: ~p~n", [Limit, FirstLink]),
      CorrectLinks = get_correct_links(Body, DomainName, VisitedLinks),
      collect_failed_links(VisitedLinks, CorrectLinks ++ RestLinks, DomainName, Limit - 1, [{200, FirstLink} | Responses], Pid);
    {Status, _Body} ->
      Pid ! {Status, FirstLink},
      io:format("Limit = ~p  |  InCorrectLink: ~p~n", [Limit, FirstLink]),
      collect_failed_links(VisitedLinks, RestLinks, DomainName, Limit - 1, [{Status, FirstLink}| Responses], Pid);
    _ ->
      void
  end.

get_response(URL) ->
  case httpc:request(get, {URL, []}, [{timeout, timer:seconds(10)}], []) of
    {ok, {{_Version, StatusCode, _}, _Headers, Body}} ->
      {StatusCode, Body};
    _ -> 
      {timeout, ok}
  end.

get_links_from_page(Body, Domain) ->
  RegExp = lists:flatten(io_lib:format("href=\"(?:http:\/\/)?(?:www\.)?(?:~s)?\/(?<href>[^\"]*)?\"", [Domain])),
  case re:run(Body, RegExp, [global, {capture, [href], list}]) of
    {match, PageLinks} ->
      [X || [X] <- PageLinks];
    _ ->
      []
  end.

get_correct_links(Body, DomainName, VisitedLinks) ->
    LinksOnPage = get_links_from_page(Body, DomainName),
    NormalizedLinks = lists:map(fun(Link) ->
                        normalize_link(DomainName, Link) end, LinksOnPage),
    CorrectedLinks = lists:filter(fun(Link) -> 
      not sets:is_element(Link, VisitedLinks) end, lists:usort(NormalizedLinks)),
    CorrectedLinks.


normalize_link(DomainName, ResourcePath) ->
  lists:flatten(io_lib:format("http://~s/~s", [DomainName, ResourcePath])).
