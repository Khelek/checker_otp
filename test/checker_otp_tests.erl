-module(checker_otp_tests).
-include_lib("eunit/include/eunit.hrl").

checker_otp_test_() ->
  {foreach,
   fun start/0,
   fun stop/1,
   [fun check/0
   ]}.

start() ->
  checker_otp:start().

stop(_) ->
  checker_otp:stop().

check() ->
  meck:new(httpc),
  MockFunc = fun(get, {"http://erlang.fake/", _}, _, _) -> 
                  {ok, {{null, 200, null}, null, "hag<a href=\"/link1\"></a>asyhg<a href=\"erlang.fake/link2\">eoya<a href=\"/good\">oegnasohe"}};
                 (get, {"http://erlang.fake/link1", _}, _, _) -> 
                  {ok, {{null, 200, null}, null, "hag<a href=\"/link1\"></a>asy<a href=\"http://erlang.fake/evil\"></a>hg<a href=\"http://erlang.fake\">oegna"}};
                 (get, {"http://erlang.fake/link2", _}, _, _) -> 
                  {ok, {{null, 200, null}, null, "hag<a href=\"/very_evil\"></a>asy<a href=\"http://erlang.fake/evil\"></a>h"}};
                 (get, _, _, _) ->
                  {ok, {{null, 404, null}, null, "Body"}}
                 end,
  meck:expect(httpc, request, MockFunc),
  checker_otp:check("http://erlang.fake/", 5),
  
  ResultList = catch_check_messages([]),
  ExpectedList = [{404,"http://erlang.fake/very_evil"},
                  {200,"http://erlang.fake/link2"},
                  {404,"http://erlang.fake/evil"},
                  {200,"http://erlang.fake/link1"},
                  {200,"http://erlang.fake/"}],
  ?_assertEqual(lists:sort(ExpectedList), lists:sort(ResultList)),
  ?_assert(meck:validate(httpc)),

  meck:unload(httpc).

catch_check_messages(ListResults) ->
  receive
    {ok, proccess_end} ->
      ListResults;
    {StatusCode, Link} ->
      catch_check_messages([{StatusCode, Link} | ListResults])
  end.
