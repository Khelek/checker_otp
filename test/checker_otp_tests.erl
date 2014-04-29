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
  meck:new(ibrowse),
  MockFunc = fun("http://erlang.fake/", _, get, _, [{stream_to, Pid}]) -> 
                 ibrowse_response(Pid, 200, "hag<a href=\"/link1\"></a>asyhg<a href=\"erlang.fake/link2\">eoya<a href=\"/good\">oegnasohe");
                ("http://erlang.fake/link1", _, get, _, [{stream_to, Pid}]) -> 
                 ibrowse_response(Pid, 200, "hag<a href=\"/link1\"></a>asy<a href=\"http://erlang.fake/evil\"></a>hg<a href=\"http://erlang.fake\">oegna");
                ("http://erlang.fake/link2", _, get, _, [{stream_to, Pid}]) -> 
                 ibrowse_response(Pid, 200, "hag<a href=\"/very_evil\"></a>asy<a href=\"http://erlang.fake/evil\"></a>h");
                (_, _, get, _, [{stream_to, Pid}]) ->
                 ibrowse_response(Pid, 404, "404 error")
                end,
  meck:expect(ibrowse, send_req, MockFunc),
  checker_otp:check("http://erlang.fake/", 5),
  
  ResultList = catch_check_messages([]),
  ExpectedList = [{404,"http://erlang.fake/very_evil"},
                  {200,"http://erlang.fake/link2"},
                  {404,"http://erlang.fake/evil"},
                  {200,"http://erlang.fake/link1"},
                  {200,"http://erlang.fake/"}],
  ?assert(meck:validate(ibrowse)),
  meck:unload(ibrowse),
  ?assertEqual(lists:sort(ExpectedList), lists:sort(ResultList)).

catch_check_messages(ListResults) ->
  receive
    {ok, process_end} ->
      ListResults;
    {StatusCode, Link} ->
      catch_check_messages([{StatusCode, Link} | ListResults])
  end.

ibrowse_response(Pid, Status, Body) -> 
  ReqID = {Pid, random:uniform()},
  Pid ! {ibrowse_async_headers, ReqID, Status, ["headers"]},
  Pid ! {ibrowse_async_response, ReqID, Body},
  {ibrowse_req_id, ReqID}.
