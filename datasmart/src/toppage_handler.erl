%%%-------------------------------------------------------------------
%%% @author evangelosp
%%% @copyright (C) 2014, evalonlabs
%%% @doc
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(toppage_handler).
-author("evangelosp").

-export([init/3]).
-export([handle/2]).
-export([terminate/3]).

init(_Transport, Req, []) ->
  {ok, Req, undefined}.

handle(Req, State) ->
  {Method, _} = cowboy_req:method(Req),
  HasBody = cowboy_req:has_body(Req),
  {ok, Req3} = maybe_response(Method, HasBody, Req),
  {ok, Req3, State}.

maybe_response(<<"POST">>, true, Req) ->
  {CtypeBin, _} = cowboy_req:header(<<"content-type">>, Req),
  Ctype = binary:bin_to_list(CtypeBin),
  [Actual_Ctype | _] = string:tokens(Ctype, ";"),
  io:format("~p", [Actual_Ctype]),
  case handle_body(string:to_lower(Actual_Ctype), Req) of
    X -> echo(io_lib:format("{\"status\": 200, \"message\": \"nice! ~p\"}", [X]), Req)
  end;

maybe_response(<<"POST">>, false, Req) ->
  cowboy_req:reply(400, [], <<"Missing body.">>, Req);

maybe_response(_, _, Req) ->
  %% Method not allowed.
  cowboy_req:reply(405, Req).

handle_body("text/plain", Req) ->
  io:format("111111"),
  {ok, 200, "text/plain :)", Req};
handle_body("application/json", Req) ->
  io:format("222222"),
  {ok, 200, "text/plain :)", Req};
handle_body("application/x-www-form-urlencoded", Req) ->
  io:format("3333333"),
%%   {ok, PostVals, Req2} = cowboy_req:body_qs(Req),
%%   case {
%%     proplists:is_defined(<<"echo">>, PostVals),
%%     proplists:is_defined(<<"echo">>, PostVals)
%%   }
%%   of
%%     {true, true} ->
%%       Echo = proplists:get_value(<<"echo">>, PostVals),
%%       echo(Echo, Req2);
%%     _ -> cowboy_req:reply(400, [], <<"Bad body Request.">>, Req)
%%   end,
  {ok, 200, "application/x-www-form-urlencoded :)", Req};
handle_body(_, Req) ->
  {error, 400, <<"{\"status\": 400, \"error\": \"Bad body Request.\"}">>, Req}.

echo(Echo, Req) ->
  cowboy_req:reply(200, [
    {<<"content-type">>, <<"application/json; charset=utf-8">>}
  ], Echo, Req).

terminate(_Reason, _Req, _State) ->
  ok.