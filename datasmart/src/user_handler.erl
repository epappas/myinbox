%%%-------------------------------------------------------------------
%%% @author evangelosp
%%% @copyright (C) 2014, <COMPANY>
%%% @doc
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(user_handler).
-author("evangelosp").

-export([init/3]).
-export([handle/2]).
-export([terminate/3]).

init(_Transport, Req, []) ->
  {ok, Req, undefined}.

handle(Req, State) ->
  {Method, _} = cowboy_req:method(Req),
  {ok, Req3} = maybe_response(Method, Req),
  {ok, Req3, State}.

maybe_response(<<"GET">>, Req) ->
  {OUkey, Req2} = cowboy_req:qs_val(<<"user">>, Req),
  case handle_query({userinfo, OUkey}, Req2) of
    {info, UProfile, _} ->
      echo(200, jiffy:encode({[
        {profile, UProfile}
      ]}), Req2);
    _ ->
      echo(400, jiffy:encode({[
        {code, 400},
        {status, error},
        {error, "Uknown Error"}
      ]}), Req)
  end;

maybe_response(_, Req) ->
  echo(405, jiffy:encode({[
    {code, 405},
    {status, error},
    {error, "Method not allowed."}
  ]}), Req).

handle_query({userinfo, OUkey}, Req) ->
  {ok, Ukey} = user_server:match_ouKey(OUkey),
  {ok, UProfile} = user_server:getuser(Ukey),
  {info, ensureUUProfileJson(UProfile), Req}.

echo(Status, Echo, Req) ->
  cowboy_req:reply(Status, [
    {<<"content-type">>, <<"application/json; charset=utf-8">>},
    {<<"server">>, <<"myinbox-datastore">>}
  ], Echo, Req).

terminate(_Reason, _Req, _State) ->
  ok.

ensureUUProfileJson(Json) -> Json.
%%   [
%%     {user, proplists:get_value(<<"oukey">>, Json)},
%%     {name, proplists:get_value(<<"name">>, Json)},
%%     {settings, proplists:get_value(<<"settings">>, Json)},
%%     {alias, proplists:get_value(<<"alias">>, Json)}
%%   ].