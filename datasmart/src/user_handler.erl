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
  {OUkeyBin, Req2} = cowboy_req:qs_val(<<"user">>, Req),

  OUkey = binary:bin_to_list(OUkeyBin),

  case handle_query({userinfo, OUkey}, Req2) of
    {info, UProfile, _} ->
      io:format("Profile: ~p", [UProfile]),
      echo(200, jiffy:encode({[
        {profile, {UProfile}}
      ]}), Req2);
    _ ->
      echo(400, jiffy:encode({[
        {code, 400},
        {status, error},
        {error, "Uknown Error"}
      ]}), Req)
  end;

maybe_response(<<"POST">>, Req) ->
  {ok, Params, Req2} = cowboy_req:body_qs(Req),

  EmailBin = proplists:get_value(<<"email">>, Params),
  PasswordBin = proplists:get_value(<<"password">>, Params),

  Email = binary:bin_to_list(EmailBin),
  Password = binary:bin_to_list(PasswordBin),
  case handle_query({register, Email, Password}, Req2) of
    {ok, Resp, _} ->
      echo(200, jiffy:encode({Resp}), Req2);
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
  {info, ensureUUProfileJson(UProfile), Req};

handle_query({register, Email, Password}, Req) ->
  {ok, Registred} = user_server:register(Email, Password),
  {ok, Registred, Req}.

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