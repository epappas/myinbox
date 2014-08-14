%%%-------------------------------------------------------------------
%%% @author evangelosp
%%% @copyright (C) 2014, <COMPANY>
%%% @doc
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(susr_handler).
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
  {OUkeyBin, Req2} = cowboy_req:qs_val(<<"oukey">>, Req),
  {OUnameBin, Req2} = cowboy_req:qs_val(<<"uname">>, Req),
  {OPassBin, Req2} = cowboy_req:qs_val(<<"pass">>, Req),
  {OApiKeyBin, Req2} = cowboy_req:qs_val(<<"apiKey">>, Req),
  {OSecretBin, Req2} = cowboy_req:qs_val(<<"secret">>, Req),

  FetchUsrInfo = fun(OUkey, MyReq) ->
    case handle_query({susrinfo, OUkey}, MyReq) of
      {info, UProfile, _} ->
        echo(200, jiffy:encode({[
          {profile, {UProfile}}
        ]}), MyReq);
      _ ->
        echo(400, jiffy:encode({[
          {code, 400},
          {status, error},
          {error, "Uknown Error"}
        ]}), MyReq)
    end
  end,

  case {OUkeyBin, OUnameBin, OApiKeyBin} of
    {undefined, undefined, undefined} ->
      echo(400, jiffy:encode({[
        {code, 400},
        {status, error},
        {error, "No Valid Arguments"}
      ]}), Req);
    {OUkeyBin, undefined, undefined} ->
      OUkey = binary:bin_to_list(OUkeyBin),
      FetchUsrInfo(OUkey, Req2);
    {undefined, OUnameBin, undefined} ->
      OUname = binary:bin_to_list(OUnameBin),
      OPass = binary:bin_to_list(OPassBin),

      case susr_server:checksusr(OUname, OPass) of
        {ok, Ukey} ->
          {ok, UProfile} = susr_server:getsusr(Ukey),
          %% {info, ensureUUProfileJson(UProfile), Req};
          echo(200, jiffy:encode({[
            {profile, {UProfile}}
          ]}), Req2);
        {error, _} ->
          echo(400, jiffy:encode({[
            {code, 400},
            {status, error},
            {error, "No Valid Arguments"}
          ]}), Req)
      end;
    {undefined, undefined, OApiKeyBin} ->
      OApiKey = binary:bin_to_list(OApiKeyBin),
      OSecret = binary:bin_to_list(OSecretBin),

      case susr_server:checksusr(OApiKey, OSecret) of
        {ok, Ukey} ->
          {ok, UProfile} = susr_server:getsusr(Ukey),
          %% {info, ensureUUProfileJson(UProfile), Req};
          echo(200, jiffy:encode({[
            {profile, {UProfile}}
          ]}), Req2);
        {error, _} ->
          echo(400, jiffy:encode({[
            {code, 400},
            {status, error},
            {error, "No Valid Arguments"}
          ]}), Req)
      end
  end;

maybe_response(<<"POST">>, Req) ->
  {ok, Params, Req2} = cowboy_req:body_qs(Req),

  OApiKeyBin = proplists:get_value(<<"apiKey">>, Params),
  OSecretBin = proplists:get_value(<<"secret">>, Params),
  OUkeyBin = proplists:get_value(<<"oukey">>, Params),
  EmailBin = proplists:get_value(<<"email">>, Params),
  PasswordBin = proplists:get_value(<<"password">>, Params),

  case {OApiKeyBin, EmailBin} of
    {undefined, undefined} ->
      echo(400, jiffy:encode({[
        {code, 400},
        {status, error},
        {error, "No Valid Arguments"}
      ]}), Req);
    {OApiKeyBin, undefined} ->
      OApiKey = binary:bin_to_list(OApiKeyBin),
      OSecret = binary:bin_to_list(OSecretBin),
      OUkey = binary:bin_to_list(OUkeyBin),

      {ok, Ukey} = susr_server:match_ouKey(OUkey),

      case susr_server:add_aukey(Ukey, OApiKey, OSecret) of
        {ok, Resp} -> echo(200, jiffy:encode({Resp}), Req2);
        _ ->
          echo(400, jiffy:encode({[
            {code, 400},
            {status, error},
            {error, "Uknown Error"}
          ]}), Req)
      end;

    {undefined, EmailBin} ->
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
      end
  end;

maybe_response(_, Req) ->
  echo(405, jiffy:encode({[
    {code, 405},
    {status, error},
    {error, "Method not allowed."}
  ]}), Req).

handle_query({susrinfo, OUkey}, Req) ->
  {ok, Ukey} = susr_server:match_ouKey(OUkey),
  {ok, UProfile} = susr_server:getsusr(Ukey),
  {info, ensureUUProfileJson(UProfile), Req};

handle_query({register, Email, Password}, Req) ->
  {ok, Registred} = susr_server:register(Email, Password),
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