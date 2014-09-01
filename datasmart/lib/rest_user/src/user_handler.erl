%%%-------------------------------------------------------------------
%%% @author evangelosp
%%% @copyright (C) 2014, <COMPANY>
%%% @doc
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(user_handler).
-author("epappas").

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
    case handle_query({userinfo, OUkey}, MyReq) of
      {info, UProfile, _} ->
        echo(200, jiffy:encode({[
          {profile, {UProfile}}
        ]}), MyReq);
      _ ->
        end_with_failure(400, "No Valid Arguments", MyReq)
    end
  end,

  case {OUkeyBin, OUnameBin, OApiKeyBin} of
    {undefined, undefined, undefined} ->
      end_with_failure(400, "No Valid Arguments", Req);
    {OUkeyBin, undefined, undefined} ->
      OUkey = binary:bin_to_list(OUkeyBin),
      FetchUsrInfo(OUkey, Req2);
    {undefined, OUnameBin, undefined} ->
      OUname = binary:bin_to_list(OUnameBin),
      OPass = binary:bin_to_list(OPassBin),

      case user_server:checkuser(OUname, OPass) of
        {ok, Ukey} ->
          {ok, UProfile} = user_server:getuser(Ukey),
          %% {info, ensureUUProfileJson(UProfile), Req};
          echo(200, jiffy:encode({[
            {profile, {UProfile}}
          ]}), Req2);
        {error, _} ->
          end_with_failure(400, "No Valid Arguments", Req)
      end;
    {undefined, undefined, OApiKeyBin} ->
      OApiKey = binary:bin_to_list(OApiKeyBin),
      OSecret = binary:bin_to_list(OSecretBin),

      case user_server:checkuser(OApiKey, OSecret) of
        {ok, Ukey} ->
          {ok, UProfile} = user_server:getuser(Ukey),
          %% {info, ensureUUProfileJson(UProfile), Req};
          echo(200, jiffy:encode({[
            {profile, {UProfile}}
          ]}), Req2);
        {error, _} ->
          end_with_failure(400, "No Valid Arguments", Req)
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
      end_with_failure(400, "No Valid Arguments", Req);
    {OApiKeyBin, undefined} ->
      OApiKey = binary:bin_to_list(OApiKeyBin),
      OSecret = binary:bin_to_list(OSecretBin),
      OUkey = binary:bin_to_list(OUkeyBin),

      {ok, Ukey} = user_server:match_ouKey(OUkey),

      case user_server:add_aukey(Ukey, OApiKey, OSecret) of
        {ok, Resp} -> echo(200, jiffy:encode({Resp}), Req2);
        _ ->
          end_with_failure(400, "Uknown Error", Req)
      end;

    {undefined, EmailBin} ->
      Email = binary:bin_to_list(EmailBin),
      Password = binary:bin_to_list(PasswordBin),

      case handle_query({register, Email, Password}, Req2) of
        {ok, Resp, _} ->
          echo(200, jiffy:encode({Resp}), Req2);
        _ ->
          end_with_failure(400, "Uknown Error", Req)
      end
  end;

maybe_response(_, Req) ->
  end_with_failure(405, "Method not allowed.", Req).

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

end_with_failure(Code, Message, Req) ->
  echo(Code, jiffy:encode({[
    {code, Code},
    {status, error},
    {error, Message}
  ]}), Req).

terminate(_Reason, _Req, _State) ->
  ok.

ensureUUProfileJson(Json) -> Json.
%%   [
%%     {user, proplists:get_value(<<"oukey">>, Json)},
%%     {name, proplists:get_value(<<"name">>, Json)},
%%     {settings, proplists:get_value(<<"settings">>, Json)},
%%     {alias, proplists:get_value(<<"alias">>, Json)}
%%   ].