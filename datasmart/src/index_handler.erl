%%%-------------------------------------------------------------------
%%% @author evangelosp
%%% @copyright (C) 2014, evalonlabs
%%% @doc
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(index_handler).
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
  CType = binary:bin_to_list(CtypeBin),
  [Actual_CType | _] = string:tokens(CType, ";"),
  case handle_body(string:to_lower(Actual_CType), Req) of
    {ok, GivenJson, _} ->
      Json = ensureJson(GivenJson),
      echo(200, jiffy:encode({[
        {status, ok},
        {ukey, proplists:get_value(ukey, Json, 0)},
        {messageid, proplists:get_value(messageid, Json, 0)}
      ]}), Req);
    {error, Error, _} ->
      echo(400, jiffy:encode({[
        {code, 400},
        {status, error},
        {error, Error}
      ]}), Req);
    _ ->
      echo(400, jiffy:encode({[
        {code, 400},
        {status, error},
        {error, "Uknown Error"}
      ]}), Req)
  end;

maybe_response(<<"POST">>, false, Req) ->
  echo(400, jiffy:encode({[
    {code, 400},
    {status, error},
    {error, "Missing body."}
  ]}), Req);

maybe_response(_, _, Req) ->
  echo(405, jiffy:encode({[
    {code, 405},
    {status, error},
    {error, "Method not allowed."}
  ]}), Req).

handle_body("text/plain", Req) ->
  {ok, Body, Req2} = cowboy_req:body(Req),
  try jiffy:decode(Body) of
    {Json} -> {ok, [{binary_to_atom(Key, utf8), Val} || {Key, Val} <- Json], Req2}
  catch
    _ -> {error, "Bad body Request", Req2}
  end;

handle_body("application/json", Req) ->
  {ok, Body, Req2} = cowboy_req:body(Req),
  try jiffy:decode(Body) of
    {Json} -> {ok, [{binary_to_atom(Key, utf8), Val} || {Key, Val} <- Json], Req2}
  catch
    _ -> {error, "Bad body Request", Req2}
  end;

handle_body("application/x-www-form-urlencoded", Req) ->
  {ok, PostVals, _} = cowboy_req:body_qs(Req),
  case {
    proplists:is_defined(<<"from">>, PostVals),
    proplists:is_defined(<<"to">>, PostVals),
    proplists:is_defined(<<"ukey">>, PostVals),
%%     proplists:is_defined(<<"messageid">>, PostVals),
    proplists:is_defined(<<"date">>, PostVals),
    proplists:is_defined(<<"subject">>, PostVals),
    proplists:is_defined(<<"body">>, PostVals)
  }
  of
    {true, true} ->
      {ok, [
%%         {messageid, proplists:get_value(<<"messageid">>, PostVals)},
        {from, proplists:get_value(<<"from">>, PostVals)},
        {to, proplists:get_value(<<"to">>, PostVals)},
        {ukey, proplists:get_value(<<"ukey">>, PostVals)},
        {date, proplists:get_value(<<"date">>, PostVals)},
        {subject, proplists:get_value(<<"subject">>, PostVals)},
        {body, proplists:get_value(<<"body">>, PostVals)},
        {meta, proplists:get_value(<<"meta">>, PostVals, [])}
      ], Req};
    _ -> {error, "Bad Query parameters", Req}
  end;

handle_body(_, Req) ->
  {error, "Bad body Request", Req}.

echo(Status, Echo, Req) ->
  cowboy_req:reply(Status, [
    {<<"content-type">>, <<"application/json; charset=utf-8">>},
    {<<"server">>, <<"myinbox-datastore">>}
  ], Echo, Req).

terminate(_Reason, _Req, _State) ->
  ok.

ensureJson(Json) ->
  [
    {messageid, proplists:get_value(<<"messageid">>, Json, uuid:to_string(uuid:uuid3(uuid:uuid4(), uuid:to_string(uuid:uuid1()))))},
    {from, proplists:get_value(<<"from">>, Json)},
    {to, proplists:get_value(<<"to">>, Json)},
    {ukey, proplists:get_value(<<"ukey">>, Json)},
    {date, proplists:get_value(<<"date">>, Json)},
    {subject, proplists:get_value(<<"subject">>, Json)},
    {body, proplists:get_value(<<"body">>, Json)},
    {meta, proplists:get_value(<<"meta">>, Json, [])}
  ].