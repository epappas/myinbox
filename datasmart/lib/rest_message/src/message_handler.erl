%%%-------------------------------------------------------------------
%%% @author evangelosp
%%% @copyright (C) 2014, <COMPANY>
%%% @doc
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(message_handler).
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
  {MessageidBin, Req2} = cowboy_req:qs_val(<<"messageid">>, Req),
  {OUkeyBin, Req3} = cowboy_req:qs_val(<<"user">>, Req2),

  Messageid = binary:bin_to_list(MessageidBin),
  OUkey = binary:bin_to_list(OUkeyBin),

  case handle_query({msginfo, OUkey, Messageid}, true, Req3) of
    {message, Message, _} ->
      echo(200, jiffy:encode(Message), Req);
    _ ->
      echo(400, jiffy:encode({[
        {code, 400},
        {status, error},
        {error, "Uknown Error"}
      ]}), Req)
  end;

maybe_response(<<"POST">>, Req) ->
  {ok, Params, Req2} = cowboy_req:body_qs(Req),

  OUkeyBin = proplists:get_value(<<"user">>, Params),
  Messageid = case proplists:get_value(<<"messageid">>, Params) of
                undefined -> uuid:to_string(uuid:uuid3(uuid:uuid4(), uuid:to_string(uuid:uuid1())));
                MessageidBin -> binary:bin_to_list(MessageidBin)
              end,
  OUkey = binary:bin_to_list(OUkeyBin),

  case handle_query({msgstore, OUkey, Messageid, Params}, true, Req2) of
    {message, Messageid, _} ->
      echo(200, jiffy:encode({[{messageId, list_to_binary(Messageid)}]}), Req);
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

handle_query({msginfo, OUkey, Messageid}, _, Req) ->
  {ok, Ukey} = user_server:match_ouKey(OUkey),
  {ok, MessageJson} = message_server:msginfo(Messageid, Ukey),
  {message, ensureMessageJson(MessageJson), Req};

handle_query({msgstore, OUkey, Messageid, Params}, _, Req) ->
  {ok, Ukey} = user_server:match_ouKey(OUkey),

  message_server:store(Messageid, Ukey, [
    {body, binary:bin_to_list(proplists:get_value(<<"body">>, Params, <<"">>))},
    {subject, binary:bin_to_list(proplists:get_value(<<"subject">>, Params, <<"">>))},
    {from, binary:bin_to_list(proplists:get_value(<<"from">>, Params, <<"">>))},
    {meta, binary:bin_to_list(proplists:get_value(<<"meta">>, Params, <<"[]">>))}
  ]),

  {message, Messageid, Req};

handle_query(_, _, Req) ->
  {error, "Bad body Request", Req}.

echo(Status, Echo, Req) ->
  cowboy_req:reply(Status, [
    {<<"content-type">>, <<"application/json; charset=utf-8">>},
    {<<"server">>, <<"myinbox-datastore">>}
  ], Echo, Req).

terminate(_Reason, _Req, _State) ->
  ok.

ensureMessageJson({Json}) ->
  {[
    {messageid, proplists:get_value(messageid, Json)},
    {from, proplists:get_value(from, Json)},
    {to, proplists:get_value(to, Json)},
    {date, proplists:get_value(date, Json)},
    {subject, proplists:get_value(subject, Json)},
    {body, proplists:get_value(body, Json, "")},
    {meta, proplists:get_value(meta, Json, [])}
  ]}.
