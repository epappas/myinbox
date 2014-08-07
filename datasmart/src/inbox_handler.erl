%%%-------------------------------------------------------------------
%%% @author evangelosp
%%% @copyright (C) 2014, <COMPANY>
%%% @doc
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(inbox_handler).
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
  {FromBin, Req2} = cowboy_req:qs_val(<<"from">>, Req),
  {ToBin, Req2} = cowboy_req:qs_val(<<"to">>, Req),
  {ModeBin, Req2} = cowboy_req:qs_val(<<"mode">>, Req),

  OUkey = binary:bin_to_list(OUkeyBin),
  From = binary:bin_to_list(FromBin),
  To = binary:bin_to_list(ToBin), %% ds_util:timestamp() - (3600 * 24 * 1000)
  Mode = binary:bin_to_list(ModeBin),

  FunFetch = fun(Mode) ->
    case Mode of
       "bydatetime" -> handle_query(bydatetime, {OUkey, From, To}, Req2);
       "bypagination" -> handle_query(bypagination, {OUkey, From, To}, Req2)
     end
  end,

  case FunFetch(Mode) of
    {list, MList, _} ->
      echo(200, jiffy:encode({[
        {status, ok},
        {list, MList}
      ]}), Req);
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

handle_query(bydatetime, {OUkey, From, To}, Req) ->
  {ok, Ukey} = user_server:match_ouKey(OUkey),

  case message_server:msgList(date, Ukey, From, To) of
    {ok, {[]}} -> {list, [], Req};
    {ok, {InboxList}} ->
      {list, [ensureInboxItemJson(Item) || Item <- InboxList], Req}
  end;

handle_query(bypagination, {OUkey, From, To}, Req) ->
  {ok, Ukey} = user_server:match_ouKey(OUkey),

  case message_server:msgList(numeric, Ukey, From, To) of
    {ok, {[]}} -> {list, [], Req};
    {ok, {InboxList}} ->
      {list, [ensureInboxItemJson(Item) || Item <- InboxList], Req}
  end.

echo(Status, Echo, Req) ->
  cowboy_req:reply(Status, [
    {<<"content-type">>, <<"application/json; charset=utf-8">>},
    {<<"server">>, <<"myinbox-datastore">>}
  ], Echo, Req).

terminate(_Reason, _Req, _State) ->
  ok.

ensureInboxItemJson(Json) ->
  [
    {messageid, proplists:get_value(messageid, Json)},
    {from, proplists:get_value(<<"from">>, Json)},
    {date, proplists:get_value(<<"date">>, Json)},
    {subject, proplists:get_value(<<"subject">>, Json)}
  ].