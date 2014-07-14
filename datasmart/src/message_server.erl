%%%-------------------------------------------------------------------
%%% @author evangelosp
%%% @copyright (C) 2014, evalonlabs
%%% @doc
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(message_server).
-author("evangelosp").

-behaviour(gen_server).

%% API
-export([start_link/0,
  store/3,
  index/3,
  msginfo/2,
  msgList/4
]).

%% gen_server callbacks
-export([init/1,
  handle_call/3,
  handle_cast/2,
  handle_info/2,
  terminate/2,
  code_change/3]).

-define(SERVER, ?MODULE).

-record(state, {}).

%%%===================================================================
%%% API
%%%===================================================================

start_link() ->
  gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

store(MessageID, Ukey, Message) ->
  gen_server:cast(?MODULE, {store, MessageID, Ukey, Message}),
  {ok, MessageID}.

index(MessageID, Ukey, Message) ->
  gen_server:call(?MODULE, {index, MessageID, Ukey, Message}),
  {ok, MessageID}.

msginfo(MessageID, Ukey) ->
  {ok, gen_server:call(?MODULE, {msginfo, MessageID, Ukey})}.

msgList(date, Ukey, FromTS, ToTS) ->
  {ok, gen_server:call(?MODULE, {listbydate, Ukey, FromTS, ToTS})};

msgList(numeric, Ukey, FromN, ToN) ->
  {ok, gen_server:call(?MODULE, {listbynumeric, Ukey, FromN, ToN})}.

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

init([]) ->
  {ok, #state{}}.

handle_call({index, MessageID, Ukey, Message}, _From, State) ->
  {reply, {ok, doIndex(MessageID, Ukey, Message)}, State};

handle_call({msginfo, MessageID, Ukey}, _From, State) ->
  {reply, {ok, fetchMsginfo(MessageID, Ukey)}, State};

handle_call({listbydate, Ukey, FromTS, ToTS}, _From, State) ->
  {reply, {ok, fetchMsgList(listbydate, Ukey, FromTS, ToTS)}, State};

handle_call(_Request, _From, State) ->
  {reply, ok, State}.

handle_cast({store, MessageID, Ukey, Message}, State) ->
  Now = ds_util:timestamp(),
  IVec = uuid:to_string(uuid:uuid3(uuid:uuid4(), uuid:to_string(uuid:uuid1()))),
  Body = proplists:get_value(body, Message),
  Copydrip = string:substr(Body, 1, 20),
  Subject = proplists:get_value(subject, Message),
  From = proplists:get_value(from, Message),
  Meta = proplists:get_value(meta, Message),
  MetaJson = jiffy:encode(Meta),

  {ok, Encrypted} = user_server:encrypt(Ukey, IVec, Body),
  In64 = base64:encode_to_string(Encrypted),
  Compressed = doCompress(In64),

  qredis:q(["HSET", lists:concat(["datasmart:users:", Ukey, ":inbox:messages"]), MessageID, Compressed]),
  qredis:q(["HSET", lists:concat(["datasmart:users:", Ukey, ":inbox:meta"]), MessageID, MetaJson]),
  qredis:q(["HSET", lists:concat(["datasmart:users:", Ukey, ":inbox:subject"]), MessageID, Subject]),
  qredis:q(["HSET", lists:concat(["datasmart:users:", Ukey, ":inbox:copydrip"]), MessageID, Copydrip]),
  qredis:q(["HSET", lists:concat(["datasmart:users:", Ukey, ":inbox:sender"]), MessageID, From]),
  qredis:q(["HSET", lists:concat(["datasmart:users:", Ukey, ":inbox:ivec"]), MessageID, IVec]),
  qredis:q(["SADD", lists:concat(["datasmart:users:", Ukey, ":inbox:list"]), Now, MessageID]),

  {noreply, State};

handle_cast(_Request, State) ->
  {noreply, State}.

handle_info(_Info, State) ->
  {noreply, State}.

terminate(_Reason, _State) ->
  ok.

code_change(_OldVsn, State, _Extra) ->
  {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================

doCompress(Text) -> zlib:gzip(Text).

doUncompress(Data) -> zlib:gunzip(Data).

doIndex(MessageID, Ukey, Message) ->
%%   TODO Index meesage to ES or Other search engine
%%   TODO Break message to Ngram of 1-5
  {[]}.

fetchMsgList(listbydate, Ukey, FromTS, ToTS) ->
  {ok, Result} = qredis:q(["ZREVRANGEBYSCORE", lists:concat(["datasmart:users:", Ukey, ":inbox:list"]), FromTS, ToTS]),
  KeyValList = ds_util:list_to_keyval_rev(Result),
  FoldedList = lists:foldl(fun({Timestamp, MessageID}, AccIn) ->
    {ok, Subject} = qredis:q(["HGET", lists:concat(["datasmart:users:", Ukey, ":inbox:subject"]), MessageID]),
    {ok, From} = qredis:q(["HGET", lists:concat(["datasmart:users:", Ukey, ":inbox:sender"]), MessageID]),
    {ok, Copydrip} = qredis:q(["HGET", lists:concat(["datasmart:users:", Ukey, ":inbox:copydrip"]), MessageID]),
    AccIn ++ [
      {datetime, Timestamp},
      {messageID, MessageID},
      {subject, Subject},
      {from, From},
      {copydrip, Copydrip}
    ]
  end, [], KeyValList),
  {FoldedList}.

fetchMsginfo(MessageID, Ukey) ->
  {ok, Compressed} = qredis:q(["HGET", lists:concat(["datasmart:users:", Ukey, ":inbox:messages"]), MessageID]),
  {ok, MetaJson} = qredis:q(["HGET", lists:concat(["datasmart:users:", Ukey, ":inbox:meta"]), MessageID]),
  {ok, Subject} = qredis:q(["HGET", lists:concat(["datasmart:users:", Ukey, ":inbox:subject"]), MessageID]),
  {ok, From} = qredis:q(["HGET", lists:concat(["datasmart:users:", Ukey, ":inbox:sender"]), MessageID]),
  {ok, IVec} = qredis:q(["HGET", lists:concat(["datasmart:users:", Ukey, ":inbox:ivec"]), MessageID]),
  {ok, Email} = qredis:q(["HGET", lists:concat(["datasmart:users:", Ukey, ":profile"]), "email"]),
  {ok, Timestamp} = qredis:q(["ZSCORE", lists:concat(["datasmart:users:", Ukey, ":inbox:list"]), MessageID]),

%% TODO Fetch Date from the List
  Decompressed = doUncompress(Compressed),
  Encrypted = base64:decode_to_string(Decompressed),
  {ok, Dencrypted} = user_server:dencrypt(Ukey, IVec, Encrypted),

  {[
    {messageid, MessageID},
    {from, From},
    {to, Email},
    {date, Timestamp},
    {subject, Subject},
    {body, Dencrypted},
    {meta, jiffy:decode(MetaJson)}
  ]}.