%%%-------------------------------------------------------------------
%%% @author evangelosp
%%% @copyright (C) 2014, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 25. Jun 2014 22:10
%%%-------------------------------------------------------------------
-module(message_server).
-author("evangelosp").

-behaviour(gen_server).

%% API
-export([start_link/0,
  store/3
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
  Now = os:timestamp(),
  IVec = uuid:to_string(uuid:uuid3(uuid:uuid4(), uuid:to_string(uuid:uuid1()))),
  Body = proplists:get_value(body, Message),
  Copydrip = string:substr(Body, 1, 20),
  Subject = proplists:get_value(subject, Message),
  From = proplists:get_value(from, Message),
  Meta = proplists:get_value(meta, Message),
  MetaJson = jiffy:encode(Meta),

  {ok, Encrypted} = user_server:encrypt(Ukey, IVec, Body),
  In64 = base64:encode(Encrypted),
  Compressed = doCompress(In64),

  qredis:q(["HSET", lists:concat(["datasmart:users:", Ukey, ":inbox:messages"]), MessageID, Compressed]),
  qredis:q(["HSET", lists:concat(["datasmart:users:", Ukey, ":inbox:meta"]), MessageID, MetaJson]),
  qredis:q(["HSET", lists:concat(["datasmart:users:", Ukey, ":inbox:subject"]), MessageID, Subject]),
  qredis:q(["HSET", lists:concat(["datasmart:users:", Ukey, ":inbox:copydrip"]), MessageID, Copydrip]),
  qredis:q(["HSET", lists:concat(["datasmart:users:", Ukey, ":inbox:sender"]), MessageID, From]),
  qredis:q(["HSET", lists:concat(["datasmart:users:", Ukey, ":inbox:ivec"]), MessageID, IVec]),
  qredis:q(["SADD", lists:concat(["datasmart:users:", Ukey, ":inbox:list"]), Now, MessageID]),

  {ok, Compressed}.

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

init([]) ->
  {ok, #state{}}.

handle_call({compress, Message}, _From, State) -> {reply, {ok, doCompress(Message)}, State};

handle_call({index, Message}, _From, State) -> {reply, {ok, doCompress(Message)}, State};

handle_call({store, Message}, _From, State) -> {reply, {ok, doCompress(Message)}, State};

handle_call(_Request, _From, State) ->
  {reply, ok, State}.

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
