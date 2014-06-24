%%%-------------------------------------------------------------------
%%% @author evangelosp
%%% @copyright (C) 2014, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 23. Jun 2014 21:59
%%%-------------------------------------------------------------------
-module(user_server).
-author("evangelosp").

-behaviour(gen_server).

%% API
-export([start_link/0,
  register/2,
  getukey/1,
  getuser/1,
  checkuser/2
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

register(Email, Password) -> gen_server:call(?MODULE, {register, Email, Password}).

getukey(Email) -> gen_server:call(?MODULE, {getukey, Email}).

getuser(Ukey) -> gen_server:call(?MODULE, {getuser, Ukey}).

checkuser(Email, Password) -> gen_server:call(?MODULE, {checkuser, Email, Password}).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

init([]) ->
  {ok, #state{}}.

handle_call({register, Email, Password}, _From, State) ->
  MD5Key = binary_to_list(erlang:md5(Email)),
  Secret = uuid:to_string(uuid:uuid3(uuid:uuid4(), uuid:to_string(uuid:uuid1()))),
  Salt = uuid:to_string(uuid:uuid3(uuid:uuid4(), uuid:to_string(uuid:uuid1()))),
  case qredis:q(["GET", lists:concat(["datasmart:users:", MD5Key, ":profile"])]) of
    {ok, undefined} ->
      qredis:q(["SET", lists:concat(["datasmart:users:", MD5Key, ":profile"]), jiffy:encode({[
        {email, Email},
        {ukey, MD5Key},
        {password, hashPass(Password, Salt, 20)}
      ]})]),
      qredis:q(["SET", lists:concat(["datasmart:users:", MD5Key, ":salt"]), Salt]),
      qredis:q(["SET", lists:concat(["datasmart:users:", MD5Key, ":secret"]), Secret]),
      qredis:q(["HSET", "datasmart:alias", Email, Email]),
      qredis:q(["HSET", "datasmart:alias:keys", Email, MD5Key]),
      {reply, {ok, [ {email, Email}, {ukey, MD5Key} ]}, State};
    _ -> {reply, {error, "Registration Failure"}, State}
  end;

handle_call({getukey, Email}, _From, State) ->
  case qredis:q(["HGET", "datasmart:alias:keys", Email]) of
    {ok, Ukey} -> {ok, binary_to_list(Ukey)};
    _ -> {reply, {error, "Registration Failure"}, State}
  end;

handle_call({getuser, Ukey}, _From, State) ->
  case qredis:q(["GET", lists:concat(["datasmart:users:", Ukey, ":profile"])]) of
    {ok, Result} ->
      {Json} = jiffy:decode(Result),
      User = [{binary_to_atom(Key, utf8), Val} || {Key, Val} <- Json],
      {ok, User};
    _ -> {reply, {error, "No User was Found"}, State}
  end;

handle_call({checkuser, Email, Password}, _From, State) ->
  case gen_server:call(?MODULE, {getukey, Email}) of
    {ok, Ukey} ->
      case qredis:q(["GET", lists:concat(["datasmart:users:", Ukey, ":salt"])]) of
        {ok, BSalt} ->
          Salt = binary_to_list(BSalt),
          HashPass = hashPass(Password, Salt, 20),
          {ok, User} = gen_server:call(?MODULE, {getuser, Ukey}),
          UserPass = proplists:get_value(password, User),
          case UserPass =:= HashPass of
            true -> {ok, Ukey};
            false -> {error, false}
          end
      end;
    _ -> {reply, {error, "Wrong User Details"}, State}
  end.

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

hashPass(Password, Salt, 0) ->
  erlang:md5( lists:concat([Password, Salt]) );

hashPass(Password, Salt, Factor) when (Factor rem 2) > 0 ->
  hashPass(erlang:md5( lists:concat([Password, Salt]) ), Salt, Factor - 1 );

hashPass(Password, Salt, Factor) ->
  hashPass(erlang:md5( lists:concat([Salt, Password]) ), Salt, Factor - 1 ).