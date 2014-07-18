%%%-------------------------------------------------------------------
%%% @author evangelosp
%%% @copyright (C) 2014, evalonlabs
%%% @doc
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(user_server).
-author("evangelosp").

-behaviour(gen_server).

%% API
-export([start_link/0,
  register/2,
  getukey/1,
  match_ouKey/1,
  match_auKey/1,
  getuser/1,
  updateprofile/2,
  encrypt/3,
  dencrypt/3,
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

match_ouKey(OUkey) -> gen_server:call(?MODULE, {match_ouKey, OUkey}).

match_auKey(AUkey) -> gen_server:call(?MODULE, {match_auKey, AUkey}).

getuser(Ukey) -> gen_server:call(?MODULE, {getuser, Ukey}).

updateprofile(Ukey, KeyValList) -> gen_server:call(?MODULE, {updateprofile, Ukey, KeyValList}).

encrypt(Ukey, IVec, Text) ->
  {ok, SecretBin} = qredis:q(["GET", lists:concat(["datasmart:users:", Ukey, ":secret"])]),
  SecretList = binary_to_list(SecretBin),
  Secret = lists:sublist(SecretList, 8),
  IvacBin = list_to_binary(case is_binary(IVec) of
    false -> lists:sublist(IVec, 8);
    true -> lists:sublist(binary_to_list(IVec), 8)
  end),
  TextChecked = case length(Text) rem 8 of
    0 -> Text;
    N -> Text ++ [" " || _S <- lists:seq(1, 8 - N)]
  end,
  {ok, crypto:des_cbc_encrypt(Secret, IvacBin, TextChecked)}.

dencrypt(Ukey, IVec, Text) ->
  {ok, SecretBin} = qredis:q(["GET", lists:concat(["datasmart:users:", Ukey, ":secret"])]),
  SecretList = binary_to_list(SecretBin),
  Secret = lists:sublist(SecretList, 8),
  IvacBin = list_to_binary(case is_binary(IVec) of
                             false -> lists:sublist(IVec, 8);
                             true -> lists:sublist(binary_to_list(IVec), 8)
                           end),
  {ok, crypto:des_cbc_decrypt(Secret, IvacBin, Text)}.

checkuser(Email, Password) -> gen_server:call(?MODULE, {checkuser, Email, Password}).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

init([]) ->
  {ok, #state{}}.

handle_call({register, Email, Password}, _From, State) ->
  MD5Key = hash_md5:build(Email),
  OpenSalt = uuid:to_string(uuid:uuid3(uuid:uuid4(), uuid:to_string(uuid:uuid1()))),
  OUKey = hashPass(MD5Key, OpenSalt, 2),
  Secret = uuid:to_string(uuid:uuid3(uuid:uuid4(), uuid:to_string(uuid:uuid1()))),
  Salt = uuid:to_string(uuid:uuid3(uuid:uuid4(), uuid:to_string(uuid:uuid1()))),
  case qredis:q(["HGETALL", lists:concat(["datasmart:users:", MD5Key, ":profile"])]) of
    {ok, []} ->
      qredis:q(["HSET", lists:concat(["datasmart:users:", MD5Key, ":profile"]), "email", Email]),
      qredis:q(["SET", lists:concat(["datasmart:openkey:", OUKey]), MD5Key]),
      qredis:q(["SET", lists:concat(["datasmart:users:", MD5Key, ":password"]), hashPass(Password, Salt, 20)]),
      qredis:q(["SET", lists:concat(["datasmart:users:", MD5Key, ":salt"]), Salt]),
      qredis:q(["SET", lists:concat(["datasmart:users:", MD5Key, ":secret"]), Secret]),
      qredis:q(["HSETNX", "datasmart:alias", Email, Email]),
      qredis:q(["HSETNX", "datasmart:alias:keys", Email, MD5Key]),
      {reply, {ok, [{email, list_to_binary(Email)}, {oukey, list_to_binary(OUKey)}]}, State};
    _ -> {reply, {error, "Registration Failure, User Exists"}, State}
  end;

handle_call({getukey, Email}, _From, State) ->
  {reply, doGetukey(Email), State};

handle_call({match_ouKey, OUkey}, _From, State) ->
  {reply, doMatchOUKey(OUkey), State};

handle_call({match_auKey, AUkey}, _From, State) ->
  {reply, doMatchAUKey(AUkey), State};

handle_call({getuser, Ukey}, _From, State) ->
  {reply, doGetUser(Ukey), State};

handle_call({updateprofile, Ukey, KeyValList}, _From, State) ->
  {reply, doUpdateProfile(Ukey, KeyValList), State};

handle_call({checkuser, Email, Password}, _From, State) ->
  {reply, doCheckuser(Email, Password), State}.

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

doCheckuser(Email, Password) ->
  case doGetukey(Email) of
    {ok, Ukey} ->
      case qredis:q(["GET", lists:concat(["datasmart:users:", Ukey, ":salt"])]) of
        {ok, undefined} -> {error, "Wrong User Details"};
        {ok, BSalt} ->
          Salt = binary_to_list(BSalt),
          HashPass = hashPass(Password, Salt, 20),
          {ok, User} = doGetUser(Ukey),
          UserPass = proplists:get_value(password, User),
          case UserPass =:= HashPass of
            true -> {ok, Ukey};
            false -> {error, false}
          end;
        _ -> {error, "Something Went wrong getting User's Details"}
      end;
    _ -> {error, "Wrong User Details"}
  end.

doGetukey(Email) ->
  case qredis:q(["HGET", "datasmart:alias:keys", Email]) of
    {ok, undefined} -> {error, "Uknown Email"};
    {ok, Ukey} -> {ok, binary_to_list(Ukey)};
    _ -> {error, "Uknown Email"}
  end.

doMatchOUKey(OUKey) ->
  case qredis:q(["GET", lists:concat(["datasmart:openkey:", OUKey])]) of
    {ok, undefined} -> {error, "Uknown Key"};
    {ok, Ukey} -> {ok, binary_to_list(Ukey)};
    _ -> {error, "Uknown Key"}
  end.

doMatchAUKey(AUKey) ->
  case qredis:q(["GET", lists:concat(["datasmart:acesskey:", AUKey])]) of
    {ok, undefined} -> {error, "Uknown Key"};
    {ok, Ukey} -> {ok, binary_to_list(Ukey)};
    _ -> {error, "Uknown Key"}
  end.

doGetUser(Ukey) ->
  case qredis:q(["HGETALL", lists:concat(["datasmart:users:", Ukey, ":profile"])]) of
    {ok, Result} ->
      Json = ds_util:list_to_keyval(Result),
      User = [{binary_to_atom(Key, utf8), Val} || {Key, Val} <- Json],
      {ok, User};
    _ -> {error, "No User was Found"}
  end.

doUpdateProfile(Ukey, KeyValList) ->
  case qredis:q(["HEXISTS", lists:concat(["datasmart:users:", Ukey, ":profile"]), "email"]) of
    {ok, 1} ->
      [qredis:q(["HSET", lists:concat(["datasmart:users:", Ukey, ":profile"]), Key, Val]) ||
        {Key, Val} <- KeyValList],
      {ok, List} = qredis:q(["HGETALL", lists:concat(["datasmart:users:", Ukey, ":profile"])]),
      Json = ds_util:list_to_keyval(List),
      User = [{binary_to_atom(Key, utf8), Val} || {Key, Val} <- Json],
      {ok, User};
    _ -> {error, "No User was Found"}
  end.

hashPass(Password, Salt, 0) ->
  hash_md5:build(lists:concat([Password, Salt]));

hashPass(Password, Salt, Factor) when (Factor rem 2) > 0 ->
  hashPass(hash_md5:build(lists:concat([Password, Salt])), Salt, Factor - 1);

hashPass(Password, Salt, Factor) ->
  hashPass(hash_md5:build(lists:concat([Salt, Password])), Salt, Factor - 1).