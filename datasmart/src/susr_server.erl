%%%-------------------------------------------------------------------
%%% @author evangelosp
%%% @copyright (C) 2014, evalonlabs
%%% @doc
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(susr_server).
-author("evangelosp").

-behaviour(gen_server).

%% API
-export([start_link/0,
  register/2,
  add_aukey/3,
  getukey/1,
  match_ouKey/1,
  match_auKey/2,
  getsusr/1,
  updateprofile/2,
  encrypt/3,
  dencrypt/3,
  checksusr/2
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

add_aukey(Ukey, AUKey, Secret) -> gen_server:call(?MODULE, {addapikey, Ukey, AUKey, Secret}).

getukey(Email) -> gen_server:call(?MODULE, {getukey, Email}).

match_ouKey(OUkey) -> gen_server:call(?MODULE, {match_ouKey, OUkey}).

match_auKey(AUkey, Secret) -> gen_server:call(?MODULE, {match_auKey, AUkey, Secret}).

getsusr(Ukey) -> gen_server:call(?MODULE, {getsusr, Ukey}).

checksusr(Email, Password) -> gen_server:call(?MODULE, {checksusr, Email, Password}).

updateprofile(Ukey, KeyValList) -> gen_server:call(?MODULE, {updateprofile, Ukey, KeyValList}).

encrypt(Ukey, IVec, Text) ->
  {ok, SecretBin} = qredis:q(["GET", lists:concat(["datasmart:susrs:", Ukey, ":secret"])]),
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
  {ok, SecretBin} = qredis:q(["GET", lists:concat(["datasmart:susrs:", Ukey, ":secret"])]),
  SecretList = binary_to_list(SecretBin),
  Secret = lists:sublist(SecretList, 8),
  IvacBin = list_to_binary(case is_binary(IVec) of
                             false -> lists:sublist(IVec, 8);
                             true -> lists:sublist(binary_to_list(IVec), 8)
                           end),
  {ok, crypto:des_cbc_decrypt(Secret, IvacBin, Text)}.

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
  case qredis:q(["HGETALL", lists:concat(["datasmart:susrs:", MD5Key, ":profile"])]) of
    {ok, []} ->
      qredis:q(["HSET", lists:concat(["datasmart:susrs:", MD5Key, ":profile"]), "email", Email]),
      qredis:q(["SET", lists:concat(["datasmart:openkey:", OUKey]), MD5Key]),
      qredis:q(["SET", lists:concat(["datasmart:susrs:", MD5Key, ":password"]), hashPass(Password, Salt, 20)]),
      qredis:q(["SET", lists:concat(["datasmart:susrs:", MD5Key, ":salt"]), Salt]),
      qredis:q(["SET", lists:concat(["datasmart:susrs:", MD5Key, ":secret"]), Secret]),
      qredis:q(["HSETNX", "datasmart:alias", Email, Email]),
      qredis:q(["HSETNX", "datasmart:alias:keys", Email, MD5Key]),
      {reply, {ok, [{email, list_to_binary(Email)}, {oukey, list_to_binary(OUKey)}]}, State};
    _ -> {reply, {error, "Registration Failure, SUsr Exists"}, State}
  end;

handle_call({addaukey, Ukey, AUKey, Secret}, _From, State) ->
  {ok, BSalt} = qredis:q(["GET", lists:concat(["datasmart:susrs:", Ukey, ":salt"])]),
  Salt = binary_to_list(BSalt),

  qredis:q(["SET", lists:concat(["datasmart:acesskey:", AUKey, ":ukey"]), Ukey]),

  HashSecret = hashPass(Secret, Salt, 20),
  qredis:q(["SET", lists:concat(["datasmart:acesskey:", AUKey, ":secret"]), HashSecret]),

  {reply, {ok, [{ukey, Ukey}, {aukey, AUKey}, {secret, Secret}]}, State};

handle_call({getukey, Email}, _From, State) ->
  {reply, doGetukey(Email), State};

handle_call({match_ouKey, OUkey}, _From, State) ->
  {reply, doMatchOUKey(OUkey), State};

handle_call({match_auKey, AUkey, Secret}, _From, State) ->
  {reply, doMatchAUKey(AUkey, Secret), State};

handle_call({getsusr, Ukey}, _From, State) ->
  {reply, doGetSUsr(Ukey), State};

handle_call({updateprofile, Ukey, KeyValList}, _From, State) ->
  {reply, doUpdateProfile(Ukey, KeyValList), State};

handle_call({checksusr, Email, Password}, _From, State) ->
  {reply, doChecksusr(Email, Password), State}.

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

doChecksusr(Email, Password) ->
  case doGetukey(Email) of
    {ok, Ukey} ->
      case qredis:q(["GET", lists:concat(["datasmart:susrs:", Ukey, ":salt"])]) of
        {ok, undefined} -> {error, "Wrong SUsr Details"};
        {ok, BSalt} ->
          Salt = binary_to_list(BSalt),
          HashPass = hashPass(Password, Salt, 20),
          {ok, PassBin} = qredis:q(["GET", lists:concat(["datasmart:susrs:", Ukey, ":password"])]),
          SUsrPass = binary_to_list(PassBin),
          case SUsrPass =:= HashPass of
            true -> {ok, Ukey};
            false -> {error, false}
          end;
        _ -> {error, "Something Went wrong getting SUsr's Details"}
      end;
    _ -> {error, "Wrong SUsr Details"}
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

doMatchAUKey(AUKey, Secret) ->
  case qredis:q(["GET", lists:concat(["datasmart:acesskey:", AUKey, ":ukey"])]) of
    {ok, undefined} -> {error, "Uknown Key"};
    {ok, UkeyBin} ->
      Ukey = {ok, binary_to_list(UkeyBin)},
      case qredis:q(["GET", lists:concat(["datasmart:susrs:", Ukey, ":salt"])]) of
        {ok, undefined} -> {error, "Wrong SUsr Details"};
        {ok, BSalt} ->
          Salt = binary_to_list(BSalt),
          HashSecret = hashPass(Secret, Salt, 20),
          {ok, StoredSecretBin} = qredis:q(["GET", lists:concat(["datasmart:acesskey:", AUKey, ":secret"])]),
          StoredSecret = binary_to_list(StoredSecretBin),
          case StoredSecret =:= HashSecret of
            true -> {ok, Ukey};
            false -> {error, false}
          end;
        _ -> {error, "Something Went wrong getting SUsr's Details"}
      end;
    _ -> {error, "Uknown Key"}
  end.

doGetSUsr(Ukey) ->
  case qredis:q(["HGETALL", lists:concat(["datasmart:susrs:", Ukey, ":profile"])]) of
    {ok, Result} ->
      Json = ds_util:list_to_keyval(Result),
      SUsr = [{binary_to_atom(Key, utf8), Val} || {Key, Val} <- Json],
      {ok, SUsr};
    _ -> {error, "No SUsr was Found"}
  end.

doUpdateProfile(Ukey, KeyValList) ->
  case qredis:q(["HEXISTS", lists:concat(["datasmart:susrs:", Ukey, ":profile"]), "email"]) of
    {ok, 1} ->
      [qredis:q(["HSET", lists:concat(["datasmart:susrs:", Ukey, ":profile"]), Key, Val]) ||
        {Key, Val} <- KeyValList],
      {ok, List} = qredis:q(["HGETALL", lists:concat(["datasmart:susrs:", Ukey, ":profile"])]),
      Json = ds_util:list_to_keyval(List),
      SUsr = [{binary_to_atom(Key, utf8), Val} || {Key, Val} <- Json],
      {ok, SUsr};
    _ -> {error, "No SUsr was Found"}
  end.

hashPass(Password, Salt, 0) ->
  hash_md5:build(lists:concat([Password, Salt]));

hashPass(Password, Salt, Factor) when (Factor rem 2) > 0 ->
  hashPass(hash_md5:build(lists:concat([Password, Salt])), Salt, Factor - 1);

hashPass(Password, Salt, Factor) ->
  hashPass(hash_md5:build(lists:concat([Salt, Password])), Salt, Factor - 1).