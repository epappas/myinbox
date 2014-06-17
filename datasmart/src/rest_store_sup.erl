%%%-------------------------------------------------------------------
%%% @author evangelosp
%%% @copyright (C) 2014, evalonlabs
%%% @doc
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(rest_store_sup).
-author("evangelosp").

-behaviour(supervisor).

%% API.
-export([start_listeners/0, start_link/0]).

%% supervisor.
-export([init/1]).

%% API.

start_listeners() ->
  {ok, Application} = application:get_application(),
  Port = application:get_env(Application, http_port, 4420),
  ListenerCount = application:get_env(Application, http_listener_count, 50),

  Dispatch = compile(),
  {ok, _} = cowboy:start_http(http, ListenerCount, [{port, Port}], [
    {env, [{dispatch, Dispatch}]},
    {timeout, 12000}
  ]).

start_link() ->
  supervisor:start_link({local, ?MODULE}, ?MODULE, []).

%%% Routing
compile() ->
  List = [index_handler],
  cowboy_router:compile([
    %% {URIHost, list({URIPath, Handler, Opts})}
    {'_', [route(Name) || Name <- List]}
  ]).

route(index_handler) ->
  {"/index", index_handler, []}.


%% supervisor.

init([]) ->
  pg2:create(datastore_rest_listeners),
  {ok, {{one_for_one, 10, 10}, [
    {rest_store,
      {rest_store_sup, start_listeners, []},
      permanent, 1000, worker,
      [rest_store_sup]},
    {myredis_serve,
      {myredis, start_link, []},
      permanent, 1000, worker,
      [myredis]}
  ]}}.
