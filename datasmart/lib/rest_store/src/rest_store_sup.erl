%%%-------------------------------------------------------------------
%%% @author evangelosp
%%% @copyright (C) 2014, evalonlabs
%%% @doc
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(rest_store_sup).
-author("epappas").

-behaviour(supervisor).

%% API.
-export([start_listeners/0, start_link/0]).

%% Supervisor callbacks
-export([init/1]).

%% Helper macro for declaring children of supervisor
-define(CHILD(I, Type), {I, {I, start_link, []}, permanent, 5000, Type, [I]}).

%% ===================================================================
%% API functions
%% ===================================================================

start_link() ->
  supervisor:start_link({local, ?MODULE}, ?MODULE, []).

start_listeners() ->
  {ok, Application} = application:get_application(),
  Port = application:get_env(Application, http_port, 4421),
  ListenerCount = application:get_env(Application, http_listener_count, 100),

  Dispatch = routing_dispatch(),
  {ok, _} = cowboy:start_http(http, ListenerCount, [{port, Port}], [
    {env, [{dispatch, Dispatch}]},
    {timeout, 12000}
  ]).

%% ===================================================================
%% Supervisor callbacks
%% ===================================================================

init([]) ->
  pg2:create(datastore_rest_listeners),
  {ok, {{one_for_one, 10, 10}, [
    {qredis,
      {qredis, start_link, []},
      permanent, 1000, worker,
      [qredis]},
    {rest_store,
      {rest_store_sup, start_listeners, []},
      permanent, 1000, worker,
      [rest_store_sup]}
  ]}}.

%% ===================================================================
%%% Routing
%% ===================================================================
routing_dispatch() ->
  List = [root, index_key, message, message_key, inbox, inbox_folder, user, user_key, susr, susr_key],
  cowboy_router:compile([
    %% {URIHost, list({URIPath, Handler, Opts})}
    {'_', [route(Name) || Name <- List]}
  ]).

route(root) ->
  {"/", index_handler, []};

route(index_key) ->
  {"/index/:key", index_handler, []};

route(message) ->
  {"/message", message_handler, []};

route(message_key) ->
  {"/message/:key", message_handler, []};

route(inbox) ->
  {"/inbox", inbox_handler, []};

route(inbox_folder) ->
  {"/inbox/:folder", inbox_handler, []};

route(user) ->
  {"/user", user_handler, []};

route(user_key) ->
  {"/user/:key", user_handler, []};

route(susr) ->
  {"/susr", susr_handler, []};

route(susr_key) ->
  {"/susr/:key", susr_handler, []}.


