%%%-------------------------------------------------------------------
%%% @author evangelosp
%%% @copyright (C) 2014, evalonlabs
%%% @doc
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(rest_basic_auth_app).
-author("evangelosp").

-behaviour(application).

%% API.
-export([start/2]).
-export([stop/1]).

%% API.

start(_Type, _Args) ->
	Dispatch = compile(),
	{ok, _} = cowboy:start_http(http, 100, [{port, 8080}], [
		{env, [{dispatch, Dispatch}]}
	]),
	rest_basic_auth_sup:start_link().

stop(_State) ->
	ok.

%%% Routing
compile() ->
  List = [toppage_handler],
  cowboy_router:compile([
    %% {URIHost, list({URIPath, Handler, Opts})}
    {'_', [route(Name) || Name <- List]}
  ]).

route(toppage_handler) ->
  {"/", toppage_handler, []}.
