%%%-------------------------------------------------------------------
%%% @author evangelosp
%%% @copyright (C) 2014, evalonlabs
%%% @doc
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(rest_basic_auth).
-author("evangelosp").

%% API.
-export([start/0]).

%% API.
start() ->
  application:start(crypto),
  application:start(ranch),
  application:start(cowlib),
  application:start(cowboy),
  application:start(rest_basic_auth).
