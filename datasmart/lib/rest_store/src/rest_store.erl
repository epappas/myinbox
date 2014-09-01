%%%-------------------------------------------------------------------
%%% @author evangelosp
%%% @copyright (C) 2014, evalonlabs
%%% @doc
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(rest_store).
-author("epappas").

%% API.
-export([start/0]).

%% API.
start() ->
  application:start(crypto),
  application:start(bcrypt),
  application:start(ranch),
  application:start(cowlib),
  application:start(cowboy),
  application:start(rest_store),
  application:start(rest_user),
  application:start(rest_susr),
  application:start(rest_message).
