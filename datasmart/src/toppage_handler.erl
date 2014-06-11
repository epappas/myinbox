%%%-------------------------------------------------------------------
%%% @author evangelosp
%%% @copyright (C) 2014, evalonlabs
%%% @doc
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(toppage_handler).
-author("evangelosp").

-export([init/3]).
-export([content_types_provided/2]).
-export([is_authorized/2]).
-export([to_text/2]).

init(_Transport, _Req, []) ->
	{upgrade, protocol, cowboy_rest}.

is_authorized(Req, State) ->
	{ok, Auth, Req1} = cowboy_req:parse_header(<<"authorization">>, Req),
	case Auth of
		{<<"basic">>, {User = <<"Alladin">>, <<"open sesame">>}} ->
			{true, Req1, User};
		_ ->
			{{false, <<"Basic realm=\"cowboy\"">>}, Req1, State}
	end.

content_types_provided(Req, State) ->
	{[
		{<<"text/plain">>, to_text}
	], Req, State}.

to_text(Req, User) ->
	{<< "Hello, ", User/binary, "!\n" >>, Req, User}.
