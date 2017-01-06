-module(echo_tcp_server).
-behaviour(gen_server).

-export([start_link/0, init/1, handle_call/3, handle_cast/2, handle_info/2, code_change/3, terminate/2]).

start_link() ->
	gen_server:start_link(?MODULE, [], []).

init(_Args) ->
	io:format("Init~n"),
	{ok, Socket} = gen_tcp:listen(4802, [{active, true}, binary]),
	io:format("Wanted to accept~n"),
	gen_server:cast(self(), accept),
	{ok, Socket}.

handle_call(_Msg, _From, State) ->
	{noreply, State}.

handle_cast(accept, State) ->
	io:format("Start listening~n"),
	{ok, AcceptSocket} = gen_tcp:accept(State),
	io:format("Connection accepted"),
	gen_tcp:send(AcceptSocket, "Connection accepted"),
	gen_tcp:close(AcceptSocket),
	{noreply, State};
handle_cast(_Msg, State) ->
	{noreply, State}.

handle_info(_Msg, State) ->
	{noreply, State}.

code_change(_OldVsn, State, _Extra) ->
	{ok, State}.

terminate(_Reason, _State) ->
	ok.
