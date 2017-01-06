-module(echo_tcp).
-behaviour(application).

-export([start/2, stop/1]).

start(normal, _Args) ->
	io:format("Start~n"),
	echo_tcp_server:start_link().


stop(_State) ->
	echo_tcp_server:stop().
