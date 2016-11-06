-module(event).
-export([start/2, start_link/2, cancel/1]).
-export([init/3, loop/1]).
-record(state, {server, name="", to_go=0}).

start(EventName, Delay) ->
  spawn(?MODULE, init, [self(), EventName, Delay]).

start_link(EventName, Delay) ->
  spawn_link(?MODULE, init, [self(), EventName, Delay]).

cancel(Pid) ->
  Ref = erlang:monitor(process, Pid),
  Pid ! {self(), Ref, cancel},
  receive
    {Ref, ok} ->
      erlang:demonitor(Ref, [flush]),
      ok;
    {'DOWN', Ref, process, Pid, _Reason} ->
      ok
    end.

init(Server, EventName, Delay) ->
  loop(#state{server=Server, name=EventName, to_go=Delay}).

loop(S = #state{server=Server}) ->
  receive
    {Server, Ref, cancel} ->
      Server ! {Ref, ok}
    after S#state.to_go * 1000 ->
      Server ! {done, S#state.name}
    end.
