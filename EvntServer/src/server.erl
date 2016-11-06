-module(server).
-export([start/0, start_link/0, terminate/0, subscribe/1, add_event/3, cancel/1, listen/1]).
-export([init/0, loop/1, send_to_clients/2]).

-record(state, {events, clients}).
-record(event, {name="", description="", pid, timeout=0}).

init() ->
  loop(#state{events=orddict:new(), clients=orddict:new()}).

start() ->
  register(?MODULE, Pid=spawn(?MODULE, init, [])),
  Pid.

start_link() ->
  register(?MODULE, Pid=spawn_link(?MODULE, init, [])),
  Pid.

terminate() ->
  ?MODULE ! shutdown.

subscribe(Pid) ->
  Ref = erlang:monitor(process, whereis(?MODULE)),
  ?MODULE ! {self(), Ref, {subscribe, Pid}},
  receive
    {Ref, ok} ->
      {ok, Ref};
    {'DOWN', Ref, process, _Pid, Reason} ->
      {error, Reason}
  after 5000 ->
    {error, timeout}
  end.

add_event(Name, Description, Timeout) ->
  Ref = make_ref(),
  ?MODULE ! {self(), Ref, {add, Name, Description, Timeout}},
  receive
    {Ref, Msg} -> Msg
  after 5000 ->
    {error, timeout}
  end.

cancel(Name) ->
  Ref = make_ref(),
  ?MODULE ! {self(), Ref, {cancel, Name}},
  receive
    {Ref, ok} -> ok
  after 5000 ->
    {error, timeout}
  end.

listen(Delay) ->
  receive
    M = {done, _Name, _Description} ->
      [M|listen(0)]
  after Delay * 1000 ->
    []
  end.

loop(S=#state{})->
  receive
    {Pid, MsgRef, {subscribe, Client}} ->
      Ref = erlang:monitor(process, Client),
      NewClients = orddict:store(Ref, Client, S#state.clients),
      Pid ! {MsgRef, ok},
      loop(#state{events=S#state.events, clients=NewClients});
    {Pid, MsgRef, {add, Name, Description, TimeOut}} ->
      EventPid = event:start_link(Name, TimeOut),
      NewEvents = orddict:store(Name, #event{name=Name, description=Description, pid=EventPid, timeout=TimeOut}, S#state.events),
      Pid ! {MsgRef, ok},
      loop(#state{events=NewEvents, clients=S#state.clients});
    {Pid, MsgRef, {cancel, Name}} ->
      Events = case orddict:find(Name, S#state.events) of
        {ok, E} ->
          event:cancel(E#event.pid),
          orddict:erase(Name, S#state.events)
        end,
      Pid ! {MsgRef, ok},
      loop(#state{events=Events, clients=S#state.clients});
    {done, Name} ->
      case orddict:find(Name, S#state.events) of
        {ok, E} ->
          send_to_clients({done, E#event.name, E#event.description}, S#state.clients),
          NewEvents = orddict:erase(Name, S#state.events),
          loop(#state{events=NewEvents, clients=S#state.clients});
        error ->
          loop(S)
      end;
    shutdown ->
      exit(shutdown);
    {'DOWN', Ref, process, _Pid, _Reason} ->
      NewClients = orddict:erase(Ref, S#state.clients),
      loop(#state{events=S#state.events, clients=NewClients});
    code_change ->
      ?MODULE:loop(S);
    Unknown ->
      io:format("Unknown: ~p~n", [Unknown]),
      loop(S)
  end.

send_to_clients(Msg, Clients) ->
  orddict:map(fun(_Ref, Pid) -> Pid ! Msg end, Clients).
