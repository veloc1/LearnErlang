-module (simple_fsm).
-behaviour (gen_fsm).

%% Simple fsm about traveler person
%% he can go, he can rest, he can eat

-export ([start/0, start_link/0, test/0, travel/1, stop/1]).
-export ([rest/2, rest/3, walk/2, walk/3]).
-export ([init/1, handle_event/3, handle_sync_event/4, handle_info/3, terminate/3, code_change/4]).

%% test our fsm
test() ->
  {ok, Pid} = start_link(),
  io:format("Started~n"),
  timer:sleep(500),
  travel(Pid),
  travel(Pid),
  timer:sleep(500),
  stop(Pid).

%% methods for start
start() ->
  gen_fsm:start(?MODULE, [], []).

start_link() ->
  gen_fsm:start_link(?MODULE, [], []).

%% public methods for control
travel(Pid) ->
  gen_fsm:send_event(Pid, walk).

stop(Pid) ->
  gen_fsm:send_all_state_event(Pid, stop).


%% private contrl, states
init(Args) ->
  {ok, rest, Args}.

%% states and events
rest(walk, Data) ->
  log(rest, walk, Data),
  {next_state, walk, Data};
rest(State, Data) ->
  log(rest, State, Data),
  {next_state, rest, Data}.

rest(State, From, Data) ->
  log(rest, State, Data),
  From ! ok,
  {next_state, rest, Data}.

walk(State, Data) ->
  log(walk, State, Data),
  {next_state, rest, Data}.

walk(State, From, Data) ->
  log(walk, State, Data),
  From ! ok,
  {next_state, rest, Data}.


%% events for all states
handle_event(stop, _StateName, Data) ->
  {stop, normal, Data};
handle_event(Event, StateName, Data) ->
  unexpected(Event, StateName),
  {next_state, StateName, Data}.

handle_sync_event(Event, _From, StateName, Data) ->
  unexpected(Event, StateName),
  {next_state, StateName, Data}.

handle_info(Info, StateName, Data) ->
  unexpected(Info, StateName),
  {next_state, StateName, Data}.

code_change(_Old, StateName, Data, _Extra) ->
  {ok, StateName, Data}.

terminate(Info, _State, _Data) ->
  io:format("Terminated: ~p~n", [Info]).


%% helpers
log(State, Arg, Data) ->
  io:format("~p: got an event ~p, current data are ~p~n", [State, Arg, Data]).

unexpected(Msg, State) ->
  io:format("~p got unexpected message ~p in state ~p~n", [self(), Msg, State]).
