-module(musicians).
-behaviour (gen_server).

-export([start_link/2, stop/1]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, code_change/3, terminate/2]).

-record(state, {name="", role, skill=good}).
-define(DELAY, 750).

start_link(Role, Skill) ->
  gen_server:start_link({local, Role}, ?MODULE, [Role, Skill], []).

stop(Role) -> gen_server:call(Role, stop).

init([Role, Skill]) ->
  process_flag(trap_exit, true),
  random:seed(erlang:timestamp()),
  TimeToPlay = 100,
  Name = pick_name(),
  StrRole = atom_to_list(Role),
  io:format("~s playing the ~s comes here~n", [Name, StrRole]),
  {ok, #state{name=Name, role=StrRole, skill=Skill}, TimeToPlay}.

handle_call(stop, _From, S=#state{}) ->
  {stop, normal, ok, S};
handle_call(_Message, _From, S) ->
  {noreply, S, ?DELAY}.

handle_cast(_Message, S) ->
  {noreply, S, ?DELAY}.

handle_info(timeout, S=#state{name=N, skill=good}) ->
  io:format("~s sounds~n", [N]),
  {noreply, S, ?DELAY};
handle_info(timeout, S=#state{name=N, skill=bad}) ->
  case rand:uniform(5) of
    1 ->
      io:format("~s plays bad~n", [N]),
      {stop, bad_note, S};
    _ ->
      io:format("~s sounds~n", [N]),
      {noreply, S, ?DELAY}
    end;
handle_info(_Message, S) ->
  {noreply, S, ?DELAY}.

code_change(_Old, State, _Extra) ->
  {ok, State}.

terminate(normal, S) ->
  io:format("~s done here~n", [S#state.name]);
terminate(bad_note, S) ->
  io:format("~s sucks~n", [S#state.name]);
terminate(shutdown, S) ->
  io:format("shutdown was called, everyone are free now~n");
terminate(_Reason, S) ->
  io:format("Unexpected exit~n").

pick_name() ->
  lists:nth(rand:uniform(5), firstnames()) ++
  " " ++
  lists:nth(rand:uniform(5), lastnames()).

firstnames() ->
  ["Frank", "Cody", "Valery", "Peter", "Paul"].

lastnames() ->
  ["Sinatra", "Davidson", "Tint", "Parker", "Cury"].
