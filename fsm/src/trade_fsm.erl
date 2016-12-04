-module (trade_fsm).
-behaviour (gen_fsm).

-export ([start/1, start_link/1, trade/2, accept_trade/1, make_offer/2, retract_offer/2, ready/1, cancel/1]).
-export ([init/1, handle_event/3, handle_sync_event/4, handle_info/3, terminate/3, code_change/4, idle/2, idle/3, idle_wait/2, idle_wait/3, negotiate/2, negotiate/3, wait/2, ready/2, ready/3]).

-record(state, {name="", other, ownitems=[], otheritems=[], monitor, from}).

%% public part
start(Name) ->
  gen_fsm:start(?MODULE, [Name], []).

start_link(Name) ->
  gen_fsm:start_link(?MODULE, [Name], []).

trade(Self, Other) ->
  gen_fsm:sync_send_event(Self, {negotiate, Other}, 30000).

accept_trade(Self) ->
  gen_fsm:sync_send_event(Self, accept_negotiate).

make_offer(Self, Item) ->
  gen_fsm:send_event(Self, {make_offer, Item}).

retract_offer(Self, Item) ->
  gen_fsm:send_event(Self, {retract_offer, Item}).


ready(Self) ->
  gen_fsm:sync_send_event(Self, ready, infinity).

cancel(Self) ->
  gen_fsm:sync_send_all_state_event(Self, cancel).

%% inner methods
ask_negotiate(Self, Other) ->
  gen_fsm:send_event(Other, {ask_negotiate, Self}).

accept_negotiate(Other, Self) ->
  gen_fsm:send_event(Other, {accept_negotiate, Self}).

do_offer(Other, Item) ->
  gen_fsm:send_event(Other, {do_offer, Item}).

undo_offer(Other, Item) ->
  gen_fsm:send_event(Other, {undo_offer, Item}).

are_you_ready(Other) ->
  gen_fsm:send_event(Other, are_you_ready).

not_yet(Other) ->
  gen_fsm:send_event(Other, not_yet).

iam_ready(Other) ->
  gen_fsm:send_event(Other, ready_for_trade).

ack_transaction(Other) ->
  gen_fsm:send_event(Other, ack).

ask_commit(Other) ->
  gen_fsm:sync_send_event(Other, ask_commit).

do_commit(Other) ->
  gen_fsm:sync_send_event(Other, do_commit).

notify_cancel(Other) ->
  gen_fsm:send_event(Other, cancel).

init(Name) ->
  io:fwrite("init ~p~n", [Name]),
  {ok, idle, #state{name=Name}}.


idle({ask_negotiate, Other}, S=#state{}) ->
  Ref = monitor(process, Other),
  notice(S, "~p wants to start negotiation", [Other]),
  {next_state, idle_wait, S#state{other=Other, monitor=Ref}};
idle(Event, Data) ->
  unexpected(Event, idle),
  {next_state, idle, Data}.

idle({negotiate, Other}, From, S=#state{}) ->
  ask_negotiate(Other, self()),
  notice(S, "asks ~p to start trading", [Other]),
  Ref = monitor(process, Other),
  {next_state, idle_wait, S#state{other=Other, monitor=Ref, from=From}};
idle(Event, _From, Data) ->
  unexpected(Event, idle),
  {next_state, idle, Data}.


idle_wait({ask_negotiate, Other}, S=#state{other=Other}) ->
  gen_fsm:reply(S#state.from, ok),
  notice(S, "start trading", []),
  {next_state, negotiate, S};
idle_wait({accept_negotiate, Other}, S=#state{other=Other}) ->
  gen_fsm:reply(S#state.from, ok),
  notice(S, "start trading", []),
  {next_state, negotiate, S};
idle_wait(Event, Data) ->
  unexpected(Event, idle_wait),
  {next_state, idle_wait, Data}.

idle_wait(accept_negotiate, _From, S=#state{other=Other}) ->
  accept_negotiate(Other, self()),
  notice(S, "start trading", []),
  {reply, ok, negotiate, S};
idle_wait(Event, _From, Data) ->
  unexpected(Event, idle_wait),
  {next_state, idle_wait, Data}.

negotiate({make_offer, Item}, S=#state{ownitems=OwnItems}) ->
  do_offer(S#state.other, Item),
  notice(S, "make offer ~p", [Item]),
  {next_state, negotiate, S#state{ownitems=add(Item, OwnItems)}};
negotiate({retract_offer, Item}, S=#state{ownitems=OwnItems}) ->
  undo_offer(S#state.other, Item),
  notice(S, "undo offer ~p", [Item]),
  {next_state, negotiate, S#state{ownitems=remove(Item, OwnItems)}};
negotiate({do_offer, Item}, S=#state{otheritems=OtherItems}) ->
  notice(S, "other player offer ~p", [Item]),
  {next_state, negotiate, S=#state{otheritems=add(Item, OtherItems)}};
negotiate({undo_offer, Item}, S=#state{otheritems=OtherItems}) ->
  notice(S, "other player undo offer ~p", [Item]),
  {next_state, negotiate, S=#state{otheritems=remove(Item, OtherItems)}};

negotiate(are_you_ready, S=#state{other=Other}) ->
  io:format("Other player want to trade ~n"),
  notice(S, "Other player wants to trade~nyou offers ~p~nother offers ~p~n", [S#state.ownitems, S#state.otheritems]),
  not_yet(Other),
  {next_state, negotiate, S};
negotiate(Event, Data) ->
  unexpected(Event, negotiate),
  {next_state, negotiate, Data}.

negotiate(ready, From, S=#state{other=Other}) ->
  are_you_ready(Other),
  notice(S, "asking other, ready for ready", []),
  {next_state, wait, S#state{from=From}};
negotiate(Event, _From, Data) ->
  unexpected(Event, negotiate),
  {next_state, negotiate, Data}.

wait({do_offer, Item}, S=#state{otheritems=OtherItems}) ->
  gen_fsm:reply(S#state.from, offer_changed),
  notice(S, "other player offer ~p", [Item]),
  {next_state, negotiate, S#state{otheritems=add(Item, OtherItems)}};
wait({undo_offer, Item}, S=#state{otheritems=OtherItems}) ->
  gen_fsm:reply(S#state.from, offer_changed),
  notice(S, "other player undo offer ~p", [Item]),
  {next_state, negotiate, S#state{otheritems=remove(Item, OtherItems)}};
wait(are_you_ready, S=#state{}) ->
  iam_ready(S#state.other),
  notice(S, "asked for ready. we are ready", []),
  {next_state, wait, S};
wait(not_yet, S=#state{}) ->
  notice(S, "other guy not ready", []),
  {next_state, wait, S};
wait(ready_for_trade, S=#state{}) ->
  iam_ready(S#state.other),
  ack_transaction(S#state.other),
  gen_fsm:reply(S#state.from, ok),
  notice(S, "other guy is ready, moving to state ready", []),
  {next_state, ready, S};
wait(Event, Data) ->
  unexpected(Event, wait),
  {next_state, wait, Data}.

ready(ack, S=#state{}) ->
  case priority(self(), S#state.other) of
    true ->
      try
        notice(S, "begin of commit",[]),
        ready_commit = ask_commit(S#state.other),
        ok = do_commit(S#state.other),
        notice(S, "commit started",[]),
        commit(S),
        notice(S, "done",[]),
        {stop, normal, S}
      catch Class:Reason ->
        notice(S, "error on commit",[]),
        {stop, {Class, Reason}, S}
      end;
    false ->
      {next_state, ready, S}
   end;
ready(Event, Data) ->
  unexpected(Event, ready),
  {next_state, ready, Data}.

ready(ask_commit, _From, S)->
  notice(S, "asked for commit, answering ready", []),
  {reply, ready_commit, ready, S};
ready(do_commit, _From, S) ->
  notice(S, "writing", []),
  commit(S),
  {stop, normal, ok, S};
ready(Event, _From, Data) ->
  unexpected(Event, ready),
  {next_state, ready, Data}.

commit(S = #state{}) ->
  io:format("items for trade: ~nplayer1: ~p~nplayer2: ~p~n", [S#state.ownitems, S#state.otheritems]).


handle_event(cancel, _StateName, S=#state{}) ->
  notice(S, "canceling", []),
  {stop, other_cancelled, S};
handle_event(Event, StateName, Data) ->
  unexpected(Event, StateName),
  {next_state, StateName, Data}.

handle_sync_event(cancel, _From, _StateName, S=#state{}) ->
  notify_cancel(S#state.other),
  notice(S, "cancelling", []),
  {stop, cancelled, ok, S};
handle_sync_event(Event, _From, StateName, Data) ->
  unexpected(Event, StateName),
  {next_state, StateName, Data}.


handle_info({'DOWN', Ref, process, Pid, Reason}, _StateName, S=#state{other=Pid, monitor=Ref}) ->
  notice(S, "connection is down", []),
  {stop, {other_down, Reason}, S};
handle_info(Info, StateName, Data) ->
  unexpected(Info, StateName),
  {next_state, StateName, Data}.


code_change(_Old, StateName, Data, _Extra) ->
  {ok, StateName, Data}.

terminate(normal, ready, S=#state{}) ->
  notice(S, "terminating", []);
terminate(_Reason, _StateName, _StateData) ->
  ok.

add(Item, Items) ->
  [Item|Items].
remove(Item, Items) ->
  Items -- [Item].

notice(#state{name=N}, Str, Args) ->
  io:format("~s: " ++ Str ++ "~n", [N|Args]).

unexpected(Msg, State=#state{}) ->
  io:format("~p got unexpected message ~p in state ~p on ~p~n", [self(), Msg, State, State#state.name]).

priority(Pid1, Pid2) when Pid1 > Pid2 -> true;
priority(Pid1, Pid2) when Pid1 < Pid2 -> false.
