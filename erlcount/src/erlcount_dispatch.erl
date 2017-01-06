%% @author Dell
%% @doc @todo Add description to erlcount_dispatch.
 

-module(erlcount_dispatch).
-behaviour(gen_fsm).
-export([init/1, handle_event/3, handle_sync_event/4, handle_info/3, terminate/3, code_change/4]).
-define(POOL, erlcount).

%% ====================================================================
%% API functions
%% ====================================================================
-export([start_link/0, complete/4, dispatching/2, listening/2]).

start_link() ->
	gen_fsm:start_link(?MODULE, [], []).

complete(Pid, Regex, Ref, Count) ->
	gen_fsm:send_all_state_event(Pid, {complete, Regex, Ref, Count}).

%% ====================================================================
%% Behavioural functions
%% ====================================================================
-record(data, {regex=[], refs=[]}).

%% init/1
%% ====================================================================
%% @doc <a href="http://www.erlang.org/doc/man/gen_fsm.html#Module:init-1">gen_fsm:init/1</a>
-spec init(Args :: term()) -> Result when
	Result :: {ok, StateName, StateData}
			| {ok, StateName, StateData, Timeout}
			| {ok, StateName, StateData, hibernate}
			| {stop, Reason}
			| ignore,
	StateName :: atom(),
	StateData :: term(),
	Timeout :: non_neg_integer() | infinity,
	Reason :: term().
%% ====================================================================
init([]) ->
    {ok, Re} = application:get_env(regex),
	{ok, Dir} = application:get_env(directory),
	{ok, MaxFiles} = application:get_env(max_files),
	ppool:start_pool(?POOL, MaxFiles, {erlcount_counter, start_link, []}),
	case lists:all(fun valid_regex/1, Re) of
		true ->
			self() ! {start, Dir},
			{ok, dispatching, #data{regex=[{R, 0} || R<- Re]}};
		false ->
			{stop, invalid_regex}
	end.

%% handle_event/3
%% ====================================================================
%% @doc <a href="http://www.erlang.org/doc/man/gen_fsm.html#Module:handle_event-3">gen_fsm:handle_event/3</a>
-spec handle_event(Event :: term(), StateName :: atom(), StateData :: term()) -> Result when
	Result :: {next_state, NextStateName, NewStateData}
			| {next_state, NextStateName, NewStateData, Timeout}
			| {next_state, NextStateName, NewStateData, hibernate}
			| {stop, Reason, NewStateData},
	NextStateName :: atom(),
	NewStateData :: term(),
	Timeout :: non_neg_integer() | infinity,
	Reason :: term().
%% ====================================================================
handle_event({complete, Regex, Ref, Count}, State, Data=#data{regex=Re, refs=Refs}) ->
	{Regex, OldCount} = lists:keyfind(Regex, 1, Re),
	NewRe = lists:keyreplace(Regex, 1, Re, {Regex, OldCount + Count}),
	NewData = Data#data{regex=NewRe, refs=Refs--[Ref]},
	case State of
		dispatching -> 
			{next_state, dispatching, NewData};
		listening ->
			listening(done, NewData)
	end.

%% handle_sync_event/4
%% ====================================================================
%% @doc <a href="http://www.erlang.org/doc/man/gen_fsm.html#Module:handle_sync_event-4">gen_fsm:handle_sync_event/4</a>
-spec handle_sync_event(Event :: term(), From :: {pid(), Tag :: term()}, StateName :: atom(), StateData :: term()) -> Result when
	Result :: {reply, Reply, NextStateName, NewStateData}
			| {reply, Reply, NextStateName, NewStateData, Timeout}
			| {reply, Reply, NextStateName, NewStateData, hibernate}
			| {next_state, NextStateName, NewStateData}
			| {next_state, NextStateName, NewStateData, Timeout}
			| {next_state, NextStateName, NewStateData, hibernate}
			| {stop, Reason, Reply, NewStateData}
			| {stop, Reason, NewStateData},
	Reply :: term(),
	NextStateName :: atom(),
	NewStateData :: term(),
	Timeout :: non_neg_integer() | infinity,
	Reason :: term().
%% ====================================================================
handle_sync_event(_Event, _From, StateName, StateData) ->
    Reply = ok,
    {reply, Reply, StateName, StateData}.


%% handle_info/3
%% ====================================================================
%% @doc <a href="http://www.erlang.org/doc/man/gen_fsm.html#Module:handle_info-3">gen_fsm:handle_info/3</a>
-spec handle_info(Info :: term(), StateName :: atom(), StateData :: term()) -> Result when
	Result :: {next_state, NextStateName, NewStateData}
			| {next_state, NextStateName, NewStateData, Timeout}
			| {next_state, NextStateName, NewStateData, hibernate}
			| {stop, Reason, NewStateData},
	NextStateName :: atom(),
	NewStateData :: term(),
	Timeout :: non_neg_integer() | infinity,
	Reason :: normal | term().
%% ====================================================================
handle_info({start, Dir}, State, Data) ->
	gen_fsm:send_event(self(), erlcount_lib:find_erl(Dir)),
	{next_state, State, Data}.

%% terminate/3
%% ====================================================================
%% @doc <a href="http://www.erlang.org/doc/man/gen_fsm.html#Module:terminate-3">gen_fsm:terminate/3</a>
-spec terminate(Reason, StateName :: atom(), StateData :: term()) -> Result :: term() when
	Reason :: normal
			| shutdown
			| {shutdown, term()}
			| term().
%% ====================================================================
terminate(_Reason, _StateName, _StatData) ->
    ok.


%% code_change/4
%% ====================================================================
%% @doc <a href="http://www.erlang.org/doc/man/gen_fsm.html#Module:code_change-4">gen_fsm:code_change/4</a>
-spec code_change(OldVsn, StateName :: atom(), StateData :: term(), Extra :: term()) -> {ok, NextStateName :: atom(), NewStateData :: term()} when
	OldVsn :: Vsn | {down, Vsn},
	Vsn :: term().
%% ====================================================================
code_change(_OldVsn, StateName, StateData, _Extra) ->
    {ok, StateName, StateData}.


dispatching({continue, File, Continuation}, Data = #data{regex=Re, refs=Refs}) ->
	F = fun({Regex, _Count}, NewRefs) ->
				Ref = make_ref(),
				ppool:async_queue(?POOL, [self(), Ref, File, Regex]),
				[Ref| NewRefs]
		end,
	NewRefs = lists:foldl(F, Refs, Re),
	gen_fsm:send_event(self(), Continuation()),
	{next_state, dispatching, Data#data{refs=NewRefs}};
dispatching(done, Data) ->
	listening(done, Data).

listening(done, #data{regex=Re, refs=[]}) ->
	[io:format("regex ~s count ~p matches~n", [R, C]) || {R, C} <- Re],
	{stop, normal, done};
listening(done, Data) ->
	{next_state, listening, Data}.

%% ====================================================================
%% Internal functions
%% ====================================================================

valid_regex(Re) ->
	try re:run("", Re) of
		_ -> true
	catch
		error:badarg -> false
	end.
