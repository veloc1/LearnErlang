%% @author Dell
%% @doc @todo Add description to erlcount_sup.


-module(erlcount_sup).
-behaviour(supervisor).
-export([init/1]).

%% ====================================================================
%% API functions
%% ====================================================================
-export([start_link/0]).



%% ====================================================================
%% Behavioural functions
%% ====================================================================

%% init/1
%% ====================================================================
%% @doc <a href="http://www.erlang.org/doc/man/supervisor.html#Module:init-1">supervisor:init/1</a>
-spec init(Args :: term()) -> Result when
	Result :: {ok, {SupervisionPolicy, [ChildSpec]}} | ignore,
	SupervisionPolicy :: {RestartStrategy, MaxR :: non_neg_integer(), MaxT :: pos_integer()},
	RestartStrategy :: one_for_all
					 | one_for_one
					 | rest_for_one
					 | simple_one_for_one,
	ChildSpec :: {Id :: term(), StartFunc, RestartPolicy, Type :: worker | supervisor, Modules},
	StartFunc :: {M :: module(), F :: atom(), A :: [term()] | undefined},
	RestartPolicy :: permanent
				   | transient
				   | temporary,
	Modules :: [module()] | dynamic.
%% ====================================================================
init([]) ->
	MaxRestart = 5,
	MaxTime = 100,
	{ok, {{one_for_one, MaxRestart, MaxTime},
		  [{dispatch, 
			{erlcount_dispatch, start_link, []},
			transient, 6000, worker,
			[erlcount_dispatch]}]}}.

%% ====================================================================
%% Internal functions
%% ====================================================================
start_link() ->
	supervisor:start_link(?MODULE, []).

