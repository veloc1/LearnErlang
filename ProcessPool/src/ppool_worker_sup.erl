%% @author Dell
%% @doc @todo Add description to ppool_worker_sup.


-module(ppool_worker_sup).
-behaviour(supervisor).
-export([start_link/1, init/1]).

%% ====================================================================
%% API functions
%% ====================================================================

start_link(MFA={_,_,_}) ->
	supervisor:start_link(?MODULE, MFA).

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
init({M, F, A}) ->
	MaxRestart = 5,
	MaxTime = 3600,
    AChild = {ppool_worker,{M, F, A},
	      temporary, 5000, worker, [M]},
    {ok,{{simple_one_for_one, MaxRestart, MaxTime}, [AChild]}}.

%% ====================================================================
%% Internal functions
%% ====================================================================


