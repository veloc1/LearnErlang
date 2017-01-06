%% @author Dell
%% @doc @todo Add description to ppool_sup.


-module(ppool_sup).
-behaviour(supervisor).
-export([start_link/3, init/1]).

%% ====================================================================
%% API functions
%% ====================================================================
start_link(Name, Limit, MFA) ->
	supervisor:start_link(?MODULE, {Name, Limit, MFA}).


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
init({Name, Limit, MFA}) ->
	MaxRestart = 1,
	MaxTime = 3600,
	{ok, {{one_for_all, MaxRestart, MaxTime}, 
		  [{serv, {ppool_serv, start_link, [Name, Limit, self(), MFA]},
			permanent, 5000, worker, [ppool_serv]
		   }
		  ]}}.


%% ====================================================================
%% Internal functions
%% ====================================================================


