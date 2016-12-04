-module (kitty_gen_server).
-behavior (gen_server).

-export([order_cat/4, return_cat/2, close_shop/1]).
-export([start_link/0, init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).
-record(cat, {name, color=grey, description}).

start_link() -> gen_server:start_link(?MODULE, [], []).

order_cat(Pid, Name, Color, Description) ->
  gen_server:call(Pid, {order, Name, Color, Description}).

return_cat(Pid, Cat=#cat{}) ->
  gen_server:cast(Pid, {return, Cat}).

close_shop(Pid) ->
  gen_server:call(Pid, terminate).


init([]) -> {ok, []}.

handle_call({order, Name, Color, Description}, _From, Cats) ->
  if Cats =:= [] ->
    {reply, make_cat(Name, Color, Description), Cats};
  Cats =/= [] ->
    {reply, hd(Cats), tl(Cats)}
  end;
handle_call(terminate, _From, Cats) ->
  {stop, normal, ok, Cats}.

handle_cast({return, Cat = #cat{}}, Cats) ->
  {noreply, [Cat|Cats]}.

handle_info(Msg, Cats) ->
  io:format("Unknown message: ~p~n", [Msg]),
  {noreply, Cats}.

terminate(normal, Cats) ->
  [io:format("~p was free now~n", [C#cat.name]) || C<-Cats],
  ok.

code_change(_, State, _) ->
  {ok, State}.

make_cat(Name, Color, Description) ->
 #cat{name=Name, color=Color, description=Description}.
