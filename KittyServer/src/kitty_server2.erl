-module(kitty_server2).
-export([start_link/0]).
-export([order_cat/4, return_cat/2, close_shop/1]).
-export([init/1, handle_call/3, handle_cast/2]).

-record(cat, {name, color=grey, description}).

start_link() -> my_server:start_link(?MODULE, []).

order_cat(Pid, Name, Color, Description) ->
  my_server:call(Pid, {order, Name, Color, Description}).

return_cat (Pid, Cat = #cat{}) ->
  my_server:cast(Pid, {return, Cat}).

close_shop(Pid) ->
  my_server:call(Pid, terminate).

init([]) -> [].

handle_call({order, Name, Color, Description}, From, Cats) ->
  if Cats =:= [] ->
    my_server:reply(From, make_cat(Name, Color, Description)),
    Cats;
  Cats =/= [] ->
    my_server:reply(From, hd(Cats)),
    tl(Cats)
  end;
handle_call(terminate, From, Cats) ->
  my_server:reply(From, ok),
  terminate(Cats).

handle_cast({return, Cat = #cat{}}, Cats) ->
  [Cat| Cats].

make_cat(Name, Color, Description) ->
 #cat{name=Name, color=Color, description=Description}.

terminate(Cats) ->
  [io:format("~p was set free~n", [C#cat.name]) || C <- Cats],
  exit(normal).
