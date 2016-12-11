-module(curling_scoreboard_hw).
-export ([add_point/1, next_round/0, set_teams/2, reset_board/0]).

set_teams(TeamA, TeamB) ->
  io:format("Team ~s vs Team ~s~n", [TeamA, TeamB]).

next_round() ->
  io:format("Next rount~n").

add_point(Team) ->
  io:format("Team ~s scores!~n", [Team]).

reset_board() ->
  io:format("Board are reseted~n").
