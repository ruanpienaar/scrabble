-module(scrabble).

-type game_id() :: pos_integer().
-type player() :: binary().
-type player_list() :: list( player() ).

-export_type([
    game_id/0,
    player_list/0
]).
