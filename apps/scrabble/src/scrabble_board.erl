-module(scrabble_board).

-include("scrabble.hrl").

-export([
    init_game_board/0
]).

init_game_board() ->
    lists:foldl(
        fun(X, XAcc) ->
            XAcc#{ X => lists:foldl(
                fun(Y, YAcc) ->
                    YAcc#{ Y => ?BOARD_TILE_UNSET }
                end,
                #{},
                lists:seq(1, 15))
            }
        end,
        #{},
        lists:seq(1, 15)
    ).