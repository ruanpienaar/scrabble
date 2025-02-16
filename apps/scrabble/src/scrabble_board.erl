-module(scrabble_board).

-include("scrabble.hrl").

-export([
    init_game_board/0,
    tile_distribution/0,
    valid_word_placement/3,
    find_valid_starting_points/2
]).

-spec init_game_board() -> scrabble:board().
init_game_board() ->
    lists:foldl(
        fun(Y, YAcc) ->
            YAcc#{ Y => lists:foldl(
                fun(X, XAcc) ->
                    XAcc#{ X => ?BOARD_TILE_UNSET }
                end,
                #{},
                lists:seq(1, 15))
            }
        end,
        #{},
        lists:seq(1, 15)
    ).

tile_distribution() ->
    duplicate_tiles(
        [{'blank', 2},
         {$e, 12},
         {$a, 9},
         {$i, 9},
         {$o, 8},
         {$n, 6},
         {$r, 6},
         {$t, 6},
         {$l, 4},
         {$s, 4},
         {$u, 4},
         {$d, 4},
         {$g, 3},
         {$b, 2},
         {$c, 2},
         {$m, 2},
         {$p, 2},
         {$f, 2},
         {$h, 2},
         {$v, 2},
         {$w, 2},
         {$y, 2},
         {$k, 1},
         {$j, 1},
         {$x, 1},
         {$q, 1},
         {$z, 1}],
        []
    ).

% No need to reverse, we will shuffle.
duplicate_tiles([], R) ->
    R;
duplicate_tiles([{Type, Num} | T], R) when Num > 0 ->
    duplicate_tiles(
        T,
        lists:foldl(
            fun(I, A) ->
                [I | A]
            end, R, [ Type || _X <- lists:seq(1, Num) ]
        )
    ).

-spec valid_word_placement(scrabble:board(), boolean(), scrabble:word()) -> boolean().
valid_word_placement(Board, _BoardEmpty=false, ProposedWord) ->
    % my first and horrible attempt at matrix checking code...
    % Possibly BREAK once true found.
    lists:any(
        fun
        (#{ y := Y, x := X }) ->
            maps:get(X-1, maps:get(Y, Board, #{}), undefined) /= undefined orelse
            maps:get(X+1, maps:get(Y, Board, #{}), undefined) /= undefined orelse
            maps:get(X, maps:get(Y-1, Board, #{}), undefined) /= undefined orelse
            maps:get(X, maps:get(Y+1, Board, #{}), undefined) /= undefined
        end,
        ProposedWord
    );
%% TODO: check that all tiles are next to each other,
%%       check that tiles form a straight line, vertically/horizontally.
valid_word_placement(_Board, _BoardEmpty=true, ProposedWord) ->
    %% Any tile over starting position
    lists:any(
        fun
        (#{ y := 8, x := 8 }) ->
            true;
        (_) ->
            false
        end,
        ProposedWord
    ).

%% TODO: would it be easier if we had a list of words, and their positions,
%%       instead of scanning the entire board?
find_valid_starting_points(Board, Hand) ->
    ok.