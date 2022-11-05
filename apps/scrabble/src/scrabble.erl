-module(scrabble).

-type game_id() :: pos_integer().
-type player_game_details() :: #{
    hand => iolist(),
    owned_tiles => iolist(),
    score => pos_integer()
}.
% -type players() :: list({player(), player_game_details()}).
-type player_maps() :: #{
    player_id() => #{
        hand => xxx,
        owned_tiles => xxx,
        score => xxx
    }
}.
-type player_id() :: binary().
-type player_id_list() :: list( player_id() ).
-type letters() :: list(letter()).
-type letter() :: iolist().
-type board_letter() :: #{ x => pos_integer(), y => pos_integer(), value => letter() }.
-type word() :: list(board_letter()).
-type board() :: #{
    1 =>
        #{1 => letter(),2 => letter(),3 => letter(),4 => letter(),
          5 => letter(),6 => letter(),7 => letter(),8 => letter(),
          9 => letter(),10 => letter(),11 => letter(),12 => letter(),
          13 => letter(),14 => letter(),15 => letter()},
    2 =>
        #{1 => letter(),2 => letter(),3 => letter(),4 => letter(),
          5 => letter(),6 => letter(),7 => letter(),8 => letter(),
          9 => letter(),10 => letter(),11 => letter(),12 => letter(),
          13 => letter(),14 => letter(),15 => letter()},
    3 =>
        #{1 => letter(),2 => letter(),3 => letter(),4 => letter(),
          5 => letter(),6 => letter(),7 => letter(),8 => letter(),
          9 => letter(),10 => letter(),11 => letter(),12 => letter(),
          13 => letter(),14 => letter(),15 => letter()},
    4 =>
        #{1 => letter(),2 => letter(),3 => letter(),4 => letter(),
          5 => letter(),6 => letter(),7 => letter(),8 => letter(),
          9 => letter(),10 => letter(),11 => letter(),12 => letter(),
          13 => letter(),14 => letter(),15 => letter()},
    5 =>
        #{1 => letter(),2 => letter(),3 => letter(),4 => letter(),
          5 => letter(),6 => letter(),7 => letter(),8 => letter(),
          9 => letter(),10 => letter(),11 => letter(),12 => letter(),
          13 => letter(),14 => letter(),15 => letter()},
    6 =>
        #{1 => letter(),2 => letter(),3 => letter(),4 => letter(),
          5 => letter(),6 => letter(),7 => letter(),8 => letter(),
          9 => letter(),10 => letter(),11 => letter(),12 => letter(),
          13 => letter(),14 => letter(),15 => letter()},
    7 =>
        #{1 => letter(),2 => letter(),3 => letter(),4 => letter(),
          5 => letter(),6 => letter(),7 => letter(),8 => letter(),
          9 => letter(),10 => letter(),11 => letter(),12 => letter(),
          13 => letter(),14 => letter(),15 => letter()},
    8 =>
        #{1 => letter(),2 => letter(),3 => letter(),4 => letter(),
          5 => letter(),6 => letter(),7 => letter(),8 => letter(),
          9 => letter(),10 => letter(),11 => letter(),12 => letter(),
          13 => letter(),14 => letter(),15 => letter()},
    9 =>
        #{1 => letter(),2 => letter(),3 => letter(),4 => letter(),
          5 => letter(),6 => letter(),7 => letter(),8 => letter(),
          9 => letter(),10 => letter(),11 => letter(),12 => letter(),
          13 => letter(),14 => letter(),15 => letter()},
    10 =>
        #{1 => letter(),2 => letter(),3 => letter(),4 => letter(),
          5 => letter(),6 => letter(),7 => letter(),8 => letter(),
          9 => letter(),10 => letter(),11 => letter(),12 => letter(),
          13 => letter(),14 => letter(),15 => letter()},
    11 =>
        #{1 => letter(),2 => letter(),3 => letter(),4 => letter(),
          5 => letter(),6 => letter(),7 => letter(),8 => letter(),
          9 => letter(),10 => letter(),11 => letter(),12 => letter(),
          13 => letter(),14 => letter(),15 => letter()},
    12 =>
        #{1 => letter(),2 => letter(),3 => letter(),4 => letter(),
          5 => letter(),6 => letter(),7 => letter(),8 => letter(),
          9 => letter(),10 => letter(),11 => letter(),12 => letter(),
          13 => letter(),14 => letter(),15 => letter()},
    13 =>
        #{1 => letter(),2 => letter(),3 => letter(),4 => letter(),
          5 => letter(),6 => letter(),7 => letter(),8 => letter(),
          9 => letter(),10 => letter(),11 => letter(),12 => letter(),
          13 => letter(),14 => letter(),15 => letter()},
    14 =>
        #{1 => letter(),2 => letter(),3 => letter(),4 => letter(),
          5 => letter(),6 => letter(),7 => letter(),8 => letter(),
          9 => letter(),10 => letter(),11 => letter(),12 => letter(),
          13 => letter(),14 => letter(),15 => letter()},
    15 =>
        #{1 => letter(),2 => letter(),3 => letter(),4 => letter(),
          5 => letter(),6 => letter(),7 => letter(),8 => letter(),
          9 => letter(),10 => letter(),11 => letter(),12 => letter(),
          13 => letter(),14 => letter(),15 => letter()}
}.

-type scrabble_game_gs_data() :: #{
    tile_bag => letters(),
    players => player_maps(),
    board => board(),
    board_empty => boolean()
}.

-export_type([
    game_id/0,
    player_id_list/0,

    %% scrabble game
    scrabble_game_gs_data/0
]).