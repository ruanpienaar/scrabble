-module(scrabble_game_tests).
-include_lib("eunit/include/eunit.hrl").

%% -------------------------------------------
%% testing a instantiated gen_server instance

instance_test_() ->
    {setup,
     fun() ->
        ok
     end,
     fun(_) ->
        ok
     end,
     [{"scrabble_game_instance_test -> start_link/1",
        fun start_link/0},
      {"scrabble_game_instance_test -> player_start/3",
        fun player_start/0},
      {"scrabble_game_instance_test -> player_take_x_tiles/3",
        fun player_take_x_tiles/0}
     ]
    }.

% Scrabble game 2-4 players
start_link() ->
    ?assertException(
        error,
        function_clause,
        scrabble_game:start_link(1)
    ),
    ?assertException(
        error,
        function_clause,
        scrabble_game:start_link(5)
    ).

player_start() ->
    ok.

player_take_x_tiles() ->
    ?assertException(
        error,
        function_clause,
        scrabble_game:player_take_x_tiles(self(), 1, 10)
    ).

%% -------------------------------------------
%% Testing Gen Server callbacks

gs_callbacks_test_() ->
    {setup,
     fun() ->
        ok
     end,
     fun(_) ->
        ok
     end,
     [{"scrabble_game_gs_callbacks_test -> init/1",
        fun init/0},
      {"scrabble_game_gs_callbacks_test -> player_take_x_tiles_handle_call/3",
        fun player_take_x_tiles_handle_call/0},
      {"scrabble_game_gs_callbacks_test -> player_take_x_tiles/2",
        fun player_take_x_tiles/0}
      % handle_cast
      % handle_info
      % terminate
      % code_change
     ]
    }.

init() ->
    ?assertMatch(
        {ok, #{players := [{2,#{hand := [],owned_tiles := [],score := 0}},
                           {1,#{hand := [],owned_tiles := [],score := 0}}],
               tile_bag := _,
               board := []
             }
        },
        scrabble_game:init(2)
    ).

player_take_x_tiles_handle_call() ->
    State =
    #{ players =>
            [{2,#{hand => [],owned_tiles => [],score => 0}},
             {1,#{hand => [],owned_tiles => [],score => 0}}],
       tile_bag =>
            [97,105,114,blank,105,110,111,116,114,110,109,101,
            120,117,105,97,111,97,109,122,115,102,114,104,116,
            99,119,121,115,103,115,99,100,101,110,111,118,101,
            101,100,112,114,116,103,105,111,119,97,104,101,
            101,118,100,108,97,110,111,113,106,107,101,110,
            111,117,116,112,108,114,115,114,108,101,97,98,105,
            111,105,97,116,101,97,101,100,121,101,97,102,
            blank,108,105,116,110,111,105,117,98,105,101,117,
            103],
       board => []
    },
    State1 =
    #{ players =>
            [{2,#{hand => [],owned_tiles => [],score => 0}},
             {1,#{hand => [97,105,114,blank,105,110,111],owned_tiles => [],score => 0}}],
       tile_bag =>
            [116,114,110,109,101,
            120,117,105,97,111,97,109,122,115,102,114,104,116,
            99,119,121,115,103,115,99,100,101,110,111,118,101,
            101,100,112,114,116,103,105,111,119,97,104,101,
            101,118,100,108,97,110,111,113,106,107,101,110,
            111,117,116,112,108,114,115,114,108,101,97,98,105,
            111,105,97,116,101,97,101,100,121,101,97,102,
            blank,108,105,116,110,111,105,117,98,105,101,117,
            103],
       board => []
    },
    ?assertEqual(
        {reply, ok, State1},
        scrabble_game:handle_call({player_take_x_tiles, 1, 7}, {pid, ref}, State)
    ).

player_take_x_tiles() ->
    State =
    #{ players =>
            [{2,#{hand => [],owned_tiles => [],score => 0}},
             {1,#{hand => [97,105,114,blank,105,110,111],owned_tiles => [],score => 0}}],
       tile_bag =>
            [116,114,110,109,101,
            120,117,105,97,111,97,109,122,115,102,114,104,116,
            99,119,121,115,103,115,99,100,101,110,111,118,101,
            101,100,112,114,116,103,105,111,119,97,104,101,
            101,118,100,108,97,110,111,113,106,107,101,110,
            111,117,116,112,108,114,115,114,108,101,97,98,105,
            111,105,97,116,101,97,101,100,121,101,97,102,
            blank,108,105,116,110,111,105,117,98,105,101,117,
            103],
       board => []
    },
    State1 =
    #{ players =>
            [{2,#{hand => [],owned_tiles => [],score => 0}},
             {1,#{hand => [105,114,blank,105,110,111],owned_tiles => [],score => 0}}],
       tile_bag =>
            [116,114,110,109,101,
            120,117,105,97,111,97,109,122,115,102,114,104,116,
            99,119,121,115,103,115,99,100,101,110,111,118,101,
            101,100,112,114,116,103,105,111,119,97,104,101,
            101,118,100,108,97,110,111,113,106,107,101,110,
            111,117,116,112,108,114,115,114,108,101,97,98,105,
            111,105,97,116,101,97,101,100,121,101,97,102,
            blank,108,105,116,110,111,105,117,98,105,101,117,
            103],
       board => [{8, 8, 97}]
    },
    ?assertEqual(
        State1,
        scrabble_game:handle_call({player_places_tile, 1, 97, 8, 8}, {pid, ref}, State)
    ).

%% -------------------------------------------
%% Simple API unit tests

unit_test_() ->
    {setup,
     fun() ->
        ok = meck:new([rand], [unstick])
     end,
     fun(_) ->
        ?assertEqual(
            [rand],
            lists:sort(meck:unload())
        )
     end,
     [  {"scrabble_game_unit_test -> tile_distribution/0", fun tile_distribution/0},
        {"scrabble_game_unit_test -> duplicate_tiles/1", fun duplicate_tiles/0},
        {"scrabble_game_unit_test -> letter_score/1", fun letter_score/0},
        {"scrabble_game_unit_test -> shuffle_tiles/1", fun shuffle_tiles_1/0},
        {"scrabble_game_unit_test -> shuffle_tiles/2", fun shuffle_tiles_2/0},
        {"scrabble_game_unit_test -> take_random_tile", fun take_random_tile/0},
        {"scrabble_game_unit_test -> players_structs/2,", fun players_structs/0},
        {"scrabble_game_unit_test -> players_struct/1", fun players_struct/0},
        {"scrabble_game_unit_test -> take_x_from_bag_into_player_hand/4", fun take_x_from_bag_into_player_hand/0}
     ]
    }.

tile_distribution() ->
    ?assertEqual(
       [122,
        113,
        120,
        106,
        107,
        121,
        121,
        119,
        119,
        118,
        118,
        104,
        104,
        102,
        102,
        112,
        112,
        109,
        109,
        99,
        99,
        98,
        98,
        103,
        103,
        103,
        100,
        100,
        100,
        100,
        117,
        117,
        117,
        117,
        115,
        115,
        115,
        115,
        108,
        108,
        108,
        108,
        116,
        116,
        116,
        116,
        116,
        116,
        114,
        114,
        114,
        114,
        114,
        114,
        110,
        110,
        110,
        110,
        110,
        110,
        111,
        111,
        111,
        111,
        111,
        111,
        111,
        111,
        105,
        105,
        105,
        105,
        105,
        105,
        105,
        105,
        105,
        97,
        97,
        97,
        97,
        97,
        97,
        97,
        97,
        97,
        101,
        101,
        101,
        101,
        101,
        101,
        101,
        101,
        101,
        101,
        101,
        101,
        blank,
        blank
        ],
        scrabble_game:tile_distribution()
    ).

letter_score() ->
    ?assertEqual(
        0,
        scrabble_game:letter_score('blank')
    ),
    ?assertEqual(
        1,
        scrabble_game:letter_score($a)
    ),
    ?assertEqual(
        2,
        scrabble_game:letter_score($d)
    ),
    ?assertEqual(
        3,
        scrabble_game:letter_score($c)
    ),
    ?assertEqual(
        4,
        scrabble_game:letter_score($y)
    ),
    ?assertEqual(
        5,
        scrabble_game:letter_score($k)
    ),
    ?assertEqual(
        8,
        scrabble_game:letter_score($x)
    ),
    ?assertEqual(
        10,
        scrabble_game:letter_score($q)
    ).

duplicate_tiles() ->
    ?assertEqual(
        [122,97,97,97,blank],
        scrabble_game:duplicate_tiles([{blank, 1}, {$a, 3}, {$z, 1}], [])
    ).

shuffle_tiles_1() ->
    % Make sure to take first every time.
    ok = meck:expect(rand, uniform, 0, 0.21736630901077447),

    ?assertEqual(
        [5,4,3,2,1],
        scrabble_game:shuffle_tiles([1,2,3,4,5])
    ),

    % Make sure to take the last every time
    ok = meck:expect(rand, uniform, 0, 0.9484473283587827),
    ?assertEqual(
        [1,2,3,4,5],
        scrabble_game:shuffle_tiles([1,2,3,4,5])
    ),

    % Copy pasted from the shell...
    ok = meck:expect(rand, uniform, 0, 0.6484473283587827),
    ?assertEqual(
        [5,1,2,4,3],
        scrabble_game:shuffle_tiles([1,2,3,4,5])
    ),

    % Copy pasted from the shell...
    ok = meck:expect(rand, uniform, 0, 0.5561402672724101),
    ?assertEqual(
        [5,1,4,2,3],
        scrabble_game:shuffle_tiles([1,2,3,4,5])
    ).

shuffle_tiles_2() ->
    % Make sure to take first every time.
    ok = meck:expect(rand, uniform, 0, 0.21736630901077447),

    ?assertEqual(
        [5,4,3,2,1],
        scrabble_game:shuffle_tiles([1,2,3,4,5], [])
    ),

    % Make sure to take the last every time
    ok = meck:expect(rand, uniform, 0, 0.9484473283587827),
    ?assertEqual(
        [1,2,3,4,5],
        scrabble_game:shuffle_tiles([1,2,3,4,5], [])
    ),

    % Copy pasted from the shell...
    ok = meck:expect(rand, uniform, 0, 0.6484473283587827),
    ?assertEqual(
        [5,1,2,4,3],
        scrabble_game:shuffle_tiles([1,2,3,4,5], [])
    ),

    % Copy pasted from the shell...
    ok = meck:expect(rand, uniform, 0, 0.5561402672724101),
    ?assertEqual(
        [5,1,4,2,3],
        scrabble_game:shuffle_tiles([1,2,3,4,5], [])
    ).

take_random_tile() ->
    ok = meck:expect(rand, uniform, 0, 0.21736630901077447),
    ?assertEqual(
        {1, [2,3,4,5]},
        scrabble_game:take_random_tile([1,2,3,4,5])
    ),
    ok = meck:expect(rand, uniform, 0, 0.8484473283587827),
    ?assertEqual(
        {4, [1,2,3,5]},
        scrabble_game:take_random_tile([1,2,3,4,5])
    ),
    ok = meck:expect(rand, uniform, 0, 0.5),
    ?assertEqual(
        {3, [1,2,4,5]},
        scrabble_game:take_random_tile([1,2,3,4,5])
    ),

    ok = meck:expect(rand, uniform, 0, 0.9775585942657209),
    ?assertEqual(
        {5, [2]},
        scrabble_game:take_random_tile([2, 5])
    ),

    ok = meck:expect(rand, uniform, 0, 0.21736630901077447),
    ?assertEqual(
        {2, [5]},
        scrabble_game:take_random_tile([2, 5])
    ),

    ok = meck:expect(rand, uniform, 0, 0.7118513280122967),
    ?assertEqual(
        {2, []},
        scrabble_game:take_random_tile([2])
    ),

    ?assertException(
        error,
        function_clause,
        scrabble_game:take_random_tile([])
    ).

players_structs() ->
    ?assertEqual(
        [{1,#{score => 0, hand => [], owned_tiles => []}}],
        scrabble_game:players_structs(1, [])
    ),

    ?assertEqual(
        [{3,#{score => 0, hand => [], owned_tiles => []}},
         {2,#{score => 0, hand => [], owned_tiles => []}},
         {1,#{score => 0, hand => [], owned_tiles => []}}],
        scrabble_game:players_structs(3, [])
    ).

players_struct() ->
    ?assertEqual(
        {1,#{score => 0, hand => [], owned_tiles => []}},
        scrabble_game:players_struct(1)
    ).

take_x_from_bag_into_player_hand() ->
    % 1 Player
    ?assertEqual(
        {
            [116,114,110,109,101,
             120,117,105,97,111,97,109,122,115,102,114,104,116,
             99,119,121,115,103,115,99,100,101,110,111,118,101,
             101,100,112,114,116,103,105,111,119,97,104,101,
             101,118,100,108,97,110,111,113,106,107,101,110,
             111,117,116,112,108,114,115,114,108,101,97,98,105,
             111,105,97,116,101,97,101,100,121,101,97,102,
             blank,108,105,116,110,111,105,117,98,105,101,117,
             103],
            [{1, #{score => 0, hand => [97,105,114,blank,105,110,111], owned_tiles => []}}]
        },
        scrabble_game:take_x_from_bag_into_player_hand(
            1,
            7,
            [{1,#{score => 0, hand => [], owned_tiles => []}}],
            [97,105,114,blank,105,110,111,116,114,110,109,101,
             120,117,105,97,111,97,109,122,115,102,114,104,116,
             99,119,121,115,103,115,99,100,101,110,111,118,101,
             101,100,112,114,116,103,105,111,119,97,104,101,
             101,118,100,108,97,110,111,113,106,107,101,110,
             111,117,116,112,108,114,115,114,108,101,97,98,105,
             111,105,97,116,101,97,101,100,121,101,97,102,
             blank,108,105,116,110,111,105,117,98,105,101,117,
             103]
            )
    ),

    % 3 Players - with player already having tiles in hand.
    ?assertEqual(
        {
            [116,114,110,109,101,120,117,105,97,111,97,109,122,115,102,
             114,104,116,99,119,121,115,103,115,99,100,101,110,111,118,
             101,101,100,112,114,116,103,105,111,119,97,104,101,101,118,
             100,108,97,110,111,113,106,107,101,110,111,117,116,112,108,
             114,115,114,108,101,97,98,105,111,105,97,116,101,97,101,
             100,121,101,97,102,blank,108,105,116,110,111,105,117,98,
             105,101,117,103],
            [{3,#{hand => [],owned_tiles => [],score => 0}},
             {2,#{hand => [105,110,111,97,105,114,blank], owned_tiles => [],score => 0}},
             {1,#{hand => [],owned_tiles => [],score => 0}}]
        },
        scrabble_game:take_x_from_bag_into_player_hand(
            2,
            3,
            [{3,#{score => 0, hand => [], owned_tiles => []}},
             {2,#{score => 0, hand => [97,105,114,blank], owned_tiles => []}},
             {1,#{score => 0, hand => [], owned_tiles => []}}],
            [105,110,111,116,114,110,109,101,
             120,117,105,97,111,97,109,122,115,102,114,104,116,
             99,119,121,115,103,115,99,100,101,110,111,118,101,
             101,100,112,114,116,103,105,111,119,97,104,101,
             101,118,100,108,97,110,111,113,106,107,101,110,
             111,117,116,112,108,114,115,114,108,101,97,98,105,
             111,105,97,116,101,97,101,100,121,101,97,102,
             blank,108,105,116,110,111,105,117,98,105,101,117,
             103]
        )
    ),

    %% TODO: Not sure if we should crash, but if bad amount, don't change hand.
    ?assertEqual(
        {
            [105,110,111,116,114,110,109,101,
             120,117,105,97,111,97,109,122,115,102,114,104,116,
             99,119,121,115,103,115,99,100,101,110,111,118,101,
             101,100,112,114,116,103,105,111,119,97,104,101,
             101,118,100,108,97,110,111,113,106,107,101,110,
             111,117,116,112,108,114,115,114,108,101,97,98,105,
             111,105,97,116,101,97,101,100,121,101,97,102,
             blank,108,105,116,110,111,105,117,98,105,101,117,
             103],
            [{3,#{hand => [],owned_tiles => [],score => 0}},
             {2,#{hand => [97,105,114,blank], owned_tiles => [],score => 0}},
             {1,#{hand => [],owned_tiles => [],score => 0}}]
        },
        scrabble_game:take_x_from_bag_into_player_hand(
            2,
            7,
            [{3,#{score => 0, hand => [], owned_tiles => []}},
             {2,#{score => 0, hand => [97,105,114,blank], owned_tiles => []}},
             {1,#{score => 0, hand => [], owned_tiles => []}}],
            [105,110,111,116,114,110,109,101,
             120,117,105,97,111,97,109,122,115,102,114,104,116,
             99,119,121,115,103,115,99,100,101,110,111,118,101,
             101,100,112,114,116,103,105,111,119,97,104,101,
             101,118,100,108,97,110,111,113,106,107,101,110,
             111,117,116,112,108,114,115,114,108,101,97,98,105,
             111,105,97,116,101,97,101,100,121,101,97,102,
             blank,108,105,116,110,111,105,117,98,105,101,117,
             103]
        )
    ).