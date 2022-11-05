-module(scrabble_game_tests).
-include_lib("eunit/include/eunit.hrl").

-define(GAME_ID, 1).

%% TODO: get example EMPTY STATE function.

%% -------------------------------------------
%% testing a instantiated gen_server instance

instance_test_() ->
    {setup,
     fun() ->
        ok
     end,
     fun(_) ->
        is_pid(Pid = whereis(scrabble_game:name(?GAME_ID))) andalso
            erlang:unlink(Pid) andalso
            erlang:exit(Pid, kill)
     end,
     [{"scrabble_game_instance_test -> start_link/1",
        fun start_link/0}
      % {"scrabble_game_instance_test -> player_take_x_tiles/3",
      %   fun gs_instance_player_take_x_tiles/0}
     ]
    }.

% Scrabble game 1-4 players
start_link() ->
    ?assertException( %% Too little
        error,
        function_clause,
        scrabble_game:start_link(?GAME_ID, [])
    ),
    ?assertException( %% Too many
        error,
        function_clause,
        scrabble_game:start_link(?GAME_ID, [
            <<"playerid1">>,
            <<"playerid2">>,
            <<"playerid3">>,
            <<"playerid4">>,
            <<"playerid5">>
        ])
    ),
    ?assertMatch( %% ok
        {ok, _},
        scrabble_game:start_link(?GAME_ID, [
            <<"playerid1">>,
            <<"playerid2">>
        ])
    ),

    ProcStateData = sys:get_state(
        scrabble_game:name(?GAME_ID)
    ),
    ?assertMatch(
        #{board :=
               #{1 :=
                     #{1 := undefined,2 := undefined,3 := undefined,4 := undefined,
                       5 := undefined,6 := undefined,7 := undefined,8 := undefined,
                       9 := undefined,10 := undefined,11 := undefined,12 := undefined,
                       13 := undefined,14 := undefined,15 := undefined},
                 2 :=
                     #{1 := undefined,2 := undefined,3 := undefined,4 := undefined,
                       5 := undefined,6 := undefined,7 := undefined,8 := undefined,
                       9 := undefined,10 := undefined,11 := undefined,12 := undefined,
                       13 := undefined,14 := undefined,15 := undefined},
                 3 :=
                     #{1 := undefined,2 := undefined,3 := undefined,4 := undefined,
                       5 := undefined,6 := undefined,7 := undefined,8 := undefined,
                       9 := undefined,10 := undefined,11 := undefined,12 := undefined,
                       13 := undefined,14 := undefined,15 := undefined},
                 4 :=
                     #{1 := undefined,2 := undefined,3 := undefined,4 := undefined,
                       5 := undefined,6 := undefined,7 := undefined,8 := undefined,
                       9 := undefined,10 := undefined,11 := undefined,12 := undefined,
                       13 := undefined,14 := undefined,15 := undefined},
                 5 :=
                     #{1 := undefined,2 := undefined,3 := undefined,4 := undefined,
                       5 := undefined,6 := undefined,7 := undefined,8 := undefined,
                       9 := undefined,10 := undefined,11 := undefined,12 := undefined,
                       13 := undefined,14 := undefined,15 := undefined},
                 6 :=
                     #{1 := undefined,2 := undefined,3 := undefined,4 := undefined,
                       5 := undefined,6 := undefined,7 := undefined,8 := undefined,
                       9 := undefined,10 := undefined,11 := undefined,12 := undefined,
                       13 := undefined,14 := undefined,15 := undefined},
                 7 :=
                     #{1 := undefined,2 := undefined,3 := undefined,4 := undefined,
                       5 := undefined,6 := undefined,7 := undefined,8 := undefined,
                       9 := undefined,10 := undefined,11 := undefined,12 := undefined,
                       13 := undefined,14 := undefined,15 := undefined},
                 8 :=
                     #{1 := undefined,2 := undefined,3 := undefined,4 := undefined,
                       5 := undefined,6 := undefined,7 := undefined,8 := undefined,
                       9 := undefined,10 := undefined,11 := undefined,12 := undefined,
                       13 := undefined,14 := undefined,15 := undefined},
                 9 :=
                     #{1 := undefined,2 := undefined,3 := undefined,4 := undefined,
                       5 := undefined,6 := undefined,7 := undefined,8 := undefined,
                       9 := undefined,10 := undefined,11 := undefined,12 := undefined,
                       13 := undefined,14 := undefined,15 := undefined},
                 10 :=
                     #{1 := undefined,2 := undefined,3 := undefined,4 := undefined,
                       5 := undefined,6 := undefined,7 := undefined,8 := undefined,
                       9 := undefined,10 := undefined,11 := undefined,12 := undefined,
                       13 := undefined,14 := undefined,15 := undefined},
                 11 :=
                     #{1 := undefined,2 := undefined,3 := undefined,4 := undefined,
                       5 := undefined,6 := undefined,7 := undefined,8 := undefined,
                       9 := undefined,10 := undefined,11 := undefined,12 := undefined,
                       13 := undefined,14 := undefined,15 := undefined},
                 12 :=
                     #{1 := undefined,2 := undefined,3 := undefined,4 := undefined,
                       5 := undefined,6 := undefined,7 := undefined,8 := undefined,
                       9 := undefined,10 := undefined,11 := undefined,12 := undefined,
                       13 := undefined,14 := undefined,15 := undefined},
                 13 :=
                     #{1 := undefined,2 := undefined,3 := undefined,4 := undefined,
                       5 := undefined,6 := undefined,7 := undefined,8 := undefined,
                       9 := undefined,10 := undefined,11 := undefined,12 := undefined,
                       13 := undefined,14 := undefined,15 := undefined},
                 14 :=
                     #{1 := undefined,2 := undefined,3 := undefined,4 := undefined,
                       5 := undefined,6 := undefined,7 := undefined,8 := undefined,
                       9 := undefined,10 := undefined,11 := undefined,12 := undefined,
                       13 := undefined,14 := undefined,15 := undefined},
                 15 :=
                     #{1 := undefined,2 := undefined,3 := undefined,4 := undefined,
                       5 := undefined,6 := undefined,7 := undefined,8 := undefined,
                       9 := undefined,10 := undefined,11 := undefined,12 := undefined,
                       13 := undefined,14 := undefined,15 := undefined}},
           board_empty := true,
           players := #{
                <<"playerid1">> :=
                    #{hand := _, owned_tiles := [], score := 0},
                <<"playerid2">> :=
                    #{hand := _, owned_tiles := [], score := 0}
           },
           tile_bag :=
               _
        },
        ProcStateData
    ),

    ?assertEqual(
        86,
        length(maps:get(tile_bag, ProcStateData))
    ),
    ?assertEqual(
        7,
        length(maps:get(hand, maps:get(<<"playerid1">>, maps:get(players, ProcStateData))))
    ),
    ?assertEqual(
        7,
        length(maps:get(hand, maps:get(<<"playerid2">>, maps:get(players, ProcStateData))))
    ).

% gs_instance_player_take_x_tiles() ->
%     ?assertException(
%         error,
%         function_clause,
%         scrabble_game:player_take_x_tiles(self(), 1, 10)
%     ).

%% -------------------------------------------
%% Testing Gen Server callbacks

gs_callbacks_test_() ->
    {setup,
     fun() ->
        ok
     end,
     fun(_) ->
        % is_pid(Pid = whereis(scrabble_game:name(?GAME_ID))) andalso
        %     erlang:unlink(Pid) andalso
        %     erlang:exit(Pid, kill)
        ok
     end,
     [{"scrabble_game_gs_callbacks_test -> init/1",
        fun init/0},
      {"scrabble_game_gs_callbacks_test -> get_game_details/1",
        fun get_game_details/0},
      {"scrabble_game_gs_callbacks_test -> place_word/3",
        fun place_word/0}
      % {"scrabble_game_gs_callbacks_test -> player_take_x_tiles_handle_call/3",
      %   fun player_take_x_tiles_handle_call/0}
      % {"scrabble_game_gs_callbacks_test -> player_take_x_tiles/2",
      %   fun player_take_x_tiles/0}
      % handle_cast
      % handle_info
      % terminate
      % code_change
     ]
    }.

init() ->
    ?assertMatch(
        {
            ok,
            #{
                players := #{
                    <<"playerid1">> :=
                        #{hand := _, owned_tiles := [], score := 0},
                    <<"playerid2">> :=
                        #{hand := _, owned_tiles := [], score := 0}
               },
                tile_bag := _,
                board := _,
                board_empty := true
             }
        },
        scrabble_game:init([
            <<"playerid1">>,
            <<"playerid2">>
        ])
    ).

get_game_details() ->
    {ok, State} = scrabble_game:init([
        <<"playerid1">>,
        <<"playerid2">>
    ]),
    ?assertMatch(
        {reply,#{board :=
                  #{1 :=
                        #{1 := undefined,2 := undefined,
                          3 := undefined,4 := undefined,
                          5 := undefined,6 := undefined,
                          7 := undefined,8 := undefined,
                          9 := undefined,10 := undefined,
                          11 := undefined,12 := undefined,
                          13 := undefined,14 := undefined,
                          15 := undefined},
                    2 :=
                        #{1 := undefined,2 := undefined,
                          3 := undefined,4 := undefined,
                          5 := undefined,6 := undefined,
                          7 := undefined,8 := undefined,
                          9 := undefined,10 := undefined,
                          11 := undefined,12 := undefined,
                          13 := undefined,14 := undefined,
                          15 := undefined},
                    3 :=
                        #{1 := undefined,2 := undefined,
                          3 := undefined,4 := undefined,
                          5 := undefined,6 := undefined,
                          7 := undefined,8 := undefined,
                          9 := undefined,10 := undefined,
                          11 := undefined,12 := undefined,
                          13 := undefined,14 := undefined,
                          15 := undefined},
                    4 :=
                        #{1 := undefined,2 := undefined,
                          3 := undefined,4 := undefined,
                          5 := undefined,6 := undefined,
                          7 := undefined,8 := undefined,
                          9 := undefined,10 := undefined,
                          11 := undefined,12 := undefined,
                          13 := undefined,14 := undefined,
                          15 := undefined},
                    5 :=
                        #{1 := undefined,2 := undefined,
                          3 := undefined,4 := undefined,
                          5 := undefined,6 := undefined,
                          7 := undefined,8 := undefined,
                          9 := undefined,10 := undefined,
                          11 := undefined,12 := undefined,
                          13 := undefined,14 := undefined,
                          15 := undefined},
                    6 :=
                        #{1 := undefined,2 := undefined,
                          3 := undefined,4 := undefined,
                          5 := undefined,6 := undefined,
                          7 := undefined,8 := undefined,
                          9 := undefined,10 := undefined,
                          11 := undefined,12 := undefined,
                          13 := undefined,14 := undefined,
                          15 := undefined},
                    7 :=
                        #{1 := undefined,2 := undefined,
                          3 := undefined,4 := undefined,
                          5 := undefined,6 := undefined,
                          7 := undefined,8 := undefined,
                          9 := undefined,10 := undefined,
                          11 := undefined,12 := undefined,
                          13 := undefined,14 := undefined,
                          15 := undefined},
                    8 :=
                        #{1 := undefined,2 := undefined,
                          3 := undefined,4 := undefined,
                          5 := undefined,6 := undefined,
                          7 := undefined,8 := undefined,
                          9 := undefined,10 := undefined,
                          11 := undefined,12 := undefined,
                          13 := undefined,14 := undefined,
                          15 := undefined},
                    9 :=
                        #{1 := undefined,2 := undefined,
                          3 := undefined,4 := undefined,
                          5 := undefined,6 := undefined,
                          7 := undefined,8 := undefined,
                          9 := undefined,10 := undefined,
                          11 := undefined,12 := undefined,
                          13 := undefined,14 := undefined,
                          15 := undefined},
                    10 :=
                        #{1 := undefined,2 := undefined,
                          3 := undefined,4 := undefined,
                          5 := undefined,6 := undefined,
                          7 := undefined,8 := undefined,
                          9 := undefined,10 := undefined,
                          11 := undefined,12 := undefined,
                          13 := undefined,14 := undefined,
                          15 := undefined},
                    11 :=
                        #{1 := undefined,2 := undefined,
                          3 := undefined,4 := undefined,
                          5 := undefined,6 := undefined,
                          7 := undefined,8 := undefined,
                          9 := undefined,10 := undefined,
                          11 := undefined,12 := undefined,
                          13 := undefined,14 := undefined,
                          15 := undefined},
                    12 :=
                        #{1 := undefined,2 := undefined,
                          3 := undefined,4 := undefined,
                          5 := undefined,6 := undefined,
                          7 := undefined,8 := undefined,
                          9 := undefined,10 := undefined,
                          11 := undefined,12 := undefined,
                          13 := undefined,14 := undefined,
                          15 := undefined},
                    13 :=
                        #{1 := undefined,2 := undefined,
                          3 := undefined,4 := undefined,
                          5 := undefined,6 := undefined,
                          7 := undefined,8 := undefined,
                          9 := undefined,10 := undefined,
                          11 := undefined,12 := undefined,
                          13 := undefined,14 := undefined,
                          15 := undefined},
                    14 :=
                        #{1 := undefined,2 := undefined,
                          3 := undefined,4 := undefined,
                          5 := undefined,6 := undefined,
                          7 := undefined,8 := undefined,
                          9 := undefined,10 := undefined,
                          11 := undefined,12 := undefined,
                          13 := undefined,14 := undefined,
                          15 := undefined},
                    15 :=
                        #{1 := undefined,2 := undefined,
                          3 := undefined,4 := undefined,
                          5 := undefined,6 := undefined,
                          7 := undefined,8 := undefined,
                          9 := undefined,10 := undefined,
                          11 := undefined,12 := undefined,
                          13 := undefined,14 := undefined,
                          15 := undefined}},
              players :=
                  #{<<"playerid1">> :=
                        #{hand := _,owned_tiles := [],
                          score := 0},
                    <<"playerid2">> :=
                        #{hand := _,owned_tiles := [],
                          score := 0}}},
            #{board :=
                  #{1 :=
                        #{1 := undefined,2 := undefined,
                          3 := undefined,4 := undefined,
                          5 := undefined,6 := undefined,
                          7 := undefined,8 := undefined,
                          9 := undefined,10 := undefined,
                          11 := undefined,12 := undefined,
                          13 := undefined,14 := undefined,
                          15 := undefined},
                    2 :=
                        #{1 := undefined,2 := undefined,
                          3 := undefined,4 := undefined,
                          5 := undefined,6 := undefined,
                          7 := undefined,8 := undefined,
                          9 := undefined,10 := undefined,
                          11 := undefined,12 := undefined,
                          13 := undefined,14 := undefined,
                          15 := undefined},
                    3 :=
                        #{1 := undefined,2 := undefined,
                          3 := undefined,4 := undefined,
                          5 := undefined,6 := undefined,
                          7 := undefined,8 := undefined,
                          9 := undefined,10 := undefined,
                          11 := undefined,12 := undefined,
                          13 := undefined,14 := undefined,
                          15 := undefined},
                    4 :=
                        #{1 := undefined,2 := undefined,
                          3 := undefined,4 := undefined,
                          5 := undefined,6 := undefined,
                          7 := undefined,8 := undefined,
                          9 := undefined,10 := undefined,
                          11 := undefined,12 := undefined,
                          13 := undefined,14 := undefined,
                          15 := undefined},
                    5 :=
                        #{1 := undefined,2 := undefined,
                          3 := undefined,4 := undefined,
                          5 := undefined,6 := undefined,
                          7 := undefined,8 := undefined,
                          9 := undefined,10 := undefined,
                          11 := undefined,12 := undefined,
                          13 := undefined,14 := undefined,
                          15 := undefined},
                    6 :=
                        #{1 := undefined,2 := undefined,
                          3 := undefined,4 := undefined,
                          5 := undefined,6 := undefined,
                          7 := undefined,8 := undefined,
                          9 := undefined,10 := undefined,
                          11 := undefined,12 := undefined,
                          13 := undefined,14 := undefined,
                          15 := undefined},
                    7 :=
                        #{1 := undefined,2 := undefined,
                          3 := undefined,4 := undefined,
                          5 := undefined,6 := undefined,
                          7 := undefined,8 := undefined,
                          9 := undefined,10 := undefined,
                          11 := undefined,12 := undefined,
                          13 := undefined,14 := undefined,
                          15 := undefined},
                    8 :=
                        #{1 := undefined,2 := undefined,
                          3 := undefined,4 := undefined,
                          5 := undefined,6 := undefined,
                          7 := undefined,8 := undefined,
                          9 := undefined,10 := undefined,
                          11 := undefined,12 := undefined,
                          13 := undefined,14 := undefined,
                          15 := undefined},
                    9 :=
                        #{1 := undefined,2 := undefined,
                          3 := undefined,4 := undefined,
                          5 := undefined,6 := undefined,
                          7 := undefined,8 := undefined,
                          9 := undefined,10 := undefined,
                          11 := undefined,12 := undefined,
                          13 := undefined,14 := undefined,
                          15 := undefined},
                    10 :=
                        #{1 := undefined,2 := undefined,
                          3 := undefined,4 := undefined,
                          5 := undefined,6 := undefined,
                          7 := undefined,8 := undefined,
                          9 := undefined,10 := undefined,
                          11 := undefined,12 := undefined,
                          13 := undefined,14 := undefined,
                          15 := undefined},
                    11 :=
                        #{1 := undefined,2 := undefined,
                          3 := undefined,4 := undefined,
                          5 := undefined,6 := undefined,
                          7 := undefined,8 := undefined,
                          9 := undefined,10 := undefined,
                          11 := undefined,12 := undefined,
                          13 := undefined,14 := undefined,
                          15 := undefined},
                    12 :=
                        #{1 := undefined,2 := undefined,
                          3 := undefined,4 := undefined,
                          5 := undefined,6 := undefined,
                          7 := undefined,8 := undefined,
                          9 := undefined,10 := undefined,
                          11 := undefined,12 := undefined,
                          13 := undefined,14 := undefined,
                          15 := undefined},
                    13 :=
                        #{1 := undefined,2 := undefined,
                          3 := undefined,4 := undefined,
                          5 := undefined,6 := undefined,
                          7 := undefined,8 := undefined,
                          9 := undefined,10 := undefined,
                          11 := undefined,12 := undefined,
                          13 := undefined,14 := undefined,
                          15 := undefined},
                    14 :=
                        #{1 := undefined,2 := undefined,
                          3 := undefined,4 := undefined,
                          5 := undefined,6 := undefined,
                          7 := undefined,8 := undefined,
                          9 := undefined,10 := undefined,
                          11 := undefined,12 := undefined,
                          13 := undefined,14 := undefined,
                          15 := undefined},
                    15 :=
                        #{1 := undefined,2 := undefined,
                          3 := undefined,4 := undefined,
                          5 := undefined,6 := undefined,
                          7 := undefined,8 := undefined,
                          9 := undefined,10 := undefined,
                          11 := undefined,12 := undefined,
                          13 := undefined,14 := undefined,
                          15 := undefined}},
              board_empty := true,
              players :=
                  #{<<"playerid1">> :=
                        #{hand := _,owned_tiles := [],
                          score := 0},
                    <<"playerid2">> :=
                        #{hand := _,owned_tiles := [],
                          score := 0}},
              tile_bag :=
                  _}
        },
        scrabble_game:handle_call({get_game_details}, from, State)
    ).

place_word() ->
    {ok, #{
        board := Board,
        tile_bag := TBag
    } = State} = scrabble_game:init([
        <<"playerid1">>
    ]),
    ?assertEqual(
        93,
        length(TBag)
    ),
    PlayerInfo = scrabble_game:handle_call(
        {get_player_info, <<"playerid1">>},
        from,
        State
    ),
    ?assertMatch(
        {
            reply,
            #{ hand := _, owned_tiles := _, score := _ },
            State
        },
        PlayerInfo
    ),
    {reply, #{ hand := Hand }, _} = PlayerInfo,
    %% Bad player id
    ?assertMatch(
        {
            reply,
            {error, bad_spid},
            #{
                board := Board,
                board_empty := true,
                players := #{ <<"playerid1">> := #{ hand := Hand, score := 0 }}
            }
        },
        scrabble_game:handle_call({place_word, <<"xxxx">>, [#{ x => 8, y => 8, value => $x }]}, from, State)
    ),
    NotInHandLetter = hd(find_letter_thats_not_in_hand(Hand)),
    ?debugFmt("Letter ~p not in hand (~p).", [NotInHandLetter, Hand]),
    %% Letters not from hand!
    ?assertMatch(
        {
            reply,
            {error, proposed_word_not_in_hand},
            #{
                board := Board,
                board_empty := true,
                players := #{ <<"playerid1">> := #{ hand := Hand, score := 0 }}
            }
        },
        scrabble_game:handle_call({place_word, <<"playerid1">>, [#{ x => 1, y => 1, value => NotInHandLetter }]}, from, State)
    ),
    FirstLetterInHand = hd(Hand),
    ?debugFmt("First letter in hand ~p", [FirstLetterInHand]),
    %% Starting position incorrect
    ?assertMatch(
        {
            reply,
            {error, not_starting_square},
            #{
                board := Board,
                board_empty := true,
                players := #{ <<"playerid1">> := #{ hand := Hand, score := 0 }}
            }
        },
        scrabble_game:handle_call({place_word, <<"playerid1">>, [#{ x => 1, y => 1, value => FirstLetterInHand }]}, from, State)
    ),
    SucessPlacedWord1 = scrabble_game:handle_call(
        {
            place_word,
            <<"playerid1">>,
            element(1,
                lists:foldl(
                    fun(HandLetter, {Acc, Pos}) ->
                        {lists:append(Acc, [#{ x => Pos, y => 8, value => HandLetter }]), Pos+1}
                    end,
                    {[], 8},
                    Hand
                )
            )
        },
        from,
        State
    ),
    ?assertMatch(
        {
            reply,
            ok,
            #{
                board := _,
                board_empty := false,
                players := #{ <<"playerid1">> := #{ hand := Hand2, score := Score2 }}
            }
        } when Hand2 /= Hand andalso
               Score2 /= 0,
        SucessPlacedWord1
    ),
    {reply, ok, State2} = SucessPlacedWord1,
    #{
        board_empty := false,
        board := Board2,
        tile_bag := TileBag2,
        players := #{
            <<"playerid1">> := #{
                hand := Hand2
            }
        }
    } = State2,
    ?assert(maps:get(8, maps:get(8, Board2)) /= undefined ),
    ?assert(maps:get(9, maps:get(8, Board2)) /= undefined ),
    ?assert(maps:get(10, maps:get(8, Board2)) /= undefined ),
    ?assert(maps:get(11, maps:get(8, Board2)) /= undefined ),
    ?assert(maps:get(12, maps:get(8, Board2)) /= undefined ),
    ?assert(maps:get(13, maps:get(8, Board2)) /= undefined ),
    ?assert(maps:get(14, maps:get(8, Board2)) /= undefined ),
    ?assertEqual(
        86,
        length(TileBag2)
    ),

    %% Place adjacent word!
    %% intersect at x=11 y8
    SucessPlacedWord2 = scrabble_game:handle_call(
        {
            place_word,
            <<"playerid1">>,
            element(1,
                lists:foldl(
                    fun(HandLetter, {Acc, Pos}) ->
                        case Pos =:= 8 of
                            true ->
                                {Acc, Pos+1};
                            false ->
                                {lists:append(Acc, [#{ x => 11, y => Pos, value => HandLetter }]), Pos+1}
                        end
                    end,
                    {[], 4},
                    Hand2
                )
            )
        },
        from,
        State2
    ),
    {reply, ok, State3} = SucessPlacedWord2,
    #{
        board_empty := false,
        board := Board3,
        tile_bag := TileBag3,
        players := #{
            <<"playerid1">> := #{
                hand := Hand3
            }
        }
    } = State3,
    ?assert(maps:get(11, maps:get(4, Board3)) /= undefined ),
    ?assert(maps:get(11, maps:get(5, Board3)) /= undefined ),
    ?assert(maps:get(11, maps:get(6, Board3)) /= undefined ),
    ?assert(maps:get(11, maps:get(7, Board3)) /= undefined ),
    ?assert(maps:get(11, maps:get(8, Board3)) /= undefined ),
    ?assert(maps:get(11, maps:get(9, Board3)) /= undefined ),
    ?assert(maps:get(11, maps:get(10, Board3)) /= undefined ),
    ?assertEqual(
        80,
        length(TileBag3)
    ),

    ok.


find_letter_thats_not_in_hand(Hand) ->
    lists:filter(
        fun(L) ->
            not lists:member(L, Hand)
        end,
        lists:seq(97, 122)
    ).

% player_take_x_tiles_handle_call() ->
%     State =
%     #{ players =>
%             [{<<"playerid1">>,#{hand => [],owned_tiles => [],score => 0}},
%              {<<"playerid2">>,#{hand => [],owned_tiles => [],score => 0}}],
%        tile_bag =>
%             [97,105,114,blank,105,110,111,116,114,110,109,101,
%             120,117,105,97,111,97,109,122,115,102,114,104,116,
%             99,119,121,115,103,115,99,100,101,110,111,118,101,
%             101,100,112,114,116,103,105,111,119,97,104,101,
%             101,118,100,108,97,110,111,113,106,107,101,110,
%             111,117,116,112,108,114,115,114,108,101,97,98,105,
%             111,105,97,116,101,97,101,100,121,101,97,102,
%             blank,108,105,116,110,111,105,117,98,105,101,117,
%             103],
%        board => #{},
%        board_empty => true
%     },
%     State1 =
%     #{ players =>
%             [{<<"playerid1">>,#{hand => [97,105,114,blank,105,110,111],owned_tiles => [],score => 0}},
%              {<<"playerid2">>,#{hand => [],owned_tiles => [],score => 0}}],
%        tile_bag =>
%             [116,114,110,109,101,
%             120,117,105,97,111,97,109,122,115,102,114,104,116,
%             99,119,121,115,103,115,99,100,101,110,111,118,101,
%             101,100,112,114,116,103,105,111,119,97,104,101,
%             101,118,100,108,97,110,111,113,106,107,101,110,
%             111,117,116,112,108,114,115,114,108,101,97,98,105,
%             111,105,97,116,101,97,101,100,121,101,97,102,
%             blank,108,105,116,110,111,105,117,98,105,101,117,
%             103],
%        board => #{},
%        board_empty => true
%     },
%     ?assertEqual(
%         {reply, ok, State1},
%         scrabble_game:handle_call({player_take_x_tiles, <<"playerid1">>, 7}, {pid, ref}, State)
%     ).

% player_take_x_tiles() ->
%     State =
%     #{ players =>
%             [{<<"playerid1">>,#{hand => [97,105,114,blank,105,110,111],owned_tiles => [],score => 0}},
%              {<<"playerid2">>,#{hand => [],owned_tiles => [],score => 0}}
%             ],
%        tile_bag =>
%             [116,114,110,109,101,
%             120,117,105,97,111,97,109,122,115,102,114,104,116,
%             99,119,121,115,103,115,99,100,101,110,111,118,101,
%             101,100,112,114,116,103,105,111,119,97,104,101,
%             101,118,100,108,97,110,111,113,106,107,101,110,
%             111,117,116,112,108,114,115,114,108,101,97,98,105,
%             111,105,97,116,101,97,101,100,121,101,97,102,
%             blank,108,105,116,110,111,105,117,98,105,101,117,
%             103],
%        board => []
%     },
%     State1 =
%     #{ players =>
%             [{<<"playerid1">>,#{hand => [105,114,blank,105,110,111],owned_tiles => [],score => 0}},
%              {<<"playerid2">>,#{hand => [],owned_tiles => [],score => 0}}],
%        tile_bag =>
%             [116,114,110,109,101,
%             120,117,105,97,111,97,109,122,115,102,114,104,116,
%             99,119,121,115,103,115,99,100,101,110,111,118,101,
%             101,100,112,114,116,103,105,111,119,97,104,101,
%             101,118,100,108,97,110,111,113,106,107,101,110,
%             111,117,116,112,108,114,115,114,108,101,97,98,105,
%             111,105,97,116,101,97,101,100,121,101,97,102,
%             blank,108,105,116,110,111,105,117,98,105,101,117,
%             103],
%        board => [{8, 8, 1, 97}]
%     },

%     SubmittedBoard = [#{ x => 8, y => 8, value => <<"a">>}],

%     ?assertEqual(
%         {reply, true, State1},
%         scrabble_game:handle_call({place_word, <<"playerid1">>, SubmittedBoard}, {pid, ref}, State)
%     ).

% %% -------------------------------------------
% %% Simple API unit tests

% unit_test_() ->
%     {setup,
%      fun() ->
%         ok = meck:new([rand], [unstick])
%      end,
%      fun(_) ->
%         ?assertEqual(
%             [rand],
%             lists:sort(meck:unload())
%         )
%      end,
%      [  {"scrabble_game_unit_test -> tile_distribution/0", fun tile_distribution/0},
%         {"scrabble_game_unit_test -> duplicate_tiles/1", fun duplicate_tiles/0},
%         {"scrabble_game_unit_test -> letter_score/1", fun letter_score/0},
%         {"scrabble_game_unit_test -> shuffle_tiles/1", fun shuffle_tiles_1/0},
%         {"scrabble_game_unit_test -> shuffle_tiles/2", fun shuffle_tiles_2/0},
%         {"scrabble_game_unit_test -> take_random_tile", fun take_random_tile/0},
%         {"scrabble_game_unit_test -> players_structs/2,", fun players_structs/0},
%         {"scrabble_game_unit_test -> players_struct/1", fun players_struct/0},
%         {"scrabble_game_unit_test -> take_x_from_bag_into_player_hand/4", fun take_x_from_bag_into_player_hand/0}
%      ]
%     }.

% tile_distribution() ->
%     ?assertEqual(
%        [122,
%         113,
%         120,
%         106,
%         107,
%         121,
%         121,
%         119,
%         119,
%         118,
%         118,
%         104,
%         104,
%         102,
%         102,
%         112,
%         112,
%         109,
%         109,
%         99,
%         99,
%         98,
%         98,
%         103,
%         103,
%         103,
%         100,
%         100,
%         100,
%         100,
%         117,
%         117,
%         117,
%         117,
%         115,
%         115,
%         115,
%         115,
%         108,
%         108,
%         108,
%         108,
%         116,
%         116,
%         116,
%         116,
%         116,
%         116,
%         114,
%         114,
%         114,
%         114,
%         114,
%         114,
%         110,
%         110,
%         110,
%         110,
%         110,
%         110,
%         111,
%         111,
%         111,
%         111,
%         111,
%         111,
%         111,
%         111,
%         105,
%         105,
%         105,
%         105,
%         105,
%         105,
%         105,
%         105,
%         105,
%         97,
%         97,
%         97,
%         97,
%         97,
%         97,
%         97,
%         97,
%         97,
%         101,
%         101,
%         101,
%         101,
%         101,
%         101,
%         101,
%         101,
%         101,
%         101,
%         101,
%         101,
%         blank,
%         blank
%         ],
%         scrabble_game:tile_distribution()
%     ).

% letter_score() ->
%     ?assertEqual(
%         0,
%         scrabble_game:letter_score('blank')
%     ),
%     ?assertEqual(
%         1,
%         scrabble_game:letter_score($a)
%     ),
%     ?assertEqual(
%         2,
%         scrabble_game:letter_score($d)
%     ),
%     ?assertEqual(
%         3,
%         scrabble_game:letter_score($c)
%     ),
%     ?assertEqual(
%         4,
%         scrabble_game:letter_score($y)
%     ),
%     ?assertEqual(
%         5,
%         scrabble_game:letter_score($k)
%     ),
%     ?assertEqual(
%         8,
%         scrabble_game:letter_score($x)
%     ),
%     ?assertEqual(
%         10,
%         scrabble_game:letter_score($q)
%     ).

% duplicate_tiles() ->
%     ?assertEqual(
%         [122,97,97,97,blank],
%         scrabble_game:duplicate_tiles([{blank, 1}, {$a, 3}, {$z, 1}], [])
%     ).

% shuffle_tiles_1() ->
%     % Make sure to take first every time.
%     ok = meck:expect(rand, uniform, 0, 0.21736630901077447),

%     ?assertEqual(
%         [5,4,3,2,1],
%         scrabble_game:shuffle_tiles([1,2,3,4,5])
%     ),

%     % Make sure to take the last every time
%     ok = meck:expect(rand, uniform, 0, 0.9484473283587827),
%     ?assertEqual(
%         [1,2,3,4,5],
%         scrabble_game:shuffle_tiles([1,2,3,4,5])
%     ),

%     % Copy pasted from the shell...
%     ok = meck:expect(rand, uniform, 0, 0.6484473283587827),
%     ?assertEqual(
%         [5,1,2,4,3],
%         scrabble_game:shuffle_tiles([1,2,3,4,5])
%     ),

%     % Copy pasted from the shell...
%     ok = meck:expect(rand, uniform, 0, 0.5561402672724101),
%     ?assertEqual(
%         [5,1,4,2,3],
%         scrabble_game:shuffle_tiles([1,2,3,4,5])
%     ).

% shuffle_tiles_2() ->
%     % Make sure to take first every time.
%     ok = meck:expect(rand, uniform, 0, 0.21736630901077447),

%     ?assertEqual(
%         [5,4,3,2,1],
%         scrabble_game:shuffle_tiles([1,2,3,4,5], [])
%     ),

%     % Make sure to take the last every time
%     ok = meck:expect(rand, uniform, 0, 0.9484473283587827),
%     ?assertEqual(
%         [1,2,3,4,5],
%         scrabble_game:shuffle_tiles([1,2,3,4,5], [])
%     ),

%     % Copy pasted from the shell...
%     ok = meck:expect(rand, uniform, 0, 0.6484473283587827),
%     ?assertEqual(
%         [5,1,2,4,3],
%         scrabble_game:shuffle_tiles([1,2,3,4,5], [])
%     ),

%     % Copy pasted from the shell...
%     ok = meck:expect(rand, uniform, 0, 0.5561402672724101),
%     ?assertEqual(
%         [5,1,4,2,3],
%         scrabble_game:shuffle_tiles([1,2,3,4,5], [])
%     ).

% take_random_tile() ->
%     ok = meck:expect(rand, uniform, 0, 0.21736630901077447),
%     ?assertEqual(
%         {1, [2,3,4,5]},
%         scrabble_game:take_random_tile([1,2,3,4,5])
%     ),
%     ok = meck:expect(rand, uniform, 0, 0.8484473283587827),
%     ?assertEqual(
%         {4, [1,2,3,5]},
%         scrabble_game:take_random_tile([1,2,3,4,5])
%     ),
%     ok = meck:expect(rand, uniform, 0, 0.5),
%     ?assertEqual(
%         {3, [1,2,4,5]},
%         scrabble_game:take_random_tile([1,2,3,4,5])
%     ),

%     ok = meck:expect(rand, uniform, 0, 0.9775585942657209),
%     ?assertEqual(
%         {5, [2]},
%         scrabble_game:take_random_tile([2, 5])
%     ),

%     ok = meck:expect(rand, uniform, 0, 0.21736630901077447),
%     ?assertEqual(
%         {2, [5]},
%         scrabble_game:take_random_tile([2, 5])
%     ),

%     ok = meck:expect(rand, uniform, 0, 0.7118513280122967),
%     ?assertEqual(
%         {2, []},
%         scrabble_game:take_random_tile([2])
%     ),

%     ?assertException(
%         error,
%         function_clause,
%         scrabble_game:take_random_tile([])
%     ).

% players_structs() ->
%     ?assertEqual(
%         [{1,#{score => 0, hand => [], owned_tiles => []}}],
%         scrabble_game:players_structs(1, [])
%     ),

%     ?assertEqual(
%         [{3,#{score => 0, hand => [], owned_tiles => []}},
%          {2,#{score => 0, hand => [], owned_tiles => []}},
%          {1,#{score => 0, hand => [], owned_tiles => []}}],
%         scrabble_game:players_structs(3, [])
%     ).

% players_struct() ->
%     ?assertEqual(
%         {1,#{score => 0, hand => [], owned_tiles => []}},
%         scrabble_game:players_struct(1)
%     ).

% take_x_from_bag_into_player_hand() ->
%     % 1 Player
%     ?assertEqual(
%         {
%             [116,114,110,109,101,
%              120,117,105,97,111,97,109,122,115,102,114,104,116,
%              99,119,121,115,103,115,99,100,101,110,111,118,101,
%              101,100,112,114,116,103,105,111,119,97,104,101,
%              101,118,100,108,97,110,111,113,106,107,101,110,
%              111,117,116,112,108,114,115,114,108,101,97,98,105,
%              111,105,97,116,101,97,101,100,121,101,97,102,
%              blank,108,105,116,110,111,105,117,98,105,101,117,
%              103],
%             [{1, #{score => 0, hand => [97,105,114,blank,105,110,111], owned_tiles => []}}]
%         },
%         scrabble_game:take_x_from_bag_into_player_hand(
%             1,
%             7,
%             [{1,#{score => 0, hand => [], owned_tiles => []}}],
%             [97,105,114,blank,105,110,111,116,114,110,109,101,
%              120,117,105,97,111,97,109,122,115,102,114,104,116,
%              99,119,121,115,103,115,99,100,101,110,111,118,101,
%              101,100,112,114,116,103,105,111,119,97,104,101,
%              101,118,100,108,97,110,111,113,106,107,101,110,
%              111,117,116,112,108,114,115,114,108,101,97,98,105,
%              111,105,97,116,101,97,101,100,121,101,97,102,
%              blank,108,105,116,110,111,105,117,98,105,101,117,
%              103]
%             )
%     ),

%     % 3 Players - with player already having tiles in hand.
%     ?assertEqual(
%         {
%             [116,114,110,109,101,120,117,105,97,111,97,109,122,115,102,
%              114,104,116,99,119,121,115,103,115,99,100,101,110,111,118,
%              101,101,100,112,114,116,103,105,111,119,97,104,101,101,118,
%              100,108,97,110,111,113,106,107,101,110,111,117,116,112,108,
%              114,115,114,108,101,97,98,105,111,105,97,116,101,97,101,
%              100,121,101,97,102,blank,108,105,116,110,111,105,117,98,
%              105,101,117,103],
%             [{3,#{hand => [],owned_tiles => [],score => 0}},
%              {2,#{hand => [105,110,111,97,105,114,blank], owned_tiles => [],score => 0}},
%              {1,#{hand => [],owned_tiles => [],score => 0}}]
%         },
%         scrabble_game:take_x_from_bag_into_player_hand(
%             2,
%             3,
%             [{3,#{score => 0, hand => [], owned_tiles => []}},
%              {2,#{score => 0, hand => [97,105,114,blank], owned_tiles => []}},
%              {1,#{score => 0, hand => [], owned_tiles => []}}],
%             [105,110,111,116,114,110,109,101,
%              120,117,105,97,111,97,109,122,115,102,114,104,116,
%              99,119,121,115,103,115,99,100,101,110,111,118,101,
%              101,100,112,114,116,103,105,111,119,97,104,101,
%              101,118,100,108,97,110,111,113,106,107,101,110,
%              111,117,116,112,108,114,115,114,108,101,97,98,105,
%              111,105,97,116,101,97,101,100,121,101,97,102,
%              blank,108,105,116,110,111,105,117,98,105,101,117,
%              103]
%         )
%     ),

%     %% TODO: Not sure if we should crash, but if bad amount, don't change hand.
%     ?assertEqual(
%         {
%             [105,110,111,116,114,110,109,101,
%              120,117,105,97,111,97,109,122,115,102,114,104,116,
%              99,119,121,115,103,115,99,100,101,110,111,118,101,
%              101,100,112,114,116,103,105,111,119,97,104,101,
%              101,118,100,108,97,110,111,113,106,107,101,110,
%              111,117,116,112,108,114,115,114,108,101,97,98,105,
%              111,105,97,116,101,97,101,100,121,101,97,102,
%              blank,108,105,116,110,111,105,117,98,105,101,117,
%              103],
%             [{3,#{hand => [],owned_tiles => [],score => 0}},
%              {2,#{hand => [97,105,114,blank], owned_tiles => [],score => 0}},
%              {1,#{hand => [],owned_tiles => [],score => 0}}]
%         },
%         scrabble_game:take_x_from_bag_into_player_hand(
%             2,
%             7,
%             [{3,#{score => 0, hand => [], owned_tiles => []}},
%              {2,#{score => 0, hand => [97,105,114,blank], owned_tiles => []}},
%              {1,#{score => 0, hand => [], owned_tiles => []}}],
%             [105,110,111,116,114,110,109,101,
%              120,117,105,97,111,97,109,122,115,102,114,104,116,
%              99,119,121,115,103,115,99,100,101,110,111,118,101,
%              101,100,112,114,116,103,105,111,119,97,104,101,
%              101,118,100,108,97,110,111,113,106,107,101,110,
%              111,117,116,112,108,114,115,114,108,101,97,98,105,
%              111,105,97,116,101,97,101,100,121,101,97,102,
%              blank,108,105,116,110,111,105,117,98,105,101,117,
%              103]
%         )
%     ).
