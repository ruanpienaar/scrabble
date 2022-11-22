%% TODO: migrate board related code to scrabble board
%% TODO: migrate player related code to scrabble_player.
%%       Same for hand
%% TODO: create scrabble_score.erl
%% TODO: should this code be a gen_statem ?
%% TODO: do we place all app logic in 1 gen_statem?
%%       lobby/wait/game/scoreboard?

-module(scrabble_game).

-include("scrabble.hrl").

-export([
    name/1,
    safe_game_id/1,
    start_link/2,
    % player_start/2,
    %player_take_x_tiles/3,
    %player_places_tile/5,
    get_player_info/2,
    get_game_details/1,
    %get_board/2,
    %player_leaves/3
    place_word/3,
    print_board/1
]).

-ifdef(TEST).
-export([
%     tile_distribution/0,
%     duplicate_tiles/2,
%     % letter_score/1,
%     shuffle_tiles/1,
%     shuffle_tiles/2,
%     take_random_tile/1,
%     players_structs/2,
%     players_struct/1,
%     take_tiles_after_turn_end/4
]).
-endif.

-behaviour(gen_server).
-export([
    init/1,
    handle_call/3,
    handle_cast/2,
    handle_info/2,
    terminate/2,
    code_change/3
]).

%% -------------------------------------------
%% Api

%% Possible PLAYER actions:
%% - start game
%% - player places word
%% - PRINT board to shell
%%   - player joins
%%   - player leaves game
%% Possible SYSTEM actions:
%% - get game details ( players, boards, scores )


%% the naming is poor. dynamic atoms are not a good idea.
%% We'll reach a hard limit.
% scrabble_game_1, scrabble_game_2, scrabble_game_3
name(GID) ->
    list_to_atom(atom_to_list(?MODULE)++"_"++integer_to_list(GID)).

safe_game_id(GID) when is_binary(GID) ->
    safe_game_id(binary_to_list(GID));
safe_game_id(GID) when is_list(GID) ->
    list_to_integer(GID);
safe_game_id(GID) when is_integer(GID) ->
    GID.

-spec start_link(scrabble:game_id(), scrabble:player_list()) -> {ok, pid()}.
start_link(GID, PlayerList)
        when length(PlayerList) >= 1 andalso
             length(PlayerList) =< 4 ->
    gen_server:start_link({local, name(GID)}, ?MODULE, PlayerList, []).

% player_start(Pid, SPID) ->
%     player_take_x_tiles(Pid, SPID, ?HAND_SIZE).

% player_take_x_tiles(Pid, SPID, Amount) when Amount >= 1 andalso Amount =< ?HAND_SIZE ->
%     gen_server:call(Pid, {player_take_x_tiles, SPID, Amount}).

% player_places_tile(Pid, SPID, Tile, X, Y) ->
%     gen_server:call(Pid, {player_places_tile, SPID, Tile, X, Y}).

get_game_details(Pid) ->
    gen_server:call(Pid, {?FUNCTION_NAME}).

% get_board(Pid, SPID) ->
%     gen_server:call(Pid, {get_board, SPID}).

% player_leaves(Pid, SPID, GID) ->
%     gen_server:call(Pid, {player_leaves, SPID, GID}).

-spec place_word(pid(), scrabble:player_id(), scrabble:word()) -> ok.
place_word(Pid, SPID, ProposedWord) ->
    gen_server:call(Pid, {?FUNCTION_NAME, SPID, ProposedWord}).

print_board(Pid) ->
    _ = scrabble_notify:action({word_placed, 1}),
    gen_server:call(Pid, {?FUNCTION_NAME}).

get_player_info(Pid, SPID) ->
    gen_server:call(Pid, {?FUNCTION_NAME, SPID}).

% -----------------
% Data API

% get(K, Map) ->
%     #{ K := V } = Map,
%     V.

% set(K, V, Map) ->
%     Map#{ K => V }.

%% -------------------------------------------
%% Gen Server

%% TODO: take tiles when game starts...
%%       but not as gs call
% ok = scrabble_game:player_take_x_tiles(Pid, 1, ?HAND_SIZE),
-spec init(scrabble:player_id_list()) -> scrabble:scrabble_game_gs_data().
init(PlayerIds) when is_list(PlayerIds) ->
    PlayersStructs = players_structs(PlayerIds),
    GameBoard = scrabble_board:init_game_board(),
    State1 = set_player_turn_order(
        (initial_state_data())#{
            tile_bag => init_shuffled_tile_bag(),
            players => PlayersStructs,
            board => GameBoard
        }
    ),
    #{ tile_bag := ShuffledTileBag } = State1,
    log({player_turn, maps:get(player_turn, State1)}),
    log({player_turn_order, maps:get(player_turn_order, State1)}),
    % check who starts first
    % then get players tiles
    {AllPlayersTakenHandFromTileBag, UpdatedPlayers} =
        all_player_get_tiles_from_bag(
            {ShuffledTileBag, PlayersStructs},
            ?HAND_SIZE,
            PlayerIds
        ),
    % {AllPlayersTakenHandFromTileBag, UpdatedPlayers} =
    %     take_tiles_after_turn_end(SPID, Amount, Players, TBag),
    %% TODO: simplify code in init/1 with player_take_x_tiles/3
    {ok, State1#{
        tile_bag => AllPlayersTakenHandFromTileBag,
        players => UpdatedPlayers
    }}.

init_shuffled_tile_bag() ->
    shuffle_tiles(
        scrabble_board:tile_distribution()
    ).

initial_state_data() ->
    #{
        tile_bag => [],
        players => [],
        board => #{},
        board_empty => true,
        player_turn_order => [],
        player_turn => undefined
    }.

% handle_call({player_take_x_tiles, SPID, Amount}, _From,
%             #{ players := Players, tile_bag := TBag } = State) ->
%     {NewBag, UpdatedPlayers} =
%         take_tiles_after_turn_end(SPID, Amount, Players, TBag),
%     {reply, ok, State#{
%         tile_bag => NewBag,
%         players => UpdatedPlayers
%     }};
% handle_call({player_places_tile, SPID, Tile, X, Y}, _From,
%             #{ players := Players, board := Board } = State) ->
%     case find_tile_on_board(X, Y, Board) of
%         empty ->
%             % Update hand
%             {ok, PlayerMap} = get_player(SPID, Players),
%             UpdatedPlayerMap = take_tile_from_hand(PlayerMap, Tile),
%             UpdatedPlayers = update_players(SPID, UpdatedPlayerMap, Players),
%             {reply, true, State#{
%                 players => UpdatedPlayers,
%                 board => place_tile_on_board(X, Y, SPID, Tile, Board)
%             }};
%         not_empty ->
%             {reply, false, State}
%     end;
handle_call({get_game_details}, _From, State) ->
    #{
        board := Board,
        players := PlayersStructs,
        player_turn := PlayerTurn
    } = State,
    %#{ hand := Hand } = maps:get(SPID, PlayersStructs),
    GameDetails = #{
        board => Board,
        players => PlayersStructs,
        player_turn => PlayerTurn
    },
    {reply, GameDetails, State};
% handle_call({get_board, _SPID}, _From,
%         #{ board := Board } = State) ->
%     {reply, {ok, Board}, State};
% handle_call({player_leaves, SPID, _GID}, _From,
%         #{ players := Players } = State) ->
%     log({player, SPID, leaves}),
%     log({all_players, Players}),
%     {ok, UpdatedPlayers} = remove_player(SPID, Players),
%     % Check if last player left...
%     log({updated_players, UpdatedPlayers}),
%     {reply, ok, State#{
%         players => UpdatedPlayers
%     }};
handle_call({place_word, SPID, ProposedWord}, _From, #{ player_turn := SPID } = State) ->
    #{
        board_empty := BE,
        board := Board,
        players := PlayersStruct,
        tile_bag := TBag,
        player_turn := SPID
    } = State,
    PlayerMap = maps:get(SPID, PlayersStruct),
    log({submittedboard, ProposedWord}),
    case is_proposed_word_in_hand(ProposedWord, PlayerMap) of
        true ->
            case valid_placement(Board, BE, ProposedWord) of
                true ->
                    case is_valid_word(ProposedWord) of
                        true ->
                            UpdatedBoard =
                                place_tiles_on_board(ProposedWord, Board),
                            {NewHand, #{ score := CurrentPlayerScore } = PlayerMap2} =
                                take_tiles_from_hand(PlayerMap, ProposedWord),
                            NewPlayerScore =
                                CurrentPlayerScore +
                                scrabble_score:word_score(Board, ProposedWord),
                            PlayerMap3 = PlayerMap2#{ score => NewPlayerScore },
                            log({new_hand, NewHand}),
                            PlayersStruct2 = update_players(SPID, PlayerMap3, PlayersStruct),
                            {NewBag, UpdatedPlayerMap2} =
                                player_get_tiles_from_bag(
                                    SPID,
                                    number_of_new_tiles_to_take(NewHand),
                                    PlayersStruct2,
                                    TBag
                                ),
                            {reply, ok, State#{
                                tile_bag => NewBag,
                                board => UpdatedBoard,
                                players => UpdatedPlayerMap2,
                                board_empty => false
                            }};
                        false ->
                            {reply, {error, invalid_word}, State}
                    end;
                false ->
                    {reply, {error, invalid_placement}, State}
            end;
        false ->
            {reply, {error, proposed_word_not_in_hand}, State}
    end;
handle_call({place_word, SPID, ProposedWord}, _From, State) ->
    % case maps:get(SPID, PlayersStruct, undefined) of
    %% TODO: how do we simplify player checking? create player statem ?
    %% TODO: how to test? ( maybe player_turn and player leaves? -)
    %%       but then a player who leaves, should update next player turn
    %% TODO: add {reply, {error, bad_spid}, State};
    case maps:get(player_turn, State) =/= SPID of
        true ->
            log({player, SPID, out_of_turn}),
            {reply, {error, player_out_of_turn}, State};
        false ->
            log({place_word_issue, SPID, ProposedWord, State}),
            {reply, {error, internal_error}, State}
    end;
handle_call({print_board}, _From, #{ empty_board := true } = State) ->
    {reply, empty_board, State};
handle_call({print_board}, _From, #{ board := Board } = State) ->
    io:format("\t ", []),
    [ io:format("~p    ", [X]) || X <- lists:seq(1, 16) ],
    io:format("\n", []),
    maps:foreach(
        fun(YK, YV) ->
            io:format("~p\t", [YK]),
            maps:foreach(
                fun(_XK, XV) ->
                    io:format(" ~p ", [
                        case XV of
                            undefined ->
                                " ";
                            _ ->
                                [XV]
                        end
                    ])
                end,
                YV
            ),
            io:format("\n")
        end,
        Board
    ),
    {reply, ok, State};
handle_call({get_player_info, SPID}, _From, #{ players := PlayersStruct } = State) ->
    case maps:get(SPID, PlayersStruct, undefined) of
        undefined ->
            {reply, {error, bad_spid}, State};
        PlayerMap ->
            {reply, PlayerMap, State}
    end;
handle_call(_Request, _From, State) ->
    {reply, {error, unknown_call}, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%% -------------------------------------------
%% Internal

shuffle_tiles(Tiles) ->
    shuffle_tiles(Tiles, []).

%% Fisher–Yates shuffle
%% https://en.wikipedia.org/wiki/Fisher%E2%80%93Yates_shuffle
shuffle_tiles([], R) ->
    R;
shuffle_tiles(L, R) ->
    {RandomTile, RestTiles} = take_random_tile(L),
    shuffle_tiles(RestTiles, [ RandomTile | R ]).

-spec take_random_tile(list()) -> {term(), list()}.
take_random_tile([T]) ->
    {T, []};
take_random_tile(L) when L > [] ->
    RandPos =
        case erlang:round(rand:uniform() * length(L)) of
            0 -> % When round return's '0', cannot split with 0.
                1;
            R ->
                R
        end,
    {L1, L2} = lists:split(RandPos, L),
    {lists:last(L1), lists:droplast(L1) ++ L2}.

players_structs(PlayerIds) ->
    lists:foldl(
        fun(PlayerId, Acc) ->
            Acc#{
                PlayerId => initial_players_struct()
            }
        end,
        #{},
        PlayerIds
    ).

initial_players_struct() ->
    #{
       score => 0,
       hand => [],
       owned_tiles => [] % {letter, x, y} Ex: {$a, 6, 10}
    }.

all_player_get_tiles_from_bag({TileBag, PlayersStructs}, NumTilesToTake, PlayerIds) ->
    lists:foldl(
        fun(SPID, {AccTileBag, AccPlayers}) ->
            take_tiles_at_start(
                SPID,
                %% TODO: should we check player hand size?
                %%       should be 0 at startup.
                NumTilesToTake,
                AccPlayers,
                AccTileBag
            )
        end,
        {TileBag, PlayersStructs},
        PlayerIds
    ).

%% TODO: could potentially just be
%%       player_file_hand_tiles(SPID, State)
%% This function seems superfluous now.
player_get_tiles_from_bag(
    SPID, NumTilesToTake, PlayersStruct2, TBag) ->
    take_tiles_after_turn_end(
        SPID,
        NumTilesToTake,% number_of_new_tiles_to_take(NewHand),
        PlayersStruct2,
        TBag
    ).

% [player1, player2]
set_player_turn_order(
        #{
            players := PlayersStructs,
            tile_bag := ShuffledTileBag
        } = State
    ) ->
    log({tile_bag, ShuffledTileBag}),
    %% Player tile with distance closest to A
    %% starts
    %% Any player with blank tile, goes first!
    % Take Leaders/Draw players,
    % and run function again, return tiles
    %% shuffle bag, pick tiles for players that drawed.
    %% until we have a clear starter/starting-order.
    %% each player taking 1 tile:
    {_, UpdatedPlayers} =
        all_player_get_tiles_from_bag(
            {ShuffledTileBag, PlayersStructs},
            ?START_SIZE,
            maps:keys(PlayersStructs)
        ),
    log({players_took_one_tile, UpdatedPlayers}),
    PlayerScores =
        maps:to_list(
            maps:map(
                fun(SPID, #{ hand := [Letter] }) ->
                    Score = starting_letter_score(Letter),
                    log({SPID, letter, [Letter], score, Score}),
                    Score
                end,
                UpdatedPlayers
            )
        ),
    log({player_scores, PlayerScores}),
    PlayerTurnOrder =
        lists:sort(
            fun({_SPID1, Distance1}, {_SPID2, Distance2}) ->
                Distance1 < Distance2
            end,
            PlayerScores
        ),
    log({player_turn_order, PlayerTurnOrder}),
    %% case count of players > 1
    %% minimum case for re-attempt is if shortest(winner) and second place
    %% has the same distance, then re-do the pick-1 and counting
    %% else just proceed
    case maps:size(UpdatedPlayers) >= 2 of
        true ->
            %% check for a tie.
            [{PlayerX, ScoreX}, {PlayerY, ScoreY}| _] = PlayerTurnOrder,
            case ScoreX =:= ScoreY of
                true ->
                    %% redo tile drawing again!
                    log({start_score_draw, {PlayerX, ScoreX}, {PlayerY, ScoreY} }),
                    %% reshuffle-bag!
                    set_player_turn_order(
                        State#{ tile_bag => init_shuffled_tile_bag() }
                    );
                false ->
                    complete_player_election(State, PlayerTurnOrder)
            end;
        false ->
            complete_player_election(State, PlayerTurnOrder)
    end.

complete_player_election(State, PlayerTurnOrder) ->
    %% take back each player hand, and reshuffle.
    PlayerTurnOrder2 = lists:map(
        fun({SPID, _}) -> SPID end,
        PlayerTurnOrder
    ),
    PlayerTurn = hd(PlayerTurnOrder2),
    State#{
        tile_bag => init_shuffled_tile_bag(),
        player_turn_order => PlayerTurnOrder2,
        player_turn => PlayerTurn
    }.

starting_letter_score(blank) ->
    -1;
starting_letter_score(Letter) ->
    abs(Letter - $a).

%get_player(SPID, Players) ->
    % log({get_players, SPID, Players}),
    % {SPID, PlayerMap} = lists:keyfind(SPID, 1, Players),
    % {ok, PlayerMap}.

% remove_player(SPID, Players) ->
%     {ok, lists:keydelete(SPID, 1, Players)}.

take_tiles_at_start(SPID, HandSize, PlayersStruct, TBag) ->
    log({SPID, take_tiles_at_start, HandSize, tiles, players, PlayersStruct}),
    #{ hand := ExistingHand } = PlayerMap = maps:get(SPID, PlayersStruct),
    {ToTake, NewBag} = lists:split(HandSize, TBag),
    {
        NewBag,
        set_player_hand(SPID, PlayerMap, ToTake ++ ExistingHand, PlayersStruct)
    }.

%% TODO: encapsulate the amount to take.
%%       make sure that tiles that're placed on the board
%%       are removed before calling this function.
take_tiles_after_turn_end(SPID, HandSize, PlayersStruct, TBag) ->
    log({SPID, take_tiles_after_turn_end, HandSize, tiles, players, PlayersStruct}),
    % {ok,  =
    %     get_player(SPID, PlayersStruct),
    #{ hand := ExistingHand } = PlayerMap = maps:get(SPID, PlayersStruct),
    case erlang:abs(?HAND_SIZE - length(ExistingHand)) of
        HandSize -> % Pattern Match on HandSize requested
            {ToTake, NewBag} = lists:split(HandSize, TBag),
            {
                NewBag,
                set_player_hand(SPID, PlayerMap, ToTake ++ ExistingHand, PlayersStruct)
            };
        % TODO: Should we crash, alert, trying to take more
        % than allowed in hand (?HAND_SIZE)
        % Maybe log?
        _BadAmount ->
            {
                TBag,
                PlayersStruct
            }
    end.

set_player_hand(SPID, PlayerMap, NewHand, PlayersStruct) ->
    update_players(SPID, PlayerMap#{ hand => NewHand }, PlayersStruct).

update_players(SPID, PlayerMap, PlayersStruct) ->
    %lists:keyreplace(SPID, 1, PlayersStruct, {SPID, PlayerMap}).
    maps:put(SPID, PlayerMap, PlayersStruct).

% find_tile_on_board(X, Y, Board) ->
%     case {lists:keyfind(X, 1, Board), lists:keyfind(Y, 2, Board)} of
%         {false, false} ->
%             empty;
%         {{X, Y, SPID, Tile}, {X, Y, SPID, Tile}} ->
%             not_empty
%     end.

% place_tile_on_board(X, Y, SPID, Tile, Board) ->
%     [{X, Y, SPID, Tile}|Board].

take_tiles_from_hand(#{ hand := Hand } = Player, ProposedWord) ->
    NewHand =
        lists:foldl(
            fun(SubmitLetter, HandAcc) ->
                #{ value := LetterValue } = SubmitLetter,
                log({subtract, HandAcc, [LetterValue]}),
                HandAcc -- [LetterValue]
            end,
            Hand,
            ProposedWord
        ),
    {
        NewHand,
        Player#{ hand => NewHand }
    }.

log(X) ->
    io:format("~p ~p~n", [?MODULE, X]).

is_proposed_word_in_hand(ProposedWord, #{ hand := Hand }) ->
    lists:all(
        fun
        (#{ value := V }) ->
            lists:member(V, Hand)
        end,
        ProposedWord
    ).

valid_placement(Board, _BoardEmpty=false, ProposedWord) ->
    % my first and horrible attempt at matrix checking code...
    % Possibly BREAK once true found.
    lists:any(
        fun
        (#{ y := Y, x := X }) ->
            %% check adjacent squares
            % x - 1 || x + 1 ( is a ExistnigLetter ) ?
            % y - 1 || y + 1 ( is a ExistnigLetter ) ?
            % x == ExistingLetter || y == ExistnigLetter
            maps:get(X-1, maps:get(Y, Board, #{}), undefined) /= undefined orelse
            maps:get(X+1, maps:get(Y, Board, #{}), undefined) /= undefined orelse
            maps:get(X, maps:get(Y-1, Board, #{}), undefined) /= undefined orelse
            maps:get(X, maps:get(Y+1, Board, #{}), undefined) /= undefined
        end,
        ProposedWord
    );
valid_placement(_Board, _BoardEmpty=true, ProposedWord) ->
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

is_valid_word(_ProposedWord) ->
    %% TODO: is_valid_letter
    %% TODO: check dictionary API
    %% TODO: check all adjacent words for validity.
    true.

% is_all_letters(ProposedWord) ->
%     lists:all(
%         fun
%         (#{ value := V }) when V >= 97 orelse V =< 122 ->
%             true;
%         (_) ->
%             false
%         end,
%         ProposedWord
%     ).

place_tiles_on_board(ProposedWord, Board) ->
    lists:foldl(
        fun(SubmitLetter, BoardAcc) ->
            #{
                x := X,
                y := Y,
                value := LetterValue
            } = SubmitLetter,
            BoardRow = maps:get(Y, BoardAcc),
            BoardCell = maps:get(X, BoardRow),
            case BoardCell of
                ?BOARD_TILE_UNSET ->
                    BoardAcc#{
                        Y => BoardRow#{
                            X => LetterValue
                        }
                    };
                ExistingVal ->
                    throw({error, {letter_already_set, X, Y, ExistingVal}})
            end
        end,
        Board,
        ProposedWord
    ).

number_of_new_tiles_to_take(ExistingHandAfterSubmitted) ->
    ?HAND_SIZE - length(ExistingHandAfterSubmitted).