%% TODO: migrate board related code to scrabble board
%% TODO: migrate player related code to scrabble_player.
%%       Same for hand
%% TODO: create scrabble_score.erl

-module(scrabble_game).

-include("scrabble.hrl").

-export([
    name/1,
    start_link/2,
    % player_start/2,
    %player_take_x_tiles/3,
    %player_places_tile/5,
    get_game_details/1,
    %get_board/2,
    %player_leaves/3
    place_word/3,
    print_board/1
]).

-ifdef(TEST).
-export([
    get_player_info/2
%     tile_distribution/0,
%     duplicate_tiles/2,
%     % letter_score/1,
%     shuffle_tiles/1,
%     shuffle_tiles/2,
%     take_random_tile/1,
%     players_structs/2,
%     players_struct/1,
%     take_x_from_bag_into_player_hand/4
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


% scrabble_game_1, scrabble_game_2, scrabble_game_3
name(GID) ->
    list_to_atom(atom_to_list(?MODULE)++"_"++integer_to_list(GID)).

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
    Tiles = scrabble_board:tile_distribution(),
    TileBag = shuffle_tiles(Tiles),
    PlayersStructs = players_structs(PlayerIds),
    GameBoard = scrabble_board:init_game_board(),
    InitialState = (initial_state_data())#{
        tile_bag => TileBag,
        players => PlayersStructs,
        board => GameBoard
    },
    % log({tilebag, TileBag}),
    % log({players, PlayersStructs}),
    % log({board, GameBoard}),
    % check who starts first
    % then get players tiles
    {NewBag, UpdatedPlayers} =
        lists:foldl(
            fun(SPID, {AccTileBag, AccPlayers}) ->
                take_x_from_bag_into_player_hand(
                    SPID,
                    ?HAND_SIZE,
                    AccPlayers,
                    AccTileBag
                )
            end,
            {TileBag, PlayersStructs},
            PlayerIds
        ),
    % {NewBag, UpdatedPlayers} =
    %     take_x_from_bag_into_player_hand(SPID, Amount, Players, TBag),
    %% TODO: simplify code in init/1 with player_take_x_tiles/3
    {ok, InitialState#{
        tile_bag => NewBag,
        players => UpdatedPlayers
    }}.

initial_state_data() ->
    #{
        tile_bag => [],
        players => [],
        board => #{},
        board_empty => true
    }.

% handle_call({player_take_x_tiles, SPID, Amount}, _From,
%             #{ players := Players, tile_bag := TBag } = State) ->
%     {NewBag, UpdatedPlayers} =
%         take_x_from_bag_into_player_hand(SPID, Amount, Players, TBag),
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
        players := PlayersStructs
    } = State,
    %#{ hand := Hand } = maps:get(SPID, PlayersStructs),
    GameDetails = #{
        board => Board,
        players => PlayersStructs
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
%% First word!
handle_call({place_word, SPID, ProposedWord}, _From, #{ board_empty := true } = State) ->
    #{
        board := Board,
        players := PlayersStruct,
        tile_bag := TBag
    } = State,
    %% TODO: how we can we protect nicer than checking each time ?
    %%       create player statem ?
    case maps:get(SPID, PlayersStruct, undefined) of
        undefined ->
            {reply, {error, bad_spid}, State};
        PlayerMap ->
            log({submittedboard, ProposedWord}),
            case is_proposed_word_in_hand(ProposedWord, PlayerMap) of
                true ->
                    case is_starting_cell_submitted(ProposedWord) of
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
                                        take_x_from_bag_into_player_hand(
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
                            {reply, {error, not_starting_square}, State}
                    end;
                false ->
                    {reply, {error, proposed_word_not_in_hand}, State}
            end
    end;
%% Consecutive words
handle_call({place_word, _SPID, ProposedWord}, _From, #{ board_empty := false } = State) ->
    #{
        board := Board,
        players := _Players
    } = State,
    % 10> scrabble_game [
    %                    #{value => <<"a">>,x => 8,y => 9},
    %                    #{value => <<"a">>,x => 8,y => 10}]
    %% NB:

    %% TODO: check ProposedWord valid
    %% TODO: get adjacent tiles connected to ProposedWord from Board.
    %%       Are they adjacent to any other tiles? No -> return error
    %% TODO: merge adjacent + SubmitBoard into FullWord.
    %%       Is valid word? No -> Error invalid word
    %% TODO: place word on board
    %% TODO: remove letters from player hand
    %%       update player hand with missing letters from tilebag.
    UpdatedBoard = place_tiles_on_board(ProposedWord, Board),
    {reply, ok, State#{
        board => UpdatedBoard
    }};
handle_call({print_board}, _From, #{ empty_board := true } = State) ->
    {reply, empty_board, State};
handle_call({print_board}, _From, #{ board := Board } = State) ->
    io:format("\t ", []),
    [ io:format("~p   ", [X-1]) || X <- lists:seq(1, 16) ],
    io:format("\n", []),
    maps:foreach(
        fun(YK, YV) ->
            io:format("~p\t", [YK]),
            maps:foreach(
                fun(_XK, XV) ->
                    io:format(" ~p ", [
                        case XV of
                            undefined ->
                                "";
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

%get_player(SPID, Players) ->
    % log({get_players, SPID, Players}),
    % {SPID, PlayerMap} = lists:keyfind(SPID, 1, Players),
    % {ok, PlayerMap}.

% remove_player(SPID, Players) ->
%     {ok, lists:keydelete(SPID, 1, Players)}.

%% TODO: encapsulate the amount to take.
%%       make sure that tiles that're placed on the board
%%       are removed before calling this function.
take_x_from_bag_into_player_hand(SPID, HandSize, PlayersStruct, TBag) ->
    log({take, HandSize, tiles, players, PlayersStruct}),
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

is_starting_cell_submitted(ProposedWord) ->
    lists:any(
        fun
        (#{ y := 8, x := 8, value := V }) when V /= [] ->
            true;
        (_) ->
            false
        end,
        ProposedWord
    ).

is_valid_word(ProposedWord) ->
    %% TODO: is_valid_letter
    %% TODO: check dictionary API
    %is_all_letters(ProposedWord).
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