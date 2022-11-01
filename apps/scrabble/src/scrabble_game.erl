%% TODO: migrate board related code to scrabble board
%% TODO: migrate player related code to scrabble_player.
%%       Same for hand
%% TODO: create scrabble_score.erl

-module(scrabble_game).

-include("scrabble.hrl").

-export([
    name/1,
    start_link/2,
    player_start/2,
    player_take_x_tiles/3,
    player_places_tile/5,
    get_player_hand/2,
    get_board/2,
    player_leaves/3,
    place_word/3
]).

-ifdef(TEST).
-export([
    tile_distribution/0,
    duplicate_tiles/2,
    letter_score/1,
    shuffle_tiles/1,
    shuffle_tiles/2,
    take_random_tile/1,
    players_structs/2,
    players_struct/1,
    take_x_from_bag_into_player_hand/4
]).
-endif.

-define(HAND_SIZE, 7).

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

% scrabble_game_1, scrabble_game_2, scrabble_game_3
name(GID) ->
    list_to_atom(atom_to_list(?MODULE)++"_"++integer_to_list(GID)).

-spec start_link(scrabble:game_id(), scrabble:player_list()) -> {ok, pid()}.
start_link(GID, PlayerList)
        when length(PlayerList) >= 1 andalso
             length(PlayerList) =< 4 ->
    gen_server:start_link({local, name(GID)}, ?MODULE, PlayerList, []).

player_start(Pid, SPID) ->
    player_take_x_tiles(Pid, SPID, ?HAND_SIZE).

player_take_x_tiles(Pid, SPID, Amount) when Amount >= 1 andalso Amount =< 7 ->
    gen_server:call(Pid, {player_take_x_tiles, SPID, Amount}).

player_places_tile(Pid, SPID, Tile, X, Y) ->
    gen_server:call(Pid, {player_places_tile, SPID, Tile, X, Y}).

get_player_hand(Pid, SPID) ->
    gen_server:call(Pid, {get_player_hand, SPID}).

get_board(Pid, SPID) ->
    gen_server:call(Pid, {get_board, SPID}).

player_leaves(Pid, SPID, GID) ->
    gen_server:call(Pid, {player_leaves, SPID, GID}).

place_word(Pid, SPID, Tiles) ->
    gen_server:call(Pid, {?FUNCTION_NAME, SPID, Tiles}).

% -----------------
% Data API

get(K, Map) ->
    #{ K := V } = Map,
    V.

% set(K, V, Map) ->
%     Map#{ K => V }.

%% -------------------------------------------
%% Gen Server

init(PlayerList) when is_list(PlayerList) ->

    Tiles = tile_distribution(),

    % ok = scrabble_game:player_take_x_tiles(Pid, 1, 7),
    % ok = scrabble_game:player_take_x_tiles(Pid, 1, 7),

    TileBag = shuffle_tiles(Tiles),
    Players = players_structs(PlayerList, []),
    GameBoard = scrabble_board:init_game_board(),

    InitialState = #{
        empty => true,
        tile_bag => TileBag,
        players => Players,
        board => GameBoard
    },

    log({tilebag, TileBag}),
    log({players, Players}),
    log({board, GameBoard}),

    % check who starts first

    % then get players tiles

    {NewBag, UpdatedPlayers} =
        lists:foldl(fun({SPID, _PlayerMap}, {AccTileBag, AccPlayers}) ->
            take_x_from_bag_into_player_hand(SPID, 7, AccPlayers, AccTileBag)
        end, {TileBag, Players}, Players),

    % {NewBag, UpdatedPlayers} =
    %     take_x_from_bag_into_player_hand(SPID, Amount, Players, TBag),

    %% TODO: simplify code in init/1 with player_take_x_tiles/3
    {ok, InitialState#{
        tile_bag => NewBag,
        players => UpdatedPlayers
    }}.

handle_call({player_take_x_tiles, SPID, Amount}, _From,
            #{ players := Players, tile_bag := TBag } = State) ->
    {NewBag, UpdatedPlayers} =
        take_x_from_bag_into_player_hand(SPID, Amount, Players, TBag),
    {reply, ok, State#{
        tile_bag => NewBag,
        players => UpdatedPlayers
    }};
handle_call({player_places_tile, SPID, Tile, X, Y}, _From,
            #{ players := Players, board := Board } = State) ->
    case find_tile_on_board(X, Y, Board) of
        empty ->
            % Update hand
            {ok, PlayerMap} = get_player(SPID, Players),
            UpdatedPlayerMap = take_tile_from_hand(PlayerMap, Tile),
            UpdatedPlayers = update_player(SPID, UpdatedPlayerMap, Players),
            {reply, true, State#{
                players => UpdatedPlayers,
                board => place_tile_on_board(X, Y, SPID, Tile, Board)
            }};
        not_empty ->
            {reply, false, State}
    end;
handle_call({get_player_hand, SPID}, _From, #{ players := Players } = State) ->
    {ok, PlayerMap} = get_player(SPID, Players),
    PlayerHand = get(hand, PlayerMap),
    {reply, PlayerHand, State};
handle_call({get_board, _SPID}, _From,
        #{ board := Board } = State) ->
    {reply, {ok, Board}, State};
handle_call({player_leaves, SPID, _GID}, _From,
        #{ players := Players } = State) ->
    log({player, SPID, leaves}),
    log({all_players, Players}),
    {ok, UpdatedPlayers} = remove_player(SPID, Players),
    % Check if last player left...
    log({updated_players, UpdatedPlayers}),
    {reply, ok, State#{
        players => UpdatedPlayers
    }};

%% First word!
handle_call({place_word, SPID, SubmittedBoard}, _From, #{ empty := true } = State) ->
    #{
        board := Board,
        players := Players
    } = State,
    % 10> scrabble_game [#{value => <<"a">>,x => 8,y => 8},
    %                    #{value => <<"a">>,x => 8,y => 9},
    %                    #{value => <<"a">>,x => 8,y => 10}]
    case is_starting_cell_submitted(SubmittedBoard) of
        true ->
            case is_valid_word(SubmittedBoard) of
                true ->

                    % {ok, PlayerMap} = get_player(SPID, Players),
                    % PlayerHand = get(hand, PlayerMap),

                    %% TODO: remove letters from player hand
                    %% TODO: update player hand with missing letters from tilebag.

                    UpdatedBoard = place_tiles_on_board(SubmittedBoard, Board),
                    {reply, ok, State#{
                        board => UpdatedBoard,
                        empty => false
                    }};
                false ->
                    {reply, {error, invalid_word}, State}
            end;
        false ->
            {reply, {error, not_starting_square}, State}
    end;
%% Consecutive words
handle_call({place_word, SPID, SubmittedBoard}, _From, #{ empty := false } = State) ->
    #{
        board := Board,
        players := _Players
    } = State,
    %% NB:

    %% TODO: check SubmittedBoard valid
    %% TODO: get adjacent tiles connected to SubmittedBoard from Board.
    %%       Are they adjacent to any other tiles? No -> return error
    %% TODO: merge adjacent + SubmitBoard into FullWord.
    %%       Is valid word? No -> Error invalid word
    %% TODO: place word on board
    %% TODO: remove letters from player hand
    %%       update player hand with missing letters from tilebag.



        %% -> ( left-right, diag-down, diag-up, top-bottom )
    %% are all the tiles next to one another ( word )
    %% Is the submitted word a valid word ?

    % io:format(" Board in STATE ~p \n\n Board submitted is ~p\n\n", [Board, SubmittedBoard]),

    %% io:format("UpdatedBoard : ~p\n", [UpdatedBoard]),
    {reply, ok, State#{
        % board => UpdatedBoard
    }};
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

letter_score('blank') ->
    0;
letter_score(L) when L == $e orelse L == $a orelse L == $i orelse L == $o
                     orelse L == $n orelse L == $r orelse L == $t
                     orelse L == $l orelse L == $s orelse L == $u ->
    1;
letter_score(L) when L == $d orelse L == $g ->
    2;
letter_score(L) when L == $b orelse L == $c orelse L == $m orelse L == $p ->
    3;
letter_score(L) when L == $f orelse L == $h orelse L == $v orelse L == $w
                     orelse L == $y ->
    4;
letter_score(L) when L == $k ->
    5;
letter_score(L) when L == $j orelse L == $x ->
    8;
letter_score(L) when L == $q orelse L == $z ->
    10.

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

players_structs([], R) ->
    lists:reverse(R);
players_structs([SPID|T], R) when SPID > 0 ->
    players_structs(T, [players_struct(SPID) | R]).

players_struct(SPID) ->
    {SPID,
     #{
        score => 0,
        hand => [],
        owned_tiles => [] % {letter, x, y} Ex: {$a, 6, 10}
     }
    }.

get_player(SPID, Players) ->
    log({get_players, SPID, Players}),
    {SPID, PlayerMap} = lists:keyfind(SPID, 1, Players),
    {ok, PlayerMap}.

remove_player(SPID, Players) ->
    {ok, lists:keydelete(SPID, 1, Players)}.

take_x_from_bag_into_player_hand(SPID, Amount, Players, TBag) ->
    log({take, Amount, tiles, players, Players}),
    {ok, #{ hand := ExistingHand } = PlayerMap} = get_player(SPID, Players),
    case erlang:abs(?HAND_SIZE - length(ExistingHand)) of
        Amount -> % Pattern Match on Amount requested
            {ToTake, Rest} = lists:split(Amount, TBag),
            {
                Rest,
                update_player(SPID, PlayerMap#{ hand => ToTake ++ ExistingHand }, Players)
            };
        % TODO: Should we crash, alert, trying to take more
        % than allowed in hand (7)
        % Maybe log?
        _BadAmount ->
            {
                TBag,
                Players
            }
    end.

update_player(SPID, PlayerMap, Players) ->
    lists:keyreplace(SPID, 1, Players, {SPID, PlayerMap}).

find_tile_on_board(X, Y, Board) ->
    case {lists:keyfind(X, 1, Board), lists:keyfind(Y, 2, Board)} of
        {false, false} ->
            empty;
        {{X, Y, SPID, Tile}, {X, Y, SPID, Tile}} ->
            not_empty
    end.

place_tile_on_board(X, Y, SPID, Tile, Board) ->
    [{X, Y, SPID, Tile}|Board].

take_tile_from_hand(#{ hand := Hand } = Map, Tile) ->
    NewHand = Hand -- [Tile],
    Map#{ hand => NewHand }.

log(X) ->
    io:format("~p ~p~n", [?MODULE, X]).

is_starting_cell_submitted(SubmittedBoard) ->
    % 10> scrabble_game [#{value => <<"a">>,x => 8,y => 8},
    %                    #{value => <<"a">>,x => 8,y => 9},
    %                    #{value => <<"a">>,x => 8,y => 10}]
    lists:any(
        fun
        (#{ x := 8, y := 8 }) ->
            true;
        (_) ->
            false
        end,
        SubmittedBoard
    ).

is_valid_word(_SubmittedBoard) ->
    %% TODO: is_valid_letter
    %% TODO: check dictionary API
    true.

place_tiles_on_board(SubmittedBoard, Board) ->
    lists:foldl(
        fun(SubmitLetter, BoardAcc) ->
            #{
                x := X,
                y := Y,
                value := LetterValue
            } = SubmitLetter,
            BoardRow = maps:get(X, BoardAcc),
            BoardCell = maps:get(Y, BoardRow),
            case BoardCell of
                ?BOARD_TILE_UNSET ->
                    BoardAcc#{
                        X => BoardRow#{
                            Y => LetterValue
                        }
                    };
                ExistingVal ->
                    throw({error, {letter_already_set, X, Y, ExistingVal}})
            end
        end,
        Board,
        SubmittedBoard
    ).