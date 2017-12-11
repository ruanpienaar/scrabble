-module(scrabble_game).

-export([
    start_link/1,
    player_start/2,
    player_take_x_tiles/3
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
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

%% -------------------------------------------
%% Api

start_link(NmrPlayers) when NmrPlayers >= 2 andalso NmrPlayers =< 4 ->
    gen_server:start_link(?MODULE, NmrPlayers, []).

player_start(Pid, PlayerNmr) ->
    player_take_x_tiles(Pid, PlayerNmr, ?HAND_SIZE).

player_take_x_tiles(Pid, PlayerNmr, Amount) when Amount >= 1 andalso Amount =< 7 ->
    gen_server:call(Pid, {player_take_x_tiles, PlayerNmr, Amount}).

player_places_tile(Pid, Player, Tile, X, Y) ->
    gen_server:call(Pid, {player_places_tile, Player, Tile, X, Y}).

%% -------------------------------------------
%% Gen Server

init(NmrPlayers) ->
    Tiles = tile_distribution(),
    {ok, #{
        tile_bag => shuffle_tiles(Tiles),
        players => players_structs(NmrPlayers, []),
        board => []
    }}.

handle_call({player_take_x_tiles, PlayerNmr, Amount}, _From,
            #{ players := Players, tile_bag := TBag } = State) ->
    {NewBag, UpdatedPlayers} =
        take_x_from_bag_into_player_hand(PlayerNmr, Amount, Players, TBag),
    {reply, ok, State#{
        tile_bag => NewBag,
        players => UpdatedPlayers
    }};
handle_call({player_places_tile, Player, Tile, X, Y}, _From, State) ->
    {reply, ok, State#{
        board = NewBoard
    }}
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

players_structs(0, R) ->
    lists:reverse(R);
players_structs(PlayerNmr, R) when PlayerNmr > 0 ->
    players_structs(PlayerNmr -1, [players_struct(PlayerNmr) | R]).

players_struct(PlayerNmr) ->
    {PlayerNmr,
     #{
        score => 0,
        hand => [],
        owned_tiles => [] % {letter, x, y} Ex: {$a, 6, 10}
     }
    }.

take_x_from_bag_into_player_hand(PlayerNmr, Amount, Players, TBag) ->
    {PlayerNmr, #{ hand := ExistingHand } = PlayerStruct} =
        lists:keyfind(PlayerNmr, 1, Players),

    case erlang:abs(?HAND_SIZE - length(ExistingHand)) of
        Amount -> % Pattern Match on Amount requested
            {ToTake, Rest} = lists:split(Amount, TBag),
            {
                Rest,
                lists:keyreplace(PlayerNmr, 1, Players,
                    {PlayerNmr, PlayerStruct#{ hand => ToTake ++ ExistingHand }}
                )
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
