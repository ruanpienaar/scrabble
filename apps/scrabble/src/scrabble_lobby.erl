-module(scrabble_lobby).

-export([
    start_link/0,
    register_player/2,
    deregister_player/2,
    all_players/0,
    create_game/0,
    all_games/0,
    join_game/2,
    leave_game/2,
    spectate_game/2,
    get_game/1,
    player_ready/2
]).

% Reading data API
-export([
    get/2,
    set/3
]).

-define(MAX_PLAYERS, 4).
-define(MIN_PLAYERS, 2).

%% TODO: build expiry on users...

-behaviour(gen_server).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-define(SERVER, ?MODULE).

% -----------------
% Players
% SPID = Scrabble Player Identification

start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, {}, []).

register_player(SPID, GUID) ->
    gen_server:call(?MODULE, {register_player, SPID, GUID}).

deregister_player(SPID, GUID) ->
    gen_server:call(?MODULE, {deregister_player, SPID, GUID}).

all_players() ->
    gen_server:call(?MODULE, {all_players}).

create_game() ->
    gen_server:call(?MODULE, {create_game}).

all_games() ->
    gen_server:call(?MODULE, {all_games}).

join_game(SPID, GID) ->
    gen_server:call(?MODULE, {join_game, SPID, GID}).

leave_game(SPID, GID) ->
    gen_server:call(?MODULE, {leave_game, SPID, GID}).

spectate_game(SPID, GID) ->
    gen_server:call(?MODULE, {spectate_game, SPID, GID}).

% return {ok, game_map} | error
get_game(GID) ->
    gen_server:call(?MODULE, {get_game, GID}).

player_ready(SPID, GID) ->
    gen_server:call(?MODULE, {player_ready, SPID, GID}).

% -----------------
% Data API

get(K, Map) ->
    #{ K := V } = Map,
    V.

set(K, V, Map) ->
    Map#{ K => V }.

% -----------------

init({}) ->
    {ok,
        #{
            players => [],
            games => #{}
        }
    }.

handle_call({register_player, SPID, GUID}, _From,
            #{ players := Players } = State) ->
    NewPlayers =
        case lists:member({SPID, GUID}, Players) of
            true ->
                log({player_and_guid_already_registered, {SPID, GUID}}),
                Players;
            false ->
                [{SPID, GUID} | Players]
        end,
    {reply, proplists:get_keys(NewPlayers), State#{ players => NewPlayers }};
handle_call({deregister_player, SPID, _GUID}, _From, #{ players := Players } = State) ->
    NewPlayers = lists:keydelete(SPID, 1, Players),
    {reply, proplists:get_keys(NewPlayers), State#{ players => NewPlayers }};
handle_call({all_players}, _From, #{ players := Players } = State) ->
    {reply, proplists:get_keys(Players), State};
handle_call({create_game}, _From, #{ games := Games } = State) ->
    NewCount = maps:size(Games)+1,
    NewGames = Games#{ NewCount => game_struct(NewCount) },
    {reply, NewGames, State#{ games => NewGames }};
handle_call({all_games}, _From, #{ games := Games } = State) ->
    {reply, Games, State};
handle_call({join_game, SPID, GID}, _From, #{ games := Games } = State) ->
    case check_player_in_game(GID, SPID, Games) of
        {true, _Game} ->
            {reply, true, State};
        {false, Game} ->
            {Reply, NewGames} =
                case can_planyer_join_game(Game) of
                    true ->
                        {true, set(GID, set(players, add_player(SPID, Game), Game), Games)};
                    false ->
                        {false, Games}
                end,
            {reply, Reply, State#{games => NewGames}};
        error ->
            {reply, false, State}
    end;
handle_call({leave_game, SPID, GID}, _From, #{ games := Games } = State) ->
    case check_player_in_game(GID, SPID, Games) of
        {true, Game} ->
            NewGames = set(GID, set(players, remove_player(SPID, Game), Game), Games),
            {reply, true, State#{games => NewGames}};
        {false, _Game} ->
            {reply, false, State};
        error ->
            {reply, false, State}
    end;
handle_call({spectate_game, _SPID, _GameNum}, _From, #{ games := _Games } = State) ->
    {reply, false, State};
handle_call({get_game, GID}, _From, #{ games := Games } = State) ->
    Game = find_game(GID, Games),
    {reply, Game, State};
handle_call({player_ready, SPID, GID}, _From, #{ games := Games } = State) ->
    case check_player_in_game(GID, SPID, Games, not_ready) of
        {Reply = true, Game} ->
            NewGame = set_player_ready(Game, SPID),
            NewGame2 =
                case can_game_start(NewGame) of
                    true ->
                        set(state, starting, NewGame);
                    false ->
                        NewGame
                end,
            NewGames = set(GID, NewGame2, Games),
            {reply, Reply, State#{games => NewGames}};
        _ ->
            % Do nothing
            {reply, false, State}
    end;
handle_call({start_game, GID}, _From, #{ games := Games } = State) ->
    % io:format("[~p] !!!!!!!!! GOING TO START GAME ~p !!!!!!!!!~n",
    %           [?MODULE, GID]),
    {ok, #{ state := starting} = Game} = find_game(GID, Games),
    NewGame = set(state, started, Game),
    NewGames = set(GID, NewGame, Games),
    {reply, ok, State#{games => NewGames}};
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

% -----------

game_struct(GID) ->
    % Rather than a map, maybe use a erlang record, and mnesia...
    #{number => GID,
      state => lobby,
      players => [],
      starting_time => undefined
    }.

find_game(GID, Games) ->
    maps:find(GID, Games).

check_player_in_game(GID, SPID, Games) ->
    case find_game(GID, Games) of
        {ok, Game} ->
            Players = get(players, Game),
            InGame = lists:member([{spid, SPID}, {game_state, not_ready}], Players) orelse
                     lists:member([{spid, SPID}, {game_state, ready}], Players) orelse
                     lists:member([{spid, SPID}, {game_state, in_game}], Players) orelse
                     lists:member([{spid, SPID}, {game_state, spectate}], Players),
            {InGame, Game};
        error ->
            error
    end.

check_player_in_game(GID, SPID, Games, GameState) ->
    case find_game(GID, Games) of
        {ok, Game} ->
            Players = get(players, Game),
            InGame = lists:member([{spid, SPID}, {game_state, GameState}], Players),
            {InGame, Game};
        error ->
            error
    end.

% return {boolean, Updated Game}
can_planyer_join_game(Game) ->
    Players = get(players, Game),
    % Test if adding the player, goes over the allowed limit
    (length(Players)+1) =< (?MAX_PLAYERS).

set_player_ready(Game, SPID) ->
    Players = get(players, Game),
    NewPlayers = lists:map(fun
        ([{spid, S}, {game_state, not_ready}]) when S =:= SPID ->
            [{spid, SPID}, {game_state, ready}];
        (I) ->
            I
    end, Players),
    set(players, NewPlayers, Game).


log(X) ->
    io:format("~p ~p~n", [?MODULE, X]).

% can be done much better...
add_player(SPID, Game) ->
    [player_struct(SPID)|get(players, Game)].

remove_player(SPID, Game) ->
    lists:filter(fun
        ([{spid, S}, {game_state, _}]) when S =:= SPID ->
            false;
        (_) ->
            true
    end, get(players, Game)).

player_struct(SPID) ->
    [{spid, SPID}, {game_state, not_ready}].

% #{number =>
%       1,
%   players =>
%       [[{spid,<<"ruan">>},{game_state,not_ready}],
%        [{spid,<<"safari">>},{game_state,ready}]],
%   state =>
%       lobby
% }
can_game_start(Game) ->
    % Minimum 2, maximum 4 players.
    % once 2 players are ready, a 30 sec timer will commence.
    % Once the timer has ran out, then the game starts.
    GID = get(number, Game),
    Players = get(players, Game),
    case at_least_x_players_ready(Players) of
        true ->
            start_game_countdown(GID),
            scrabble_notify:action({game_ready, GID}),
            true;
        false ->
            false
    end.

at_least_x_players_ready(Players) ->
    at_least_x_players_ready(Players, 0).

at_least_x_players_ready(_, 2) ->
    true;
at_least_x_players_ready([], _Count) ->
    false;
at_least_x_players_ready([[{spid, _SPID}, {game_state, ready}]|T], Count) ->
    at_least_x_players_ready(T, Count+1);
at_least_x_players_ready([[{spid, _SPID}, {game_state, _}]|T], Count) ->
    at_least_x_players_ready(T, Count).

start_game_countdown(GID) ->
    {ok, _TRef} = timer:apply_after(30000, gen_server, call, [?MODULE, {start_game, GID}]).



