%% @doc
%%
%% @end
-module(scrabble_lobby).

-export([
    start_link/0,
    register_player/2,
    deregister_player/2,
    all_players/0,
    all_player_details/0,
    create_game/1,
    all_games/0,
    join_game/2,
    leave_game/2,
    spectate_game/2,
    get_game/1,
    player_ready/2,
    start_game/2
]).

% Reading data API
-export([
    get/2,
    set/3,
    clear_all_lobby_games/0
]).

-define(MAX_PLAYERS, 4).
-define(MIN_PLAYERS, 1).

%% TODO: build expiry on users...

-behaviour(gen_server).
-export([
    init/1,
    handle_call/3,
    handle_cast/2,
    handle_info/2,
    terminate/2,
    code_change/3
]).

% -----------------
% Players
% SPID = Scrabble Player Identification

%% TODO: user ?FUNCTION_NAME

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, {}, []).

register_player(SPID, GUID) ->
    gen_server:call(?MODULE, {?FUNCTION_NAME, SPID, GUID}).

deregister_player(SPID, GUID) ->
    gen_server:call(?MODULE, {?FUNCTION_NAME, SPID, GUID}).

all_players() ->
    gen_server:call(?MODULE, {?FUNCTION_NAME}).

all_player_details() ->
    gen_server:call(?MODULE, {?FUNCTION_NAME}).

create_game(SPID) ->
    gen_server:call(?MODULE, {?FUNCTION_NAME, SPID}).

all_games() ->
    gen_server:call(?MODULE, {?FUNCTION_NAME}).

join_game(SPID, GID) ->
    gen_server:call(?MODULE, {?FUNCTION_NAME, SPID, GID}).

leave_game(SPID, GID) ->
    gen_server:call(?MODULE, {?FUNCTION_NAME, SPID, GID}).

spectate_game(SPID, GID) ->
    gen_server:call(?MODULE, {?FUNCTION_NAME, SPID, GID}).

% return {ok, game_map} | error
get_game(GID) ->
    gen_server:call(?MODULE, {?FUNCTION_NAME, GID}).

player_ready(SPID, GID) ->
    gen_server:call(?MODULE, {?FUNCTION_NAME, SPID, GID}).

start_game(SPID, GID) ->
    gen_server:call(?MODULE, {?FUNCTION_NAME, SPID, GID}).

% TESTS ONLY

clear_all_lobby_games() ->
    gen_server:call(?MODULE, {?FUNCTION_NAME}).

% -----------------
% Data API

get(K, Map) ->
    #{ K := V } = Map,
    V.

set(K, V, Map) ->
    Map#{ K => V }.

% -----------------

init({}) ->
    process_flag(trap_exit, true),
    {ok,
        #{
            players => [],
            games => #{}
        }
    }.

handle_call({register_player, SPID, GUID}, _From,
            #{ players := Players } = State) ->
    SPIDPresent = lists:keyfind(SPID, 1, Players),
    {NewPlayers, RegisteredBool} =
        case SPIDPresent of
            {SPID, _} ->
                log({player_already_registered, {SPID, GUID}}),
                {Players, false};
            false ->
                {[{SPID, GUID} | Players], true}
        end,
    {reply, RegisteredBool, State#{ players => NewPlayers }};
handle_call({deregister_player, SPID, _GUID}, _From, #{ players := Players } = State) ->
    io:format("deregister SPID ~p\n", [SPID]),
    NewPlayers = lists:keydelete(SPID, 1, Players),
    io:format("New players ~p\n", [NewPlayers]),
    {reply, proplists:get_keys(NewPlayers), State#{ players => NewPlayers }};
handle_call({all_players}, _From, #{ players := Players } = State) ->
    {reply, proplists:get_keys(Players), State};
handle_call({all_player_details}, _From, #{ players := Players } = State) ->
    {reply, Players, State};
handle_call({create_game, SPID}, _From, #{ games := Games } = State) ->
    GID = get_new_game_id(Games),
    NewGames = Games#{ GID => game_struct(SPID, GID) },
    {reply, NewGames, State#{ games => NewGames }};
handle_call({remove_game, GID}, _From, #{ games := Games } = State) ->
    NewGames = do_remove_game(GID, Games),
    {reply, ok, State#{ games => NewGames }};
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
            % If last player leaves, then end game
            case remove_player(SPID, Game) of
                [] ->
                    NewGames = do_remove_game(GID, Games),
                    {reply, true, State#{games => NewGames}};
                NewPlayers ->
                    NewGames = set(GID, set(players, NewPlayers, Game), Games),
                    {reply, true, State#{games => NewGames}}
            end;
        {false, _Game} ->
            {reply, false, State};
        error ->
            {reply, false, State}
    end;
handle_call({spectate_game, _SPID, _GameNum}, _From, #{ games := _Games } = State) ->
    {reply, false, State};
handle_call({get_game, GID}, _From, #{ games := Games } = State) ->
    {ok, Game} = find_game(GID, Games),
    {reply, {ok, Game}, State};
handle_call({player_ready, SPID, GID}, _From, #{ games := Games } = State) ->
    case check_player_in_game(GID, SPID, Games, not_ready) of
        {Reply = true, Game} ->
            NewGame = set_player_ready(Game, SPID),
            NewGame2 =
                case can_start_game(NewGame) of
                    {true, TRef} ->
                        NG1 = set(state, can_start, NewGame),
                        set(start_timer, TRef, NG1);
                    false ->
                        NewGame
                end,
            NewGames3 = set(GID, NewGame2, Games),
            {reply, Reply, State#{games => NewGames3}};
        _ ->
            % Do nothing
            {reply, false, State}
    end;
% When a player starts a game early....
handle_call({start_game, SPID, GID}, _From, #{ games := Games } = State) ->
    log({"Player Starting game", SPID, GID}),
    % Here we check if the player is ready or not
    case find_game(GID, Games) of
        {ok, #{ state := lobby }} ->
            log({"Player not ready yet, game not starting", SPID, GID}),
            {reply, {error, not_ready}, State};
        {ok, #{ state := started }} ->
            log({"player tried rejoining game", SPID, GID}),
            {reply, {error, {not_implemented, cannot_rejoin}}, State};
        {ok, #{ state := can_start } = Game} ->
            % Cancel timer
            case maps:is_key(start_timer, Game) of
                true ->
                    TRef = maps:get(start_timer, Game),
                    {ok, cancel} = timer:cancel(TRef);
                false ->
                    ok
            end,
            NewGames = do_start_game(GID, Games, Game),
            {reply, ok, State#{games => NewGames}}
    end;
% When the countdown runs out...( rename start above to be countdown )
handle_call({start_game, GID}, _From, #{ games := Games } = State) ->
    log({"Starting game", GID}),
    % Here we should be ready, where state should be set internaly
    {ok, #{ state := can_start } = Game} = find_game(GID, Games),
    NewGames = do_start_game(GID, Games, Game),
    {reply, ok, State#{games => NewGames}};
% Clear all games [ for testing ONLY ]
handle_call({clear_all_lobby_games}, _From, #{ games := Games } = State) ->
    {reply, maps:fold(fun(GID, Game, _) ->
        %% take each game and kill the pids
        %% Just make the 'Games' struct again ( map )
        do_remove_game(GID, #{ GID => Game })
    end, undefined, Games), State#{ games => #{} }};
handle_call(_Request, _From, State) ->
    {reply, {error, unknown_call}, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(Info, State) ->
    log({hanfle_info, Info, State}),
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

% -----------

get_new_game_id(Games) ->
    NewCount = maps:size(Games)+1.

game_struct(SPID, GID) ->
    % Rather than a map, maybe use a erlang record, and mnesia...
    #{host => SPID,
      number => GID,
      state => lobby,
      players => [],
      starting_time => undefined,
      game_pid => undefined
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

get_players(#{ players := Players } = Game) ->
    lists:map(fun(X) ->
        element(2, lists:keyfind(spid, 1, X))
    end, Players).

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
can_start_game(Game) ->
    % Minimum ?MIN_PLAYERS, maximum ?MAX_PLAYERS players.
    % once 2 players are ready, a 30 sec timer will commence.
    % Once the timer has ran out, then the game starts.
    GID = get(number, Game),
    Players = get(players, Game),
    case at_least_x_players_ready(Players) of
        true ->
            {ok, TRef} = start_game_countdown(GID),
            scrabble_notify:action({game_ready, GID}),
            {true, TRef};
        false ->
            false
    end.

at_least_x_players_ready(Players) ->
    at_least_x_players_ready(Players, 0).

at_least_x_players_ready(_, ?MIN_PLAYERS) ->
    true;
at_least_x_players_ready([], _Count) ->
    false;
at_least_x_players_ready([[{spid, _SPID}, {game_state, ready}]|T], Count) ->
    at_least_x_players_ready(T, Count+1);
at_least_x_players_ready([[{spid, _SPID}, {game_state, _}]|T], Count) ->
    at_least_x_players_ready(T, Count).

start_game_countdown(GID) ->
    log("Starting game timer now..."),
    {ok, _TRef} = timer:apply_after(30000, gen_server, call,
        [?MODULE, {start_game, GID}]).

do_start_game(GID, Games, Game) ->
    log({game, Game}),
    PlayerList = get_players(Game),
    log({players, PlayerList}),
    {ok, P} = scrabble_game_sup:start_child(GID, PlayerList),
    NewGame = set(state, started, Game),
    NewGame2 = set(game_pid, P, NewGame),
    NewGames = set(GID, NewGame2, Games),
    _ = scrabble_notify:action({game_starting, GID}),
    NewGames.

do_remove_game(GID, Games) ->
    {ok, Game} = find_game(GID, Games),
    case maps:get(game_pid, Game) of
        undefined ->
            maps:without([GID], Games);
        P when is_pid(P) ->
            true = erlang:unlink(P),
            true = erlang:exit(P),
            maps:without([GID], Games)
    end.
