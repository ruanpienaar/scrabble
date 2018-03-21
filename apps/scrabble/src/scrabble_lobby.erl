-module(scrabble_lobby).

-export([
    start_link/0,
    register_player/2,
    deregister_player/2,
    all_players/0
]).

-export([
    create_game/0,
    all_games/0,
    join_game/2,
    spectate_game/2,
    get_game/1
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

% Games

create_game() ->
    gen_server:call(?MODULE, {create_game}).

all_games() ->
    gen_server:call(?MODULE, {all_games}).

join_game(SPID, GameNum) ->
    gen_server:call(?MODULE, {join_game, SPID, GameNum}).

spectate_game(SPID, GameNum) ->
    gen_server:call(?MODULE, {spectate_game, SPID, GameNum}).

% return {ok, game_map} | error
get_game(GameNum) ->
    gen_server:call(?MODULE, {get_game, GameNum}).

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
            lobby_games => #{},
            active_games => []
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
handle_call({create_game}, _From, #{ lobby_games := LGames } = State) ->
    NewCount = maps:size(LGames)+1,
    NewLGames = LGames#{ NewCount => new_game_struct(NewCount) },
    {reply, NewLGames, State#{ lobby_games => NewLGames }};
handle_call({all_games}, _From, #{ lobby_games := LGames } = State) ->
    {reply, LGames, State};
handle_call({join_game, SPID, GameNum}, _From, #{ lobby_games := LGames } = State) ->
    case check_player_in_game(GameNum, SPID, LGames) of
        {true, _Game} ->
            {reply, true, State};
        {false, Game} ->
            {Reply, NewLGames} =
                case can_planyer_join_game(Game) of
                    true ->
                        NewPlayer = [{spid, SPID}, {game_state, not_ready}],
                        NewGame = set(players, [NewPlayer|get(players, Game)], Game),
                        {true, set(GameNum, NewGame, LGames)};
                    false ->
                        {false, LGames}
                end,
            {reply, Reply, State#{lobby_games => NewLGames}};
        error ->
            {reply, false, State}
    end;
handle_call({spectate_game, _SPID, _GameNum}, _From, #{ lobby_games := _LGames } = State) ->
    {reply, false, State};
handle_call({get_game, GameNum}, _From, #{ lobby_games := LGames } = State) ->
    Game = find_game(GameNum, LGames),
    {reply, Game, State};
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

new_game_struct(GameNum) ->
    % Rather than a map, maybe use a erlang record, and mnesia...
    #{number => GameNum,
      state => lobby,
      players => []
    }.

find_game(GameNum, LGames) ->
    maps:find(GameNum, LGames).

check_player_in_game(GameNum, SPID, LGames) ->
    case find_game(GameNum, LGames) of
        {ok, Game} ->
            Players = get(players, Game),
            % {lists:member(SPID, Players), Game};
            % {case lists:keyfind(SPID, 1, Players) of
            %      {SPID, _State} -> true;
            %      false          -> false
            %  end,
            % Game};
            InGame = lists:member([{spid,SPID},{game_state,not_ready}], Players) orelse
                     lists:member([{spid,SPID},{game_state,ready}], Players) orelse
                     lists:member([{spid,SPID},{game_state,in_game}], Players),
            {InGame, Game};
        error ->
            error
    end.

% return {boolean, Updated Game}
can_planyer_join_game(Game) ->
    Players = get(players, Game),
    % Test if adding the player, goes over the allowed limit
    (length(Players)+1) =< (?MAX_PLAYERS).

log(X) ->
    io:format("~p ~p~n", [?MODULE, X]).
