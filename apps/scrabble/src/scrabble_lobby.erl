-module(scrabble_lobby).

-export([
    start_link/0,
    register_player/2,
    deregister_player/2,
    all_players/0
]).

%% TODO: build expiry on users...

-behaviour(gen_server).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-define(SERVER, ?MODULE).

% -----------------

start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, #{ players => [] }, []).

register_player(PlayerName, GUID) ->
    gen_server:call(?MODULE, {register_player, PlayerName, GUID}).

deregister_player(PlayerName, GUID) ->
    gen_server:call(?MODULE, {deregister_player, PlayerName, GUID}).

all_players() ->
    gen_server:call(?MODULE, {all_players}).

% -----------------

init(State) ->
    {ok, State}.

handle_call({register_player, PlayerName, GUID}, _From, 
            #{ players := Players } = State) ->
    NewPlayers = 
        case lists:member({PlayerName, GUID}, Players) of
            true ->
                log({player_and_guid_already_registered, {PlayerName, GUID}}),
                Players;
            false ->
                [{PlayerName, GUID} | Players]
        end,
    {reply, proplists:get_keys(NewPlayers), State#{ players => NewPlayers }};
handle_call({deregister_player, PlayerName, GUID}, _From, #{ players := Players } = State) ->
    NewPlayers = lists:keydelete(PlayerName, 1, Players),
    {reply, proplists:get_keys(NewPlayers), State#{ players => NewPlayers }};
handle_call({all_players}, _From, #{ players := Players } = State) ->
    {reply, proplists:get_keys(Players), State};
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

log(X) ->
    io:format("~p ~p~n", [?MODULE, X]).