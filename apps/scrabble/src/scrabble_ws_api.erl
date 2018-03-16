-module(scrabble_ws_api).

-export([init/2]).
-export([websocket_init/1]).
-export([websocket_handle/2]).
-export([websocket_info/2]).

init(Req, Opts) ->
    {cowboy_websocket, Req, Opts}.

websocket_init(State) ->
    % io:format("[~p] websocket init ~p ~p ~n", [?MODULE, self(), State]),
    true = scrabble_notify:subscribe(lobby_players),
    true = scrabble_notify:subscribe(lobby_games),
    true = scrabble_notify:subscribe(active_games),
    {ok, State}.

websocket_handle({text, Msg}, State) ->
    {reply, {text, handle_msg(Msg)}, State};
websocket_handle(Data, State) ->
    io:format("[~p] websocket handle ~p ~n", [?MODULE, Data]),
    {ok, State}.

websocket_info({timeout, _Ref, Msg}, State) ->
    {reply, {text, Msg}, State};
% {scrabble_notify,lobby_players, {new_lobby_player,<<"safari">>}}
websocket_info({scrabble_notify,lobby_players,{A, NewPlayer}}, State) 
        when A == new_lobby_player orelse
             A == rem_lobby_player ->
    Json = jsx:encode([{lobby_players, scrabble_lobby:all_players()}]),
    {reply, {text, Json}, State};
websocket_info(Info, State) ->
    io:format("[~p] websocket info ~p ~n", [?MODULE, Info]),
    {ok, State}.

handle_msg(ReqJson) ->
    Json = jsx:decode(ReqJson),
    handle_decoded(Json).

handle_decoded([{<<"register_lobby_player">>, PlayerName},
                {<<"guid">>, GUID}]) ->
    scrabble_notify:action({new_lobby_player, PlayerName}),
    NewPlayers = scrabble_lobby:register_player(PlayerName, GUID),
    jsx:encode([{lobby_players, NewPlayers}]);
handle_decoded([{<<"deregister_lobby_player">>, PlayerName},
                {<<"guid">>, GUID}]) ->
    scrabble_notify:action({rem_lobby_player, PlayerName}),
    NewPlayers = scrabble_lobby:deregister_player(PlayerName, GUID),
    jsx:encode([{lobby_players, NewPlayers}]);
handle_decoded([{<<"request">>,<<"lobby_players">>}]) ->
    jsx:encode([{lobby_players, scrabble_lobby:all_players()}]);
handle_decoded(Json) ->
    io:format("[~p] received ~p Json~n", [?MODULE, Json]),
    jsx:encode(Json).
