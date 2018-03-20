-module(scrabble_ws_api).

-export([
    init/2,
    websocket_init/1,
    websocket_handle/2,
    websocket_info/2,
    terminate/3
]).

init(Req, Opts) ->
    scrabble_ws_mon:monitor_ws_pid(self()),
    {cowboy_websocket, Req, Opts}.

websocket_init(State) ->
    % io:format("[~p] websocket init ~p ~p ~n", [?MODULE, self(), State]),
    true = scrabble_notify:subscribe(lobby_players),
    true = scrabble_notify:subscribe(lobby_games),
    true = scrabble_notify:subscribe(active_games),
    {ok, State}.

websocket_handle({text, Msg}, State) ->
    case handle_msg(Msg) of
        ok ->
            {ok, State};
        Json ->
            {reply, {text, Json}, State}
    end;
websocket_handle(Data, State) ->
    io:format("[~p] websocket handle ~p ~n", [?MODULE, Data]),
    {ok, State}.

websocket_info({scrabble_notify,lobby_players,{A, _NewPlayer}}, State)
        when A == new_lobby_player orelse
             A == rem_lobby_player ->
    Json = jsx:encode([{lobby_players, scrabble_lobby:all_players()}]),
    {reply, {text, Json}, State};
websocket_info({scrabble_notify,lobby_games,new_game}, State) ->
    AllGames = scrabble_lobby:all_games(),
    Json = jsx:encode([{lobby_games, json_lobby_games(AllGames)}]),
    {reply, {text, Json}, State};
websocket_info(Info, State) ->
    io:format("[~p] websocket info ~p ~n", [?MODULE, Info]),
    {ok, State}.

terminate(_State, _HandlerState, _Reason) ->
    % io:format("[~p] State ~p, HandlerState ~p, Reason ~p~n",
    %          [?MODULE, State, HandlerState, Reason]).
    ok.

handle_msg(ReqJson) ->
    Json = jsx:decode(ReqJson),
    handle_decoded(Json).

handle_decoded([{<<"register_lobby_player">>, SPID},
                {<<"guid">>, GUID}]) ->
    scrabble_notify:action({new_lobby_player, SPID}),
    _NewPlayers = scrabble_lobby:register_player(SPID, GUID),
    ok;
handle_decoded([{<<"deregister_lobby_player">>, SPID},
                {<<"guid">>, GUID}]) ->
    scrabble_notify:action({rem_lobby_player, SPID}),
    _NewPlayers = scrabble_lobby:deregister_player(SPID, GUID),
    ok;
handle_decoded([{<<"request">>,<<"lobby_players">>}]) ->
    jsx:encode([{lobby_players, scrabble_lobby:all_players()}]);
handle_decoded([{<<"request">>,<<"create_new_game">>}]) ->
    AllGames = scrabble_lobby:create_game(),
    scrabble_notify:action(new_game),
    jsx:encode([{lobby_games, json_lobby_games(AllGames)}]);
handle_decoded([{<<"request">>,<<"lobby_games">>}]) ->
    AllGames = scrabble_lobby:all_games(),
    jsx:encode([{lobby_games, json_lobby_games(AllGames)}]);
handle_decoded([{<<"request">>,<<"echo">>}]) ->
    jsx:encode([{response, echo_reply}]);
handle_decoded([{<<"join_game">>,[{<<"spid">>,SPID},{<<"game">>,GameNum}]}]) ->
    case scrabble_lobby:join_game(SPID, GameNum) of
        game_full ->
            jsx:encode([{response, [{game_full, GameNum}]}]);
        ok ->
            jsx:encode([{response, awaiting_players}])
    end;
handle_decoded(Json) ->
    io:format("[~p] received ~p Json~n", [?MODULE, Json]),
    jsx:encode(Json).

json_lobby_games(AllGames) ->
    maps:fold(fun(K, V, A) ->
        [V|A]
    end, [], AllGames).