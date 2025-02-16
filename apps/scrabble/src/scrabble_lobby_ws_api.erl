-module(scrabble_lobby_ws_api).

-include_lib("kernel/include/logger.hrl").

-export([
    init/2,
    websocket_init/1,
    websocket_handle/2,
    websocket_info/2,
    terminate/3
]).

init(Req, Opts) ->
    _ = scrabble_ws_mon:monitor_ws_pid(self()),
    {cowboy_websocket, Req, Opts}.

websocket_init(State) ->
    true = do_subscribe(),
    {
        [
            {text, jsx:encode(get_all_players_json_term())},
            {text, jsx:encode(get_all_games_json_term())}
        ],
        State
    }.

do_subscribe() ->
    true = scrabble_notify:subscribe(lobby_players),
    true = scrabble_notify:subscribe(lobby_games),
    true = scrabble_notify:subscribe(active_games).

websocket_handle(Msg, State) when is_atom(Msg) ->
    do_handle_message(Msg, State);
websocket_handle({text, Msg}, State) ->
    do_handle_message(Msg, State);
websocket_handle(Data, State) ->
    log("[~p] websocket handle ~p ~n", [?MODULE, Data]),
    {ok, State}.

do_handle_message(Msg, State) ->
    log("[~p] handle ~p\n", [?MODULE, Msg]),
    case handle_msg(Msg) of
        ok ->
            {ok, State};
        Json when is_binary(Json) ->
            {reply, {text, Json}, State}
    end.

websocket_info({scrabble_notify, lobby_players,{A, _NewPlayer}}, State)
        when A == new_lobby_player orelse
             A == rem_lobby_player ->
    Json = jsx:encode( get_all_players_json_term() ),
    {reply, {text, Json}, State};
websocket_info({scrabble_notify, lobby_games, new_game}, State) ->
    Json = jsx:encode( get_all_games_json_term() ),
    {reply, {text, Json}, State};
websocket_info({scrabble_notify, lobby_games, {player_joined_game, _SPID, _GID}}, State) ->
    Json = jsx:encode( get_all_games_json_term() ),
    {reply, {text, Json}, State};
websocket_info({scrabble_notify, lobby_games, {player_leave, _SPID, _GID}}, State) ->
    Json = jsx:encode( get_all_games_json_term() ),
    {reply, {text, Json}, State};
websocket_info({scrabble_notify,{game_status, GID}, {game_starting, GID}}, State) ->
    Json = jsx:encode([{<<"action">>, <<"start_game">>}]),
    {reply, {text, Json}, State};
websocket_info(Info, State) ->
    log("[~p] websocket info ~p ~n", [?MODULE, Info]),
    {ok, State}.

get_all_players_json_term() ->
    AllPlayers = scrabble_lobby:all_players(),
    [{lobby_players, AllPlayers}].

get_all_games_json_term() ->
    AllGames = scrabble_lobby:all_games(),
    [{lobby_games, json_lobby_games(AllGames)}].

terminate(_State, _HandlerState, _Reason) ->
    % io:format("[~p] State ~p, HandlerState ~p, Reason ~p~n",
    %          [?MODULE, State, HandlerState, Reason]).
    ok.

handle_msg(ping) ->
    jsx:encode([{response, ping_reply}]);
handle_msg(ReqJson) ->
    %% TODO: LOG
    % io:format("ReqJson ~p\n", [ReqJson]),
    Json = jsx:decode(ReqJson, [return_maps]),
    handle_decoded(Json).

%% TODO: change some of these to "request" items.
% handle_decoded([{<<"register_lobby_player">>, SPID},
%                 {<<"guid">>, GUID}]) ->
handle_decoded(#{
        <<"register_lobby_player">> := SPID,
        <<"guid">> := GUID
    }) ->
    case scrabble_lobby:register_player(SPID, GUID) of
        true ->
            _ = scrabble_notify:action({new_lobby_player, SPID}),
            jsx:encode([{player_registered, SPID}]);
        false ->
            jsx:encode([{error, <<"username already taken">>}])
    end;
% handle_decoded([{<<"deregister_lobby_player">>, SPID},
%                 {<<"guid">>, GUID}]) ->
handle_decoded(#{
        <<"deregister_lobby_player">> := SPID,
        <<"guid">> := GUID
    }) ->
    _ = scrabble_lobby:deregister_player(SPID, GUID),
    scrabble_notify:action({rem_lobby_player, SPID}),
    ok;
% handle_decoded([{<<"request">>,<<"lobby_players">>}]) ->
handle_decoded(#{ <<"request">> := <<"lobby_players">> }) ->
    jsx:encode(get_all_players_json_term());
% handle_decoded([{<<"request">>,<<"create_new_game">>},
%                 {<<"spid">>, SPID}]) ->
handle_decoded(#{
        <<"request">> := <<"create_new_game">>,
        <<"spid">> := SPID
    }) ->
    scrabble_notify:action(new_game),
    _AllGames = scrabble_lobby:create_game(SPID),
    jsx:encode( get_all_games_json_term() );
% handle_decoded([{<<"request">>,<<"lobby_games">>}]) ->
handle_decoded(#{ <<"request">> := <<"lobby_games">>}) ->
    jsx:encode( get_all_games_json_term() );
% handle_decoded([{<<"request">>,<<"ping">>}]) ->#
handle_decoded(#{ <<"request">> := <<"ping">> }) ->
    jsx:encode([{response, ping_reply}]);
% handle_decoded([{<<"join_game">>,[{<<"spid">>,SPID},{<<"game">>,GameNum}]}]) ->
handle_decoded(#{
        <<"join_game">> := [
            #{
                <<"spid">> := SPID,
                <<"game">> := GameNum
            }
        ]
    }) ->
    case scrabble_lobby:join_game(SPID, GameNum) of
        game_full ->
            jsx:encode([{response, [{game_full, GameNum}]}]);
        ok ->
            jsx:encode([{response, awaiting_players}])
    end;
handle_decoded(Json) ->
    log("[~p] received ~p Json~n", [?MODULE, Json]),
    jsx:encode(Json).

json_lobby_games(AllGames) ->
    maps:fold(fun(_K, V, A) ->
        [maps:without([start_timer, game_pid], V)|A]
    end, [], AllGames).

log(F, A) ->
    logger:notice(F, A).