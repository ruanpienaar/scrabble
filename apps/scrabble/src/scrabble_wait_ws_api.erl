-module(scrabble_wait_ws_api).

-export([
    init/2,
    websocket_init/1,
    websocket_handle/2,
    websocket_info/2,
    terminate/3
]).

init(Req, _Opts) ->
    scrabble_ws_mon:monitor_ws_pid(self()),
    {cowboy_websocket, Req, #{req => Req}}.

websocket_init(#{ req := Req } = State) ->
    % io:format("[~p] websocket_init ~p~n", [?MODULE, State]),
    UriMap =
        try
            cowboy_req:match_qs([a, gid, spid], Req)
        catch
            C:E ->
                io:format("[~p] Crash ~p ~p ~p~n",
                         [?MODULE, C, E, erlang:get_stacktrace()]),
                #{}
        end,
    % io:format("[~p] URI MAP ~p~n", [?MODULE, UriMap]),
    case find_map_values([a, gid, spid], UriMap) of
        {ok, [_, _, <<"undefined">>]} ->
            Json = jsx:encode([{'redirect', 'index.html'}]),
            {reply, {text, Json}, State};
        {ok, [<<"j">>, GameNum, SPID]} -> % Join game;
            GameNumInt = scrabble_utils:ens_int(GameNum),
            case scrabble_lobby:join_game(SPID, GameNumInt) of
                true ->
                    do_subscribe(GameNumInt),
                    scrabble_notify:action({player_joined_game, SPID, GameNumInt}),
                    {reply, {text, game_json(GameNumInt)}, State};
                false ->
                    Json = jsx:encode([{'redirect', 'index.html'}]),
                    {reply, {text, Json}, State}
            end;
        % {ok, [<<"s">>, GameNum, _SPID]} ->
        %     % spectate
        %     GameNumInt = scrabble_utils:ens_int(GameNum),
        %     do_subscribe(GameNumInt),
        %     Json = jsx:encode([]),
        %     {reply, {text, Json}, State};
        error ->
            Json = jsx:encode([{'redirect', 'index.html'}]),
            {reply, {text, Json}, State}
    end.

do_subscribe(GID) ->
    true = scrabble_notify:subscribe({game_players, GID}),
    true = scrabble_notify:subscribe({game_status, GID}).

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

websocket_info({scrabble_notify, {game_players, GID}, {player_ready, _SPID, GID}}, State) ->
    % TODO: check SPID
    {reply, {text, game_json(GID)}, State};
websocket_info({scrabble_notify, {game_players, GID}, {player_joined_game, _SPID, GID}}, State) ->
    {reply, {text, game_json(GID)}, State};
websocket_info({scrabble_notify, {game_status, GID}, {game_ready, GID}}, State) ->
    {reply, {text, game_json(GID)}, State};
websocket_info({scrabble_notify, {game_status, GID}, {game_starting, GID}}, State) ->
    {reply, {text, jsx:encode([{'redirect', 'game.html'}])}, State};
websocket_info(Info, State) ->
    io:format("[~p - ~p] websocket info ~p ~n", [?MODULE, self(), Info]),
    {ok, State}.

terminate(_State, _HandlerState, _Reason) ->
    % io:format("[~p] State ~p, HandlerState ~p, Reason ~p~n",
    %           [?MODULE, State, HandlerState, Reason]).
    ok.

handle_msg(ReqJson) ->
    Json = jsx:decode(ReqJson),
    handle_decoded(Json).

handle_decoded([{<<"request">>,<<"echo">>}]) ->
    jsx:encode([{response, echo_reply}]);
handle_decoded([{<<"player_leave">>, SPID},
                {<<"gid">>, GID}]) ->
    GameNumInt = scrabble_utils:ens_int(GID),
    case scrabble_lobby:leave_game(SPID, GameNumInt) of
        true ->
            scrabble_notify:action({player_leave, SPID, GameNumInt}),
            jsx:encode([{'redirect', 'index.html'}]);
        Else ->
            io:format("[~p] Player ~p could not leave ~p reason ~p~n",
                      [?MODULE, SPID, GameNumInt, Else]),
            ok
    end;
handle_decoded([{<<"player_ready">>, SPID},
                {<<"gid">>, GID}]) ->
    GameNumInt = scrabble_utils:ens_int(GID),
    case scrabble_lobby:player_ready(SPID, GameNumInt) of
        true ->
            scrabble_notify:action({player_ready, SPID, GameNumInt}),
            ok;
        Else ->
            io:format("[~p] Player ~p not set to ready ~p reason ~p~n",
                      [?MODULE, SPID, GameNumInt, Else]),
            ok
    end;
handle_decoded([{<<"game_start">>, SPID},
                {<<"gid">>, GID}]) ->
    GameNumInt = scrabble_utils:ens_int(GID),
    ok = scrabble_lobby:start_game(SPID, GameNumInt);
handle_decoded(Json) ->
    io:format("[~p] handle_decoded ~p ~n", [?MODULE, Json]),
    jsx:encode(Json).

find_map_values(Values, Map) ->
    find_map_values(Values, Map, []).

find_map_values([], _Map, R) ->
    {ok, lists:reverse(R)};
find_map_values([H|T], Map, R) ->
    case maps:find(H, Map) of
        {ok, Value} ->
            find_map_values(T, Map, [Value | R]);
        error ->
            error
    end.

game_json(GameNumInt) ->
    {ok, Game} = scrabble_lobby:get_game(GameNumInt),
    jsx:encode([{'awaiting_game', maps:without([start_timer], Game)}]).
