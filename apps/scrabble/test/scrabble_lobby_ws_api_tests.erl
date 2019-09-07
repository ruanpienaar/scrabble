-module(scrabble_lobby_ws_api_tests).

-include_lib("eunit/include/eunit.hrl").

-define(WS, erlang_testing_web_socket).
-define(HOST, "localhost").
-define(PORT, 10987).
-define(WS_RESPONSE_TIMEOUT, 1000).
-define(TEST_PLAYER, <<"test_player">>).
-define(PLAYER_GUID, <<"250118664537367603809877603809875373621080192024">>).

-define(NODEBUG, true).

%% -------------------------------------------
%% Tesing websocket request responses

websocket_test_() ->
    {setup,
        fun() ->
            {ok, Apps2} = application:ensure_all_started(gun),
            ok = application:set_env(scrabble, http_port, ?PORT),
            {ok, Apps} = application:ensure_all_started(scrabble),
            lists:append(Apps, Apps2)
        end,
        fun(Apps) ->
            lists:foreach(fun(App) -> ok = application:stop(App) end, lists:reverse(Apps))
        end,
        [
            {foreachx,
                fun(_) ->
                    ?debugFmt("\n--- Setup ---\n", []),
                    {ok, LobbyWsClientPid} = ?WS:start_client(?HOST, ?PORT, "/sws"),
                    LobbyWsClientPid
                end,
                fun(_, LobbyWsClientPid) ->
                    ?debugFmt("\n--- Cleanup ---\n", []),
                    ?WS:cleanup_client_pid(LobbyWsClientPid),
                    Players = scrabble_lobby:all_player_details(),
                    lists:foreach(fun({SPID, GUID}) ->
                        scrabble_lobby:deregister_player(SPID, GUID)
                    end, Players),
                    scrabble_lobby:clear_all_lobby_games()
                end,
                [
                    {"ping test",
                        fun(_Test, LobbyWsClientPid) ->
                            ?_test(ping_test(LobbyWsClientPid))
                        end
                    },

                    % register lobby players
                    {"test register_lobby_player",
                        fun(_Test, LobbyWsClientPid) ->
                            ?_test(register_lobby_player(LobbyWsClientPid))
                        end
                    },

                    % deregister lobby players
                    {"test deregister_lobby_player",
                        fun(_Test, LobbyWsClientPid) ->
                            ?_test(deregister_lobby_player(LobbyWsClientPid))
                        end
                    },

                    % create new game
                    {"test create_new_game",
                        fun(_Test, LobbyWsClientPid) ->
                            ?_test(create_new_game(LobbyWsClientPid))
                        end
                    },

                    % request lobby games
                    {"test get_lobby_games",
                        fun(_Test, LobbyWsClientPid) ->
                            ?_test(get_lobby_games(LobbyWsClientPid))
                        end
                    }

                    % echo request

                    % join game


                ]
            }
        ]
    }.

ping_test(LobbyWsClientPid) ->
    erlang_testing_web_socket:send_ws_request(
        self(),
        LobbyWsClientPid,
        ping
    ),
    receive
        X ->
            ?assertEqual(
                {response,{text,<<"pong">>}},
                X
            )
    after
        ?WS_RESPONSE_TIMEOUT ->
            ?debugFmt("!!! TIMEOUT !!!", []),
            erlang_testing_web_socket:cleanup_client_pid(LobbyWsClientPid),
            erlang:exit(self(), {test, ?FUNCTION_NAME, failed, line, ?LINE})
    end.

register_lobby_player(LobbyWsClientPid) ->
    % Get lobby players
    erlang_testing_web_socket:send_ws_request(
        self(),
        LobbyWsClientPid,
        lobby_players_req()
    ),
    receive
        X1 ->
            ?debugFmt("~p\n", [X1]),
            ?assertEqual(
                {response,{text,<<"{\"lobby_players\":[]}">>}},
                X1
            )
    after
        ?WS_RESPONSE_TIMEOUT ->
            ?debugFmt("!!! TIMEOUT !!!", []),
            erlang_testing_web_socket:cleanup_client_pid(LobbyWsClientPid),
            erlang:exit(self(), {test, ?FUNCTION_NAME, failed, line, ?LINE})
    end,

    % Register lobby player
    erlang_testing_web_socket:send_ws_request(
        self(),
        LobbyWsClientPid,
        register_player_req(
            ?TEST_PLAYER,
            ?PLAYER_GUID
        )
    ),
    receive
        X2 ->
            ?assertEqual(
                {response,{text,<<"{\"lobby_players\":[\"test_player\"]}">>}},
                X2
            )
    after
        ?WS_RESPONSE_TIMEOUT ->
            ?debugFmt("!!! TIMEOUT !!!", []),
            erlang_testing_web_socket:cleanup_client_pid(LobbyWsClientPid),
            erlang:exit(self(), {test, ?FUNCTION_NAME, failed, line, ?LINE})
    end,

    % Get lobby players
    erlang_testing_web_socket:send_ws_request(
        self(),
        LobbyWsClientPid,
        lobby_players_req()
    ),
    receive
        X3 ->
            ?debugFmt("~p\n", [X3]),
            ?assertEqual(
                {response,{text,<<"{\"lobby_players\":[\"test_player\"]}">>}},
                X3
            )
    after
        ?WS_RESPONSE_TIMEOUT ->
            ?debugFmt("!!! TIMEOUT !!!", []),
            erlang_testing_web_socket:cleanup_client_pid(LobbyWsClientPid),
            erlang:exit(self(), {test, ?FUNCTION_NAME, failed, line, ?LINE})
    end.

deregister_lobby_player(LobbyWsClientPid) ->
    register_lobby_player(LobbyWsClientPid),

    % Deregister lobby player
    erlang_testing_web_socket:send_ws_request(
        self(),
        LobbyWsClientPid,
        derergister_lobby_players_req(?TEST_PLAYER, ?PLAYER_GUID)
    ),
    receive
        X1 ->
            ?debugFmt("~p\n", [X1]),
            ?assertEqual(
                {response,{text,<<"{\"lobby_players\":[]}">>}},
                X1
            )
    after
        ?WS_RESPONSE_TIMEOUT ->
            ?debugFmt("!!! TIMEOUT !!!", []),
            erlang_testing_web_socket:cleanup_client_pid(LobbyWsClientPid),
            erlang:exit(self(), {test, ?FUNCTION_NAME, failed, line, ?LINE})
    end,
    % Get lobby players
    erlang_testing_web_socket:send_ws_request(
        self(),
        LobbyWsClientPid,
        lobby_players_req()
    ),
    receive
        X2 ->
            ?debugFmt("~p\n", [X2]),
            ?assertEqual(
                {response,{text,<<"{\"lobby_players\":[]}">>}},
                X2
            )
    after
        ?WS_RESPONSE_TIMEOUT ->
            ?debugFmt("!!! TIMEOUT !!!", []),
            erlang_testing_web_socket:cleanup_client_pid(LobbyWsClientPid),
            erlang:exit(self(), {test, ?FUNCTION_NAME, failed, line, ?LINE})
    end.

create_new_game(LobbyWsClientPid) ->
    register_lobby_player(LobbyWsClientPid),

    % Create new game
    erlang_testing_web_socket:send_ws_request(
        self(),
        LobbyWsClientPid,
        create_new_game_req(?TEST_PLAYER)
    ),
    receive
        X1 ->
            ?debugFmt("~p\n", [X1]),
            ?assertEqual(
                {response,{text,<<"{\"lobby_games\":[{\"host\":\"test_player\",\"number\":1,\"players\":[],\"starting_time\":\"undefined\",\"state\":\"lobby\"}]}">>}},
                X1
            )
    after
        ?WS_RESPONSE_TIMEOUT ->
            ?debugFmt("!!! TIMEOUT !!!", []),
            erlang_testing_web_socket:cleanup_client_pid(LobbyWsClientPid),
            erlang:exit(self(), {test, ?FUNCTION_NAME, failed, line, ?LINE})
    end.

get_lobby_games(LobbyWsClientPid) ->
    register_lobby_player(LobbyWsClientPid),

    % Get lobby games
    erlang_testing_web_socket:send_ws_request(
        self(),
        LobbyWsClientPid,
        get_lobby_games_req()
    ),
    receive
        X1 ->
            ?debugFmt("~p\n", [X1]),
            ?assertEqual(
                {response,{text,<<"{\"lobby_games\":[]}">>}},
                X1
            )
    after
        ?WS_RESPONSE_TIMEOUT ->
            ?debugFmt("!!! TIMEOUT !!!", []),
            erlang_testing_web_socket:cleanup_client_pid(LobbyWsClientPid),
            erlang:exit(self(), {test, ?FUNCTION_NAME, failed, line, ?LINE})
    end,

    % Create new game
    erlang_testing_web_socket:send_ws_request(
        self(),
        LobbyWsClientPid,
        create_new_game_req(?TEST_PLAYER)
    ),
    receive
        X2 ->
            ?debugFmt("~p\n", [X2]),
            ?assertEqual(
                {response,{text,<<"{\"lobby_games\":[{\"host\":\"test_player\",\"number\":1,\"players\":[],\"starting_time\":\"undefined\",\"state\":\"lobby\"}]}">>}},
                X2
            )
    after
        ?WS_RESPONSE_TIMEOUT ->
            ?debugFmt("!!! TIMEOUT !!!", []),
            erlang_testing_web_socket:cleanup_client_pid(LobbyWsClientPid),
            erlang:exit(self(), {test, ?FUNCTION_NAME, failed, line, ?LINE})
    end,

    % Get lobby games
    erlang_testing_web_socket:send_ws_request(
        self(),
        LobbyWsClientPid,
        get_lobby_games_req()
    ),
    receive
        X3 ->
            ?debugFmt("~p\n", [X3]),
            ?assertEqual(
                {response,{text,<<"{\"lobby_games\":[]}">>}},
                X3
            )
    after
        ?WS_RESPONSE_TIMEOUT ->
            ?debugFmt("!!! TIMEOUT !!!", []),
            erlang_testing_web_socket:cleanup_client_pid(LobbyWsClientPid),
            erlang:exit(self(), {test, ?FUNCTION_NAME, failed, line, ?LINE})
    end.


%% -----------------------------------------------------------------------------
%% Requests

register_player_req(Spid, Guid) when is_binary(Spid) andalso
                                     is_binary(Guid) ->
    jsx:encode([
        {<<"register_lobby_player">>, Spid},
        {<<"guid">>, Guid}
    ]).

lobby_players_req() ->
    jsx:encode([{<<"request">>, <<"lobby_players">>}]).

derergister_lobby_players_req(SPID, GUID) ->
    jsx:encode([
        {<<"deregister_lobby_player">>, SPID},
        {<<"guid">>, GUID}
    ]).

create_new_game_req(SPID) ->
    jsx:encode([
        {<<"request">>,<<"create_new_game">>},
        {<<"spid">>, SPID}
    ]).

get_lobby_games_req() ->
    jsx:encode([
        {<<"request">>, <<"lobby_games">>}
    ]).
