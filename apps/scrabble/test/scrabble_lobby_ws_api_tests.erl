-module(scrabble_lobby_ws_api_tests).

-include_lib("eunit/include/eunit.hrl").

-define(WS, erlang_testing_web_socket).
-define(HOST, "localhost").
-define(PORT, 9876).
-define(WS_RESPONSE_TIMEOUT, 1000).

%% -------------------------------------------
%% Tesing websocket request responses

websocket_test_() ->
    {setup,
        fun() ->
            ok
        end,
        fun(_) ->
            ok
        end,
        [
            {foreachx,
                fun(_) ->
                    {ok, Apps} = application:ensure_all_started(scrabble),
                    {ok, LobbyWsClientPid, StartedApps} = ?WS:start_client(?HOST, ?PORT, "/sws"),
                    {LobbyWsClientPid,
                     StartedApps,
                     Apps}
                end,
                fun(_, {LobbyWsClientPid, StartedApps, Apps}) ->
                    ?WS:cleanup_client_pid(LobbyWsClientPid),
                    lists:foreach(fun(App) -> ok = application:stop(App) end, lists:reverse(StartedApps)),
                    lists:foreach(fun(App) -> ok = application:stop(App) end, lists:reverse(Apps))
                end,
                [
                    {"test get_lobby_players",
                        fun(_Test, LobbyWsClientPid) ->
                            ?_test(get_lobby_players(LobbyWsClientPid))
                        end
                    } %,
                    % {"test get_lobby_games",
                    %     fun(_Test, LobbyWsClientPid) ->
                    %         ?_test(get_lobby_games(LobbyWsClientPid))
                    %     end
                    % },
                    % {"test register_lobby_player",
                    %     fun(_Test, LobbyWsClientPid) ->
                    %         ?_test(register_lobby_player(LobbyWsClientPid))
                    %     end
                    % }
                ]
            }
        ]
    }.

get_lobby_players(LobbyWsClientPid) ->
    erlang_testing_web_socket:send_ws_request(
        self(),
        LobbyWsClientPid,
        lobby_players_req()
    ),
    receive
        X ->
            ?assertEqual(
                [],
                X
            ),
            erlang_testing_web_socket:cleanup_client_pid(LobbyWsClientPid)
    after
        ?WS_RESPONSE_TIMEOUT ->
            erlang_testing_web_socket:cleanup_client_pid(LobbyWsClientPid),
            erlang:exit(self(), {test, ?FUNCTION_NAME, failed, line, ?LINE})
    end.


% get_lobby_games(LobbyWsClientPid) ->
%     ok.

% register_lobby_player(LobbyWsClientPid) ->

%     %% register player 1

%     %% create game 1

%     % erlang_testing_web_socket:send_ws_request(self(), LobbyWsClientPid,
%     %     register_req(1, 1)),
%     % receive
%     %     X ->
%     %         ok,
%     %         erlang_testing_web_socket:cleanup_client_pid(LobbyWsClientPid)
%     % after
%     %     ?WS_RESPONSE_TIMEOUT ->
%     %         erlang_testing_web_socket:cleanup_client_pid(LobbyWsClientPid),
%     %         erlang:exit(self(), {test, ?FUNCTION_NAME, failed, line, ?LINE})
%     % end.

%     ok.

lobby_players_req() ->
    jsx:encode([{<<"request">>, <<"lobby_players">>}]).

% register_req(SPID, GUID) ->
%     jsx:encode([{<<"register_lobby_player">>, SPID},
%                 {<<"guid">>, GUID}]).
