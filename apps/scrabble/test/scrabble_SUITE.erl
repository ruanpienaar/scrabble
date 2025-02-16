-module(scrabble_SUITE).

%% TODO: use _ping/_pong timeout config function, instead of hardcoding 20/etc

-export([
    all/0,
    suite/0,
    groups/0,
    init_per_suite/1,
    end_per_suite/1,
    group/1,
    init_per_group/2,
    end_per_group/2,
    init_per_testcase/2,
    end_per_testcase/2
]).
% Success Tests
-export([
    join_lobby/1
]).
% Failure tests
-export([

]).

-include_lib("common_test/include/ct.hrl").
-include_lib("kernel/include/logger.hrl").

% Returns a list of all test cases and groups in the suite. (Mandatory)
all() ->
    [
    {group, success_test_group}
    %,{group, failure_test_group}
    ].

all_success() ->
    [
        join_lobby
    ].

% all_failure() ->
%     [
%     ].

% Information function used to return properties for the suite. (Optional)
suite() ->
    [{timetrap, {minutes, 10}} % wait for 10, better than the default 30min wait.
    ].

% For declaring test case groups. (Optional)
groups() ->
    [
        {success_test_group,
            % [shuffle,{repeat,10}],
            [],
            all_success()
        }
     %  ,{failure_test_group, [shuffle,{repeat,10}], all_failure()}
    ].

% Suite level configuration function, executed before the first test case. (Optional)
init_per_suite(Config) ->
    [] = os:cmd("epmd -daemon"),
    {ok, _} = erlang_testing:start_distrib(test_node, shortnames),
    ok = application:load(gun),
    {ok, DepApps} = application:ensure_all_started(gun),
    [{dep_apps, DepApps} | Config].

% Suite level configuration function, executed after the last test case. (Optional)
end_per_suite(Config) ->
    {dep_apps, DepApps} = lists:keyfind(dep_apps, 1, Config),
    [ ok = application:stop(D) || D <- DepApps ],
    erlang_testing:stop_distrib().

% Information function used to return properties for a test case group. (Optional)
group(_GroupName) ->
    [].

% Configuration function for a group, executed before the first test case. (Optional)
init_per_group(_GroupName, Config) ->
    ?LOG_NOTICE(#{ config => Config }),
    Config.

% Configuration function for a group, executed after the last test case. (Optional)
end_per_group(_GroupName, _Config) ->
    ok.

% Configuration function for a testcase, executed before each test case. (Optional)
init_per_testcase(TestCase, Config) ->
    case TestCase of
        join_lobby ->
            {ok, _} = dbg:tracer(),
            {ok, _} = dbg:p(all, call),
            % {ok, _} = dbg:tpl(gun, cx),
            % {ok, _} = dbg:tpl(gun_default_event_h, cx),
            % {ok, _} = dbg:tpl(holster_ws, cx),
            % {ok, _} = dbg:tpl(holster, cx),
            % {ok, _} = dbg:tpl(scrabble_lobby_ws_api, handle_decoded, cx),
            ok;
        _ ->
            ok
    end,
    % _ = ct:print("Here ~p\n", [file:get_cwd()]),
    % CWD set to: "/storage/code/scrabble/_build/test/logs/ct_run.nonode@nohost.2023-03-05_15.34.08"
    ReleasePath = "/storage/code/scrabble/_build/default/lib/*/ebin",
    % _build/default/lib/*/ebin

    %% TODO: slaves still running if test fails badly.
    %%       find a way to terminate slaves here.

    {ok, SlaveName} = ct_slave:start(
        scrabble_application_node,
        [
            {monitor_master, true},
            {erl_flags, " -pa " ++ ReleasePath ++ " -config /storage/code/scrabble/config/test.config "},
            {startup_functions, [{application, ensure_all_started, [scrabble]}]}
            % {env, [{EnvVar, Value}]}
        ]
    ),
    Slaves = [SlaveName],
    % _ = ct:pal("Test case config ~p\n", [Config]),
    % {slaves, Slaves} = lists:keyfind(slaves, 1, Config),
    % {ok, ScrabbleApplicationNode} = get_scrabble_applicaion_node(Slaves),
    % rpc:call()
    % _ = ct:print("Hello, World!", []),
    % _ = ct:pal("SN apps ~p\n", [rpc:call(ScrabbleApplicationNode, code, get_path, [])]),
    % _ = ct:pal("SN apps ~p\n", [rpc:call(ScrabbleApplicationNode, application, which_applications, [])]),
    % _ = ct:pal("Connect Scrabble Node ~p\n", [gen_tcp:connect("localhost", 9876, [])]),
    % {ok, WsPid, WsHeaders} =
    %     holster:ws_connect(
    %         "ws://localhost:9876/sws",
    %         #{
    %             connected_idle_timeout => infinity
    %         }
    %     ),
    {ok, WsPid, _WsHeaders} =
        holster:ws_connect(
            "ws://localhost:9876/sws",
            #{
                connect_timeout => timer:seconds(60)
            },
            [
                {ws_await_up_timeout, timer:seconds(60)},
                {connected_idle_timeout, timer:seconds(60)},
                {ws_upgrade_timeout, timer:seconds(60)}
            ],
            [],
            #{}
        ),
    % ct:pal("WsHeaders ~p\n", [WsHeaders]),
    % {ok, ScrabbleApplicationNode} = get_scrabble_applicaion_node(Slaves),
    % ok = rpc:call(ScrabbleApplicationNode, application, ensure_all_started, [scrabble]),
    [{slaves, Slaves}, {ws_pid, WsPid} | Config].

% Configuration function for a testcase, executed after each test case. (Optional)
end_per_testcase(_TestCase, Config) ->
    ok = dbg:stop_clear(),
    % true = ets:delete(node_table),
    {slaves, Slaves} = lists:keyfind(slaves, 1, Config),
    true = erlang_testing:cleanup_slaves(Slaves).

%% ---------------------------------------------------------------
%% Test cases

join_lobby(Config) ->
{ws_pid, WsPid} = proplists:lookup(ws_pid, Config),
%% --- REGISTER/JOIN ------

    %% Lobby players
    receive
        {ws_response,{text,<<"{\"lobby_players\":[]}">>}} ->
            ct:print("Received Lobby players\n", []),
            % ct:print("Received ~p\n", [X])
            ok
    after
        timer:seconds(5) ->
            ct:print("Did not receive ~p\n", [lobby_players]),
            erlang:halt({?FUNCTION_NAME, ?LINE})
    end,

    %% Lobby Games
    receive
        {ws_response,{text,<<"{\"lobby_games\":[]}">>}} ->
            ct:print("Received Lobby games\n", []),
            % ct:print("Received ~p\n", [X])
            ok
    after
        timer:seconds(5) ->
            ct:print("Did not receive ~p\n", [lobby_games]),
            erlang:halt({?FUNCTION_NAME, ?LINE})
    end,

%% --- END OF REGISTER/JOIN ------

%% --- JOIN LOBBY ------

    ok = holster_ws:ws_send(
        WsPid,
        {
            text,
            jsx:encode(#{
                register_lobby_player => <<"dumbo">>,
                guid => 25011866453736114000537365863153624
            })
        }
    ),
    receive
        {ws_response, {text, <<"{\"player_registered\":\"dumbo\"}">>}} ->
            ct:print("Received register player\n", []),
            ok
    after
        timer:seconds(5) ->
            ct:print("Did not receive ~p\n", [xxx]),
            erlang:halt({?FUNCTION_NAME, ?LINE})
    end,
    %% Notification to all clients ( new player registered / joined )
    receive
        {ws_response, {text, <<"{\"lobby_players\":[\"dumbo\"]}">>}} ->
            % ct:print("Received register player AAA : ~p\n", []),
            ok
    after
        timer:seconds(5) ->
            ct:print("Did not receive ~p\n", [xxx]),
            erlang:halt({?FUNCTION_NAME, ?LINE})
    end,

    %% Register again
    ok = holster_ws:ws_send(
        WsPid,
        {
            text,
            jsx:encode(#{
                register_lobby_player => <<"dumbo">>,
                guid => 25011866453736114000537365863153624
            })
        }
    ),
    receive
        {ws_response, {text, <<"{\"error\":\"username already taken\"}">>}} ->
            % ct:print("Received register player: ~p\n", [B]),
            ok
    after
        timer:seconds(5) ->
            ct:print("Did not receive ~p\n", [xxx]),
            erlang:halt({?FUNCTION_NAME, ?LINE})
    end,

%% --- END OF JOIN LOBBY ------

%% --- PING ------

    %% Simulate the client sending a ping here!
    ok = holster_ws:ws_send(WsPid, {text, jsx:encode(#{request => <<"ping">>})}),
    receive
        {ws_response, {text, <<"{\"response\":\"ping_reply\"}">>}} ->
            ct:print("Received ping reply\n", []),
            ok;
        Any ->
            ct:print("NOTIDEAL, Received Any : ~p\n", [{?LINE, Any}])
    after
        timer:seconds(5) ->
            ct:print("Did not receive ~p\n", [xxx]),
            erlang:halt({?FUNCTION_NAME, ?LINE})
    end,

%% --- END OF PING ------

    %% End client websocket
    ct:print("SELF ~p, conn info ~p\n", [self(), erlang:process_info(WsPid)]),
    true = erlang:unlink(WsPid),
    ok = holster:ws_close(WsPid),

    ok.

%% ---------------------------------------------------------------
%% Internal Functions

get_scrabble_applicaion_node(Slaves) ->
    [ScrabbleApplicationNode] = Slaves,
    {ok, ScrabbleApplicationNode}.