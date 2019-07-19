-module(scrabble_web).

-export([
        start_link/0,
        start/0,
        stop/0
    ]).

-define(COWBOY_REF, http).

start_link() ->
    start().

start() ->
    Port = application:get_env(scrabble, http_port, 9876),
    io:format("......\nStarting cowboy webserver on port ~p\n......\n",[Port]),
    Dispatch  = cowboy_router:compile( routes() ),
    {ok, Pid} = cowboy:start_clear(?COWBOY_REF,
                                 [{port, Port}],
                                 #{env => #{dispatch => Dispatch}}
                                ),
    io:format("Cowboy Pid : ~p\n", [Pid]),
    {ok, Pid}.

routes() ->
    [
     {'_',
        [
            %% Websocket:
            {"/sws", scrabble_lobby_ws_api, []},
            {"/swp", scrabble_wait_ws_api, []},
            {"/swg", scrabble_game_ws_api, []},

            % {"/redirect", scrabble_redirect_handler, []},
            % {"/land", scrabble_land_handler, []},

            {"/", cowboy_static, {priv_file, scrabble, "www/index.html"}},
            {"/[...]", cowboy_static, {priv_dir, scrabble, "/www"}}
        ]
     }
    ].

stop() ->
    cowboy:stop_listener(?COWBOY_REF).
