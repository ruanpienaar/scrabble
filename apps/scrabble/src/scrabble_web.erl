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
    {ok, Port} = port(),
    io:format("......\nStarting cowboy on ~p\n......\n",[Port]),
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
            {"/", cowboy_static, {priv_file, scrabble, "www/index.html"}},

            %% Websocket:
            {"/sws", scrabble_ws_api, []},

            {"/[...]", cowboy_static, {priv_dir, scrabble, "/www"}}
        ]
     }
    ].

port() ->
    Port = 9876,
    {ok, application:get_env(scrabble, http_port, Port)}.

stop() ->
    cowboy:stop_listener(?COWBOY_REF).