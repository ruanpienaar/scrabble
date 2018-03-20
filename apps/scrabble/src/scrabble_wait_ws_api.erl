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
    % io:format("[~p] init ~p~n", [?MODULE, Opts]),
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
        {ok, [<<"j">>, GameNum, SPID]} ->
            % Join game;
            GameNumInt = scrabble_utils:ens_int(GameNum),
            case scrabble_lobby:join_game(SPID, GameNumInt) of
                true ->
                    {ok, Game} = scrabble_lobby:get_game(GameNumInt),
                    % Only 1 game
                    Json = jsx:encode([{'awaiting_game', Game}]),
                    {reply, {text, Json}, State};
                false ->
                    Json = jsx:encode([{'redirect', 'index.html'}]),
                    {reply, {text, Json}, State}
            end;
        {ok, [<<"s">>, _GameNum, _SPID]} ->
            % spectate
            Json = jsx:encode([]),
            {reply, {text, Json}, State};
        error ->
            Json = jsx:encode([{'redirect', 'index.html'}]),
            {reply, {text, Json}, State}
    end.

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

websocket_info(Info, State) ->
    io:format("[~p] websocket info ~p ~n", [?MODULE, Info]),
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
