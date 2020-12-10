-module(scrabble_game_ws_api).

-export([
    init/2,
    websocket_init/1,
    websocket_handle/2,
    websocket_info/2,
    terminate/3
]).

init(Req, Opts) ->
    {cowboy_websocket, Req, Opts}.

websocket_init(_State) ->
    GID = 1,
    Pid =
        case whereis(scrabble_game:name(GID)) of
            undefined ->
                {stop, #{}};
            P when is_pid(P) ->
                P
        end,
    %% TODO: implement, player pics 1 tile, see's who get's the
    %%       highest number, and then allow that player to start
    {ok, #{ pid => Pid }}.

websocket_handle({text, Msg}, #{ pid := Pid } = State) ->
    case handle_msg(Msg, Pid) of
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

terminate(State, HandlerState, Reason) ->
    io:format("[~p] State ~p, HandlerState ~p, Reason ~p~n",
              [?MODULE, State, HandlerState, Reason]),
    ok.

handle_msg(ReqJson, Pid) ->
    Json = jsx:decode(ReqJson),
    handle_decoded(Json, Pid).

handle_decoded([{<<"request">>, <<"ping">>}], _Pid) ->
    jsx:encode([{response, ping_reply}]);
handle_decoded([[{<<"request">>, <<"player_hand">>}],
                [{<<"player_id">>, SPID}],
                [{<<"guid">>, GUID}]], Pid) ->
    Hand =
        [ begin
            case Tile of
                blank ->
                    blank;
                _ ->
                    list_to_binary([Tile])
            end
          end || Tile <- scrabble_game:get_player_hand(Pid, SPID)
        ],
    jsx:encode([{player_hand, Hand}]);
handle_decoded([{<<"player_leave">>, SPID},{<<"gid">>, GID}], Pid) ->
    ok = scrabble_game:player_leaves(Pid, SPID),
    jsx:encode([{'redirect', 'index.html'}]);
handle_decoded(Json, _Pid) ->
    io:format("[~p] handle_decoded ~p ~n", [?MODULE, Json]),
    jsx:encode(Json).
