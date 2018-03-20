-module(skeleton_ws_mod).

-export([
    init/2,
    websocket_init/1,
    websocket_handle/2,
    websocket_info/2,
    terminate/3
]).

init(Req, Opts) ->
    {cowboy_websocket, Req, Opts}.

websocket_init(State) ->
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

websocket_info(Info, State) ->
    io:format("[~p] websocket info ~p ~n", [?MODULE, Info]),
    {ok, State}.

terminate(State, HandlerState, Reason) ->
    io:format("[~p] State ~p, HandlerState ~p, Reason ~p~n",
              [?MODULE, State, HandlerState, Reason]).

handle_msg(ReqJson) ->
    Json = jsx:decode(ReqJson),
    handle_decoded(Json).

handle_decoded([{<<"request">>,<<"echo">>}]) ->
    jsx:encode([{response, echo_reply}]);
handle_decoded(Json) ->
    io:format("[~p] handle_decoded ~p ~n", [?MODULE, Json]),
    jsx:encode(Json).