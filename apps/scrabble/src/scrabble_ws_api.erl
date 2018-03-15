-module(scrabble_ws_api).

-export([init/2]).
-export([websocket_init/1]).
-export([websocket_handle/2]).
-export([websocket_info/2]).

init(Req, Opts) ->
    {cowboy_websocket, Req, Opts}.

websocket_init(State) ->
    {ok, State}.

websocket_handle({text, Msg}, State) ->
    {reply, {text, handle_msg(Msg)}, State};
websocket_handle(_Data, State) ->
    {ok, State}.

websocket_info({timeout, _Ref, Msg}, State) ->
    {reply, {text, Msg}, State};
websocket_info(_Info, State) ->
    {ok, State}.

handle_msg(ReqJson) ->
    Json = jsx:decode(ReqJson),
    io:format("[~p] received ~p Json~n", [?MODULE, Json]),
    handle_decoded(Json).

% [scrabble_ws_api] received [{<<"register_lobby_player">>,<<"Ruan">>},
%                             {<<"guid">>,
%                              <<"550101335373664032821865373641080192024">>}] Json

handle_decoded([{<<"register_lobby_player">>, PlayerName},
                {<<"guid">>, GUID}]) ->
    ok;
handle_decoded(Json) ->
    jsx:encode(Json).

% [scrabble_ws_api] received [{<<"register_lobby_player">>,<<"ruan">>}] Json

% request lobby players:
% [scrabble_ws_api] received [{<<"request">>,<<"lobby_players">>}] Json

% player hand:
% [scrabble_ws_api] received [[{<<"request">>,<<"player_hand">>}],
%                             [{<<"player_id">>,<<"comeon">>}]] Json

