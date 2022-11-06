%% TODO: write a converstion function ( BE -> FE & FE -> BE )
%%       FE: <<"a">>, <<"b">>. BE: [97], [98].

%% TODO: link this process to game or monitor the game proc...

-module(scrabble_game_ws_api).

-export([
    init/2,
    websocket_init/1,
    websocket_handle/2,
    websocket_info/2,
    terminate/3
]).

init(Req, Opts) ->
    scrabble_ws_mon:monitor_ws_pid(self()),
    {cowboy_websocket, Req, Opts}.

websocket_init(State) ->
    %% TODO: get GID!
    GID = <<"1">>,
    % cowboy_req:match_qs([a, gid, spid], Req)
    % Pid =
    case
        whereis(
            scrabble_game:name(
                scrabble_game:safe_game_id(GID)
            )
        )
    of
        undefined ->
            Json = jsx:encode([{'redirect', 'index.html'}]),
            {reply, {text, Json}, State};
        Pid when is_pid(Pid) ->
            %% TODO: implement, player pics 1 tile, see's who get's the
            %%       highest number, and then allow that player to start
            true = do_subscribe(GID),
            {ok, #{ pid => Pid }}
    end.

do_subscribe(GID) ->
    io:format("[~p] do_subscribe ~n", [?MODULE]),
    true = scrabble_notify:subscribe({game_board_update, GID}).

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

websocket_info({scrabble_notify, {game_board_update, GID}, {word_placed, GID}}, State) ->
    #{ pid := Pid } = State,
    io:format("send out to clients!!! self:~p pid:~p", [self(), Pid]),
    Json = get_board_create_json_reply(Pid),
    {reply, {text, Json}, State};
websocket_info(Info, State) ->
    io:format("[~p - ~p] websocket info ~p ~n", [?MODULE, self(), Info]),
    {ok, State}.

terminate(State, HandlerState, Reason) ->
    io:format("[~p] State ~p, HandlerState ~p, Reason ~p~n",
              [?MODULE, State, HandlerState, Reason]),
    ok.

handle_msg(ReqJson, Pid) ->
    Json = jsx:decode(ReqJson, [{labels, attempt_atom}, return_maps]),
    handle_decoded(Json, Pid).

handle_decoded(
        #{
            request := <<"ping">>
        },
        _Pid
    ) ->
    jsx:encode(#{ response => ping_reply });
handle_decoded(
        #{
            request := <<"player_hand">>,
            player_id := SPID,
            gid := _GID,
            guid := _GUID
        },
        Pid
    ) ->
    #{ hand := Hand } = scrabble_game:get_player_info(Pid, SPID),
    HandEnc =
        [ begin
            case Tile of
                blank ->
                    blank;
                _ ->
                    list_to_binary([Tile])
            end
          end || Tile <- Hand
        ],
    jsx:encode(#{ player_hand => HandEnc });
handle_decoded(
        #{
            request := <<"game_board">>,
            player_id := _SPID,
            gid := _GID,
            guid := _GUID
        },
        Pid
    ) ->
    get_board_create_json_reply(Pid);
handle_decoded(
        #{
            place_word := SPID,
            gid := GID,
            tiles := Tiles
        },
        Pid
    ) ->
    io:format("Tiles: ~p\n", [Tiles]),
    case scrabble_game:place_word(Pid, SPID, backend_tile_format(Tiles)) of
        ok ->
            io:format(" !!! Word placed !!! \n"),
            X = scrabble_notify:action({word_placed, GID}),
            io:format(" !!! published action ~p !!! \n", [X]),
            % jsx:encode(#{ response => refresh_board });
            ok;
        {error, Reason} ->
            jsx:encode(#{ response => error, reason => Reason })
    end;
% handle_decoded(
%         #{
%             player_leave := SPID,
%             gid := GID
%         },
%         Pid
%     ) ->
%     ok = scrabble_game:player_leaves(Pid, SPID, GID),
%     jsx:encode(#{ redirect => <<"index.html">>});
handle_decoded(Json, _Pid) ->
    io:format("[~p] handle_decoded ~p ~n", [?MODULE, Json]),
    jsx:encode(Json).

backend_tile_format(Tiles) ->
    lists:map(
        fun(#{ value := Value } = Tile) ->
            [Char] = binary_to_list(Value),
            Tile#{ value => Char }
        end,
        Tiles
    ).

get_board_create_json_reply(Pid) ->
    #{ board := GameBoard } = scrabble_game:get_game_details(Pid),
    %% THis is some shitty code???
    GameBoardFE = maps:map(
        fun(_Y, YV) ->
            maps:map(
                fun(_X, XV) ->
                    case XV == undefined of
                        true ->
                            <<>>;
                        false ->
                            list_to_binary([XV])
                    end
                end,
                YV
            )
        end,
        GameBoard
    ),
    % io:format("GameBoardFE ~p\n", [GameBoardFE]),
    jsx:encode(#{ response => #{ game_board => GameBoardFE } }).