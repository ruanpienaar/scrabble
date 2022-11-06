-module(scrabble_notify).

-export([action/1,
         subscribe/1
]).

%% ----------
%% just to keep track of what is being sent...

%% lobby_players
action(A = {new_lobby_player, _SPID}) ->
    publish(lobby_players, A);
action(A = {player_joined_game, _SPID, GID}) ->
    publish(lobby_games, A),
    publish({game_players, GID}, A);
action(A = {player_leave, _SPID, _GID}) ->
    publish(lobby_games, A);
action(A = {rem_lobby_player, _SPID}) ->
    publish(lobby_players, A);
action(A = new_game) ->
    publish(lobby_games, A);
action(A = {player_ready, _SPID, GID}) ->
    publish({game_players, GID}, A);
action(A = {game_ready, GID}) ->
    publish({game_status, GID}, A);
action(A = {game_starting, GID}) ->
    publish({game_status, GID}, A);
action(A= {word_placed, GID}) ->
    publish({game_board_update, GID}, A).
% action({node_connected, Node, Cookie}) ->
%     publish({node_events}, {node_connected, Node, Cookie});
% action({node_disconnected, Node, Cookie}) ->
%     publish({node_events}, {node_disconnected, Node, Cookie});
% action({node_connecting, Node}) ->
%     publish({node_events}, {node_connecting, Node});
% action({node_deleted, Node}) ->
%     publish({node_events}, {node_deleted, Node}).

%% ----------

subscribe(Topic = lobby_players) ->
    gproc:reg({p, l, {?MODULE, Topic}});
subscribe(Topic = lobby_games) ->
    gproc:reg({p, l, {?MODULE, Topic}});
subscribe(Topic = active_games) ->
    gproc:reg({p, l, {?MODULE, Topic}});
subscribe(Topic = {game_players, _GID}) ->
    gproc:reg({p, l, {?MODULE, Topic}});
subscribe(Topic = {game_status, _GID}) ->
    gproc:reg({p, l, {?MODULE, Topic}});
subscribe(Topic = {game_board_update, _GID}) ->
    gproc:reg({p, l, {?MODULE, Topic}}).

publish(Topic = lobby_players, Data) ->
    gproc:send({p, l, {?MODULE, Topic}}, {?MODULE, Topic, Data});
publish(Topic = lobby_games, Data) ->
    gproc:send({p, l, {?MODULE, Topic}}, {?MODULE, Topic, Data});
publish(Topic = active_games, Data) ->
    gproc:send({p, l, {?MODULE, Topic}}, {?MODULE, Topic, Data});
publish(Topic = {game_players, _GID}, Data) ->
    gproc:send({p, l, {?MODULE, Topic}}, {?MODULE, Topic, Data});
publish(Topic = {game_status, _GID}, Data) ->
    gproc:send({p, l, {?MODULE, Topic}}, {?MODULE, Topic, Data});
publish(Topic = {game_board_update, _GID}, Data) ->
    gproc:send({p, l, {?MODULE, Topic}}, {?MODULE, Topic, Data}).

%% ----------
