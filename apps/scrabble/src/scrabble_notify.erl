-module(scrabble_notify).

-export([action/1,
         subscribe/1
]).

%% ----------
%% just to keep track of what is being sent...

%% lobby_players
action(A = {new_lobby_player, PlayerName}) ->
    publish(lobby_players, A);
action(A = {rem_lobby_player, PlayerName}) ->
    publish(lobby_players, A).

% action({node_connected, Node, Cookie}) ->
%     publish({node_events}, {node_connected, Node, Cookie});
% action({node_disconnected, Node, Cookie}) ->
%     publish({node_events}, {node_disconnected, Node, Cookie});
% action({node_connecting, Node}) ->
%     publish({node_events}, {node_connecting, Node});
% action({node_deleted, Node}) ->
%     publish({node_events}, {node_deleted, Node}).

%% ----------

subscribe(Topic) when Topic == lobby_players orelse
                      Topic == lobby_games orelse 
                      Topic == active_games ->
    gproc:reg({p, l, {?MODULE, Topic}}).

publish(Topic, Data) when Topic == lobby_players orelse
                          Topic == lobby_games orelse 
                          Topic == active_games ->
    gproc:send({p, l, {?MODULE, Topic}}, {?MODULE, Topic, Data}).

%% ----------