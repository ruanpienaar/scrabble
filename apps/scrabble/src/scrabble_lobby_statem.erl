-module(scrabble_lobby_statem).

%% @doc a lobby for games
%% A lobby is started when a game is created.
%% the lobby and the game processes are seperate gen_statem'
%% @end

%% API
-export([
    start_link/0
]).

%% State M exports
-export([
    % init/1, callback_mode/0, terminate/3
]).

-define(REG_NAME, {local, ?MODULE}).

%% -----------------------------------------------------------------------------
%% API

start_link() ->
    % gen_statem:start_link(?REG_NAME, ?MODULE, #{ slices => 0 }, []).
    {ok, self()}.

%% -----------------------------------------------------------------------------

% init(Data) ->
%     {ok, initial_state, Data}.
