-module(scrabble_sup).

-behaviour(supervisor).

%% API
-export([start_link/0]).

%% Supervisor callbacks
-export([init/1]).

-include("scrabble.hrl").

%% ===================================================================
%% API functions
%% ===================================================================

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

%% ===================================================================
%% Supervisor callbacks
%% ===================================================================

init([]) ->
    {ok, { {one_for_one, 5, 10}, [
        #{
            id => scrabble_game_sup,
            start => {scrabble_game_sup, start_link, []},
            restart => temporary,
            type => supervisor,
            shutdown => 30000
        },
        #{
            id => scrabble_ws_mon,
            start => {scrabble_ws_mon, start_link, []},
            restart => temporary,
            type => supervisor,
            shutdown => 30000
        },
        #{
            id => scrabble_lobby,
            start => {scrabble_lobby, start_link, []},
            restart => temporary,
            type => supervisor,
            shutdown => 30000
        },
        #{
            id => scrabble_web,
            start => {scrabble_web, start_link, []},
            restart => temporary,
            type => supervisor,
            shutdown => 30000
        }
    ]} }.
