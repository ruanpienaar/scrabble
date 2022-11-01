-module(scrabble_game_sup).

-export([
    start_link/0,
    start_child/2,
    show_first_child_state/0
]).

-behaviour(supervisor).

-export([init/1]).

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, {}).

init({}) ->
    {ok, {#{intensity => 60, period => 5}, []}}.

start_child(GID, PlayerList) ->
    supervisor:start_child(?MODULE, child_spec(GID, PlayerList)).

child_spec(GID, PlayerList) ->
    #{
        id => {game, GID},
        start => {scrabble_game, start_link, [GID, PlayerList]},
        restart => temporary,
        shutdown => 5000
    }.

show_first_child_state() ->
    sys:get_state(element(2, hd(supervisor:which_children(?MODULE)))).