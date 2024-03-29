%% @doc
%%
%% @end
-module(scrabble_app).

-behaviour(application).

%% Application callbacks
-export([
    start/2,
    stop/1
]).

-include("scrabble.hrl").

start(_StartType, _StartArgs) ->
    {ok, _} = scrabble_sup:start_link().

stop(_State) ->
    ok.
