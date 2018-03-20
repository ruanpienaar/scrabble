-module(scrabble_utils).

-export([ens_int/1]).

ens_int(V) when is_list(V) ->
    list_to_integer(V);
ens_int(V) when is_binary(V) ->
    binary_to_integer(V);
ens_int(V) when is_integer(V) ->
    V.