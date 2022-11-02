-module(scrabble_score).

-export([
    letter_score/1
]).

letter_score('blank') ->
    0;
letter_score(L) when L == $e orelse L == $a orelse L == $i orelse L == $o
                     orelse L == $n orelse L == $r orelse L == $t
                     orelse L == $l orelse L == $s orelse L == $u ->
    1;
letter_score(L) when L == $d orelse L == $g ->
    2;
letter_score(L) when L == $b orelse L == $c orelse L == $m orelse L == $p ->
    3;
letter_score(L) when L == $f orelse L == $h orelse L == $v orelse L == $w
                     orelse L == $y ->
    4;
letter_score(L) when L == $k ->
    5;
letter_score(L) when L == $j orelse L == $x ->
    8;
letter_score(L) when L == $q orelse L == $z ->
    10.