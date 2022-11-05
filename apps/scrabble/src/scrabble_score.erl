-module(scrabble_score).

-include("scrabble.hrl").

-export([ word_score/2 ]).

%% NB!
% The blank tile acts as a wildcard, allowing the player to choose any letter in the alphabet, but not adding anything to the score of the word.
letter_score('blank') ->
    0;
letter_score(L) when L == $e orelse L == $a orelse L == $i orelse
                     L == $o orelse L == $n orelse L == $r orelse
                     L == $t orelse L == $l orelse L == $s orelse
                     L == $u ->
    1;
letter_score(L) when L == $d orelse L == $g ->
    2;
letter_score(L) when L == $b orelse L == $c orelse L == $m orelse
                     L == $p ->
    3;
letter_score(L) when L == $f orelse L == $h orelse L == $v orelse
                     L == $w orelse L == $y ->
    4;
letter_score(L) when L == $k ->
    5;
letter_score(L) when L == $j orelse L == $x ->
    8;
letter_score(L) when L == $q orelse L == $z ->
    10.

multiplier_tiles(X, Y) when
        (X==1 andalso Y==1) orelse
        (X==8 andalso Y==1) orelse
        (X==15 andalso Y==1) orelse

        (X==1 andalso Y==8) orelse
        (X==15 andalso Y==8) orelse

        (X==1 andalso Y==15) orelse
        (X==8 andalso Y==15) orelse
        (X==15 andalso Y==15) ->
    #{
        type => word,
        multiplier => 3
    };
multiplier_tiles(X, Y) when
    (X==6 andalso Y==2) orelse
    (X==10 andalso Y==2) orelse

    (X==2 andalso Y==6) orelse
    (X==6 andalso Y==6) orelse
    (X==10 andalso Y==6) orelse
    (X==14 andalso Y==6) orelse

    (X==2 andalso Y==10) orelse
    (X==6 andalso Y==10) orelse
    (X==10 andalso Y==10) orelse
    (X==14 andalso Y==10) orelse

    (X==6 andalso Y==14) orelse
    (X==10 andalso Y==14) ->
    #{
        type => letter,
        multiplier => 3
    };
multiplier_tiles(X, Y) when
    (X==2 andalso Y==2) orelse
    (X==3 andalso Y==3) orelse
    (X==4 andalso Y==4) orelse
    (X==5 andalso Y==5) orelse

    (X==14 andalso Y==2) orelse
    (X==13 andalso Y==3) orelse
    (X==12 andalso Y==4) orelse
    (X==11 andalso Y==5) orelse

    (X==8 andalso Y==8) orelse

    (X==5 andalso Y==11) orelse
    (X==4 andalso Y==12) orelse
    (X==3 andalso Y==13) orelse
    (X==2 andalso Y==14) orelse

    (X==11 andalso Y==11) orelse
    (X==12 andalso Y==12) orelse
    (X==13 andalso Y==13) orelse
    (X==14 andalso Y==14) ->
    #{
        type => word,
        multiplier => 2
    };
multiplier_tiles(X, Y) when
    (X==4 andalso Y==1) orelse
    (X==12 andalso Y==1) orelse

    (X==7 andalso Y==3) orelse
    (X==9 andalso Y==3) orelse

    (X==1 andalso Y==4) orelse
    (X==8 andalso Y==4) orelse
    (X==15 andalso Y==4) orelse

    (X==3 andalso Y==7) orelse
    (X==7 andalso Y==7) orelse
    (X==9 andalso Y==7) orelse
    (X==13 andalso Y==7) orelse

    (X==4 andalso Y==8) orelse
    (X==12 andalso Y==8) orelse

    (X==3 andalso Y==9) orelse
    (X==7 andalso Y==9) orelse
    (X==9 andalso Y==9) orelse
    (X==13 andalso Y==9) orelse

    (X==1 andalso Y==12) orelse
    (X==8 andalso Y==12) orelse
    (X==15 andalso Y==12) orelse

    (X==7 andalso Y==13) orelse
    (X==9 andalso Y==13) orelse

    (X==4 andalso Y==15) orelse
    (X==12 andalso Y==15) ->
    #{
        type => letter,
        multiplier => 2
    };
multiplier_tiles(X, Y) ->
        na.

%% Special squares (double letter score, triple letter score, and so forth) only count the first time a letter is played on them.

%% The light blue double letter square doubles the point value for the tile played on it
%% and the blue triple letter square triples the value for the tile.
%% If a blank tile is played on either of these special squares,
%% it still contributes nothing toward the total word score because a blank tile has a score of zero.

%% The pink double word squares double the entire word value and the red triple word scores triple the entire word value.
%% This remains true even if a blank tile covers the double word score or triple word score square.
%% The pink square in the middle of the board used to start the game is a double word score.

%% Double letter and triple letter squares are counted first to obtain the initial word score and
%% then any double word score or triple word score squares are counted.

%% A player using all seven letters in their rack gains a bonus 50 points to their score after all other special squares are counted.
%% The 50 bonus points do not count toward double and triple word scores.

%% At the end of the game, all tiles in a player's rack are subtracted from their score and the highest score wins.
%% If two players tie, the person with the fewest points subtracted from their score wins.

word_score(Board, Word) ->
    %% TODO: multipliers are only valid, if not scored yet ( have not been played yet )
    %% word score multiplier ( letter score multiplier )
    %% TODO: check the connecting words ( also adds to score NB: Letter/word multipliers not valid any more )
    LettersScored = lists:foldl(
        fun(#{ x := X, y := Y, value := V } = Letter, Score) ->
            LetterScore = letter_score(V),
            LetterMultiplied =
                case multiplier_tiles(X, Y) of
                    #{ type := letter, multiplier := M } ->
                        %% TODO: check board if tile-multiplier has been scared yet?
                        LetterScore * M;
                    _ ->
                        LetterScore
                end,
            io:format("score letter ~p : ~p\n", [[V], LetterMultiplied]),
            Score + LetterMultiplied
        end,
        0,
        Word
    ),
    io:format("Letters scored ~p\n", [LettersScored]),
    WordScored = lists:foldl(
        fun(#{ x := X, y := Y, value := Value }, Score) ->
            case multiplier_tiles(X, Y) of
                #{ type := word, multiplier := M } ->
                    %% TODO: check board if tile-multiplier has been scared yet?
                    Score * M;
                _ ->
                    Score
            end
        end,
        LettersScored,
        Word
    ),
    io:format("WordScored ~p\n", [WordScored]),
    case length(Word) == ?HAND_SIZE of
        true ->
            WordScored + 50;
        false ->
            WordScored
    end.


%% At the end of game, all remaining tiles in hand, is added up, and subtracted from Total player score.