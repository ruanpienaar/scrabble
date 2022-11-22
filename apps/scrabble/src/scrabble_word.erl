-module(scrabble_word).

-include_lib("stdlib/include/ms_transform.hrl").

-export([
    load_words/0,
    search_for_word/1
]).

load_words() ->
    _ = init_ets_tbl(),
    ets:insert(
        words,
        lists:map(
            %% Should we store it as a list of chars, or string?
            fun(Word) -> {Word} end,
            read_file_words()
        )
    ).

init_ets_tbl() ->
    _ = ets:new(
        words,
        [
            named_table,
            public,
            set,
            {read_concurrency, true},
            {write_concurrency, true}
        ]
    ).
%% Collins Scrabble Words (2019). 279,496 words. Words only.
read_file_words() ->
    {ok, FPID} = file:open("words-2019.txt", [read]),
    Lines = read_more_lines(FPID, rl(FPID), []),
    ok = file:close(FPID),
    Lines.

read_more_lines(_FPID, eof, R) ->
    R;
read_more_lines(FPID, {ok, D}, R) ->
    [Word] = [D--"\n"],
    read_more_lines(FPID, rl(FPID), [Word|R]).

rl(FPID) ->
    file:read_line(FPID).

%% Try and match a word with wordslist from auto-generated hand.

%% Start by using largest words, then fallback to smaller words?

%% start
%% search using all entries for word with letter 1

%% result1 ( words that have letter 1 )
%% search using result1 for word with letter 2

%% result2
%% search using result2 for word with letter 3

%% And so forth

%% Until searching words with letter returns false

%% then select word with highest score.

% Hand: strozew, possible word: towers.



%% first word attempt WIP.
%% Need to consider overlapping attempts when board is populated.LATER

% "strozew"
search_for_word(Hand) ->
    Len = length(Hand),
    keep_searching(
        Hand,
        Len-1,
        search_using_letter(
            Hand,
            get_words_by_length(Len)
        )
    ).

keep_searching(_Hand, 0, Words) ->
    Words;
% %% tried once, no words matched ( Ex: "strozew" )
keep_searching(Hand, WordSearchLength, []) ->
    keep_searching(
        Hand,
        WordSearchLength-1,
        search_using_letter(
            Hand,
            get_words_by_length(WordSearchLength)
        )
     );
%% include smaller lettered words too
keep_searching(Hand, WordSearchLength, Words) ->
    keep_searching(
        Hand,
        WordSearchLength-1,
        lists:append(
            search_using_letter(
                Hand,
                get_words_by_length(WordSearchLength)
            ),
            Words
        )
     ).

search_using_letter(Hand, SpecificLengthWords) ->
    lists:filter(
        fun(Word) -> checking_word(Word, Hand) end,
        SpecificLengthWords
    ).

checking_word(Word, Hand) ->
    %% transform word "word" and "Hand", into proplist:
    %% Ex: ["{"w", 1}, {"o", 2}, {"r", 3}, {"d", 4}, "]
    WordProplist =
        lists:zip(
            Word,
            lists:seq(1, length(Word))
        ),
    HandProplist =
        lists:zip(
            Hand,
            lists:seq(1, length(Hand))
        ),

    check_word_chars_fit_hand(WordProplist, HandProplist).


check_word_chars_fit_hand([], _HandProplist) ->
    true;
check_word_chars_fit_hand([{WordLetter, _}|Rest], HandProplist) ->
    case lists:keytake(WordLetter, 1, HandProplist) of
        false ->
            false; %% TODO: log - to see how often.
        {value, _Tuple, NewHandProplist} ->
            check_word_chars_fit_hand(Rest, NewHandProplist)
    end.


get_words_by_length(Len) ->
    ets:select(
        words,
        ets:fun2ms(fun({Word}) when length(Word) =:= Len -> Word end)
    ).


%% get words same size os hand length
%% loop words - check first word char in hand list, if hand char missing filter out.
%% carry on, util at last letter
%% IF results -> now we know, the list of words include all chars in HAND.
%% If no words have all hand letters, then deduct search space to hand-1

%% get words same size os hand length-1
%% loop words - check first word char in hand list, if hand char missing filter out.
%% carry on, util at last letter
%% IF results -> now we know, the list of words include all chars in HAND.
%% If no words have all hand letters, then deduct search space to hand-2