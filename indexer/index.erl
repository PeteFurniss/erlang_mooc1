-module(index).
-export([tests/0, index_file/1, get_file_contents/1, show_file_contents/1]).

% The index module provides an index_file function that takes the name of a text file and creates an
% index of the words contained in the file and the line numbers on which they appear.
%
% Each entry in the generated index has the following form:
%
% {"word", [{l1, l2}, {l3, l4}, ...]}
%
% where each {li, lj} represents a range of lines on which the word appears.
%
% For example, where the word "youth" appears on lines 1, 2, 4, the following index entry is generated:
%
% {"youth", [{1,2},{4,4}]} 
%
% The index entries are ordered alpha-numerically by the indexed word. 
% Capitalised forms of the word are indexed separately from non-capitalised forms e.g. "Dog" and "dog"
% have distinct index entries.
%
% Words containing hyphens (-) and apostrophes (') are handled correctly, however, currently "words" comprising
% solely hyphens or apostrophes are also indexed, e.g. "--" gets its own index entry.

% Function to create an index of the words in a file of text by line number
%
% The basic structure of the program is as follows:
% - Read the file contents into a list of strings, one per line.
% - Parse the file into a list of tuples of the form: {<Word>, <Line Number>}. The list is unsorted at this stage.
% - Sort the word list into alpha-numeric order, with multiple entries per word sorted by line number.
% - Build an index of word to line number mappings, with the line numbers being represented by ranges as described
%   above.
% - Display the generated index to the screen.

index_file(Name) ->
    FileContents = get_file_contents(Name),
    UnsortedWordLinePairs = parse_lines(FileContents),
    SortedWordLinePairs = sort_word_line_pairs(UnsortedWordLinePairs),
    Index = build_ranges(build_index(SortedWordLinePairs)),
    display_index(Index).

% Used to read a file into a list of lines.
% Example files available in:
%   gettysburg-address.txt (short)
%   dickens-christmas.txt  (long)
  

% Get the contents of a text file into a list of lines.
% Each line has its trailing newline removed.

get_file_contents(Name) ->
    {ok,File} = file:open(Name,[read]),
    Rev = get_all_lines(File,[]),
lists:reverse(Rev).

% Auxiliary function for get_file_contents.
% Not exported.

get_all_lines(File,Partial) ->
    case io:get_line(File,"") of
        eof -> file:close(File),
               Partial;
        Line -> {Strip,_} = lists:split(length(Line)-1,Line),
                get_all_lines(File,[Strip|Partial])
    end.

% Function to parse a list of lines of text into a list of the unique words that appear on each line

parse_lines(Lines) -> parse_lines(Lines, 1, []).
parse_lines([], _LineNumber, WordLineList) -> lists:reverse(WordLineList);
parse_lines([HeadLine|TailLines], LineNumber, WordLineList) ->
    ParsedLine = lists:reverse(unique(parse_single_line(HeadLine))),
    WordLinePairs = generate_word_line_pairs(LineNumber, ParsedLine),
    parse_lines(TailLines, LineNumber + 1, lists:append(WordLinePairs, WordLineList)).

% Function to parse a single line of text into a list of the (non-unique) words that appear in it

parse_single_line(Text) -> parse_single_line(Text, "", []).
parse_single_line([], Token, Words) when Token =/= "" -> [Token|Words];
parse_single_line([], Token, Words) when Token =:= "" -> Words;
parse_single_line([HeadChar|TailChars], Token, Words) ->
    case lists:member(HeadChar, "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789-'") of
        true -> parse_single_line(TailChars, Token ++ [HeadChar], Words);
        false when Token =/= "" -> parse_single_line(TailChars, "", [Token|Words]);
        false when Token =:= "" -> parse_single_line(TailChars, "", Words)
    end.

% Function to return the unique members of a list
% Returns the last occurence of each value in the list

unique([]) -> [];
unique([HeadElement|TailElements]) ->
    case lists:member(HeadElement, TailElements) of
        true -> unique(TailElements);
        false -> [HeadElement | unique(TailElements)]
    end.

% Function to generate a list of word:line-number pairs for a given line-number and list of words

generate_word_line_pairs(LineNumber, WordList) -> generate_word_line_pairs(LineNumber, WordList, []).
generate_word_line_pairs(_LineNumber, [], WordLinePairs) -> WordLinePairs;
generate_word_line_pairs(LineNumber, [HeadWord|TailWords], WordLinePairs) ->
    generate_word_line_pairs(LineNumber, TailWords, [{HeadWord, LineNumber}|WordLinePairs]).

% Function to sort a list of word:line-number pairs into dictionary order
% Uses the standard lists:sort function with a custom function for comparing two word:line tuples.

sort_word_line_pairs(UnsortedWordLinePairs) ->
    OrderingFunction =
        fun({Word1, LineNumber1}, {Word2, LineNumber2}) ->
            LCWord1 = string:to_lower(Word1),
            LCWord2 = string:to_lower(Word2),
            if
                LCWord1 < LCWord2 -> true;
                (LCWord1 =:= LCWord2) andalso (Word1 < Word2) -> true;
                (Word1 =:= Word2) andalso (LineNumber1 < LineNumber2) -> true;
                (Word1 =:= Word2) andalso (LineNumber1 =:= LineNumber2) -> true;
                true -> false
            end
        end,
    lists:sort(OrderingFunction, UnsortedWordLinePairs).

% Function to build an index that maps each word to the lines on which it appears

build_index(SortedWordLinePairs) -> lists:reverse(build_index(SortedWordLinePairs, "", [], [])).
build_index([], "", _, Index) -> Index;
build_index([], CurrentWord, CurrentLineNumberList, Index) -> [{CurrentWord, CurrentLineNumberList}|Index];
build_index([{NextWord, NextLineNumber}|TailWordLinePairs], NextWord, CurrentLineNumberList, Index) ->
        build_index(TailWordLinePairs, NextWord, CurrentLineNumberList ++ [NextLineNumber], Index);
build_index([{NextWord, NextLineNumber}|TailWordLinePairs], CurrentWord, CurrentLineNumberList, Index)
    when (CurrentWord =/= "") ->
        build_index(TailWordLinePairs, NextWord, [NextLineNumber], [{CurrentWord, CurrentLineNumberList}|Index]);
build_index([{NextWord, NextLineNumber}|TailWordLinePairs], CurrentWord, _CurrentLineNumberList, Index)
    when (CurrentWord =:= "") ->
        build_index(TailWordLinePairs, NextWord, [NextLineNumber], Index).

% Function to build line-number ranges into the index

build_ranges(Index) -> lists:reverse(build_ranges(Index, [])).
build_ranges([], IndexWithRanges) -> IndexWithRanges;
build_ranges([{HeadIndexWord, HeadIndexLineNumbers}|TailIndexEntries], IndexWithRanges) ->
    build_ranges(TailIndexEntries, [{HeadIndexWord, generate_ranges(HeadIndexLineNumbers)}|IndexWithRanges]).

% Function to generate a list of line ranges from a list of line numbers

generate_ranges(LineNumberList) -> lists:reverse(generate_ranges(LineNumberList, {}, [])).
generate_ranges([], {}, LineNumberRangeList) -> LineNumberRangeList;
generate_ranges([], {FirstLine, LastLine}, LineNumberRangeList) -> [{FirstLine, LastLine}|LineNumberRangeList];
generate_ranges([HeadLineNumber|TailLineNumbers], {}, LineNumberRangeList) ->
    generate_ranges(TailLineNumbers, {HeadLineNumber, HeadLineNumber}, LineNumberRangeList);
generate_ranges([HeadLineNumber|TailLineNumbers], {FirstLine, LastLine}, LineNumberRangeList) when HeadLineNumber =:= LastLine + 1->
    generate_ranges(TailLineNumbers, {FirstLine, HeadLineNumber}, LineNumberRangeList);
generate_ranges([HeadLineNumber|TailLineNumbers], {FirstLine, LastLine}, LineNumberRangeList) ->
    generate_ranges(TailLineNumbers, {HeadLineNumber, HeadLineNumber}, [{FirstLine, LastLine}|LineNumberRangeList]).

% Function to display the contents of the index

display_index([]) -> ok;
display_index([HeadIndexEntry|TailIndexEntries]) ->
    io:format("~p~n", [HeadIndexEntry]),
    display_index(TailIndexEntries).
     
% Show the contents of a list of strings.
% Can be used to check the results of calling get_file_contents.

show_file_contents([L|Ls]) ->
    io:format("~s~n",[L]),
    show_file_contents(Ls);
 show_file_contents([]) ->
    ok.    

tests() ->
    % Test parse_single_line
    ["World", "Hello"] = parse_single_line("Hello  World!"),
    ["Hello", "World", "Hello"] = parse_single_line("Hello  World! Hello!"),
    ["Hello", "Hello-World"] = parse_single_line("Hello-World! Hello!"),
    ["Hello", "Hello-World"] = parse_single_line("Hello-World! (Hello!)"),

    % Test unique
    [] = unique([]),
    [2, 3, 1, "xyz", "abc"] = unique([1,2,3,3,1,"abc","xyz","xyz","abc"]),

    % Test generate_word_line_pairs
    [] = generate_word_line_pairs(2, []),
    [{"said", 2}, {"he", 2}, {"World", 2}, {"Hello", 2}] = generate_word_line_pairs(2, ["Hello", "World", "he", "said"]),

    % Test parse_lines
    [] = parse_lines([]),
    [{"Hello",1},{"World",1},{"he",1},{"said",1},{"to",2},{"his",2},{"adoring",2},{"audience",2}] = parse_lines(["Hello World, he said", "to his adoring audience."]),

    % Test sort_word_line_pairs
    [{"dog", 1}, {"Doggy", 1}] = sort_word_line_pairs([{"dog",1}, {"Doggy", 1}]),
    [{"dog", 1}, {"Doggy", 1}] = sort_word_line_pairs([{"Doggy",1}, {"dog", 1}]),
    [{"Dog", 2}, {"dog", 1}] = sort_word_line_pairs([{"dog",1}, {"Dog", 2}]),
    [{"Dog", 2}, {"dog", 1}] = sort_word_line_pairs([{"Dog",2}, {"dog", 1}]),
    [{"dog", 1}, {"dog", 2}] = sort_word_line_pairs([{"dog",2}, {"dog", 1}]),
    [{"dog", 1}, {"dog", 2}] = sort_word_line_pairs([{"dog",1}, {"dog", 2}]),
    [{"dog", 1}, {"dog", 1}] = sort_word_line_pairs([{"dog",1}, {"dog", 1}]),
    [{"Dog", 1}, {"Dog", 2}, {"dog", 1}, {"Doggy", 1}] = sort_word_line_pairs([{"Doggy", 1}, {"dog",1}, {"Dog", 2}, {"Dog", 1}]),

    % Test build_index
    [] = build_index([]),
    [{"Hello", [1]}] = build_index([{"Hello", 1}]),
    [{"Hello", [1,2]}] = build_index([{"Hello", 1}, {"Hello", 2}]),
    [{"Hello", [1,2]}, {"hello", [1,2]}] = build_index([{"Hello", 1}, {"Hello", 2}, {"hello", 1}, {"hello", 2}]),

    % Test build ranges
    [] = build_ranges([]),
    [{"Hello", [{1,2},{4,4}]}, {"World", [{1,1}]}] = build_ranges([{"Hello", [1,2,4]}, {"World", [1]}]),

    % Return atom to indicate that the tests worked successfully - if they don't, an error will be displayed
    tests_worked.
