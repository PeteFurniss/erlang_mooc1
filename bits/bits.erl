-module(bits).
-export([bits1/1, bits2/1, tests/0]).

% Function bits1 takes a positive integer and returns the number of 1s the integer contains when
% represented in binary bits1 calls an internal function calc_bits1 with an accumulator for the number
% of 1s initialised to 0.

bits1(N) ->
    calc_bits1(N, 0).

% calc_bits1 is highly optimised and brief.
% N is progressively divided by 2 in each recursion until it reaches 0 when the accumulated number of
% 1s is returned.
% The NumOnes accumulator starts as 0 and is incremented by 1 each time N divided by 2 mod 2 = 1.
% Each successive division by 2 is effectively a right shift by one bit of N with the least-significant
% bit being represented by N rem 2.

calc_bits1(0, NumOnes) ->
    NumOnes;
calc_bits1(N, NumOnes) ->
    calc_bits1(N div 2, NumOnes + (N rem 2)).

% Function bits2 is identical to bits1 but calls a different internal function that provides an
% alternative implementation of the function to calculate the number of 1s in the binary
% representation of N.

bits2(N) ->
    calc_bits2(N, 0).

% calc_bits2 is less optimised but is perhaps more logical and more maintainable implementation
% of the calculation of the number of 1s in the binary representation of N.
% In this function, the least significant bit of N on each successive division is calculated
% explicitly as LSB. The new number of 1s is also calculated explicitly depending on whether LSB
% is 1 or 0.

calc_bits2(0, NumOnes) ->
    NumOnes;
calc_bits2(N, NumOnes) ->
    LSB = N rem 2,
    if LSB == 0 ->
        NewNumOnes = NumOnes;
    true ->
        NewNumOnes = NumOnes + 1
    end,
    calc_bits2(N div 2, NewNumOnes).

% Function tests runs both of the implementations above with the integers 0 - 8 and checks that the
% expected result is returned.
% If all the tests pass, the atom tests_worked is returned.

tests() ->
    1 = bits1(8),
    3 = bits1(7),
    2 = bits1(6),
    2 = bits1(5),
    1 = bits1(4),
    2 = bits1(3),
    1 = bits1(2),
    1 = bits1(1),
    0 = bits1(0),
    1 = bits2(8),
    3 = bits2(7),
    2 = bits2(6),
    2 = bits2(5),
    1 = bits2(4),
    2 = bits2(3),
    1 = bits2(2),
    1 = bits2(1),
    0 = bits2(0),
    tests_worked.

