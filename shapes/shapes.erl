-module(shapes).
-export([perimeter/1, area/1, enclose/1, tests/0]).

% The shapes module provides a number of utility functions for a given shape.
% Currently, shapes provides the functions:
% perimeter - this calculates the perimeter of the given shape
% area - this calculates the area of the given shape
% enclose - this returns the smallest enclosing rectangle for the given shape
%
% Each shape must be represented as a tuple that describes the shape.
% Currently, shapes supports the following shapes
% {square, <Length of one side of the square>}
% {circle, <Length of the radius of the circle>}
% {triangle, <Length of side1>, <Length of side2>, <Length of side3>}
% {rectangle, <Width of the rectangle>, <Height of the rectangle>}


% Calculate the PERIMETER
% Of a square
perimeter({square, SideLength}) when is_float(SideLength) ->
    4 * SideLength;

% Of a circle
perimeter({circle, Radius}) when is_float(Radius) ->
    2 * math:pi() * Radius;

% Of a triangle
perimeter({triangle, SideLength1, SideLength2, SideLength3}) when is_float(SideLength1)
                                                             andalso is_float(SideLength2)
                                                             andalso is_float(SideLength3) ->
    SideLength1 + SideLength2 + SideLength3;

% Of a rectangle
perimeter({rectangle, SideLength1, SideLength2}) when is_float(SideLength1)
                                                 andalso is_float(SideLength2) ->
    (2 * SideLength1) + (2 * SideLength2).


% Calculate the AREA
% Of a square
area({square, SideLength}) when is_float(SideLength) ->
    SideLength * SideLength;

% Of a circle
area({circle, Radius}) when is_float(Radius) ->
    math:pi() * Radius * Radius;

% Of a triangle
% Uses Heron's formula to find the area of the triangle, given the lengths of the 3 sides
area({triangle, SideLength1, SideLength2, SideLength3}) when is_float(SideLength1)
                                                             andalso is_float(SideLength2)
                                                             andalso is_float(SideLength3) ->
    HalfPerimeter = perimeter({triangle, SideLength1, SideLength2, SideLength3})/2,
    math:sqrt(HalfPerimeter *
             (HalfPerimeter - SideLength1) *
             (HalfPerimeter - SideLength2) *
             (HalfPerimeter -  SideLength3));

% Of a rectangle
area({rectangle, SideLength1, SideLength2}) when is_float(SideLength1)
                                                 andalso is_float(SideLength2) ->
    SideLength1 * SideLength2.


% Calculate the SMALLEST ENCLOSING RECTANGLE
% For a square
enclose({square, SideLength}) when is_float(SideLength) ->
    {rectangle, SideLength, SideLength};

% For a circle
enclose({circle, Radius}) when is_float(Radius) ->
    {rectangle, 2 * Radius, 2 * Radius};

% For a triangle
% Finds the longest side of the triangle,
% Then uses the formula Area = BaseLength * Height / 2 to find the height of the triangle
enclose({triangle, SideLength1, SideLength2, SideLength3}) when is_float(SideLength1)
                                                             andalso is_float(SideLength2)
                                                             andalso is_float(SideLength3) ->
    LongestSide = find_max(SideLength1, SideLength2, SideLength3),
    Height = 2 * area({triangle, SideLength1, SideLength2, SideLength3})/LongestSide,
    {rectangle, LongestSide, Height};
    
% For a rectangle
enclose({rectangle, SideLength1, SideLength2}) when is_float(SideLength1)
                                                 andalso is_float(SideLength2) ->
    {rectangle, SideLength1, SideLength2}.


% Internal functions

% Function find_max finds the maximum value from a list
find_max(X, Y, Z) ->
    max(X, max(Y, Z)).

% TESTS
tests() ->
    % Test square functions
    12.0 = perimeter({square, 3.0}),
    9.0 = area({square, 3.0}),
    {rectangle, 3.0, 3.0} = enclose({square, 3.0}),

    % Test circle functions
    true = 18.84 < perimeter({circle, 3.0}) andalso 18.85 > perimeter({circle, 3.0}),
    true = 28.27 < area({circle, 3.0}) andalso 28.28 > area({circle, 3.0}),
    {rectangle, 6.0, 6.0} = enclose({circle, 3.0}),

    % Test triangle functions
    18.0 = perimeter({triangle, 5.0, 5.0, 8.0}),
    12.0 = area({triangle, 5.0, 5.0, 8.0}),
    {rectangle, 8.0, 3.0} = enclose({triangle, 5.0, 5.0, 8.0}),

    % Test rectangle functions
    14.0 = perimeter({rectangle, 3.0, 4.0}),
    12.0 = area({rectangle, 3.0, 4.0}),
    {rectangle, 3.0, 4.0} = enclose({rectangle, 3.0, 4.0}),

    tests_worked.
