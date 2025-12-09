-module(day03).

-export([solve_part1/1,
         solve_part2/1
        ]).

solve_part1(Filename) ->
  solve_generic(Filename, 2).

solve_part2(Filename) ->
  solve_generic(Filename, 12).

solve_generic(Filename, N) ->
  {ok, Bin} = file:read_file(Filename),
  Lines = binary:split(Bin, <<"\n">>, [global]),
  lists:foldl(fun(<<>>, Acc) -> Acc;
                 (Line, Acc) ->
                  find_max_batteries(Line, N) + Acc
              end, 0, Lines).

%% Given Line to be a binary of ints e.g. <<"1234">>, find
%% the largest "battery" which can be constructed of length N
%% by selecting N integers from the list.
find_max_batteries(Line, N) ->
  Ints = lists:map(fun(X) -> X - $0 end, binary_to_list(Line)),
  Result = find_max_batteries(Ints, N, []),
  list_to_integer(lists:map(fun(X) -> X + $0 end, Result)).

find_max_batteries(_Ints, 0, Acc) ->
  lists:reverse(Acc);
find_max_batteries(Ints, N, Acc) ->
  {A, Rest} = find_largest_from_left(Ints, N),
  find_max_batteries(Rest, N - 1, [A|Acc]).

%% Find the largest number in list scanning from the left,
%% return a tuple of {Max, Rest} such that [Max|Rest] is at
%% least "Len" digits long.
find_largest_from_left(List, Len) ->
  find_largest_from_left(List, Len, 0, []).

find_largest_from_left([], _Len, _Max, Rest) ->
  [First|Remaining] = Rest,
  {First, Remaining};
find_largest_from_left([X|List] = L, Len, Max, _Rest) when X > Max andalso length(L) >= Len ->
  find_largest_from_left(List, Len, X, L);
find_largest_from_left([_X|List], Len, Max, Rest) ->
  find_largest_from_left(List, Len, Max, Rest).
