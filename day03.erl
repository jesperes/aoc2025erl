-module(day03).

-export([solve_part1/1,
         find_max_batteries/1]).

solve_part1(Filename) ->
  {ok, Bin} = file:read_file(Filename),
  Lines = binary:split(Bin, <<"\n">>, [global]),
  lists:foldl(fun(<<>>, Acc) -> Acc;
                 (Line, Acc) -> find_max_batteries(Line) + Acc
              end, 0, Lines).

find_max_batteries(Line) ->
  find_max_batteries(Line, 99).

find_max_batteries(Line, Joltage) when Joltage >= 11 ->
  A = Joltage div 10 + $0,
  B = Joltage rem 10 + $0,
  RE = "^.*" ++ [A] ++ ".*" ++ [B] ++ ".*$",
  case re:run(Line, RE) of
    {match, _} -> Joltage;
    nomatch -> find_max_batteries(Line, Joltage - 1)
  end.
