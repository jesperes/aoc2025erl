-module(day03).
-export([solve_part1/1, solve_part2/1]).

%% Greedy selection: at each of N steps, scan bytes[I..Len-Reserved-1] for the
%% maximum digit, then advance I to just past where that max was found.
%% Accumulate the result directly as an integer — no list ↔ string conversions,
%% no length/1 calls in guards.

solve_part1(Filename) -> solve(Filename, 2).
solve_part2(Filename) -> solve(Filename, 12).

solve(Filename, N) ->
    {ok, Bin} = file:read_file(Filename),
    scan(binary:split(Bin, <<"\n">>, [global]), N, 0).

scan([], _, Acc) -> Acc;
scan([<<>> | Rest], N, Acc) -> scan(Rest, N, Acc);
scan([Line | Rest], N, Acc) ->
    scan(Rest, N, Acc + joltage(Line, byte_size(Line), N, 0, 0, 0)).

joltage(_, _, N, Step, _, Acc) when Step >= N -> Acc;
joltage(Line, Len, N, Step, I, Acc) ->
    Limit = Len - (N - Step - 1) - 1,
    {MaxD, NewI} = scan_max(Line, I, Limit, 0, I),
    joltage(Line, Len, N, Step + 1, NewI, Acc * 10 + MaxD).

scan_max(_, J, Limit, Max, MaxI) when J > Limit -> {Max, MaxI + 1};
scan_max(Line, J, Limit, Max, MaxI) ->
    D = binary:at(Line, J) - $0,
    if D > Max -> scan_max(Line, J + 1, Limit, D, J);
       true    -> scan_max(Line, J + 1, Limit, Max, MaxI)
    end.
