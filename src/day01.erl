-module(day01).

-export([ solve_part1/1
        , solve_part2/1
        ]).

-define(PERIOD, 100).

%% Part 1: simulate dial position mod 100, count how many times it lands on 0.
%% O(N instructions).

solve_part1(Filename) ->
    {ok, Bin} = file:read_file(Filename),
    scan_p1(Bin, 50, 0).

scan_p1(<<>>, _Pos, Count) ->
    Count;
scan_p1(<<$\n, Rest/binary>>, Pos, Count) ->
    scan_p1(Rest, Pos, Count);
scan_p1(<<Dir, Rest0/binary>>, Pos, Count) ->
    Delta = if Dir =:= $L -> -1; true -> 1 end,
    {Clicks, Rest1} = parse_uint(Rest0, 0),
    NewPos = (Pos + ?PERIOD + Delta * Clicks) rem ?PERIOD,
    scan_p1(Rest1, NewPos, Count + if Pos =:= 0 -> 1; true -> 0 end).

%% Part 2: count all intermediate zero-crossings (one per click).
%% O(total_clicks) naively. Instead, use the analytical formula:
%%
%% For start position S, delta D (±1), and C clicks, we want
%%   #{k ∈ [1,C] : (S + k*D) ≡ 0 (mod 100)}
%% The first such k is k0 = (-S*D) mod 100, or 100 if that is 0.
%% Subsequent hits are every 100 clicks, so the count is:
%%   0                       if C < k0
%%   (C - k0) div 100 + 1   otherwise
%% This is O(1) per instruction, O(N) total.

solve_part2(Filename) ->
    {ok, Bin} = file:read_file(Filename),
    scan_p2(Bin, 50, 0).

scan_p2(<<>>, _Pos, Count) ->
    Count;
scan_p2(<<$\n, Rest/binary>>, Pos, Count) ->
    scan_p2(Rest, Pos, Count);
scan_p2(<<Dir, Rest0/binary>>, Pos, Count) ->
    Delta = if Dir =:= $L -> -1; true -> 1 end,
    {Clicks, Rest1} = parse_uint(Rest0, 0),
    NewPos = (Pos + ?PERIOD + Delta * Clicks) rem ?PERIOD,
    scan_p2(Rest1, NewPos, Count + count_zeros(Pos, Delta, Clicks)).

count_zeros(Start, Delta, Clicks) ->
    K = ((-Start * Delta) rem ?PERIOD + ?PERIOD) rem ?PERIOD,
    K0 = if K =:= 0 -> ?PERIOD; true -> K end,
    if Clicks < K0 -> 0;
       true        -> (Clicks - K0) div ?PERIOD + 1
    end.

parse_uint(<<D, Rest/binary>>, Acc) when D >= $0, D =< $9 ->
    parse_uint(Rest, Acc * 10 + (D - $0));
parse_uint(Rest, Acc) ->
    {Acc, Rest}.
