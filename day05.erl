-module(day05).

-export([
    solve_part1/1,
    solve_part2/1,
    benchmark/1
]).

solve_part1(Filename) ->
    {Ranges, Ids} = parse(Filename),
    number_of_fresh_ids(Ranges, Ids).

solve_part2(Filename) ->
    {Ranges, _Ids} = parse(Filename),
    total_fresh_ids(Ranges).

repeat(Fun, 1) -> Fun();
repeat(Fun, N) -> Fun(), repeat(Fun, N - 1).

benchmark(Filename) ->
    Repeats = 1000,
    {T, V} = timer:tc(fun() ->
        repeat(fun() ->
            {Ranges, Ids} = parse(Filename),
            P1 = number_of_fresh_ids(Ranges, Ids),
            P2 = total_fresh_ids(Ranges),
            {P1, P2}
        end, Repeats)
    end),
    io:format("Time: ~p usecs~n", [T / Repeats]),
    V.

parse(Filename) ->
    {ok, Bin} = file:read_file(Filename),
    [Ranges, Ids] = binary:split(Bin, <<"\n\n">>),
    L1 = lists:map(fun(B) ->
            [A1, A2] = split(B, <<"-">>),
            N1 = binary_to_integer(A1),
            N2 = binary_to_integer(A2),
            {N1, N2}
        end,
        split(Ranges, <<"\n">>)),
    L2 = lists:map(fun(B) ->
            binary_to_integer(B)
        end,
        split(Ids, <<"\n">>)),
    {L1, L2}.

split(Bin, Sep) ->
    lists:filter(
        fun(<<>>) -> false;
           (_) -> true
        end, binary:split(Bin, Sep, [global])).

number_of_fresh_ids(Ranges, Ids) ->
    lists:foldl(
        fun(Id, Acc) ->
            Acc + is_fresh(Id, Ranges)
        end, 0, Ids).

is_fresh(Id, [{Min, Max}|_Ranges]) when Id >= Min andalso Id =< Max ->
    1;
is_fresh(Id, [_|Ranges]) ->
    is_fresh(Id, Ranges);
is_fresh(_Id, []) ->
    0.

total_fresh_ids(Ranges) ->
    MergedRanges = merge_ranges_until_done(Ranges),
    count_ranges(MergedRanges, 0).

count_ranges([], N) -> N;
count_ranges([{A1, A2}|Rest], N) ->
    count_ranges(Rest, A2 - A1 + 1 + N).

merge_ranges_until_done(Ranges) ->
    SortedRanges = lists:sort(Ranges),
    {Ranges0, Merges} = merge_ranges(SortedRanges, [], 0),
    if
        Merges == 0 -> Ranges;
        true -> merge_ranges_until_done(Ranges0)
    end.

merge_ranges([], Acc, Merges) ->
    {Acc, Merges};
merge_ranges([Range], Acc, Merges) ->
    merge_ranges([], [Range|Acc], Merges);
merge_ranges([{A1,A2}, {B1,B2}|Rest], Acc, Merges) when A2 + 1 >= B1 ->
    %% merge range, note that the B range might be fully contained within A, so
    %% take max(A2, B2) as the upper bound.
    merge_ranges([{A1,max(A2, B2)}|Rest], Acc, Merges + 1);
merge_ranges([{A1,A2}, {B1,B2}|Rest], Acc, Merges) when A2 + 1 < B1 ->
    %% disjoint range
    merge_ranges([{B1,B2}|Rest], [{A1, A2}|Acc], Merges).
