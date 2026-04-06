-module(day05).
-export([solve_part1/1, solve_part2/1, benchmark/1]).

%% Part 1: sort ranges by lo, build a prefix-max of hi, then for each ID
%%   binary-search for the insertion point and check the prefix max.
%%   O((N + M) log N) instead of O(N*M).
%%
%% Part 2: sort ranges, single-pass merge — O(N log N), not repeated passes.

solve_part1(Filename) ->
    {Ranges, Ids} = parse(Filename),
    Sorted   = lists:sort(Ranges),
    SortedT  = list_to_tuple(Sorted),
    PrefixT  = list_to_tuple(prefix_max(Sorted, 0, [])),
    N        = tuple_size(SortedT),
    lists:foldl(fun(Id, Acc) ->
        Pos = partition_point(SortedT, Id, 0, N),
        case Pos > 0 andalso element(Pos, PrefixT) >= Id of
            true  -> Acc + 1;
            false -> Acc
        end
    end, 0, Ids).

solve_part2(Filename) ->
    {Ranges, _} = parse(Filename),
    Merged = merge_sorted(lists:sort(Ranges), []),
    lists:sum([Hi - Lo + 1 || {Lo, Hi} <- Merged]).

%% ── Helpers ──────────────────────────────────────────────────────────────────

prefix_max([], _, Acc) -> lists:reverse(Acc);
prefix_max([{_, Hi} | Rest], M, Acc) ->
    M1 = max(Hi, M),
    prefix_max(Rest, M1, [M1 | Acc]).

%% Number of elements in T (sorted by lo) with lo ≤ Id.
partition_point(_, _, Lo, Hi) when Lo >= Hi -> Lo;
partition_point(T, Id, Lo, Hi) ->
    Mid = (Lo + Hi) div 2,
    {MidLo, _} = element(Mid + 1, T),
    if MidLo =< Id -> partition_point(T, Id, Mid + 1, Hi);
       true        -> partition_point(T, Id, Lo, Mid)
    end.

%% Single-pass merge of a sorted range list.
merge_sorted([], Acc)    -> lists:reverse(Acc);
merge_sorted([R], Acc)   -> lists:reverse([R | Acc]);
merge_sorted([{A1,A2}, {B1,B2} | Rest], Acc) when A2 + 1 >= B1 ->
    merge_sorted([{A1, max(A2, B2)} | Rest], Acc);
merge_sorted([R | Rest], Acc) ->
    merge_sorted(Rest, [R | Acc]).

parse(Filename) ->
    {ok, Bin} = file:read_file(Filename),
    [RangesBin, IdsBin] = binary:split(Bin, <<"\n\n">>),
    Ranges = [{binary_to_integer(A), binary_to_integer(B)}
              || L <- binary:split(RangesBin, <<"\n">>, [global]), L =/= <<>>,
                 [A, B] <- [binary:split(L, <<"-">>)]],
    Ids    = [binary_to_integer(L)
              || L <- binary:split(IdsBin, <<"\n">>, [global]), L =/= <<>>],
    {Ranges, Ids}.

benchmark(Filename) ->
    Repeats = 1000,
    {T, _} = timer:tc(fun() ->
        lists:foreach(fun(_) ->
            solve_part1(Filename),
            solve_part2(Filename)
        end, lists:seq(1, Repeats))
    end),
    io:format("Time: ~p usecs~n", [T / Repeats]).
