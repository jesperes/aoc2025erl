-module(benchmark).
-export([run/1]).

-define(DEFAULT_RUNS, 10).
-define(BENCH_TIMEOUT_US, 5_000_000). % 5 seconds

solutions() ->
    [ {1, 1, day01, solve_part1}
    , {1, 2, day01, solve_part2}
    , {2, 1, day02, solve_part1}
    , {2, 2, day02, solve_part2}
    , {3, 1, day03, solve_part1}
    , {3, 2, day03, solve_part2}
    , {4, 1, day04, solve_part1}
    , {4, 2, day04, solve_part2}
    , {5, 1, day05, solve_part1}
    , {5, 2, day05, solve_part2}
    , {6, 1, day06, solve_part1}
    , {6, 2, day06, solve_part2}
    ].

run(Opts) ->
    FilterDay  = proplists:get_value(day,  Opts, undefined),
    FilterPart = proplists:get_value(part, Opts, undefined),
    MaxRuns    = proplists:get_value(runs, Opts, ?DEFAULT_RUNS),
    JsonPath   = proplists:get_value(json, Opts, undefined),
    Filtered = [S || {D, P, _, _} = S <- solutions(),
                     filter_match(D, FilterDay),
                     filter_match(P, FilterPart)],
    aoc_fetch:start_http(),
    Days = lists:usort([D || {D, _, _, _} <- Filtered]),
    InputMap  = maps:from_list([{D, catch aoc_fetch:ensure_input(D)}   || D <- Days]),
    AnswerMap = maps:from_list([{D, catch aoc_fetch:ensure_answers(D)} || D <- Days]),
    print_top(),
    T0 = erlang:monotonic_time(microsecond),
    Results = lists:filtermap(
        fun(S) -> run_and_print(S, MaxRuns, InputMap, AnswerMap) end, Filtered),
    TotalUS = erlang:monotonic_time(microsecond) - T0,
    print_bottom(),
    io:format("~nTotal time: ~.1f ms~n", [TotalUS / 1000.0]),
    OutPath = case JsonPath of
        undefined -> "bench/erlang.json";
        _         -> JsonPath
    end,
    write_json(OutPath, Results).

filter_match(_, undefined) -> true;
filter_match(V, V)         -> true;
filter_match(_, _)         -> false.

%% Returns {true, Result} for filtermap, or false if skipped.
run_and_print({Day, Part, Mod, Fun}, MaxRuns, InputMap, AnswerMap) ->
    File = maps:get(Day, InputMap, error),
    case is_list(File) andalso filelib:is_regular(File) of
        false ->
            skip_row(Day, Part),
            false;
        true ->
            {ExpP1, ExpP2} = case maps:get(Day, AnswerMap, {undefined, undefined}) of
                {_, _} = A -> A;
                _          -> {undefined, undefined}
            end,
            Expected = if Part =:= 1 -> ExpP1; true -> ExpP2 end,
            {AnswerStr, AvgUS, Runs} = bench(fun() -> Mod:Fun(File) end, MaxRuns),
            print_row(Day, Part, AnswerStr, AvgUS, Runs, Expected),
            {true, {Day, Part, AnswerStr, AvgUS, Runs}}
    end.

bench(Fun, MaxRuns) ->
    bench_loop(Fun, MaxRuns, 0, 0, "").

bench_loop(_, 0, TotalUS, Runs, Last) ->
    {Last, TotalUS div Runs, Runs};
bench_loop(Fun, Left, TotalUS, Runs, _) ->
    T0 = erlang:monotonic_time(microsecond),
    Result = Fun(),
    Elapsed = erlang:monotonic_time(microsecond) - T0,
    NewTotal = TotalUS + Elapsed,
    NewRuns  = Runs + 1,
    Str = to_str(Result),
    if NewTotal >= ?BENCH_TIMEOUT_US ->
           {Str, NewTotal div NewRuns, NewRuns};
       true ->
           bench_loop(Fun, Left - 1, NewTotal, NewRuns, Str)
    end.

to_str(X) when is_integer(X) -> integer_to_list(X);
to_str(X) when is_float(X)   -> float_to_list(X, [{decimals, 6}, compact]);
to_str(X) when is_binary(X)  -> binary_to_list(X);
to_str(X) when is_list(X)    -> X;
to_str(X)                    -> lists:flatten(io_lib:format("~p", [X])).

%% ── JSON output ──────────────────────────────────────────────────────────────

write_json(Path, Results) ->
    TotalUS = lists:sum([AvgUS || {_, _, _, AvgUS, _} <- Results]),
    Doc = #{
        <<"language">>         => <<"erlang">>,
        <<"total_avg_micros">> => TotalUS,
        <<"solutions">>        => [solution_map(R) || R <- Results]
    },
    ok = filelib:ensure_dir(Path),
    ok = file:write_file(Path, json:encode(Doc)),
    io:format("Wrote ~s~n", [Path]).

solution_map({Day, Part, Answer, AvgUS, Runs}) ->
    #{
        <<"name">>        => list_to_binary(
                                 lists:flatten(io_lib:format("day ~w part ~w", [Day, Part]))),
        <<"day">>         => Day,
        <<"part">>        => Part,
        <<"answer">>      => list_to_binary(Answer),
        <<"avg_micros">>  => AvgUS,
        <<"runs">>        => Runs
    }.

%% ── Formatting ───────────────────────────────────────────────────────────────

pad_left(S, N) ->
    Pad = N - length(S),
    if Pad > 0 -> lists:duplicate(Pad, $\s) ++ S;
       true    -> S
    end.

pad_right(S, N) ->
    Pad = N - length(S),
    if Pad > 0 -> S ++ lists:duplicate(Pad, $\s);
       true    -> S
    end.

green(S) -> "\e[32m" ++ S ++ "\e[0m".
red(S)   -> "\e[31m" ++ S ++ "\e[0m".

%% Column layout (matches Rust reference):
%%   │ name(14) │ answer(20) │ time(10) μs │ runs(3) runs │ check(1)   │
%% Separator widths: 16, 22, 15, 10, 5

print_top() ->
    io:format("  ┌────────────────┬──────────────────────"
              "┬───────────────┬──────────┬─────┐~n").

print_bottom() ->
    io:format("  └────────────────┴──────────────────────"
              "┴───────────────┴──────────┴─────┘~n").

print_row(Day, Part, Answer, AvgUS, Runs, Expected) ->
    Name    = pad_right(lists:flatten(io_lib:format("day ~w part ~w", [Day, Part])), 14),
    AnswerP = pad_left(truncate(Answer, 20), 20),
    TimeNum = pad_left(integer_to_list(AvgUS), 10),
    TimeStr = if AvgUS < 1000 -> green(TimeNum); true -> red(TimeNum) end,
    RunsStr = pad_left(integer_to_list(Runs), 3),
    Check   = case Expected of
                  undefined -> " ";
                  Answer    -> green("✓");
                  _         -> red("✗")
              end,
    io:format("  │ ~ts │ ~ts │ ~ts μs │ ~ts runs │ ~ts   │~n",
              [Name, AnswerP, TimeStr, RunsStr, Check]).

truncate(S, N) when length(S) > N -> lists:sublist(S, N - 3) ++ "...";
truncate(S, _)                    -> S.

skip_row(Day, Part) ->
    Name = pad_right(lists:flatten(io_lib:format("day ~w part ~w", [Day, Part])), 14),
    io:format("  │ ~ts │ ~ts │ ~ts μs │ ~ts runs │     │~n",
              [Name,
               pad_left("(no input)", 20),
               pad_left("-", 10),
               pad_left("-", 3)]).
