-module(aoc2025).
-export([main/1]).

main(Args) ->
    io:setopts([{encoding, utf8}]),
    dispatch(Args).

dispatch(["bench" | Args]) ->
    Opts = parse_bench_args(Args, []),
    benchmark:run(Opts);
dispatch(["input", DayStr]) ->
    aoc_fetch:start_http(),
    Path = aoc_fetch:ensure_input(list_to_integer(DayStr)),
    io:format("~s~n", [Path]);
dispatch(["answer", DayStr]) ->
    aoc_fetch:start_http(),
    {P1, P2} = aoc_fetch:ensure_answers(list_to_integer(DayStr)),
    io:format("Part 1: ~s~nPart 2: ~s~n", [opt(P1), opt(P2)]);
dispatch(["puzzle", DayStr]) ->
    aoc_fetch:start_http(),
    Path = aoc_fetch:ensure_puzzle(list_to_integer(DayStr)),
    {ok, Bin} = file:read_file(Path),
    io:format("~ts~n", [Bin]);
dispatch([DayStr | Rest]) ->
    Day = list_to_integer(DayStr),
    InputFile = case Rest of
        [F] -> F;
        []  ->
            aoc_fetch:start_http(),
            aoc_fetch:ensure_input(Day)
    end,
    run_day(Day, InputFile);
dispatch(_) ->
    io:format("Usage:~n"),
    io:format("  aoc2025 bench [<day> [<part>]] [--runs <N>]~n"),
    io:format("  aoc2025 <day> [<input_file>]~n"),
    io:format("  aoc2025 input <day>~n"),
    io:format("  aoc2025 answer <day>~n"),
    io:format("  aoc2025 puzzle <day>~n"),
    halt(1).

opt(undefined) -> "(none)";
opt(S)         -> S.

run_day(Day, InputFile) ->
    Module = list_to_atom(lists:flatten(io_lib:format("day~2..0w", [Day]))),
    io:format("=== Day ~p ===~n", [Day]),
    P1 = Module:solve_part1(InputFile),
    io:format("Part 1: ~p~n", [P1]),
    P2 = Module:solve_part2(InputFile),
    io:format("Part 2: ~p~n", [P2]).

%% bench [<day> [<part>]] [--runs <N>]
parse_bench_args([], Opts) ->
    Opts;
parse_bench_args(["--runs", N | Rest], Opts) ->
    parse_bench_args(Rest, [{runs, list_to_integer(N)} | Opts]);
parse_bench_args(["--json", Path | Rest], Opts) when hd(Path) =/= $- ->
    parse_bench_args(Rest, [{json, Path} | Opts]);
parse_bench_args(["--json" | Rest], Opts) ->
    parse_bench_args(Rest, [{json, "bench/erlang.json"} | Opts]);
parse_bench_args([V | Rest], Opts) ->
    Key = case lists:keymember(day, 1, Opts) of
        false -> day;
        true  -> part
    end,
    parse_bench_args(Rest, [{Key, list_to_integer(V)} | Opts]).
