-module(day06).

-export([
    solve_part1/1,
    solve_part2/1
]).

solve_part1(Filename) ->
    {ok, Bin} = file:read_file(Filename),
    Lines = parse(binary:split(Bin, <<"\n">>, [global])),
    [Ops|Cols] = lists:reverse(Lines),
    summarize(Ops, Cols, 0).

parse([]) -> [];
parse([<<>>|Lines]) ->
    parse(Lines);
parse([Line|Lines]) ->
    {match, M} = re:run(Line, "(\\S+)", [global, {capture, all_but_first, binary}]),
    L = lists:map(fun([Matches]) -> Matches end, M),
    [L|parse(Lines)].

summarize([], _, N) -> N;
summarize([Op|Ops], Cols, N) ->
    {Ints, Cols0} = strip_first(Cols),
    SumOrProd = add_or_mult(Op, Ints),
    summarize(Ops, Cols0, N + SumOrProd).

add_or_mult(<<"*">>, Ints) ->
    lists:foldl(fun(X, Y) -> X * Y end, 1, btoi(Ints));
add_or_mult(<<"+">>, Ints) ->
    lists:foldl(fun(X, Y) -> X + Y end, 0, btoi(Ints)).

btoi(List) ->
    lists:map(fun(X) -> binary_to_integer(X) end, List).

strip_first(List) ->
    strip_first(List, {[], []}).
strip_first([], {Ints, Rest}) ->
    {lists:reverse(Ints), lists:reverse(Rest)};
strip_first([[L|Ls]|Xs], {Ints, Rest}) ->
    strip_first(Xs, {[L|Ints], [Ls|Rest]}).


solve_part2(Filename) ->
    {ok, Bin} = file:read_file(Filename),
    Lines = binary:split(Bin, <<"\n">>, [global]),
    [_, Ops|RevInts] = lists:reverse(Lines),
    Ints = lists:reverse(RevInts),
    Ops1 = lists:reverse(binary:bin_to_list(Ops)),
    Ints1 = lists:map(fun(S) -> lists:reverse(binary:bin_to_list(S)) end, Ints)},
    {Ops1, Ints1}.
