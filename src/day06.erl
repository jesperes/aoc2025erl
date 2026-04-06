-module(day06).
-export([solve_part1/1, solve_part2/1]).

%% Both parts build a flat padded grid (all rows the same width) and locate
%% column groups by scanning for all-space separator columns in the data rows.
%% Everything is direct O(1) byte access via binary:at/2 — no regex, no token lists.
%%
%% Part 1: for each group, read one number per DATA ROW (the number spanning
%%         those columns in that row), then apply the group's operator.
%% Part 2: for each group, read one number per COLUMN (digits stacked top-to-bottom),
%%         then apply the group's operator.

solve_part1(Filename) ->
    {Grid, Stride, NRows} = load(Filename),
    NData = NRows - 1,
    solve_p1(Grid, Stride, NData, col_groups(Grid, Stride, NData), 0).

solve_p1(_, _, _, [], Acc) -> Acc;
solve_p1(Grid, Stride, NData, [{S, E} | Gs], Acc) ->
    Op   = find_op(Grid, NData * Stride, S, E),
    Nums = row_nums(Grid, Stride, NData, S, E, 0, []),
    solve_p1(Grid, Stride, NData, Gs, Acc + apply_op(Op, Nums)).

%% Collect one number per data row within column range [S,E].
row_nums(_, _, NData, _, _, R, Acc) when R >= NData -> lists:reverse(Acc);
row_nums(Grid, Stride, NData, S, E, R, Acc) ->
    case parse_span(Grid, R*Stride + S, R*Stride + E, 0, false) of
        {ok, N} -> row_nums(Grid, Stride, NData, S, E, R+1, [N | Acc]);
        skip    -> row_nums(Grid, Stride, NData, S, E, R+1, Acc)
    end.

solve_part2(Filename) ->
    {Grid, Stride, NRows} = load(Filename),
    NData = NRows - 1,
    solve_p2(Grid, Stride, NData, col_groups(Grid, Stride, NData), 0).

solve_p2(_, _, _, [], Acc) -> Acc;
solve_p2(Grid, Stride, NData, [{S, E} | Gs], Acc) ->
    Op   = find_op(Grid, NData * Stride, S, E),
    Nums = col_nums(Grid, Stride, NData, E, S, []),
    solve_p2(Grid, Stride, NData, Gs, Acc + apply_op(Op, Nums)).

%% Collect one number per column in [S,E], reading digits top-to-bottom.
col_nums(_, _, _, C, S, Acc) when C < S -> Acc;
col_nums(Grid, Stride, NData, C, S, Acc) ->
    case parse_col(Grid, Stride, NData, C, 0, 0, false) of
        {ok, N} -> col_nums(Grid, Stride, NData, C-1, S, [N | Acc]);
        skip    -> col_nums(Grid, Stride, NData, C-1, S, Acc)
    end.

%% ── Grid construction ────────────────────────────────────────────────────────

load(Filename) ->
    {ok, Bin} = file:read_file(Filename),
    Lines  = [L || L <- binary:split(Bin, <<"\n">>, [global]), L =/= <<>>],
    Stride = lists:max([byte_size(L) || L <- Lines]),
    NRows  = length(Lines),
    Grid   = iolist_to_binary([pad(L, Stride) || L <- Lines]),
    {Grid, Stride, NRows}.

pad(B, Len) ->
    Extra = Len - byte_size(B),
    if Extra > 0 -> <<B/binary, (binary:copy(<<$\s>>, Extra))/binary>>;
       true      -> B
    end.

%% ── Column group detection ───────────────────────────────────────────────────

%% Returns [{Start, End}] inclusive ranges of contiguous non-separator columns.
col_groups(Grid, Stride, NData) ->
    col_groups(0, Stride, Grid, NData, false, 0, []).

col_groups(C, W, _, _, InGrp, Start, Acc) when C >= W ->
    G = if InGrp -> [{Start, C-1} | Acc]; true -> Acc end,
    lists:reverse(G);
col_groups(C, W, Grid, NData, InGrp, Start, Acc) ->
    Sep = col_all_space(Grid, W, NData, 0, C),
    {NG, NS, NA} = case {Sep, InGrp} of
        {true,  true}  -> {false, 0,     [{Start, C-1} | Acc]};
        {false, false} -> {true,  C,     Acc};
        _              -> {InGrp, Start, Acc}
    end,
    col_groups(C+1, W, Grid, NData, NG, NS, NA).

col_all_space(_, _, NData, R, _) when R >= NData -> true;
col_all_space(Grid, Stride, NData, R, C) ->
    case binary:at(Grid, R * Stride + C) of
        $\s -> col_all_space(Grid, Stride, NData, R+1, C);
        _   -> false
    end.

%% ── Parsing ──────────────────────────────────────────────────────────────────

%% Scan a horizontal span [I..Max] for digits, skipping spaces.
parse_span(_, I, Max, Acc, Any) when I > Max ->
    if Any -> {ok, Acc}; true -> skip end;
parse_span(Grid, I, Max, Acc, Any) ->
    case binary:at(Grid, I) of
        $\s -> parse_span(Grid, I+1, Max, Acc, Any);
        D   -> parse_span(Grid, I+1, Max, Acc*10 + (D - $0), true)
    end.

%% Scan column C from row 0 to NData-1 for digits, skipping spaces.
parse_col(_, _, NData, _, R, Acc, Any) when R >= NData ->
    if Any -> {ok, Acc}; true -> skip end;
parse_col(Grid, Stride, NData, C, R, Acc, Any) ->
    case binary:at(Grid, R * Stride + C) of
        $\s -> parse_col(Grid, Stride, NData, C, R+1, Acc, Any);
        D   -> parse_col(Grid, Stride, NData, C, R+1, Acc*10 + (D - $0), true)
    end.

%% ── Operator handling ────────────────────────────────────────────────────────

find_op(_, _, C, E) when C > E -> $+;
find_op(Grid, Base, C, E) ->
    case binary:at(Grid, Base + C) of
        $* -> $*;
        $+ -> $+;
        _  -> find_op(Grid, Base, C+1, E)
    end.

apply_op($+, Nums) -> lists:sum(Nums);
apply_op($*, Nums) -> lists:foldl(fun(X, Acc) -> X * Acc end, 1, Nums).
