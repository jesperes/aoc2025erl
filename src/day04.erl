-module(day04).
-export([solve_part1/1, solve_part2/1]).

%% Flat padded grid: 1-cell silent border on all sides.
%% Interior cell (row, col) → array index (row+1)*stride + (col+1).
%% stride = cols + 2.
%%
%% Part 1: direct binary scan — no grid construction needed.  '\n' bytes
%%   act as a natural separator so neighbour lookups never cross row
%%   boundaries.  Only bounds-check top and bottom edges.
%%
%% Part 2: O(N) queue-based removal using the erlang array module.
%%   Neighbour counts are pre-computed; removing a cell only touches its
%%   8 neighbours.  Neighbour arithmetic is unrolled (no foldl) to reduce
%%   function-call overhead.

%% ── Part 1 ────────────────────────────────────────────────────────────────

solve_part1(Filename) ->
    {ok, Bin} = file:read_file(Filename),
    {Cols, _} = binary:match(Bin, <<"\n">>),
    Stride = Cols + 1,          % includes the \n byte
    Size   = byte_size(Bin),
    scan_p1(Bin, Stride, Size, 0, 0).

scan_p1(_, _, Size, I, Acc) when I >= Size -> Acc;
scan_p1(Bin, S, Size, I, Acc) ->
    NewAcc = case binary:at(Bin, I) of
        $@ ->
            N = at(Bin,I-S-1,Size) + at(Bin,I-S,Size) + at(Bin,I-S+1,Size)
              + at(Bin,I-1,Size)                        + at(Bin,I+1,Size)
              + at(Bin,I+S-1,Size) + at(Bin,I+S,Size) + at(Bin,I+S+1,Size),
            Acc + if N < 4 -> 1; true -> 0 end;
        _ -> Acc
    end,
    scan_p1(Bin, S, Size, I + 1, NewAcc).

%% Return 1 if byte at index I is '@', 0 otherwise (including out-of-bounds).
at(Bin, I, Size) when I >= 0, I < Size ->
    case binary:at(Bin, I) of $@ -> 1; _ -> 0 end;
at(_, _, _) -> 0.

%% ── Part 2 ────────────────────────────────────────────────────────────────

solve_part2(Filename) ->
    {ok, Bin} = file:read_file(Filename),
    {Cols, _} = binary:match(Bin, <<"\n">>),
    Stride = Cols + 2,
    Rows   = (byte_size(Bin) + 1) div (Cols + 1),
    Size   = (Rows + 2) * Stride,
    Grid0  = build_grid(Bin, Cols + 1, Stride,
                        array:new([{size, Size}, {default, false}]), 0, 0),
    %% Pre-compute neighbour counts (unrolled, no foldl).
    NCnt0 = array:sparse_foldl(fun(I, _, NC) ->
        array:set(I, ncnt(I, Grid0, Stride), NC)
    end, array:new([{size, Size}, {default, 0}]), Grid0),
    %% Seed: all occupied cells with count < 4.
    Seed = array:sparse_foldl(fun(I, _, Q) ->
        N = array:get(I, NCnt0),
        if N < 4 -> [I | Q]; true -> Q end
    end, [], Grid0),
    process(Seed, Grid0, NCnt0, Stride, 0).

build_grid(<<>>, _, _, Grid, _, _) -> Grid;
build_grid(<<$\n, Rest/binary>>, W, Stride, Grid, _, Row) ->
    build_grid(Rest, W, Stride, Grid, 0, Row + 1);
build_grid(<<$@, Rest/binary>>, W, Stride, Grid, Col, Row) ->
    build_grid(Rest, W, Stride,
               array:set((Row + 1) * Stride + (Col + 1), true, Grid),
               Col + 1, Row);
build_grid(<<_, Rest/binary>>, W, Stride, Grid, Col, Row) ->
    build_grid(Rest, W, Stride, Grid, Col + 1, Row).

%% Unrolled 8-neighbour count for the array-based grid.
ncnt(I, Grid, S) ->
    b(I-S-1,Grid) + b(I-S,Grid) + b(I-S+1,Grid)
    + b(I-1,Grid)               + b(I+1,Grid)
    + b(I+S-1,Grid) + b(I+S,Grid) + b(I+S+1,Grid).

b(I, Grid) ->
    case array:get(I, Grid) of true -> 1; false -> 0 end.

%% Unrolled decrement + requeue for each of the 8 neighbours.
process([], _, _, _, Removed) -> Removed;
process([I | Rest], Grid, NCnt, S, Removed) ->
    case array:get(I, Grid) of
        false ->
            process(Rest, Grid, NCnt, S, Removed);
        true ->
            Grid1 = array:set(I, false, Grid),
            {NCnt1, Q1} = decr_nb(I-S-1, Grid1,
                          decr_nb(I-S,   Grid1,
                          decr_nb(I-S+1, Grid1,
                          decr_nb(I-1,   Grid1,
                          decr_nb(I+1,   Grid1,
                          decr_nb(I+S-1, Grid1,
                          decr_nb(I+S,   Grid1,
                          decr_nb(I+S+1, Grid1, {NCnt, Rest})))))))),
            process(Q1, Grid1, NCnt1, S, Removed + 1)
    end.

decr_nb(NI, Grid, {NC, Q}) ->
    case array:get(NI, Grid) of
        false -> {NC, Q};
        true  ->
            C  = array:get(NI, NC) - 1,
            Q1 = if C < 4 -> [NI | Q]; true -> Q end,
            {array:set(NI, C, NC), Q1}
    end.
