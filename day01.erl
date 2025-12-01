-module(day01).

-export([ solve_part1/1
        , solve_part2/1
        ]).

solve_part1(Filename) ->
  {ok, Bin} = file:read_file(Filename),
  Period = 100,
  StartPos = 50,
  {TotalZeroCount, _} =
    lists:foldl(
      fun(<<>>, Acc) ->
          Acc;
         (<<Dir, Rest/binary>>, {ZeroCount, Pos}) ->
          Clicks = binary_to_integer(Rest),
          Delta = if Dir == $L -> -1;
                       true -> 1
                    end,

          NewPos = ((Pos + Period) + (Delta * Clicks)) rem Period,
          NewZeroCount = maybe_incr_zerocount(ZeroCount, Pos),
          {NewZeroCount, NewPos}
      end, {0, StartPos}, binary:split(Bin, <<"\n">>, [global])),
  TotalZeroCount.

solve_part2(Filename) ->
  {ok, Bin} = file:read_file(Filename),
  Period = 100,
  StartPos = 50,
  {TotalZeroCount, _} =
    lists:foldl(
      fun(<<>>, Acc) ->
          Acc;
         (<<Dir, Rest/binary>>, {ZeroCount, Pos}) ->
          Clicks = binary_to_integer(Rest),
          Delta = if Dir == $L -> -1;
                       true -> 1
                    end,
          rotate_dial(Clicks, Period, Pos, Delta, ZeroCount)
      end, {0, StartPos}, binary:split(Bin, <<"\n">>, [global])),
  TotalZeroCount.

maybe_incr_zerocount(ZeroCount, 0) ->
  ZeroCount + 1;
maybe_incr_zerocount(ZeroCount, _Pos) ->
  ZeroCount.

rotate_dial(0 = _Clicks, _Period, Pos, _Delta, ZeroCount) ->
  {ZeroCount, Pos};
rotate_dial(Clicks, Period, Pos, Delta, ZeroCount) ->
  NewPos = (Pos + Period + Delta) rem Period,
  NewZeroCount = maybe_incr_zerocount(ZeroCount, NewPos),
  rotate_dial(Clicks - 1, Period, NewPos, Delta, NewZeroCount).
