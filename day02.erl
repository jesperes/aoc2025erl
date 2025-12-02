-module(day02).
-export([ solve_part1/1
        , solve_part2/1
        , is_repeated/2
        ]).

solve_part1(Filename) ->
  solve(Filename, fun invalid_id1/1).

solve_part2(Filename) ->
  solve(Filename, fun invalid_id2/1).

solve(Filename, Fun) ->
  {ok, Bin} = file:read_file(Filename),
  {match, Matches} = re:run(Bin, "(\\d+)-(\\d+)", [global, {capture, all_but_first, binary}]),
  lists:foldl(fun([], Acc) ->
                  Acc;
                 ([A, B], Acc) ->
                  Acc + num_invalid_ids(A, B, Fun)
              end, 0, Matches).

next(A) ->
  integer_to_binary(binary_to_integer(A) + 1).

num_invalid_ids(A, A, Fun) ->
  Fun(A);
num_invalid_ids(A, B, Fun) ->
  Fun(A) + num_invalid_ids(next(A), B, Fun).

invalid_id1(A) ->
  Len = byte_size(A),
  if Len rem 2 == 1 ->
      0;
     true ->
      Half = Len div 2,
      A1 = binary:part(A, 0, Half),
      A2 = binary:part(A, Half, Half),
      if A1 =:= A2 ->
          binary_to_integer(A);
         true ->
          0
      end
  end.

invalid_id2(A) ->
  {_, Total} =
    lists:foldl(
      fun(SubStrLen, {false, Sum}) ->
          case is_repeated(SubStrLen, A) of
            N when N > 0 -> {true, Sum + N};
            _ -> {false, Sum}
          end;
         (_, {true, _} = Acc) -> Acc
      end, {false, 0}, lists:seq(0, byte_size(A) div 2))
    Total.

split_at(Bin, Pos) ->
  if Pos < byte_size(Bin) ->
      {binary:part(Bin, 0, Pos), binary:part(Bin, Pos, byte_size(Bin) - Pos)};
     true ->
      {Bin, <<>>}
  end.

%% If A consists only of repeated sequences of length N, return A
is_repeated(N, A) ->
  case split_at(A, N) of
    {Prefix, Rest} when byte_size(Prefix) > 0 andalso Rest =/= <<>> ->
      case is_repeated(Prefix, N, Rest) of
        true ->
          binary_to_integer(A);
        false -> 0
      end;
    {_Prefix, _Rest} ->
      0
  end.

is_repeated(Prefix, N, Rest) ->
  case split_at(Rest, N) of
    {Prefix0, Rest0} when Prefix0 =:= Prefix andalso Rest0 =/= <<>> ->
      is_repeated(Prefix, N, Rest0);
    {Prefix0, Rest0} when Prefix0 =:= Prefix andalso Rest0 =:= <<>> ->
      true;
    _ ->
      false
  end.
