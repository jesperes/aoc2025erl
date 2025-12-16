-module(day04).

-export([
    solve_part1/1,
    solve_part2/1]).

solve_part1(Filename) ->
    Map = parse(Filename),
    count_accessible_positions(Map).

solve_part2(Filename) ->
    Map = parse(Filename),
    Removed = remove_all_until_no_more_removables(Map),
    maps:size(Map) - maps:size(Removed).

parse(Filename) ->
    {ok, Bin} = file:read_file(Filename),
    {Width, _} = binary:match(Bin, <<"\n">>),
    to_coord_map(Bin, Width + 1, 0, #{}).

count_accessible_positions(Map) ->
    Keys = maps:keys(Map),
    lists:foldl(
        fun(Pos, Count) ->
            case is_removable(Pos, Map) of
                true -> Count + 1;
                false -> Count
            end
        end, 0, Keys).

%% Return true if the roll at the given position is removable.
is_removable(Pos, Map) ->
    adjacent_rolls(Pos, Map) < 4.

%% Return the number of rolls adjacent to the given position.
adjacent_rolls({X, Y}, Map) ->
    lists:foldl(
        fun(Adj, Acc) ->
            case maps:get(Adj, Map, $.) of
                $@ -> Acc + 1;
                _ -> Acc
            end
        end, 0, [{X + Dx, Y + Dy} ||
                Dx <- [-1, 0, 1], Dy <- [-1, 0, 1],
                {Dx, Dy} =/= {0, 0}]).

%% Remove all removable positions from the map.
remove_all_until_no_more_removables(Map) ->
    Map0 = remove_one_pass(Map),
    case {maps:size(Map), maps:size(Map0)} of
        {SizeBefore, SizeAfter} when SizeBefore == SizeAfter ->
            Map0;
        _ ->
            remove_all_until_no_more_removables(Map0)
    end.

%% Make one pass through the map and remove all the removable positions.
remove_one_pass(Map) ->
    Rolls = maps:keys(Map),
    remove_one_pass(Rolls, Map).
remove_one_pass([], Map) -> Map;
remove_one_pass([Roll|Rest], Map) ->
    case is_removable(Roll, Map) of
        true -> remove_one_pass(Rest, maps:remove(Roll, Map));
        false -> remove_one_pass(Rest, Map)
    end.

%% Convert the input binary to a map of coordinates.
to_coord_map(<<>>, _, _, Map) -> Map;
to_coord_map(<<$@, Rest/binary>>, W, Idx, Map) ->
    X = Idx rem W,
    Y = Idx div W,
    to_coord_map(Rest, W, Idx + 1, maps:put({X, Y}, $@, Map));
to_coord_map(<<_, Rest/binary>>, W, Idx, Map) ->
    to_coord_map(Rest, W, Idx + 1, Map).
