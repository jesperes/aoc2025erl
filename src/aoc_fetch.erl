-module(aoc_fetch).
-export([ensure_input/1, ensure_answers/1, ensure_puzzle/1, start_http/0]).

-define(YEAR, 2025).
-define(BASE_URL, "https://adventofcode.com").
-define(USER_AGENT, "github.com/aoc2025erl").

%% Returns the input file path, downloading from AoC if not cached.
ensure_input(Day) ->
    Path = input_path(Day),
    case filelib:is_regular(Path) of
        true  -> Path;
        false ->
            Session = read_session(),
            start_http(),
            URL  = fmt("~s/~w/day/~w/input", [?BASE_URL, ?YEAR, Day]),
            Body = fetch(URL, Session),
            ok   = filelib:ensure_dir(Path),
            ok   = file:write_file(Path, Body),
            io:format(standard_error, "Downloaded input for day ~w~n", [Day]),
            Path
    end.

%% Returns {P1, P2} where each is a string or undefined.
%% Downloads from the puzzle page if not cached in the .answers file.
ensure_answers(Day) ->
    Path = answers_path(Day),
    case filelib:is_regular(Path) of
        true  -> read_answers_file(Path);
        false ->
            Session = read_session(),
            start_http(),
            URL  = fmt("~s/~w/day/~w", [?BASE_URL, ?YEAR, Day]),
            Html = fetch(URL, Session),
            {P1, P2} = parse_answers(Html),
            ok = filelib:ensure_dir(Path),
            ok = file:write_file(Path, [opt_str(P1), "\n", opt_str(P2), "\n"]),
            {P1, P2}
    end.

%% Returns the puzzle description file path, downloading if not cached.
ensure_puzzle(Day) ->
    Path = puzzle_path(Day),
    case filelib:is_regular(Path) of
        true  -> Path;
        false ->
            Session = read_session(),
            start_http(),
            URL  = fmt("~s/~w/day/~w", [?BASE_URL, ?YEAR, Day]),
            Html = fetch(URL, Session),
            Text = extract_articles(Html),
            ok   = filelib:ensure_dir(Path),
            ok   = file:write_file(Path, Text),
            io:format(standard_error, "Downloaded puzzle for day ~w~n", [Day]),
            Path
    end.

%% Start the HTTP/SSL applications. Idempotent.
start_http() ->
    application:ensure_all_started(inets),
    application:ensure_all_started(ssl).

%% ── Session ──────────────────────────────────────────────────────────────────

read_session() ->
    case os:getenv("AOC_SESSION") of
        false ->
            case file:read_file(".aoc_session") of
                {ok, Bin} ->
                    string:trim(binary_to_list(Bin));
                {error, _} ->
                    erlang:error({no_session,
                        "set AOC_SESSION env var or create .aoc_session file"})
            end;
        Val ->
            string:trim(Val)
    end.

%% ── HTTP ─────────────────────────────────────────────────────────────────────

fetch(URL, Session) ->
    Headers = [{"Cookie",     "session=" ++ Session},
               {"User-Agent", ?USER_AGENT}],
    SSLOpts = [{ssl, [{verify, verify_none}]}],
    case httpc:request(get, {URL, Headers}, SSLOpts, [{body_format, binary}]) of
        {ok, {{_, 200, _}, _RespHdrs, Body}} ->
            Body;
        {ok, {{_, Status, _}, _, Body}} ->
            erlang:error({http_error, Status, URL, Body});
        {error, Reason} ->
            erlang:error({request_failed, URL, Reason})
    end.

%% ── HTML parsing ─────────────────────────────────────────────────────────────

%% Extract "Your puzzle answer was <code>…</code>" values.
parse_answers(Html) ->
    Marker  = <<"Your puzzle answer was <code>">>,
    Answers = find_all_between(Html, Marker, <<"</code>">>),
    P1 = case Answers of [A | _] -> binary_to_list(A); [] -> undefined end,
    P2 = case Answers of [_, B | _] -> binary_to_list(B); _ -> undefined end,
    {P1, P2}.

%% Extract all <article>…</article> sections, strip tags, collapse blank lines.
extract_articles(Html) ->
    Articles = find_all_inclusive(Html, <<"<article">>, <<"</article>">>),
    Stripped = [strip_tags(A) || A <- Articles],
    Text     = lists:join("\n\n", Stripped),
    collapse_blanks(iolist_to_binary(Text)).

%% Return all content strictly between occurrences of StartMark and EndMark.
find_all_between(Bin, StartMark, EndMark) ->
    find_all_between(Bin, StartMark, byte_size(StartMark), EndMark, []).

find_all_between(Bin, SM, SL, EM, Acc) ->
    case binary:match(Bin, SM) of
        nomatch    -> lists:reverse(Acc);
        {Pos, _}   ->
            After = binary:part(Bin, Pos + SL, byte_size(Bin) - Pos - SL),
            case binary:match(After, EM) of
                nomatch        -> lists:reverse(Acc);
                {EndPos, EL}   ->
                    Found = binary:part(After, 0, EndPos),
                    Rest  = binary:part(After, EndPos + EL,
                                        byte_size(After) - EndPos - EL),
                    find_all_between(Rest, SM, SL, EM, [Found | Acc])
            end
    end.

%% Return all substrings from StartMark through EndMark (inclusive).
find_all_inclusive(Bin, StartMark, EndMark) ->
    find_all_inclusive(Bin, StartMark, EndMark, byte_size(EndMark), []).

find_all_inclusive(Bin, SM, EM, EL, Acc) ->
    case binary:match(Bin, SM) of
        nomatch  -> lists:reverse(Acc);
        {Pos, _} ->
            After = binary:part(Bin, Pos, byte_size(Bin) - Pos),
            case binary:match(After, EM) of
                nomatch      -> lists:reverse(Acc);
                {EndPos, _}  ->
                    Found = binary:part(After, 0, EndPos + EL),
                    Rest  = binary:part(After, EndPos + EL,
                                        byte_size(After) - EndPos - EL),
                    find_all_inclusive(Rest, SM, EM, EL, [Found | Acc])
            end
    end.

%% Remove all HTML tags, returning a character list.
strip_tags(Html) ->
    strip_tags(Html, false, []).

strip_tags(<<>>, _, Acc)                        -> lists:reverse(Acc);
strip_tags(<<$<, Rest/binary>>, _, Acc)         -> strip_tags(Rest, true, Acc);
strip_tags(<<$>, Rest/binary>>, _, Acc)         -> strip_tags(Rest, false, Acc);
strip_tags(<<_, Rest/binary>>, true,  Acc)      -> strip_tags(Rest, true, Acc);
strip_tags(<<C, Rest/binary>>, false, Acc)      -> strip_tags(Rest, false, [C | Acc]).

%% Collapse runs of blank lines to at most one, trim trailing whitespace per line.
collapse_blanks(Bin) ->
    Lines = binary:split(Bin, <<"\n">>, [global]),
    collapse_blanks(Lines, 0, []).

collapse_blanks([], _, Acc) ->
    iolist_to_binary(lists:join("\n", lists:reverse(Acc)));
collapse_blanks([Line | Rest], Blanks, Acc) ->
    case string:trim(binary_to_list(Line)) of
        [] when Blanks >= 1 ->
            collapse_blanks(Rest, Blanks + 1, Acc);
        [] ->
            collapse_blanks(Rest, 1, ["" | Acc]);
        _ ->
            TrimEnd = string:trim(binary_to_list(Line), trailing),
            collapse_blanks(Rest, 0, [TrimEnd | Acc])
    end.

%% ── Cached answers file ───────────────────────────────────────────────────────

read_answers_file(Path) ->
    {ok, Bin} = file:read_file(Path),
    case binary:split(Bin, <<"\n">>, [global]) of
        [L1, L2 | _] -> {to_opt(L1), to_opt(L2)};
        [L1]          -> {to_opt(L1), undefined};
        []            -> {undefined, undefined}
    end.

to_opt(<<>>)  -> undefined;
to_opt(B)     -> binary_to_list(B).

%% ── Paths & helpers ──────────────────────────────────────────────────────────

input_path(Day)   -> fmt("inputs/day~2..0w.txt",        [Day]).
answers_path(Day) -> fmt("inputs/day~2..0w.answers",    [Day]).
puzzle_path(Day)  -> fmt("inputs/day~2..0w.puzzle.txt", [Day]).

fmt(F, A) -> lists:flatten(io_lib:format(F, A)).
opt_str(undefined) -> "";
opt_str(S)         -> S.
