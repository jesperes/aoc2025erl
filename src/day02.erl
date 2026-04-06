-module(day02).
-export([solve_part1/1, solve_part2/1]).

%% Input is a single line: "lo1-hi1,lo2-hi2,..."
parse_ranges(Filename) ->
    {ok, Bin} = file:read_file(Filename),
    [Line | _] = binary:split(Bin, <<"\n">>),
    [begin
         [A, B] = binary:split(R, <<"-">>),
         {binary_to_integer(A), binary_to_integer(B)}
     end || R <- binary:split(Line, <<",">>, [global])].

%% ── Part 1 ───────────────────────────────────────────────────────────────────
%%
%% An invalid ID is a number whose digit string is a pattern repeated exactly
%% twice: N = P * (10^k + 1) for some k-digit pattern P.
%%
%% For each k, valid P values in a range [lo, hi] form a contiguous interval,
%% summed in O(1) as an arithmetic series. Total work: O(k_max * num_ranges).

solve_part1(Filename) ->
    Ranges = parse_ranges(Filename),
    MaxId  = lists:max([Hi || {_, Hi} <- Ranges]),
    p1_loop(Ranges, MaxId, 1, 0).

p1_loop(Ranges, MaxId, K, Total) ->
    Pow10K = pow10(K),
    Mult   = Pow10K + 1,
    PMin   = if K =:= 1 -> 1; true -> Pow10K div 10 end,
    case PMin * Mult > MaxId of
        true  -> Total;
        false ->
            PMax = Pow10K - 1,
            Add  = lists:sum([arith_sum(Lo, Hi, PMin, PMax, Mult)
                              || {Lo, Hi} <- Ranges]),
            p1_loop(Ranges, MaxId, K + 1, Total + Add)
    end.

%% ── Part 2 ───────────────────────────────────────────────────────────────────
%%
%% Extends to m >= 2 repetitions: N = P * (10^L - 1)/(10^k - 1), where L is
%% the total digit length and k | L, k < L.
%%
%% Different (k, m) pairs can produce the same N (e.g. 1111 = 11*101 and
%% 1*1111), so inclusion-exclusion over the proper divisors of L ensures each
%% invalid number is counted exactly once. A number periodic with periods k1
%% and k2 has period gcd(k1, k2), so intersections collapse cleanly.

solve_part2(Filename) ->
    Ranges  = parse_ranges(Filename),
    MaxId   = lists:max([Hi || {_, Hi} <- Ranges]),
    MaxL    = num_digits(MaxId),
    DivsByL = [{L, proper_divisors(L)} || L <- lists:seq(2, MaxL)],
    lists:sum([p2_range(Lo, Hi, DivsByL) || {Lo, Hi} <- Ranges]).

p2_range(Lo, Hi, DivsByL) ->
    lists:sum([
        begin
            Pow10L = pow10(L),
            LoL = max(Lo, Pow10L div 10),
            HiL = min(Hi, Pow10L - 1),
            if LoL > HiL -> 0;
               true -> incl_excl(Divs, L, LoL, HiL)
            end
        end
        || {L, Divs} <- DivsByL]).

incl_excl(Divs, L, Lo, Hi) ->
    N = length(Divs),
    lists:sum([
        begin
            PC   = popcount(Mask),
            Sign = if PC rem 2 =:= 1 -> 1; true -> -1 end,
            G    = subset_gcd(Mask, Divs, N),
            Sign * arith_sum_kl(G, L, Lo, Hi)
        end
        || Mask <- lists:seq(1, (1 bsl N) - 1)]).

%% Sum of all N = P * mult in [Lo, Hi], mult = (10^L - 1)/(10^K - 1),
%% P is a K-digit number.
arith_sum_kl(K, L, Lo, Hi) ->
    Pow10K = pow10(K),
    Mult   = (pow10(L) - 1) div (Pow10K - 1),
    PMin   = if K =:= 1 -> 1; true -> Pow10K div 10 end,
    PMax   = Pow10K - 1,
    arith_sum(Lo, Hi, PMin, PMax, Mult).

%% Sum of P*Mult for integer P in [max(ceil(Lo/Mult), PMin), min(Hi div Mult, PMax)].
arith_sum(Lo, Hi, PMin, PMax, Mult) ->
    PLo = max(ceil_div(Lo, Mult), PMin),
    PHi = min(Hi div Mult, PMax),
    if PLo > PHi -> 0;
       true ->
           Count = PHi - PLo + 1,
           Mult * Count * (PLo + PHi) div 2
    end.

%% ── Helpers ──────────────────────────────────────────────────────────────────

proper_divisors(L) ->
    [D || D <- lists:seq(1, L - 1), L rem D =:= 0].

subset_gcd(Mask, Divs, N) ->
    lists:foldl(
        fun(I, Acc) ->
            case Mask band (1 bsl I) of
                0 -> Acc;
                _ ->
                    D = lists:nth(I + 1, Divs),
                    if Acc =:= 0 -> D; true -> gcd(Acc, D) end
            end
        end, 0, lists:seq(0, N - 1)).

gcd(A, 0) -> A;
gcd(A, B) -> gcd(B, A rem B).

pow10(0) -> 1;
pow10(K) -> 10 * pow10(K - 1).

ceil_div(A, B) -> (A + B - 1) div B.

num_digits(N) when N < 10 -> 1;
num_digits(N)              -> 1 + num_digits(N div 10).

popcount(0) -> 0;
popcount(N) -> (N band 1) + popcount(N bsr 1).
