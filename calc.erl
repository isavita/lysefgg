-module(calc).
-export([rpn/1, rpn_test/0]).

rpn(L) when is_list(L) ->
    [Res] = lists:foldl(fun rpn/2, [], string:tokens(L, " ")),
    Res.

rpn("+", [N1, N2 | S]) -> [N2 + N1 | S];
rpn("-", [N1, N2 | S]) -> [N2 - N1 | S];
rpn("-", [N | S]) -> [-N | S];
rpn("*", [N1, N2 | S]) -> [N2 * N1 | S];
rpn("/", [N1, N2 | S]) -> [N2 / N1 | S];
rpn("^", [N1, N2 | S]) -> [math:pow(N2, N1) | S];
rpn("ln", [N | S]) -> [math:log(N) | S];
rpn("log10", [N | S]) -> [math:log10(N) | S];
rpn("sum", S) -> [lists:sum(S)];
rpn("prod", S) -> [lists:foldl(fun erlang:'*'/2, 1, S)];
rpn(X, S) -> [read(X) | S].

read(X) ->
    case string:to_float(X) of
        {error, no_float} -> list_to_integer(X);
        {N, _} -> N
    end.

rpn_test() ->
    5 = rpn("3 2 +"),
    87 = rpn("90 3 -"),
    -4 = rpn("10 4 3 + 2 * -"),
    -2.0 = rpn("10 4 3 + 2 * - 2 /"),
    ok =
        try
            rpn("90 34 12 33 55 66 + * - +")
        catch
            error:{badmatch, [_ | _]} -> ok
        end,
    4037 = rpn("90 34 12 33 55 66 + * - + -"),
    8.0 = rpn("2 3 ^"),
    true = math:sqrt(2) == rpn("2 0.5 ^"),
    true = math:log(2.7) == rpn("2.7 ln"),
    true = math:log10(2.7) == rpn("2.7 log10"),
    50 = rpn("10 10 10 20 sum"),
    10.0 = rpn("10 10 10 20 sum 5 /"),
    1000.0 = rpn("10 10 20 0.5 prod"),
    ok.
