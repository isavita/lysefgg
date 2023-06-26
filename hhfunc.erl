-module(hhfunc).
-compile(export_all).

one() -> 1.
two() -> 2.

add(X, Y) -> X() + Y().

increment([]) -> [];
increment([H | T]) -> [H + 1 | increment(T)].

decrement([]) -> [];
decrement([H | T]) -> [H - 1 | decrement(T)].

map(_, []) -> [];
map(F, [H | T]) -> [F(H) | map(F, T)].

incr(X) -> X + 1.
decr(X) -> X - 1.

even(Xs) -> lists:reverse(even(Xs, [])).

even([], Acc) ->
    Acc;
even([H | T], Acc) when H rem 2 == 0 ->
    even(T, [H | Acc]);
even([_ | T], Acc) ->
    even(T, Acc).

old_man(People) -> lists:reverse(old_man(People, [])).

old_man([], People) ->
    People;
old_man([Person = {male, Age} | T], People) when Age > 60 ->
    old_man(T, [Person | People]);
old_man([_ | T], People) ->
    old_man(T, People).

filter(Pred, L) -> lists:reverse(filter(Pred, L, [])).

filter(_, [], Acc) ->
    Acc;
filter(Pred, [H | T], Acc) ->
    case Pred(H) of
        true -> filter(Pred, T, [H | Acc]);
        false -> filter(Pred, T, Acc)
    end.

max([H | T]) -> max2(T, H).
max2([], Max) ->
    Max;
max2([H | T], Max) ->
    if
        H > Max -> max2(T, H);
        true -> max2(T, Max)
    end.

min([H | T]) -> min2(T, H).
min2([], Min) ->
    Min;
min2([H | T], Min) when H < Min ->
    min2(T, H);
min2([_ | T], Min) ->
    min(T, Min).

sum(L) -> sum(L, 0).
sum([], Acc) -> Acc;
sum([H | T], Acc) -> sum(T, Acc + H).

fold(_, Start, []) -> Start;
fold(F, Start, [H | T]) -> fold(F, F(H, Start), T).

reverse(Xs) ->
    fold(fun(X, L) -> [X | L] end, [], Xs).

filter2(Pred, Xs) ->
    lists:reverse(
        fold(
            fun(X, L) ->
                case Pred(X) of
                    true -> [X | L];
                    false -> L
                end
            end,
            [],
            Xs
        )
    ).

map2(F, Xs) ->
    lists:reverse(fold(fun(X, L) -> [F(X) | L] end, [], Xs)).
