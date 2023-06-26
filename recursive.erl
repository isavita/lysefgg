-module(recursive).
-compile([export_all]).

fac(N) when N > 0 ->
    N * fac(N - 1);
fac(_) ->
    1.

tail_fac(N) -> tail_fac(N, 1).
tail_fac(0, Acc) ->
    Acc;
tail_fac(N, Acc) when N > 0 ->
    tail_fac(N - 1, N * Acc).

len([]) ->
    0;
len([_ | R]) ->
    1 + len(R).

tail_len(L) -> tail_len(L, 0).
tail_len([], Acc) -> Acc;
tail_len([_ | T], Acc) -> tail_len(T, Acc + 1).

duplicate(0, _) -> [];
duplicate(N, Term) when N > 0 -> [Term | duplicate(N - 1, Term)].

tail_duplicate(N, Term) -> tail_duplicate(N, Term, []).
tail_duplicate(0, _, List) -> List;
tail_duplicate(N, Term, List) -> tail_duplicate(N - 1, Term, [Term | List]).

reverse([]) -> [];
reverse([H | T]) -> reverse(T) ++ [H].

tail_reverse(List) -> tail_reverse(List, []).
tail_reverse([], List) -> List;
tail_reverse([H | T], List) -> tail_reverse(T, [H | List]).

sublist(_, 0) ->
    [];
sublist([], _) ->
    [];
sublist([H | T], N) when N > 0 ->
    [H | sublist(T, N - 1)].

tail_sublist(List, N) -> tail_reverse(tail_sublist(List, N, [])).
tail_sublist([], _, List) -> List;
tail_sublist(_, 0, List) -> List;
tail_sublist([H | T], N, List) when N > 0 -> tail_sublist(T, N - 1, [H | List]).

zip([], _) -> [];
zip(_, []) -> [];
zip([H1 | T1], [H2 | T2]) -> [{H1, H2} | zip(T1, T2)].

tail_zip(List1, List2) -> tail_reverse(tail_zip(List1, List2, [])).
tail_zip([], _, List) -> List;
tail_zip(_, [], List) -> List;
tail_zip([H1 | T1], [H2 | T2], List) -> tail_zip(T1, T2, [{H1, H2} | List]).

quicksort([]) ->
    [];
quicksort([Pivot | Rest]) ->
    {Smaller, Larger} = partition(Pivot, Rest, [], []),
    quicksort(Smaller) ++ [Pivot] ++ quicksort(Larger).

partition(_, [], Smaller, Larger) ->
    {Smaller, Larger};
partition(Pivot, [H | T], Smaller, Larger) ->
    if
        Pivot >= H -> partition(Pivot, T, [H | Smaller], Larger);
        true -> partition(Pivot, T, Smaller, [H | Larger])
    end.

best_quicksort([]) ->
    [];
best_quicksort(L = [_ | _]) ->
    best_quicksort(L, []).

best_quicksort([], Acc) ->
    Acc;
best_quicksort([Pivot | Rest], Acc) ->
    best_partition(Pivot, Rest, {[], [Pivot], []}, Acc).

best_partition(_, [], {Smaller, Equal, Larger}, Acc) ->
    best_quicksort(Smaller, Equal ++ best_quicksort(Larger, Acc));
best_partition(Pivot, [H | T], {Smaller, Equal, Larger}, Acc) ->
    if
        Pivot > H -> partition(Pivot, T, {[H | Smaller], Equal, Larger}, Acc);
        Pivot < H -> partition(Pivot, T, {Smaller, Equal, [H | Larger]}, Acc);
        true -> partition(Pivot, T, {Smaller, [H | Equal], Larger}, Acc)
    end.
