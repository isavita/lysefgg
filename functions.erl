-module(functions).
-compile(export_all).

head([H | _]) -> H.

second([_, X | _]) -> X.

same(X, X) -> true;
same(_, _) -> false.

valid_time({Date = {X, Y, Z}, Time = {H, M, S}}) ->
    io:format("The date tuple (~p) says today is: ~p/~p/~p,~n", [Date, X, Y, Z]),
    io:format("The time tuple (~p) indicates: ~p:~p:~p.~n", [Time, H, M, S]);
valid_time(_) ->
    io:format("Stop feeding me wrong data!~n").

old_enough(Age) when is_number(Age), Age >= 18 -> true;
old_enough(_) -> false.
