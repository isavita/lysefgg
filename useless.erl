-module(useless).
-export([add/2, add/3, hello/0, greeting/2, greet_and_add_two/1]).

add(A, B) ->
    A + B.

add(A, B, C) ->
    A + B + C.

hello() ->
    io:format("Hello, World!~n").

greet_and_add_two(X) ->
    hello(),
    add(X, 2).

-define(sub(X, Y), X - Y).

greeting(male, Name) ->
    io:format("Hello, Mr. ~s~n", [Name]);
greeting(female, Name) ->
    io:format("Hello, Mrs. ~s~n", [Name]);
greeting(_, Name) ->
    io:format("Hello, ~s~n", [Name]).
