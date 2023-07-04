-module(multiproc).
-export([important/0, normal/0]).

important() ->
    receive
        {Priority, Msg} when Priority > 10 ->
            [Msg | important()]
    after 0 ->
        normal()
    end.

normal() ->
    receive
        {_, Msg} ->
            [Msg | normal()]
    after 0 ->
        []
    end.
