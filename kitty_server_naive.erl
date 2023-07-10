-module(kitty_server_naive).

-export([start_link/0, order_cat/4, return_cat/2, close_shop/1]).

-record(cat, {name, color = white, description}).
-record(state, {cats = maps:new()}).

%% Client API
start_link() -> spawn_link(fun init/0).

order_cat(Pid, Name, Color, Description) ->
    Ref = erlang:monitor(process, Pid),
    Pid ! {self(), Ref, {order, Name, Color, Description}},
    receive
        {Ref, Cat = #cat{}} ->
            erlang:demonitor(Ref, [flush]),
            Cat;
        {'DOWN', Ref, proccess, Pid, Reason} ->
            erlang:error(Reason)
    after 3000 ->
        erlang:error(timeout)
    end.

return_cat(Pid, Cat) ->
    Pid ! {return, Cat},
    ok.

close_shop(Pid) ->
    Ref = erlang:monitor(process, Pid),
    Pid ! {self(), Ref, terminate},
    receive
        {Ref, ok} ->
            erlang:demonitor(Ref, [flush]),
            ok;
        {'DOWN', Ref, process, Pid, Reason} ->
            erlang:error(Reason)
    after 3000 ->
        erlang:error(timeout)
    end.

%% Server
init() ->
    loop(#state{}).

loop(S = #state{}) ->
    receive
        {From, Ref, {order, Name, Color, Description}} ->
            case maps:find(Name, S#state.cats) of
                {ok, Cat} ->
                    From ! {Ref, Cat};
                error ->
                    From ! {Ref, make_cat(Name, Color, Description)}
            end,
            NewCats = maps:remove(Name, S#state.cats),
            loop(S#state{cats = NewCats});
        {return, Cat} ->
            Cats = S#state.cats,
            loop(S#state{cats = Cats#{Cat#cat.name => Cat}});
        {From, Ref, terminate} ->
            From ! {Ref, ok},
            terminate(S);
        Unknown ->
            %% do some logging here too
            io:format("Unknown message: ~p~n", [Unknown]),
            loop(S)
    end.

make_cat(Name, Color, Description) ->
    #cat{name = Name, color = Color, description = Description}.

terminate(S) ->
    [io:format("~p was set free.~n", [Cat#cat.name]) || {_, Cat} <- maps:to_list(S#state.cats)],
    ok.
