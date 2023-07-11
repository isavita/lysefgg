-module(kitty_gen_server).
-behaviour(gen_server).

-export([
    start_link/0,
    order_cat/4,
    return_cat/2,
    close_shop/1,
    init/1,
    handle_call/3,
    handle_cast/2,
    handle_info/2,
    terminate/2,
    code_change/3
]).

-record(cat, {name, color = "white", description}).
-record(state, {cats = maps:new()}).

start_link() -> gen_server:start_link(?MODULE, [], []).

%% Synchronous call
order_cat(Pid, Name, Color, Description) ->
    gen_server:call(Pid, {order, Name, Color, Description}).

%% Asynchronous call
return_cat(Pid, Cat = #cat{}) ->
    gen_server:cast(Pid, {return, Cat}).

%% Synchronous call
close_shop(Pid) ->
    gen_server:call(Pid, terminate).

init(_) ->
    {ok, #state{}}.

handle_call({order, Name, Color, Description}, _From, State) ->
    case maps:find(Name, State#state.cats) of
        {ok, Cat} ->
            NewState = State#state{cats = maps:remove(Name, State#state.cats)},
            {reply, Cat, NewState};
        error ->
            {reply, make_cat(Name, Color, Description), State}
    end;
handle_call(terminate, _From, State) ->
    {stop, normal, ok, State}.

handle_cast({return, Cat = #cat{}}, State) ->
    Cats = State#state.cats,
    {noreply, State#state{cats = Cats#{Cat#cat.name => Cat}}}.

handle_info(Msg, State) ->
    io:format("Unexpected message: ~p~n", [Msg]),
    {noreply, State}.

terminate(normal, State) ->
    [io:format("~p was set free.~n", [Cat#cat.name]) || {_, Cat} <- maps:to_list(State#state.cats)],
    ok.

code_change(_OldVsn, State, _Extra) ->
    %% No change planned. The function is there for the behaviour,
    %% but will not be used. Only a version on the next
    {ok, State}.

make_cat(Name, Color, Description) ->
    #cat{name = Name, color = Color, description = Description}.
