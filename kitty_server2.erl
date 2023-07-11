-module(kitty_server2).

-export([start_link/0, order_cat/4, return_cat/2, close_shop/1]).
-export([init/1, handle_call/3, handle_cast/2]).

-record(cat, {name, color = white, description}).
-record(state, {cats = maps:new()}).

%% Client API
start_link() -> my_server:start_link(?MODULE, []).

%% Synchronous call
order_cat(Pid, Name, Color, Description) ->
    my_server:call(Pid, {order, Name, Color, Description}).

%% Asynchronous call
return_cat(Pid, Cat) ->
    my_server:cast(Pid, {return, Cat}).

%% Synchronous call
close_shop(Pid) ->
    my_server:call(Pid, terminate).

%% Server
init(_) -> #state{}.

handle_call({order, Name, Color, Description}, From, State) ->
    case maps:find(Name, State#state.cats) of
        {ok, Cat} ->
            my_server:reply(From, Cat),
            State#state{cats = maps:remove(Name, State#state.cats)};
        error ->
            my_server:reply(From, make_cat(Name, Color, Description)),
            State
    end;
handle_call(terminate, From, State) ->
    my_server:reply(From, ok),
    terminate(State).

handle_cast({return, Cat}, State) ->
    Cats = State#state.cats,
    State#state{cats = Cats#{Cat#cat.name => Cat}}.

%% Private functions
make_cat(Name, Color, Description) ->
    #cat{name = Name, color = Color, description = Description}.

terminate(S) ->
    [io:format("~p was set free.~n", [Cat#cat.name]) || {_, Cat} <- maps:to_list(S#state.cats)],
    exit(normal).
