-module(exceptions).
-export([throws/1, sword/1, black_knight/1, talk/0, whoa/0, catcher/2]).

throws(F) ->
    try F() of
        _ -> ok
    catch
        error:Error -> {error, caught, Error};
        exit:Exit -> {exit, caught, Exit};
        Throw -> {throw, caught, Throw}
    end.

sword(1) -> throw(slice);
sword(2) -> erlang:error(cut_arm);
sword(3) -> exit(cut_leg);
sword(4) -> throw(punch);
sword(5) -> exit(cross_bridge).

black_knight(Attack) when is_function(Attack, 0) ->
    try Attack() of
        _ -> "None shall pass."
    catch
        throw:slice -> "It is but a scratch.";
        error:cut_arm -> "I've had worse.";
        exit:cut_leg -> "Come on you pansy!";
        _:_ -> "Just a flesh wound."
    after
        io:format("I had enough! Goodbye!~n")
    end.

talk() -> "blah blah".

whoa() ->
    try
        talk(),
        _Kinght = "None shall pass!",
        _Doubles = [N * 2 || N <- lists:seq(1, 100)],
        _WillReturnThis = tequila
    catch
        Exception:Reason -> {caught, Exception, Reason}
    end.

catcher(X, Y) ->
    case catch X / Y of
        {'EXIT', {badarith, _}} -> "uh oh";
        N -> N
    end.
