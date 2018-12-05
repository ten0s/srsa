-module(main).
-export([main/1]).

-spec reverse([T]) -> [T].
%% SOLUTION_BEGIN
reverse(Xs) ->
    reverse(Xs, []).

reverse([X | Xs], Acc) ->
    reverse(Xs, [X | Acc]);
reverse([], Acc) ->
    Acc.
%% SOLUTION_END

main(_) ->
    [] = reverse([]),
    [3,2,1] = reverse([1,2,3]),
    io:format("OK~n", []).
