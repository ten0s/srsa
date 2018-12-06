-module(reverse_list).
-export([main/1]).
-include_lib("eunit/include/eunit.hrl").

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
    case eunit:test(?MODULE) of
    ok -> halt(0);
    _  -> halt(1)
    end.

reverse_test() ->
    [] = reverse([]),
    [3,2,1] = reverse([1,2,3]).
