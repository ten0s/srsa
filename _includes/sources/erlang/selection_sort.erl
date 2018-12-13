-module(selection_sort).
-export([main/1]).
-include_lib("eunit/include/eunit.hrl").

-spec sort([T]) -> [T].
%% SOLUTION_BEGIN
sort(Xs) ->
    sort(Xs, []).

sort([], Acc) ->
    Acc;
sort(L, Acc) ->
    {Max, M} = max_del(L),
    sort(M, [Max | Acc]).

max_del(L) ->
    max_del(L, []).

max_del([Max], Acc) ->
    {Max, Acc};
max_del([A, B | L], Acc) ->
    case A > B of
    true ->
        max_del([A | L], [B | Acc]);
    false ->
        max_del([B | L], [A | Acc])
    end.
%% SOLUTION_END

main(_) ->
    case eunit:test(?MODULE) of
    ok -> halt(0);
    _  -> halt(1)
    end.

sort_test() ->
    ?assertEqual([], sort([])),
    ?assertEqual([1], sort([1])),
    ?assertEqual([1,2,3,4,5], sort([1,2,3,4,5])),
    ?assertEqual([1,2,3,4,5], sort([5,4,3,2,1])),
    ?assertEqual([1,2,3,4,5], sort([5,1,3,2,4])).
