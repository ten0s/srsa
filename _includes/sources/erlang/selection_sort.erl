-module(selection_sort).
-export([main/1]).
-include_lib("eunit/include/eunit.hrl").

-spec sort([T]) -> [T].
%% SOLUTION_BEGIN
sort([]) ->
    [];
sort(Xs) ->
    {Min, Ys} = min_del(Xs, []),
    [Min | sort(Ys)].

min_del([Min], Acc) ->
    {Min, Acc};
min_del([A, B | L], Acc) ->
    case A =< B of
    true ->
        min_del([A | L], [B | Acc]);
    false ->
        min_del([B | L], [A | Acc])
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
