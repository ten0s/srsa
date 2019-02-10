-module(selection_sort).
-export([main/1]).
-include_lib("eunit/include/eunit.hrl").

-spec sort([T]) -> [T].
%%+BEGIN_SOLUTION
sort([]) ->
    [];
sort(Xs) ->
    {Min, Ys} = min_rest(Xs),
    [Min | sort(Ys)].

min_rest([X | Xs]) ->
    min_rest(X, Xs, []).

min_rest(Min, [], Acc) ->
    {Min, Acc};
min_rest(Min, [X | Xs], Acc) ->
    case Min =< X of
    true ->
        min_rest(Min, Xs, [X | Acc]);
    false ->
        min_rest(X, Xs, [Min | Acc])
    end.
%%+END_SOLUTION

%%+BEGIN_FOLD Tests {
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
%%+END_FOLD }
