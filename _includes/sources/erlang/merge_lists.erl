-module(merge_lists).
-export([main/1]).
-include_lib("eunit/include/eunit.hrl").

-type interval() :: {integer(), integer()}.
-spec merge([interval()], [interval()]) -> [interval()].
%%+BEGIN_SOLUTION
merge([], Ys) ->
    Ys;
merge(Xs, []) ->
    Xs;
merge([X | Xs], [Y | Ys]) ->
    case are_overlapping(X, Y) of
    true ->
        M = merge_intervals(X, Y),
        merge([M | Xs], Ys);
    false when X < Y ->
        [X | merge(Xs, [Y | Ys])];
    false ->
        [Y | merge([X | Xs], Ys)]
    end.
%%+END_SOLUTION

%%+BEGIN_SOLUTION
are_overlapping({X1,X2}, {Y1,Y2}) ->
    max(X1,Y1) =< min(X2,Y2).

merge_intervals({X1,X2}, {Y1,Y2}) ->
    {min(X1,Y1), max(X2,Y2)}.
%%+END_SOLUTION

main(_) ->
    case eunit:test(?MODULE) of
    ok -> halt(0);
    _  -> halt(1)
    end.

merge_test() ->
    ?assertEqual([], merge([], [])),
    ?assertEqual([{4,5}], merge([], [{4,5}])),
    ?assertEqual([{1,2}], merge([{1,2}], [])),
    ?assertEqual([{1,2},{3,10},{11,12}], merge([{1,2},{3,9}], [{4,5},{8,10},{11,12}])).
