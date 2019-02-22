-module(quick_sort).
%%+BEGIN_FOLD Tests {
-export([main/1]).
-include_lib("eunit/include/eunit.hrl").
%%+END_FOLD }

-spec sort([T]) -> [T].
%%+BEGIN_SOLUTION
sort([]) ->
    [];
sort([P | Xs]) ->
    {L, E, G} = partition(P, Xs, {[], [P], []}),
    sort(L) ++ E ++ sort(G).

partition(_, [], LEG) ->
    LEG;
partition(P, [X | Xs], {L, E, G}) ->
    if
        X < P -> partition(P, Xs, {[X | L], E      , G      });
        X > P -> partition(P, Xs, {L      , E      , [X | G]});
        true  -> partition(P, Xs, {L      , [X | E], G      })
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
