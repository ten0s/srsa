-module(merge_sort).
-export([main/1]).
-include_lib("eunit/include/eunit.hrl").

-spec sort([T]) -> [T].
%% BEGIN_SOLUTION
sort([]) ->
    [];
sort([X]) ->
    [X];
sort(L) ->
    {L1, L2} = lists:split(length(L) div 2, L),
    merge(sort(L1), sort(L2)).

merge([], L2) ->
    L2;
merge(L1, []) ->
    L1;
merge([A | L1], [B | L2]) ->
    case A =< B of
    true ->
        [A | merge(L1, [B | L2])];
    false ->
        [B | merge([A | L1], L2)]
    end.
%% END_SOLUTION

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
