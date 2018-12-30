-module(leftist_heap).
-export([main/1]).
-include_lib("eunit/include/eunit.hrl").

-type rank() :: non_neg_integer().
-type heap(T) :: nil | {T, rank(), heap(T), heap(T)}.
-type error(_E) :: no_return().

-spec new() -> heap(_T).
%% SOLUTION_BEGIN
new() ->
    nil.
%% SOLUTION_END

%% O(1)
-spec is_empty(heap(_T)) -> boolean().
%% SOLUTION_BEGIN
is_empty(nil) ->
    true;
is_empty(_) ->
    false.
%% SOLUTION_END

%% O(lg(n))
-spec insert(T, heap(T)) -> heap(T).
%% SOLUTION_BEGIN
insert(T, H) ->
    merge({T, 0, nil, nil}, H).
%% SOLUTION_END

%% O(1)
-spec min(heap(T)) -> T | error(empty).
%% SOLUTION_BEGIN
min(nil) ->
    error(empty);
min({T, _, _, _}) ->
    T.
%% SOLUTION_END

%% O(lg(n))
-spec deleteMin(heap(T)) -> {T, heap(T)} | error(empty).
%% SOLUTION_BEGIN
deleteMin(nil) ->
    error(empty);
deleteMin({T, _, L, R}) ->
    {T, merge(L, R)}.
%% SOLUTION_END

%% O(lg(n))
-spec merge(heap(T), heap(T)) -> heap(T).
%% SOLUTION_BEGIN
merge(nil, H) ->
    H;
merge(H, nil) ->
    H;
merge({T1, _, L1, R1} = H1, {T2, _, L2, R2} = H2) ->
    case T1 =< T2 of
    true ->
        make(T1, L1, merge(R1, H2));
    false ->
        make(T2, L2, merge(H1, R2))
    end.

%% O(1)
-spec make(T, heap(T), heap(T)) -> heap(T).
make(T, H1, H2) ->
    R1 = rank(H1),
    R2 = rank(H2),
    case R1 >= R2 of
    true ->
        {T, R2+1, H1, H2};
    false ->
        {T, R1+1, H2, H1}
    end.

%% O(1)
-spec rank(heap(_T)) -> rank().
rank(nil) ->
    0;
rank({_, R, _, _}) ->
    R.
%% SOLUTION_END

%% O(n)
%% https://en.wikipedia.org/wiki/Leftist_tree#Initializing_a_height_biased_leftist_tree
-spec from_list([T]) -> heap(T).
%% SOLUTION_BEGIN
from_list([]) ->
    nil;
from_list(Ts) ->
    from_queue(queue:from_list([{T, 0, nil, nil} || T <- Ts])).

from_queue(Q) ->
    { {value, H1}, Q1} = queue:out(Q),
    case queue:out(Q1) of
    {empty, _} ->
        H1;
    { {value, H2}, Q2} ->
        from_queue(queue:in(merge(H1, H2), Q2))
    end.
%% SOLUTION_END

%% O(n)
-spec to_list(heap(T)) -> [T].
%% SOLUTION_BEGIN
to_list(nil) ->
    [];
to_list(H) ->
    {Min, H1} = deleteMin(H),
    [Min | to_list(H1)].
%% SOLUTION_END

main(_) ->
    case eunit:test(?MODULE) of
    ok -> halt(0);
    _  -> halt(1)
    end.

heap_test() ->
    H0 = new(),
    ?assert(is_empty(H0)),
    H1 = insert(1, H0),
    ?assertNot(is_empty(H1)),
    H2 = insert(2, H1),
    H3 = insert(3, H2),
    ?assertEqual(1, min(H3)),
    {1, H4} = deleteMin(H3),
    H5 = insert(4, H4),
    {2, H6} = deleteMin(H5),
    {3, H7} = deleteMin(H6),
    {4, H8} = deleteMin(H7),
    ?assert(is_empty(H8)),
    ?assertError(empty, min(H8)),
    ?assertError(empty, deleteMin(H8)),
    ?assert(is_empty(from_list([]))),
    L = [4,8,10,9,1,3,5,6,11],
    ?assertEqual(1, min(from_list(L))),
    ?assertEqual([], to_list(new())),
    ?assertEqual(lists:sort(L), to_list(from_list(L))).
