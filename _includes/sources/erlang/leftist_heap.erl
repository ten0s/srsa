-module(leftist_heap).
-export([new/0, is_empty/1, insert/2, min/1, deleteMin/1, merge/2, from_list/1, to_list/1]).
%%+BEGIN_REMOVE
-export([to_dot/1]).
%%+END_REMOVE
-export([main/1]).
-include_lib("eunit/include/eunit.hrl").

-type rank() :: non_neg_integer().
-type heap(T) :: nil | {T, rank(), heap(T), heap(T)}.
-type error(_E) :: no_return().

-spec new() -> heap(_T).
%%+BEGIN_SOLUTION
new() ->
    nil.
%%+END_SOLUTION

%% O(1)
-spec is_empty(heap(_T)) -> boolean().
%%+BEGIN_SOLUTION
is_empty(nil) ->
    true;
is_empty({_, _, _, _}) ->
    false.
%%+END_SOLUTION

%% O(lg(n))
-spec insert(T, heap(T)) -> heap(T).
%%+BEGIN_SOLUTION
insert(T, H) ->
    merge({T, 1, nil, nil}, H).
%%+END_SOLUTION

%% O(1)
-spec min(heap(T)) -> T | error(empty).
%%+BEGIN_SOLUTION
min(nil) ->
    error(empty);
min({T, _, _, _}) ->
    T.
%%+END_SOLUTION

%% O(lg(n))
-spec deleteMin(heap(T)) -> heap(T) | error(empty).
%%+BEGIN_SOLUTION
deleteMin(nil) ->
    error(empty);
deleteMin({_, _, L, R}) ->
    merge(L, R).
%%+END_SOLUTION

%% O(lg(n))
-spec merge(heap(T), heap(T)) -> heap(T).
%%+BEGIN_SOLUTION
merge(nil, H) ->
    H;
merge(H, nil) ->
    H;
merge({T1, _, L1, R1} = H1, {T2, _, L2, R2} = H2) ->
    case T1 =< T2 of
    true ->
        balance(T1, L1, merge(R1, H2));
    false ->
        balance(T2, L2, merge(H1, R2))
    end.

%% O(1)
-spec balance(T, heap(T), heap(T)) -> heap(T).
balance(T, H1, H2) ->
    R1 = rank(H1),
    R2 = rank(H2),
    R = R1 + R2 + 1,
    case R1 >= R2 of
    true ->
        {T, R, H1, H2};
    false ->
        {T, R, H2, H1}
    end.

%% O(1)
-spec rank(heap(_T)) -> rank().
rank(nil) ->
    0;
rank({_, R, _, _}) ->
    R.
%%+END_SOLUTION

%% O(n)
%% https://en.wikipedia.org/wiki/Leftist_tree#Initializing_a_height_biased_leftist_tree
-spec from_list([T]) -> heap(T).
%%+BEGIN_SOLUTION
from_list([]) ->
    nil;
from_list(Ts) ->
    from_queue(queue:from_list([{T, 1, nil, nil} || T <- Ts])).

from_queue(Q) ->
    { {value, H1}, Q1} = queue:out(Q),
    case queue:out(Q1) of
    {empty, _} ->
        H1;
    { {value, H2}, Q2} ->
        from_queue(queue:in(merge(H1, H2), Q2))
    end.
%%+END_SOLUTION

%% O(n)
-spec to_list(heap(T)) -> [T].
%%+BEGIN_SOLUTION
to_list(nil) ->
    [];
to_list(H) ->
    [min(H) | to_list(deleteMin(H))].
%%+END_SOLUTION

%%+BEGIN_REMOVE
-spec to_dot(heap(_T)) -> iolist().
to_dot(H) ->
    ["graph {\n",
      heap_to_dot(H),
     "}\n"].

heap_to_dot(nil) ->
    "";
heap_to_dot({T, Tr, nil, nil}) ->
    [io_lib:format("  ~p [label=\"~p/~p\"];~n", [T, T, Tr]),
     io_lib:format("  ~p;~n", [T])];
heap_to_dot({T, Tr, nil, {TR, _, _, _} = R}) ->
    [io_lib:format("  ~p [label=\"~p/~p\"];~n", [T, T, Tr]),
     io_lib:format("  ~p -- ~p;~n", [T, TR]),
     heap_to_dot(R)];
heap_to_dot({T, Tr, {TL, _, _, _} = L, nil}) ->
    [io_lib:format("  ~p [label=\"~p/~p\"];~n", [T, T, Tr]),
     io_lib:format("  ~p -- ~p;~n", [T, TL]),
     heap_to_dot(L)];
heap_to_dot({T, Tr, {TL, _, _, _} = L, {TR, _, _, _} = R}) ->
    [io_lib:format("  ~p [label=\"~p/~p\"];~n", [T, T, Tr]),
     io_lib:format("  ~p -- ~p;~n", [T, TL]),
     io_lib:format("  ~p -- ~p;~n", [T, TR]),
     heap_to_dot(L),
     heap_to_dot(R)].
%%+END_REMOVE

%%+BEGIN_FOLD Utils {
-spec depth(heap(_T)) -> pos_integer().
depth(nil) ->
    0;
depth({_, _, L, R}) ->
    1 + max(depth(L), depth(R)).
%%+END_FOLD }

%%+BEGIN_FOLD Tests {
main(_) ->
    case eunit:test(?MODULE) of
    ok -> halt(0);
    _  -> halt(1)
    end.

heap_test() ->
    H10 = new(),
    ?assert(is_empty(H10)),
    H11 = insert(1, H10),
    ?assertNot(is_empty(H11)),
    H12 = insert(2, H11),
    H13 = insert(3, H12),
    ?assertEqual(1, min(H13)),
    {1, H14} = {min(H13), deleteMin(H13)},
    H15 = insert(4, H14),
    {2, H16} = {min(H15), deleteMin(H15)},
    {3, H17} = {min(H16), deleteMin(H16)},
    {4, H18} = {min(H17), deleteMin(H17)},
    ?assert(is_empty(H18)),
    ?assertError(empty, min(H18)),
    ?assertError(empty, deleteMin(H18)),

    ?assert(is_empty(from_list([]))),
    ?assertEqual([], to_list(new())),
    L = [4,8,10,9,1,3,5,6,11],
    ?assertEqual(1, min(from_list(L))),
    ?assertEqual(lists:sort(L), to_list(from_list(L))),

    H20 = from_list(L),
    ?assertEqual(1, min(H20)),
    %io:format(user, to_dot(H20), []),
    ?assertEqual(5, depth(H20)).
%%+END_FOLD }
