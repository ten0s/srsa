-module(bintree).
-export([
    new/0, is_empty/1, size/1, depth/1,
    from_ordlist/1, to_ordlist/1, map/2,
    preorder/3, inorder/3, revinorder/3, postorder/3
]).
%%+BEGIN_FOLD Tests {
-export([main/1]).
-include_lib("eunit/include/eunit.hrl").
%%+END_FOLD }
%%+BEGIN_FOLD Utils {
-compile({no_auto_import, [size/1]}).
%%+END_FOLD }

-type bintree(T) :: nil | {node, T, bintree(T), bintree(T)}.
-type ordlist(T) :: list(T).

-spec new() -> bintree(_T).
%%+BEGIN_SOLUTION
new() ->
    nil.
%%+END_SOLUTION

-spec is_empty(bintree(_T)) -> boolean().
%%+BEGIN_SOLUTION
is_empty(nil) ->
    true;
is_empty({node, _, _, _}) ->
    false.
%%+END_SOLUTION

-spec size(bintree(_T)) -> non_neg_integer().
%%+BEGIN_SOLUTION
size(nil) ->
    0;
size({node, _, L, R}) ->
    1 + size(L) + size(R).
%%+END_SOLUTION

-spec depth(bintree(_T)) -> non_neg_integer().
%%+BEGIN_SOLUTION
depth(nil) ->
    0;
depth({node, _, L, R}) ->
    1 + max(depth(L), depth(R)).
%%+END_SOLUTION

-spec from_ordlist(ordlist(T)) -> bintree(T).
%%+BEGIN_SOLUTION
from_ordlist([]) ->
    nil;
from_ordlist(Vs) ->
    {Ls, [V | Rs]} = lists:split(length(Vs) div 2, Vs),
    {node, V, from_ordlist(Ls), from_ordlist(Rs)}.
%%+END_SOLUTION

-spec to_ordlist(bintree(T)) -> ordlist(T).
%%+BEGIN_SOLUTION
to_ordlist(T) ->
    revinorder(fun (V, Acc) -> [V | Acc] end, [], T).
%%+END_SOLUTION

-spec map(fun ((T) -> U), bintree(T)) -> bintree(U).
%%+BEGIN_SOLUTION
map(_Fun1, nil) ->
    nil;
map(Fun1, {node, V, L, R}) ->
    {node, Fun1(V), map(Fun1, L), map(Fun1, R)}.
%%+END_SOLUTION

-spec preorder(fun ((bintree(T), Acc) -> Acc), Acc, bintree(T)) -> Acc.
%%+BEGIN_SOLUTION
preorder(_Fun2, Acc, nil) ->
    Acc;
preorder(Fun2, Acc0, {node, V, L, R}) ->
    Acc1 = Fun2(V, Acc0),
    Acc2 = preorder(Fun2, Acc1, L),
    preorder(Fun2, Acc2, R).
%%+END_SOLUTION

-spec inorder(fun ((bintree(T), Acc) -> Acc), Acc, bintree(T)) -> Acc.
%%+BEGIN_SOLUTION
inorder(_Fun2, Acc, nil) ->
    Acc;
inorder(Fun2, Acc0, {node, V, L, R}) ->
    Acc1 = inorder(Fun2, Acc0, L),
    Acc2 = Fun2(V, Acc1),
    inorder(Fun2, Acc2, R).
%%+END_SOLUTION

-spec revinorder(fun ((bintree(T), Acc) -> Acc), Acc, bintree(T)) -> Acc.
%%+BEGIN_SOLUTION
revinorder(_Fun2, Acc, nil) ->
    Acc;
revinorder(Fun2, Acc0, {node, V, L, R}) ->
    Acc1 = revinorder(Fun2, Acc0, R),
    Acc2 = Fun2(V, Acc1),
    revinorder(Fun2, Acc2, L).
%%+END_SOLUTION

-spec postorder(fun ((bintree(T), Acc) -> Acc), Acc, bintree(T)) -> Acc.
%%+BEGIN_SOLUTION
postorder(_Fun2, Acc, nil) ->
    Acc;
postorder(Fun2, Acc0, {node, V, L, R}) ->
    Acc1 = postorder(Fun2, Acc0, L),
    Acc2 = postorder(Fun2, Acc1, R),
    Fun2(V, Acc2).
%%+END_SOLUTION

%%+BEGIN_FOLD Tests {
main(_) ->
    case eunit:test(?MODULE) of
    ok -> halt(0);
    _  -> halt(1)
    end.

bintree_test() ->
    T0 = new(),
    ?assert(is_empty(T0)),
    ?assertEqual(0, size(T0)),
    ?assertEqual(0, depth(T0)),

    T1 = from_ordlist([1]),
    ?assertNot(is_empty(T1)),
    ?assertEqual(1, size(T1)),
    ?assertEqual(1, depth(T1)),
    ?assertEqual([1], to_ordlist(T1)),

    T4 = from_ordlist([1,2,3,4]),
    ?assertEqual(4, size(T4)),
    ?assertEqual(3, depth(T4)),
    ?assertEqual([1,2,3,4], to_ordlist(T4)),
    ?assertEqual([2,4,6,8], to_ordlist(map(fun (X) -> 2*X end, T4))),

    ?assertEqual([3,2,1,4], preorder(fun (V, Acc) -> Acc ++ [V] end, [], T4)),
    ?assertEqual(10, preorder(fun erlang:'+'/2, 0, T4)),

    ?assertEqual([1,2,3,4], inorder(fun (V, Acc) -> Acc ++ [V] end, [], T4)),
    ?assertEqual(10, inorder(fun erlang:'+'/2, 0, T4)),

    ?assertEqual([4,3,2,1], revinorder(fun (V, Acc) -> Acc ++ [V] end, [], T4)),
    ?assertEqual(10, revinorder(fun erlang:'+'/2, 0, T4)),

    ?assertEqual([1,2,4,3], postorder(fun (V, Acc) -> Acc ++ [V] end, [], T4)),
    ?assertEqual(10, postorder(fun erlang:'+'/2, 0, T4)).
%%+END_FOLD }
