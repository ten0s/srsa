-module(bintree).
-export([
    new/0, is_empty/1, size/1, height/1,
    from_list/1, to_list/1, map/2, fold/3, traverse/4,
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
-export_type([bintree/1]).

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

%% The height of a tree is the depth of its deepest node, i.e.
%% the longest number of edges.
-spec height(bintree(_T)) -> non_neg_integer().
%%+BEGIN_SOLUTION
height(nil) ->
    0;
height({node, _, nil, nil}) ->
    0;
height({node, _, L, R}) ->
    1 + max(height(L), height(R)).
%%+END_SOLUTION

-spec from_list(list(T)) -> bintree(T).
%%+BEGIN_SOLUTION
from_list([]) ->
    nil;
from_list(Vs) ->
    {Ls, [V | Rs]} = lists:split(length(Vs) div 2, Vs),
    {node, V, from_list(Ls), from_list(Rs)}.
%%+END_SOLUTION

-spec to_list(bintree(T)) -> list(T).
%%+BEGIN_SOLUTION
to_list(T) ->
    revinorder(fun (V, Acc) -> [V | Acc] end, [], T).
%%+END_SOLUTION

-spec map(fun ((T) -> U), bintree(T)) -> bintree(U).
%%+BEGIN_SOLUTION
map(_Fun1, nil) ->
    nil;
map(Fun1, {node, V, L, R}) ->
    {node, Fun1(V), map(Fun1, L), map(Fun1, R)}.
%%+END_SOLUTION

-spec fold(fun ((T, Acc, Acc) -> Acc), Acc, bintree(T)) -> Acc.
%%+BEGIN_SOLUTION
fold(_Fun3, Acc, nil) ->
    Acc;
fold(Fun3, Acc, {node, V, L, R}) ->
    Fun3(V, fold(Fun3, Acc, L), fold(Fun3, Acc, R)).
%%+END_SOLUTION

%% traverse/4 is even more general than fold/3.
%% See below examples of using traverse/4 to implement
%% is_empty/1, size/1, map/2, fold/3
-type promise(A) :: fun ((A) -> A).
-spec traverse(
    fun ((bintree(T), Acc) -> Acc),
    fun ((promise(Acc), promise(Acc), promise(Acc), Acc) -> Acc),
    Acc,
    bintree(T)
) -> Acc.
%%+BEGIN_SOLUTION
traverse(_Fun2, _Gun4, Acc, nil) ->
    Acc;
traverse(Fun2, Gun4, Acc, {node, V, L, R}) ->
    Gun4(
      fun (A) -> Fun2(V, A) end,
      fun (A) -> traverse(Fun2, Gun4, A, L) end,
      fun (A) -> traverse(Fun2, Gun4, A, R) end,
      Acc
    ).
%%+END_SOLUTION

-spec preorder(fun ((bintree(T), Acc) -> Acc), Acc, bintree(T)) -> Acc.
%%+BEGIN_SOLUTION
preorder(Fun2, Init, Tree) ->
    traverse(Fun2, fun (VFun, LFun, RFun, Acc) -> RFun(LFun(VFun(Acc))) end, Init, Tree).
%%+END_SOLUTION

-spec inorder(fun ((bintree(T), Acc) -> Acc), Acc, bintree(T)) -> Acc.
%%+BEGIN_SOLUTION
inorder(Fun2, Init, Tree) ->
    traverse(Fun2, fun (VFun, LFun, RFun, Acc) -> RFun(VFun(LFun(Acc))) end, Init, Tree).
%%+END_SOLUTION

-spec revinorder(fun ((bintree(T), Acc) -> Acc), Acc, bintree(T)) -> Acc.
%%+BEGIN_SOLUTION
revinorder(Fun2, Init, Tree) ->
    traverse(Fun2, fun (VFun, LFun, RFun, Acc) -> LFun(VFun(RFun(Acc))) end, Init, Tree).
%%+END_SOLUTION

-spec postorder(fun ((bintree(T), Acc) -> Acc), Acc, bintree(T)) -> Acc.
%%+BEGIN_SOLUTION
postorder(Fun2, Init, Tree) ->
    traverse(Fun2, fun (VFun, LFun, RFun, Acc) -> VFun(RFun(LFun(Acc))) end, Init, Tree).
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
    ?assertEqual(0, height(T0)),

    T1 = from_list([1]),
    ?assertNot(is_empty(T1)),
    ?assertEqual(1, size(T1)),
    ?assertEqual(0, height(T1)),
    ?assertEqual([1], to_list(T1)),

    T4 = from_list([1,2,3,4]),
    ?assertEqual(4, size(T4)),
    ?assertEqual(2, height(T4)),
    ?assertEqual([1,2,3,4], to_list(T4)),
    ?assertEqual([2,4,6,8], to_list(map(fun (X) -> 2*X end, T4))),

    %% map/2 using fold/3
    ?assertEqual([2,4,6,8], to_list(fold(fun (V, L, R) -> {node, 2*V, L, R} end, nil, T4))),
    %% mirror
    ?assertEqual([4,3,2,1], to_list(fold(fun (V, L, R) -> {node, V, R, L} end, nil, T4))),

    ?assertEqual([3,2,1,4], preorder(fun (V, Acc) -> Acc ++ [V] end, [], T4)),
    ?assertEqual(10, preorder(fun erlang:'+'/2, 0, T4)),

    ?assertEqual([1,2,3,4], inorder(fun (V, Acc) -> Acc ++ [V] end, [], T4)),
    ?assertEqual(10, inorder(fun erlang:'+'/2, 0, T4)),

    ?assertEqual([4,3,2,1], revinorder(fun (V, Acc) -> Acc ++ [V] end, [], T4)),
    ?assertEqual(10, revinorder(fun erlang:'+'/2, 0, T4)),

    ?assertEqual([1,2,4,3], postorder(fun (V, Acc) -> Acc ++ [V] end, [], T4)),
    ?assertEqual(10, postorder(fun erlang:'+'/2, 0, T4)),

    T5 = from_list([1,2,3,4,5]),
    ?assertEqual(5, size(T5)),
    ?assertEqual(2, height(T5)),
    ?assertEqual([1,2,3,4,5], to_list(T5)),

    T7 = {node, 4, {node, 3, {node, 1, nil, nil},
                             {node, 2, nil, nil}},
                   {node, 5, nil,
                             {node, 6, {node, 7, nil, nil},
                                       nil}}},
   ?assertEqual(3, height(T7)).
%%+END_FOLD }

%% is_empty/1 using fold/3
%% is_empty(Tree) ->
%%     fold(fun (_, _, _) -> fales end, true, Tree).

%% is_empty/1 using traverse/4
%% is_empty(Tree) ->
%%     traverse(
%%         fun (_, Acc) -> Acc end,
%%         fun (_VF, _LF, _RF, _Acc) -> false end,
%%         true,
%%         Tree
%%     ).

%% size/1 using fold/3
%% size(Tree) ->
%%     fold(fun (_, LA, RA) -> 1 + LA + RA end, 0, Tree).

%% size/1 using traverse/4
%% size(Tree) ->
%%     traverse(
%%         fun (_, Acc) -> 1 + Acc end,
%%         fun (VF, LF, RF, Acc) -> VF(RF(LF(Acc))) end,
%%         0,
%%         Tree
%%     ).

%% map/2 using fold/3
%% map(Fun1, Tree) ->
%%     fold(fun (V, L, R) -> {node, Fun1(V), L, R) end, nil, Tree).

%% map/2 using traverse/4
%% map(Fun, Tree) ->
%%     traverse(
%%         fun (V, {L, R}) -> {node, Fun(V), L, R} end,
%%         fun (VF, LF, RF, Acc) -> VF({LF(Acc), RF(Acc)}) end,
%%         nil,
%%         Tree
%%     ).

%% fold/3 using traverse/4
%% fold(Fun3, Init, Tree) ->
%%     traverse(
%%         fun (V, {LA, RA}) -> Fun3(V, LA, RA) end,
%%         fun (VF, LF, RF, Acc) -> VF({LF(Acc), RF(Acc)}) end,
%%         Init,
%%         Tree
%%     ).
