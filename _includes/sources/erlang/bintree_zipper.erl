-module(bintree_zipper).
-export([
    from_bintree/1, to_bintree/1,
    left/1, right/1, up/1, top/1
]).
%%+BEGIN_FOLD Tests {
-export([main/1]).
-include_lib("eunit/include/eunit.hrl").
%%+END_FOLD }

-type bintree(T) :: nil | {node, T, bintree(T), bintree(T)}.
-type direction(T) :: {left, T, bintree(T)} | {right, T, bintree(T)}.
-type zipper(T) :: {bintree(T), list(direction(T))}.

-spec from_bintree(bintree(T)) -> zipper(T).
%%+BEGIN_SOLUTION
from_bintree(T) ->
    {T, []}.
%%+END_SOLUTION

-spec to_bintree(zipper(T)) -> bintree(T).
%%+BEGIN_SOLUTION
to_bintree(Z) ->
    {T, []} = top(Z), T.
%%+END_SOLUTION

-spec left(zipper(T)) -> {ok, zipper(T)} | error.
%%+BEGIN_SOLUTION
left({nil, _}) ->
    error;
left({ {node, V, L, R}, Ds}) ->
    {ok, {L, [{left, V, R} | Ds]}}.
%%+END_SOLUTION

-spec right(zipper(T)) -> {ok, zipper(T)} | error.
%%+BEGIN_SOLUTION
right({nil, _}) ->
    error;
right({ {node, V, L, R}, Ds}) ->
    {ok, {R, [{right, V, L} | Ds]}}.
%%+END_SOLUTION

-spec up(zipper(T)) -> {ok, zipper(T)} | error.
%%+BEGIN_SOLUTION
up({_, []}) ->
    error;
up({L, [{left, V, R} | Ds]}) ->
    {ok, { {node, V, L, R}, Ds}};
up({R, [{right, V, L} | Ds]}) ->
    {ok, { {node, V, L, R}, Ds}}.
%%+END_SOLUTION

-spec top(zipper(T)) -> zipper(T).
%%+BEGIN_SOLUTION
top(Z0) ->
    case up(Z0) of
    {ok, Z} -> top(Z);
    error   -> Z0
    end.
%%+END_SOLUTION

%%+BEGIN_FOLD Tests {
main(_) ->
    case eunit:test(?MODULE) of
    ok -> halt(0);
    _  -> halt(1)
    end.

zipper_test() ->
    T0 = bintree:from_list([1,2,3]),
    Z0 = from_bintree(T0),
    {ok, ZL0} = left(Z0),
    {ok, Z0} = up(ZL0),
    {ok, ZLL0} = left(ZL0),
    error = left(ZLL0),
    Z0 = top(ZLL0),
    T0 = to_bintree(ZLL0),
    {ok, ZR0} = right(Z0),
    {ok, Z0} = up(ZR0),
    {ok, ZRR0} = right(ZR0),
    error = right(ZRR0),
    Z0 = top(ZRR0),
    T0 = to_bintree(ZRR0).
%%+END_FOLD }
