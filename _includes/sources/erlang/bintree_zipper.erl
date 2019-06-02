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
-type turn(T) :: {left, T, bintree(T)} | {right, T, bintree(T)}.
-type zipper(T) :: {bintree(T), list(turn(T))}.

-spec from_bintree(bintree(T)) -> zipper(T).
%%+BEGIN_SOLUTION
from_bintree(N) ->
    {N, []}.
%%+END_SOLUTION

-spec to_bintree(zipper(T)) -> bintree(T).
%%+BEGIN_SOLUTION
to_bintree(Z) ->
    {N, []} = top(Z), N.
%%+END_SOLUTION

-spec left(zipper(T)) -> {ok, zipper(T)} | error.
%%+BEGIN_SOLUTION
left({nil, _}) ->
    error;
left({ {node, V, L, R}, Ts}) ->
    {ok, {L, [{left, V, R} | Ts]}}.
%%+END_SOLUTION

-spec right(zipper(T)) -> {ok, zipper(T)} | error.
%%+BEGIN_SOLUTION
right({nil, _}) ->
    error;
right({ {node, V, L, R}, Ts}) ->
    {ok, {R, [{right, V, L} | Ts]}}.
%%+END_SOLUTION

-spec up(zipper(T)) -> {ok, zipper(T)} | error.
%%+BEGIN_SOLUTION
up({_, []}) ->
    error;
up({L, [{left, V, R} | Ts]}) ->
    {ok, { {node, V, L, R}, Ts}};
up({R, [{right, V, L} | Ts]}) ->
    {ok, { {node, V, L, R}, Ts}}.
%%+END_SOLUTION

-spec top(zipper(T)) -> zipper(T).
%%+BEGIN_SOLUTION
top({N, Ts}) ->
    case up({N, Ts}) of
    {ok, {N2, Ts2}} ->
        top({N2, Ts2});
    error ->
        {N, Ts}
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
