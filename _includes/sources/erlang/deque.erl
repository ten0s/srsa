-module(deque).
-export([
    new/0, is_empty/1,
    from_list/1, to_list/1,
    in_l/2, in_r/2,
    out_l/1, out_r/1,
    peek_l/1, peek_r/1
]).
%%+BEGIN_FOLD Tests {
-export([main/1]).
-include_lib("proper/include/proper.hrl").
-include_lib("eunit/include/eunit.hrl").
-behaviour(proper_statem).
-export([
    initial_state/0,
    command/1,
    precondition/2,
    postcondition/3,
    next_state/3
]).
%%+END_FOLD }

-type deque(T) :: {F::[T], R::[T]}.
-type error(_E) :: no_return().

-spec new() -> deque(_T).
new() ->
    {[], []}.

-spec is_empty(deque(_T)) -> boolean().
is_empty({[], []}) ->
    true;
is_empty({_, _}) ->
    false.

-spec from_list(list(T)) -> deque(T).
%%+BEGIN_SOLUTION
from_list(L) ->
    {L1, L2} = lists:split(length(L) div 2, L),
    {L1, lists:reverse(L2)}.
%%+END_SOLUTION

-spec to_list(deque(T)) -> list(T).
%%+BEGIN_SOLUTION
to_list({F, R}) ->
    F ++ lists:reverse(R).
%%+END_SOLUTION

%% O(1)
-spec in_l(T, deque(T)) -> deque(T).
%%+BEGIN_SOLUTION
in_l(X, {[F], []}) ->
    {[X], [F]};
in_l(X, {F, R}) ->
    {[X | F], R}.
%%+END_SOLUTION

%% O(1)
-spec in_r(T, deque(T)) -> deque(T).
%%+BEGIN_SOLUTION
in_r(X, {[], [R]}) ->
    {[R], [X]};
in_r(X, {F, R}) ->
    {F, [X | R]}.
%%+END_SOLUTION

%% O(n)
-spec out_l(deque(T)) -> {T, deque(T)} | error(empty).
%%+BEGIN_SOLUTION
out_l({[], []}) ->
    error(empty);
out_l({[], [X]}) ->
    {X, {[], []}};
out_l({[X], [R]}) ->
    {X, {[], [R]}};
out_l({[X], R}) ->
    {R1, R2} = lists:split(length(R) div 2, R),
    {X, {lists:reverse(R2), R1}};
out_l({[X | F], R}) ->
    {X, {F, R}}.
%%+END_SOLUTION

%% O(n)
-spec out_r(deque(T)) -> {T, deque(T)} | error(empty).
%%+BEGIN_SOLUTION
out_r({[], []}) ->
    error(empty);
out_r({[X], []}) ->
    {X, {[], []}};
out_r({[F], [X]}) ->
    {X, {[F], []}};
out_r({F, [X]}) ->
    {F1, F2} = lists:split(length(F) div 2, F),
    {X, {F1, lists:reverse(F2)}};
out_r({F, [X | R]}) ->
    {X, {F, R}}.
%%+END_SOLUTION

%% O(1)
-spec peek_l(deque(T)) -> T | error(empty).
%%+BEGIN_SOLUTION
peek_l({[], []}) ->
    error(empty);
peek_l({[], [X]}) ->
    X;
peek_l({[X | _], _}) ->
    X.
%%+END_SOLUTION

%% O(1)
-spec peek_r(deque(T)) -> T | error(empty).
%%+BEGIN_SOLUTION
peek_r({[], []}) ->
    error(empty);
peek_r({[X], []}) ->
    X;
peek_r({_, [X | _]}) ->
    X.
%%+END_SOLUTION

%%+BEGIN_FOLD Tests {
main(_) ->
    case eunit:test(?MODULE) of
    ok -> halt(0);
    _  -> halt(1)
    end.

deque_test() ->
    Q0 = new(),
    ?assert(is_empty(Q0)),

    %% R-in L-out queue tests
    RLQ1 = in_r(1, Q0),
    %{[], [1]} = RLR1,
    ?assertNot(is_empty(RLQ1)),
    RLQ2 = in_r(2, RLQ1),
    %{[1], [2]} = RLQ2
    RLQ3 = in_r(3, RLQ2),
    %{[1], [3,2]} = RLQ3,
    ?assertEqual(1, peek_l(RLQ3)),
    {1, RLQ4} = out_l(RLQ3),
    %{[2], [3]} = RLQ4,
    RLQ5 = in_r(4, RLQ4),
    %{[2], [4,3]} = RLQ5,
    {2, RLQ6} = out_l(RLQ5),
    %{[3], [4]} = RLQ6,
    {3, RLQ7} = out_l(RLQ6),
    %{[], [4]} = RLQ7,
    {4, RLQ8} = out_l(RLQ7),
    ?assert(is_empty(RLQ8)),
    ?assertError(empty, peek_l(RLQ8)),
    ?assertError(empty, out_l(RLQ8)),

    %% L-in R-out queue tests
    LRQ1 = in_l(1, Q0),
    %{[1], []} = LRQ1,
    ?assertNot(is_empty(LRQ1)),
    LRQ2 = in_l(2, LRQ1),
    %{[2], [1]} = LRQ2,
    LRQ3 = in_l(3, LRQ2),
    %{[3,2], [1]} = LRQ3,
    ?assertEqual(1, peek_r(LRQ3)),
    {1, LRQ4} = out_r(LRQ3),
    %{[3], [2]} = LRQ4
    LRQ5 = in_l(4, LRQ4),
    %{[4,3], [2]} = LRQ5,
    {2, LRQ6} = out_r(LRQ5),
    %{[4], [3]} = LRQ6,
    {3, LRQ7} = out_r(LRQ6),
    %{[4], []} = LRQ7,
    {4, LRQ8} = out_r(LRQ7),
    ?assert(is_empty(LRQ8)),
    ?assertError(empty, peek_r(LRQ8)),
    ?assertError(empty, out_r(LRQ8)),

    ?assertEqual({[], []}, from_list([])),
    ?assertEqual({[], [1]}, from_list([1])),
    ?assertEqual({[1], [2]}, from_list([1,2])),
    ?assertEqual({[1], [3,2]}, from_list([1,2,3])),
    ?assertEqual({[1,2], [4,3]}, from_list([1,2,3,4])),
    ?assertEqual([1,2,3,4], to_list(from_list([1,2,3,4]))).

proper_test_() ->
    {timeout, 5,
        ?_assertEqual([], proper:module(?MODULE, [{to_file, user}, nocolors]))}.

prop_from_to_list() ->
    ?FORALL(L, list(),
        L == to_list(from_list(L))).

prop_deque() ->
    ?FORALL(Cmds, commands(?MODULE),
    begin
        {H, S, Res} = run_commands(?MODULE, Cmds),
        ?WHENFAIL(
            io:format("~nHistory:~n~p~nState:~n~p~nRes:~n~p~n~n",
                [H, S, Res]),
            aggregate(command_names(Cmds), Res =:= ok))
    end).

-record(st, {
    deque :: deque(integer()),
    model :: list(integer())
}).

initial_state() ->
    #st{deque = new(),
        model = []}.

command(#st{deque = D}) ->
    oneof([
        {call, ?MODULE, is_empty, [D]}
      , {call, ?MODULE, in_l, [integer(), D]}
      , {call, ?MODULE, in_r, [integer(), D]}
      , {call, ?MODULE, out_l, [D]}
      , {call, ?MODULE, out_r, [D]}
      , {call, ?MODULE, peek_l, [D]}
      , {call, ?MODULE, peek_r, [D]}
    ]).

precondition(S, {call, ?MODULE, out_l, [_D]}) ->
    S#st.model =/= [];
precondition(S, {call, ?MODULE, out_r, [_D]}) ->
    S#st.model =/= [];
precondition(S, {call, ?MODULE, peek_l, [_D]}) ->
    S#st.model =/= [];
precondition(S, {call, ?MODULE, peek_r, [_D]}) ->
    S#st.model =/= [];
precondition(_S, _Call) ->
    true.

next_state(S, _Ret, {call, ?MODULE, is_empty, [_D0]}) ->
    S;
next_state(S, D1, {call, ?MODULE, in_l, [V, _D0]}) ->
    S#st{deque = D1, model = prepend(V, S#st.model)};
next_state(S, D1, {call, ?MODULE, in_r, [V, _D0]}) ->
    S#st{deque = D1, model = append(S#st.model, V)};
next_state(S, T_D1,  {call, ?MODULE, out_l, [_D0]}) ->
    D1 = {call,erlang,element,[2, T_D1]},
    S#st{deque = D1, model = tail(S#st.model)};
next_state(S, T_D1,  {call, ?MODULE, out_r, [_D0]}) ->
    D1 = {call,erlang,element,[2, T_D1]},
    S#st{deque = D1, model = init(S#st.model)};
next_state(S, _Ret, _Call) ->
    S.

postcondition(S, {call, ?MODULE, is_empty, [_D]}, Res) ->
    Res == (S#st.model == []);
postcondition(S, {call, ?MODULE, in_l, [_V, _D0]}, _D1) ->
    to_list(S#st.deque) == S#st.model;
postcondition(S, {call, ?MODULE, in_r, [_V, _D0]}, _D1) ->
    to_list(S#st.deque) == S#st.model;
postcondition(S, {call, ?MODULE, out_l, [_D]}, {_V, _D1}) ->
    to_list(S#st.deque) == S#st.model;
postcondition(S, {call, ?MODULE, out_r, [_D]}, {_V, _D1}) ->
    to_list(S#st.deque) == S#st.model;
postcondition(S, {call, ?MODULE, peek_l, [_D]}, Res) ->
    Res == head(S#st.model);
postcondition(S, {call, ?MODULE, peek_r, [_D]}, Res) ->
    Res == last(S#st.model).

head(L) ->
    hd(L).

tail(L) ->
    tl(L).

init(L) ->
    {L1, [_]} = lists:split(length(L)-1, L),
    L1.

last(L) ->
    lists:last(L).

append(L, V) ->
    L ++ [V].

prepend(V, L) ->
    [V] ++ L.

%%+END_FOLD }
