-module(my_queue).
-export([
    new/0, is_empty/1,
    from_list/1, to_list/1,
    enqueue/2, dequeue/1, peek/1
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

%% Invariant must be maintained that F is empty only when R is empty.
-type queue(T) :: {F::[T], R::[T]}.
-type error(_E) :: no_return().

-spec new() -> queue(_T).
%%+BEGIN_SOLUTION
new() ->
    {[], []}.
%%+END_SOLUTION

%% O(1)
-spec is_empty(queue(_T)) -> boolean().
%%+BEGIN_SOLUTION
is_empty({[], _}) ->
    true;
is_empty({_, _}) ->
    false.
%%+END_SOLUTION

-spec from_list(list(T)) -> queue(T).
%%+BEGIN_SOLUTION
from_list(L) ->
    {L, []}.
%%+END_SOLUTION

-spec to_list(queue(T)) -> list(T).
%%+BEGIN_SOLUTION
to_list({F, R}) ->
    F ++ lists:reverse(R).
%%+END_SOLUTION

%% O(1)
-spec enqueue(T, queue(T)) -> queue(T).
%%+BEGIN_SOLUTION
enqueue(X, {[], _}) ->
    {[X], []};
enqueue(X, {F, R}) ->
    {F, [X | R]}.
%%+END_SOLUTION

-spec dequeue(queue(T)) -> {T, queue(T)} | error(empty).
%%+BEGIN_SOLUTION
dequeue({[], _}) ->
    error(empty);
dequeue({[X], R}) ->
    {X, {lists:reverse(R), []}};
dequeue({[X | F], R}) ->
    {X, {F, R}}.
%%+END_SOLUTION

%% O(1)
-spec peek(queue(T)) -> T | error(empty).
%%+BEGIN_SOLUTION
peek({[], _}) ->
    error(empty);
peek({[X | _], _}) ->
    X.
%%+END_SOLUTION

%%+BEGIN_FOLD Tests {
main(_) ->
    case eunit:test(?MODULE) of
    ok -> halt(0);
    _  -> halt(1)
    end.

queue_test() ->
    Q0 = new(),
    ?assert(is_empty(Q0)),
    Q1 = {[1], []} = enqueue(1, Q0),
    ?assertNot(is_empty(Q1)),
    Q2 = {[1], [2]} = enqueue(2, Q1),
    Q3 = {[1], [3,2]} = enqueue(3, Q2),
    ?assertEqual(1, peek(Q3)),
    {1, Q4 = {[2,3], []}} = dequeue(Q3),
    Q5 = {[2,3], [4]} = enqueue(4, Q4),
    {2, Q6} = dequeue(Q5),
    {3, Q7 = {[4], []}} = dequeue(Q6),

    {4, Q8} = dequeue(Q7),
    ?assert(is_empty(Q8)),
    ?assertError(empty, peek(Q8)),
    ?assertError(empty, dequeue(Q8)),

    L = [1,2,3,4],
    Q9 = {L, []} = from_list(L),
    {1, Q10 = {[2,3,4], []}} = dequeue(Q9),
    Q11 = {[2,3,4], [5]} = enqueue(5, Q10),
    ?assertEqual([2,3,4,5], to_list(Q11)).

proper_test_() ->
    {timeout, 5,
        ?_assertEqual([], proper:module(?MODULE, [{to_file, user}, nocolors]))}.

prop_from_to_list() ->
    ?FORALL(L, list(),
        L == to_list(from_list(L))).

prop_queue() ->
    ?FORALL(Cmds, commands(?MODULE),
    begin
        {H, S, Res} = run_commands(?MODULE, Cmds),
        ?WHENFAIL(
            io:format("~nHistory:~n~p~nState:~n~p~nRes:~n~p~n~n",
                [H, S, Res]),
            aggregate(command_names(Cmds), Res =:= ok))
    end).

-record(st, {
    queue :: queue(integer()),
    model :: list(integer())
}).

initial_state() ->
    #st{queue = new(),
        model = []}.

command(#st{queue = Q}) ->
    oneof([
        {call, ?MODULE, is_empty, [Q]}
      , {call, ?MODULE, enqueue, [integer(), Q]}
      , {call, ?MODULE, dequeue, [Q]}
      , {call, ?MODULE, peek, [Q]}
    ]).

precondition(S, {call, ?MODULE, dequeue, [_Q]}) ->
    S#st.model =/= [];
precondition(S, {call, ?MODULE, peek, [_Q]}) ->
    S#st.model =/= [];
precondition(_S, _Call) ->
    true.

next_state(S, _Ret, {call, ?MODULE, is_empty, [_Q0]}) ->
    S;
next_state(S, Q1, {call, ?MODULE, enqueue, [V, _Q0]}) ->
    S#st{queue = Q1, model = append(S#st.model, V)};
next_state(S, T_Q1,  {call, ?MODULE, dequeue, [_Q0]}) ->
    Q1 = {call,erlang,element,[2, T_Q1]},
    S#st{queue = Q1, model = tail(S#st.model)};
next_state(S, _Ret, _Call) ->
    S.

postcondition(S, {call, ?MODULE, is_empty, [_Q]}, Res) ->
    Res == (S#st.model == []);
postcondition(S, {call, ?MODULE, enqueue, [_V, _Q0]}, _Q1) ->
    to_list(S#st.queue) == S#st.model;
postcondition(S, {call, ?MODULE, dequeue, [_Q]}, {_V, _Q1}) ->
    to_list(S#st.queue) == S#st.model;
postcondition(S, {call, ?MODULE, peek, [_Q]}, Res) ->
    Res == head(S#st.model).

head(L) ->
    hd(L).

tail(L) ->
    tl(L).

append(L, V) ->
    L ++ [V].

%%+END_FOLD }
