-module(proper_test).
-export([main/1]).
-include_lib("proper/include/proper.hrl").
-include_lib("eunit/include/eunit.hrl").

prop_reverse() ->
    ?FORALL({Xs, Ys}, {list(), list()},
        lists:reverse(Xs ++ Ys) =:= lists:reverse(Ys) ++ lists:reverse(Xs)).

main(_) ->
    case eunit:test(?MODULE) of
    ok -> halt(0);
    _  -> halt(1)
    end.

proper_test_() ->
    {timeout, 5,
        ?_assertEqual([], proper:module(?MODULE, [{to_file, user}]))}.
