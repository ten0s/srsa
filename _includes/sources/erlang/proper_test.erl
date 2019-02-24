-module(proper_test).
%%+BEGIN_FOLD Tests {
-export([main/1]).
-include_lib("proper/include/proper.hrl").
-include_lib("eunit/include/eunit.hrl").
%%+END_FOLD }

prop_reverse() ->
    %%+BEGIN_SOLUTION
    ?FORALL({Xs, Ys}, {list(), list()},
        lists:reverse(Xs ++ Ys) =:= lists:reverse(Ys) ++ lists:reverse(Xs)).
    %%+END_SOLUTION

%%+BEGIN_FOLD Tests {
main(_) ->
    case eunit:test(?MODULE) of
    ok -> halt(0);
    _  -> halt(1)
    end.

proper_test_() ->
    {timeout, 5,
        ?_assertEqual([], proper:module(?MODULE, [{to_file, user}, nocolors]))}.
%%+END_FOLD }
