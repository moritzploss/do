%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% @doc
%%% @end
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%%_* Module declaration ======================================================
-module(do_traversable).

%%%_* Includes ================================================================
-include("include/do_macros.hrl").
-include("include/do_types.hrl").

%%%_* Exports =================================================================
-export([sequence/2]).
-export([traverse/2]).

%%%_* Code ====================================================================
%%%_* API ---------------------------------------------------------------------
-spec sequence(traversable(applicative(A)), atom()) -> applicative(traversable(A)).
sequence([F | Rest], Mod) when ?isF0(F) ->
  sequence([F() | Rest], Mod);
sequence([Elm | Rest], Mod) ->
  case Mod:is_right(Elm) of
    true  -> Mod:fmap(fun(Vals) -> [Mod:right(Elm) | Vals] end, sequence(Rest, Mod));
    false -> Elm
  end;
sequence([], Mod) ->
  Mod:pure([]);
sequence(Map, Mod) when is_map(Map) ->
  {Keys, Vals} = lists:unzip(maps:to_list(Map)),
  Mod:fmap(fun(Sequenced) -> maps:from_list(lists:zip(Keys, Sequenced)) end,
            sequence(Vals, Mod)).

-spec traverse(fn(A, applicative(B)), traversable(A)) -> applicative(traversable(B)).
traverse(F, Traversable) when ?isF1(F) -> do_functor:fmap(F, Traversable).

%%%_* Tests ===================================================================
-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

traverse_test() ->
  F = fun(A) -> A + 1 end,
  ?assertEqual([1, 2, 3], traverse(F, [0, 1, 2])),
  ?assertEqual(#{a => 1, b => 2, c => 3}, traverse(F, #{a => 0, b => 1, c => 2})).

-endif.
