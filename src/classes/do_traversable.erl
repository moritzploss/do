%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% @doc The Traversable Type Class.
%%% @end
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%%_* Module declaration ======================================================
-module(do_traversable).

%%%_* Exports =================================================================
-define(API, [ sequence/1,
               sequence/3,
               traverse/2]).
-export(?API).
-ignore_xref(?API).

%%%_* Includes ================================================================
-include("do_internal.hrl").
-include("do_types.hrl").

%%%_* Callbacks ===============================================================
-callback traverse(fn(A, applicative(B)), traversable(A)) -> applicative(traversable(B)).
-callback sequence(traversable(applicative(A))) -> applicative(traversable(A)).

%%%_* Code ====================================================================
%%%_* API ---------------------------------------------------------------------
-spec sequence(traversable(applicative(A))) -> applicative(traversable(A)).
sequence(Traversable) -> ?Mod(Traversable):sequence(Traversable).

-spec sequence(traversable(applicative(A)), atom(), atom()) ->
  applicative(traversable(A)).
sequence(Traversable, T, A) ->
  T:foldr(fun(VBox, AccBox) ->
    Curried = fun(V) -> fun(Acc) -> T:append(T:pure(V), Acc) end end,
    A:liftA2(A:fmap(Curried, VBox), AccBox)
  end, A:pure(T:mempty()), Traversable).

-spec traverse(fn(A, applicative(B)), traversable(A)) ->
  applicative(traversable(B)).
traverse(F, Traversable) when ?isF1(F) -> do:fmap(F, Traversable).

%%%_* Tests ===================================================================
-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

traverse_test() ->
  F = fun(A) -> A + 1 end,
  ?assertEqual([1, 2, 3], traverse(F, [0, 1, 2])),
  ?assertEqual(#{a => 1, b => 2, c => 3}, traverse(F, #{a => 0, b => 1, c => 2})).

-endif.
