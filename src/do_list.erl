%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% @doc The List Monad.
%%% @end
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%%_* Module declaration ======================================================
-module(do_list).

-behaviour(do_functor).
-behaviour(do_applicative).
-behaviour(do_monad).

%%%_* Exports =================================================================
-define(API, [ bind/2,
               do/2,
               fmap/2,
               lift/1,
               liftA2/2,
               liftm/2,
               pure/1,
               sequence/1,
               then/2]).
-export(?API).
-ignore_xref(?API).

%%%_* Includes ================================================================
-include("do_macros.hrl").
-include("do_types.hrl").
-include("do.hrl").

%%%_* Code ====================================================================
%%%_* functor -----------------------------------------------------------------
-spec fmap(fn(A, B), [A]) -> [B].
fmap(F, List) when ?isF1(F) -> lists:map(F, List).

%%%_* applicative -------------------------------------------------------------
-spec liftA2([fn(A, B)], [A]) -> [B].
liftA2(List1, List2) -> flat(fmap(fun(F) -> fmap(F, List2) end, List1)).

-spec pure(A) -> [A].
pure(A) -> [A].

-spec sequence(traversable([A])) -> [traversable(A)].
sequence(Lists) -> do_traversable:sequence(Lists, ?MODULE).

%%%_* monad -------------------------------------------------------------------
-spec bind([A], fn(A, [B])) -> [B].
bind(List, F) when ?isF1(F) -> flat(fmap(F, List)).

-spec do([A], [fn(A, [B]) | fn([B])]) -> [B].
do(List, Fs) -> do_monad:do(List, Fs, ?MODULE).

-spec lift(fn(A, B)) -> fn(monad(A), monad(B)).
lift(F) -> do_monad:lift(F, ?MODULE).

-spec liftm(fun(), [list()] | [fn(list())]) -> list().
liftm(F, Lists) -> do_monad:liftm(F, Lists, ?MODULE).

-spec then(list(), fn([A])) -> [A].
then(List, F) -> do_monad:then(List, F, ?MODULE).

%%%_* internal ----------------------------------------------------------------
flat(List) -> lists:concat(List).

%%%_* Tests ===================================================================
-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

pure_test() ->
  ?assertEqual([1], pure(1)).

lift_test() ->
  F      = fun(A) -> A + 1 end,
  Lifted = lift(F),
  ?assertEqual([2, 3, 4], Lifted([1, 2, 3])).

liftA2_test() ->
  Applicative = fmap(fun(E1) -> fun(E2) -> E1 + E2 end end, [-1, 0, 1]),
  ?assertEqual([0, 1, 2, 1, 2, 3, 2, 3, 4], liftA2(Applicative, [1, 2, 3])),
  ?assertEqual([],                          liftA2(Applicative, [])),
  ?assertEqual([],                          liftA2([],          [1, 2, 3])).

sequence_list_test() ->
  ?assertEqual([],                                               sequence([[]])),
  ?assertEqual([[1]],                                            sequence([[1]])),
  ?assertEqual([[1, 2, 3]],                                      sequence([[1], [2], [3]])),
  ?assertEqual([],                                               sequence([[1, 2], []])),
  ?assertEqual([],                                               sequence([[], [1, 2]])),
  ?assertEqual([[1, 3], [1, 4], [1, 5], [2, 3], [2, 4], [2, 5]], sequence([[1, 2], [3, 4, 5]])).

sequence_map_test() ->
  ?assertEqual([#{a => 1, b => 2}],                    sequence(#{a => [1], b => [2]})),
  ?assertEqual([#{a => 1, b => 2}, #{a => 1, b => 3}], sequence(#{a => [1], b => [2, 3]})).

do_test() ->
  Fun0 = fun() -> ?pure(3) end,
  Fun  = fun(A) -> ?pure(A + 1) end,
  ?assertEqual([3], do([3],  [])),
  ?assertEqual([4], do([3],  [Fun])),
  ?assertEqual([4], do([3],  [Fun0, Fun])),
  ?assertEqual([],  do([],   [Fun])).

-endif.
