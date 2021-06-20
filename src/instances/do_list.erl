%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% @doc The List Monad.
%%% @end
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%%_* Module declaration ======================================================
-module(do_list).

-behaviour(do_semigroup).
-behaviour(do_monoid).
-behaviour(do_foldable).
-behaviour(do_functor).
-behaviour(do_applicative).
-behaviour(do_traversable).
-behaviour(do_monad).

%%%_* Exports =================================================================
-define(API, [ append/2,
               mempty/0,
               foldr/3,
               bind/2,
               do/2,
               fmap/2,
               lift/1,
               liftA2/2,
               liftm/2,
               pure/1,
               sequence/1,
               sequencez/1,
               traverse/2,
               then/2]).
-export(?API).
-ignore_xref(?API).

%%%_* Macros ==================================================================
-define(LAZY, lazy).
-define(DEFAULT, default).

%%%_* Includes ================================================================
-include("do_internal.hrl").
-include("do_types.hrl").
-include("do.hrl").

%%%_* Code ====================================================================
%%%_* semigroup ---------------------------------------------------------------
-spec append([A], [A]) -> [A].
append(List1, List2) -> List1 ++ List2.

%%%_* monoid ------------------------------------------------------------------
-spec mempty() -> list().
mempty() -> [].

%%%_* foldable ----------------------------------------------------------------
-spec foldr(fn(A, B, B), B, [A]) -> B.
foldr(F, Acc, List) when ?isF2(F), is_list(List) -> lists:foldr(F, Acc, List).

%%%_* functor -----------------------------------------------------------------
-spec fmap(fn(A, B), [A]) -> [B].
fmap(F, List) when ?isF1(F) -> lists:map(F, List).

%%%_* applicative -------------------------------------------------------------
-spec liftA2([fn(A, B)], [A]) -> [B].
liftA2(List1, List2) -> flat(fmap(fun(F) -> fmap(F, List2) end, List1)).

-spec pure(A) -> [A].
pure(A) -> [A].

%%%_* traversable -------------------------------------------------------------
-spec sequence([(applicative(A))]) -> applicative([A]).
sequence(List) when is_list(List) -> sequence(List, ?DEFAULT).

-spec sequencez([fn((applicative(A)))]) -> applicative([A]).
sequencez([F | _] = List) when ?isF0(F) -> sequence(List, ?LAZY).

-spec traverse(fn(A, applicative(B)), [A]) -> applicative([B]).
traverse(F, List) when ?isF1(F) -> fmap(F, List).

%%%_* monad -------------------------------------------------------------------
-spec bind([A], fn(A, [B])) -> [B].
bind(List, F) when ?isF1(F) -> flat(fmap(F, List)).

-spec do([A], [fn(A, [B]) | fn([B])]) -> [B].
do(List, Fs) -> do_monad:do(List, Fs).

-spec lift(fn(A, B)) -> fn(monad(A), monad(B)).
lift(F) -> do_monad:lift(F, ?MODULE).

-spec liftm(fun(), [list()] | [fn(list())]) -> list().
liftm(F, Lists) -> do_monad:liftm(F, Lists).

-spec then(list(), fn([A])) -> [A].
then(List, F) -> do_monad:then(List, F).

%%%_* internal ----------------------------------------------------------------
flat(List) -> lists:concat(List).
 
sequence([Head | Rest], Mode) ->
  Elm         = get_elm(Head, Mode),
  Mod         = ?Mod(Elm),
  Applicative = Mod:fmap(fun(Val) -> fun(Vals) -> [Val | Vals] end end, Elm),
  case Rest of
    [] -> Mod:liftA2(Applicative, Mod:pure(Rest));
    _  -> Mod:liftA2(Applicative, sequence(Rest, Mode))
  end;
sequence([], _) -> [].

get_elm(F, ?LAZY)      -> F();
get_elm(Elm, ?DEFAULT) -> Elm.

%%%_* Tests ===================================================================
-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

append_test() ->
  ?equals(mempty(), append(mempty(), mempty())),
  ?equals(pure(1),  append(pure(1), mempty())),
  ?equals([1, 2],   append(pure(1), pure(2))),
  ?equals(pure(1),  append(mempty(), pure(1))).

foldr_test() ->
  F = fun(A, B) -> A + B end,
  ?equals(6, foldr(F, 0, [1, 2, 3])),
  ?equals(0, foldr(F, 0, [])).

pure_test() ->
  ?assertEqual([1], pure(1)).

lift_test() ->
  F      = fun(A) -> A + 1 end,
  Lifted = lift(F),
  ?assertEqual([2, 3, 4], Lifted([1, 2, 3])).

liftA2_test() ->
  Applicative = fmap(fun(E1) -> fun(E2) -> E1 + E2 end end, [-1, 0, 1]),
  ?equals([0, 1, 2, 1, 2, 3, 2, 3, 4], liftA2(Applicative, [1, 2, 3])),
  ?equals([],                          liftA2(Applicative, [])),
  ?equals([],                          liftA2([],          [1, 2, 3])).

traverse_test() ->
  F = fun(A) -> A + 1 end,
  ?equals([2, 3, 4], traverse(F, [1, 2, 3])).

sequence_list_test() ->
  ?equals([],                                               sequence([])),
  ?equals([],                                               sequence([[]])),
  ?equals([[1]],                                            sequence([[1]])),
  ?equals([[1, 2, 3]],                                      sequence([[1], [2], [3]])),
  ?equals([],                                               sequence([[1, 2], []])),
  ?equals([],                                               sequence([[], [1, 2]])),
  ?equals([[1, 3], [1, 4], [1, 5], [2, 3], [2, 4], [2, 5]], sequence([[1, 2], [3, 4, 5]])).

sequence_maybe_test() ->
  ?equals({just, [1, 2, 3]}, sequence([{just, 1}, {just, 2}, {just, 3}])),
  ?equals(nothing,           sequence([{just, 1}, nothing, {just, 3}])).

sequence_either_test() ->
  ?equals({ok, [1, 2, 3]}, sequence([{ok, 1}, {ok, 2}, {ok, 3}])),
  ?equals({error, reason}, sequence([{ok, 1}, {error, reason}, {ok, 3}])).

do_test() ->
  Fun0 = fun() -> ?pure(3) end,
  Fun  = fun(A) -> ?pure(A + 1) end,
  ?equals([3], do([3],  [])),
  ?equals([4], do([3],  [Fun])),
  ?equals([4], do([3],  [Fun0, Fun])),
  ?equals([],  do([],   [Fun])).

-endif.
