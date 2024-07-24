%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% @doc The Maybe Monad.
%%% @end
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%%_* Module declaration ======================================================
-module(do_maybe).

-behaviour(do_semigroup).
-behaviour(do_monoid).
-behaviour(do_foldable).
-behaviour(do_functor).
-behaviour(do_applicative).
-behaviour(do_traversable).
-behaviour(do_monad).

%%%_* Exports =================================================================
-define(API, [ % semigroup
               append/2,
               % monoid
               mempty/0,
               % foldable
               foldr/3,
               % functor
               fmap/2,
               % applicative
               liftA2/2,
               pure/1,
               % traversable
               sequence/1,
               traverse/2,
               % monad
               bind/2,
               do/2,
               lift/1,
               liftm/2,
               then/2,
               % maybe
               cat_maybes/1,
               from_just/1,
               from_maybe/2,
               is_just/1,
               is_nothing/1,
               list_to_maybe/1,
               map_maybes/2,
               'maybe'/3,
               maybe_to_list/1]).

-export(?API).
-ignore_xref(?API).

%%%_* Includes ================================================================
-include("do_internal.hrl").
-include("do_types.hrl").
-include("do.hrl").

%%%_* Code ====================================================================
%%%_* semigroup ---------------------------------------------------------------
-spec append('maybe'(A), 'maybe'(A)) -> 'maybe'(A).
append({just, A1}, {just, A2}) -> {just, do_semigroup:append(A1, A2)};
append(nothing, {just, A})     -> {just, A};
append({just, A}, nothing)     -> {just, A};
append(nothing, nothing)       -> nothing.

%%%_* monoid ------------------------------------------------------------------
-spec mempty() -> 'maybe'(_).
mempty() -> nothing.

%%%_* foldable ----------------------------------------------------------------
-spec foldr(fn(A, B, B), B, 'maybe'(A)) -> B.
foldr(_, Acc, nothing)   -> Acc;
foldr(F, Acc, {just, A}) -> F(A, Acc).

%%%_* functor -----------------------------------------------------------------
-spec fmap(fn(A, B), Maybe :: 'maybe'(A)) -> 'maybe'(B).
fmap(F, {just, A}) when ?isF1(F) -> {just, F(A)};
fmap(F, nothing)   when ?isF1(F) -> nothing.

%%%_* applicative -------------------------------------------------------------
-spec liftA2(Maybe :: 'maybe'(fn(A, B)), Maybe :: 'maybe'(A)) -> 'maybe'(B).
liftA2({just, F}, Maybe) when ?isF1(F) -> fmap(F, Maybe);
liftA2(nothing, _)                     -> nothing.

-spec pure(A) -> 'maybe'(A).
pure(A) -> {just, A}.

%%%_* traversable -------------------------------------------------------------
-spec sequence('maybe'(applicative(A))) -> applicative('maybe'(A)).
sequence({just, A} = Maybe) -> do_traversable:sequence(Maybe, ?MODULE, ?Mod(A));
sequence(nothing)           -> nothing.

-spec traverse(fn(A, applicative(B)), 'maybe'(A)) -> applicative('maybe'(B)).
traverse(F, Maybe) when ?isF1(F) -> fmap(F, Maybe).

%%%_* monad -------------------------------------------------------------------
-spec bind('maybe'(A), fn(A, 'maybe'(B))) -> 'maybe'(B).
bind(Maybe, F) when ?isF1(F) -> flat(fmap(F, Maybe)).

-spec do('maybe'(A), [fn(A, 'maybe'(B)) | fn('maybe'(B))]) -> 'maybe'(B).
do(Maybe, Fs) -> do_monad:do(Maybe, Fs).

-spec lift(fn(A, B)) -> fn(monad(A), monad(B)).
lift(F) -> do_monad:lift(F, ?MODULE).

-spec liftm(fun(), ['maybe'(A)]) -> 'maybe'(A).
liftm(F, Maybes) -> do_monad:liftm(F, Maybes).

-spec then('maybe'(_), fn('maybe'(A))) -> 'maybe'(A).
then(Maybe, F) -> do_monad:then(Maybe, F).

%%%_* maybe -------------------------------------------------------------------
-spec 'maybe'(B, fn(A, B), Maybe :: 'maybe'(A)) -> B.
'maybe'(B, F, nothing)   when ?isF1(F) -> B;
'maybe'(_, F, {just, A}) when ?isF1(F) -> F(A).

-spec is_just(Maybe :: 'maybe'(_)) -> boolean().
is_just({just, _}) -> true;
is_just(nothing)   -> false.

-spec is_nothing('maybe'(_)) -> boolean().
is_nothing(A) -> not is_just(A).

-spec from_just(Maybe :: 'maybe'(A)) -> A.
from_just({just, A}) -> A.

-spec from_maybe(A, Maybe :: 'maybe'(A)) -> A.
from_maybe(A, nothing)   -> A;
from_maybe(_, {just, V}) -> V.

-spec list_to_maybe([A]) -> 'maybe'(A).
list_to_maybe([A | _]) -> pure(A);
list_to_maybe([])      -> nothing.

-spec maybe_to_list(Maybe :: 'maybe'(A)) -> [A].
maybe_to_list({just, A}) -> [A];
maybe_to_list(nothing)   -> [].

-spec cat_maybes(['maybe'(A)]) -> [A].
cat_maybes(Maybes) when is_list(Maybes) ->
  lists:flatmap(fun({just, A}) -> [A];
                   (nothing)   -> [] end, Maybes).

-spec map_maybes(fn(A, 'maybe'(B)), [A]) -> [B].
map_maybes(F, List) when ?isF1(F), is_list(List) ->
  cat_maybes(do_list:fmap(F, List)).

%%%_* internal ----------------------------------------------------------------
flat({just, nothing})   -> nothing;
flat(nothing)           -> nothing;
flat({just, {just, A}}) -> {just, A}.

%%%_* Tests ===================================================================
-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

foldr_test() ->
  F = fun(A, B) -> A + B end,
  ?equals(2, foldr(F, 1, {just, 1})),
  ?equals(1, foldr(F, 1, nothing)).

append_test() ->
  ?equals(mempty(),     append(mempty(), mempty())),
  ?equals(pure(1),      append(pure(1), mempty())),
  ?equals(pure([1, 2]), append(pure([1]), pure([2]))),
  ?equals(pure(1),      append(mempty(), pure(1))).

traverse_test() ->
  F = fun(A) -> A + 1 end,
  ?equals({just, 2}, traverse(F, {just, 1})).

sequence_list_test() ->
  ?equals([{just, 1}], sequence({just, [1]})).

sequence_test() ->
  ?equals(nothing, sequence(nothing)).

pure_test() ->
  ?assertEqual({just, {just, 3}}, pure({just, 3})),
  ?assertEqual({just, 3},       pure(3)).

lift_test() ->
  F      = fun(A) -> A + 1 end,
  Lifted = lift(F),
  ?assertEqual({just, 2}, Lifted({just, 1})),
  ?assertEqual(nothing,   Lifted(nothing)).

liftA2_test() ->
  F = fun(A) -> A + 1 end,
  ?equals({just, 3}, liftA2({just, F}, {just, 2})),
  ?equals(nothing,   liftA2({just, F}, nothing)),
  ?equals(nothing,   liftA2(nothing,   {just, 2})),
  ?equals(nothing,   liftA2(nothing,   nothing)).

liftm_test() ->
  F = fun(A, B, C) -> A + B + C end,
  ?equals({just, 4}, liftm(F, [{just, 1}, {just, 2}, {just, 1}])),
  ?equals(nothing,   liftm(F, [{just, 1}, nothing,   {just, 1}])),
  ?equals(nothing,   liftm(F, [nothing,   {just, 2}, {just, 1}])),
  ?equals(nothing,   liftm(F, [nothing,   nothing,   {just, 1}])).

bind_test() ->
  Fjust    = fun(A) -> {just, A + 1} end,
  Fnothing = fun(_) -> nothing end,
  ?equals({just, 3}, bind({just, 2},Fjust)),
  ?equals(nothing,   bind(nothing, Fjust)),
  ?equals(nothing,   bind({just, 2}, Fnothing)),
  ?equals(nothing,   bind(nothing, Fnothing)).

do_test() ->
  Fun0 = fun() -> ?pure(5) end,
  Fun = fun(A) -> ?pure(A + 1) end,
  ?equals({just, 4},  do({just, 3}, [Fun])),
  ?equals({just, 6},  do({just, 3}, [Fun0, Fun])),
  ?equals(nothing,    do(nothing,   [Fun])).

maybe_test() ->
  F = fun(A) -> A + 1 end,
  ?assertEqual(1, 'maybe'(1, F, nothing)),
  ?assertEqual(2, 'maybe'(1, F, {just, 1})).

is_just_test() ->
  ?assertEqual(false, is_just(nothing)),
  ?assertEqual(true,  is_just({just, 1})).

is_nothing_test() ->
  ?assertEqual(true,  is_nothing(nothing)),
  ?assertEqual(false, is_nothing({just, 1})).

from_just_test() ->
  ?assertEqual(1,               from_just({just, 1})),
  ?assertError(function_clause, from_just(nothing)).

from_maybe_test() ->
  ?assertEqual(2, from_maybe(1, {just, 2})),
  ?assertEqual(1, from_maybe(1, nothing)).

list_to_maybe_test() ->
  ?assertEqual({just, 1}, list_to_maybe([1])),
  ?assertEqual({just, 1}, list_to_maybe([1, 2])),
  ?assertEqual(nothing,   list_to_maybe([])).

maybe_to_list_test() ->
  ?assertEqual([1], maybe_to_list({just, 1})),
  ?assertEqual([],  maybe_to_list(nothing)).

cat_maybes_test() ->
  ?assertEqual([1],    cat_maybes([{just, 1}])),
  ?assertEqual([],     cat_maybes([nothing])),
  ?assertEqual([],     cat_maybes([])),
  ?assertEqual([1, 2], cat_maybes([{just, 1}, nothing, {just, 2}])).

map_maybes_test() ->
  F = fun(N) when N < 5 -> {just, N};
         (_)            -> nothing end,
  ?assertEqual([],           map_maybes(F, [])),
  ?assertEqual([1, 2, 3, 4], map_maybes(F, [1, 2, 3, 4, 5, 6, 7])).

-endif.
