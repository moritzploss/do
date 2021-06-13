%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% @doc The Maybe Monad.
%%% @end
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%%_* Module declaration ======================================================
-module(do_maybe).

-behaviour(do_functor).
-behaviour(do_applicative).
-behaviour(do_monad).

%%%_* Exports =================================================================
-define(API, [ % functor
               fmap/2,
               % applicative
               liftA2/2,
               pure/1,
               sequence/1,
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
               liftmz/2,
               map_maybes/2,
               maybe/3,
               maybe_to_list/1]).

-export(?API).
-ignore_xref(?API).

%%%_* Includes ================================================================
-include("do_guards.hrl").
-include("do_types.hrl").
-include("do.hrl").

%%%_* Code ====================================================================
%%%_* functor -----------------------------------------------------------------
-spec fmap(fn(A, B), Maybe :: maybe(A)) -> maybe(B).
fmap(F, {just, A}) when ?isF1(F) -> {just, F(A)};
fmap(F, nothing)   when ?isF1(F) -> nothing.

%%%_* applicative -------------------------------------------------------------
-spec liftA2(Maybe :: maybe(fn(A, B)), Maybe :: maybe(A)) -> maybe(B).
liftA2({just, F}, Maybe) when ?isF1(F) -> fmap(F, Maybe);
liftA2(nothing, _)                     -> nothing.

-spec pure(A) -> maybe(A).
pure(A) -> {just, A}.

-spec sequence(traversable(maybe(A))) -> maybe(traversable(A)).
sequence(Maybes) -> do_traversable:sequence(Maybes, ?MODULE).

%%%_* monad -------------------------------------------------------------------
-spec bind(maybe(A), fn(A, maybe(B))) -> maybe(B).
bind(Maybe, F) when ?isF1(F) -> flat(fmap(F, Maybe)).

-spec do(maybe(A), [fn(A, maybe(B)) | fn(maybe(B))]) -> maybe(B).
do(Maybe, Fs) -> do_monad:do(Maybe, Fs, ?MODULE).

-spec lift(fn(A, B)) -> fn(monad(A), monad(B)).
lift(F) -> do_monad:lift(F, ?MODULE).

-spec liftm(fun(), [maybe(A)]) -> maybe(A).
liftm(F, Maybes) -> do_monad:liftm(F, Maybes, ?MODULE).

-spec then(maybe(_), fn(maybe(A))) -> maybe(A).
then(Maybe, F) -> do_monad:then(Maybe, F, ?MODULE).

%%%_* maybe -------------------------------------------------------------------
-spec liftmz(fun(), [fn(maybe(A))]) -> maybe(A).
liftmz(F, Maybes) -> do_monad:liftmz(F, Maybes, ?MODULE).

-spec maybe(B, fn(A, B), Maybe :: maybe(A)) -> B.
maybe(B, F, nothing)   when ?isF1(F) -> B;
maybe(_, F, {just, A}) when ?isF1(F) -> F(A).

-spec is_just(Maybe :: maybe(_)) -> boolean().
is_just({just, _}) -> true;
is_just(nothing)   -> false.

-spec is_nothing(maybe(_)) -> boolean().
is_nothing(A) -> not is_just(A).

-spec from_just(Maybe :: maybe(A)) -> A.
from_just({just, A}) -> A.

-spec from_maybe(A, Maybe :: maybe(A)) -> A.
from_maybe(A, nothing)   -> A;
from_maybe(_, {just, V}) -> V.

-spec list_to_maybe([A]) -> maybe(A).
list_to_maybe([A | _]) -> pure(A);
list_to_maybe([])      -> nothing.

-spec maybe_to_list(Maybe :: maybe(A)) -> [A].
maybe_to_list({just, A}) -> [A];
maybe_to_list(nothing)   -> [].

-spec cat_maybes([maybe(A)]) -> [A].
cat_maybes(Maybes) when is_list(Maybes) ->
  lists:flatmap(fun({just, A}) -> [A];
                   (nothing)   -> [] end, Maybes).

-spec map_maybes(fn(A, maybe(B)), [A]) -> [B].
map_maybes(F, List) when ?isF1(F), is_list(List) ->
  cat_maybes(do_list:fmap(F, List)).

%%%_* internal ----------------------------------------------------------------
flat({just, nothing})   -> nothing;
flat(nothing)           -> nothing;
flat({just, {just, A}}) -> {just, A}.

%%%_* Tests ===================================================================
-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

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
  ?assertEqual({just, 3}, liftA2({just, F}, {just, 2})),
  ?assertEqual(nothing,   liftA2({just, F}, nothing)),
  ?assertEqual(nothing,   liftA2(nothing,   {just, 2})),
  ?assertEqual(nothing,   liftA2(nothing,   nothing)).

liftm_test() ->
  F = fun(A, B, C) -> A + B + C end,
  ?assertEqual({just, 4}, liftm(F, [{just, 1}, {just, 2}, {just, 1}])),
  ?assertEqual(nothing,   liftm(F, [{just, 1}, nothing,   {just, 1}])),
  ?assertEqual(nothing,   liftm(F, [nothing,   {just, 2}, {just, 1}])),
  ?assertEqual(nothing,   liftm(F, [nothing,   nothing,   {just, 1}])).

liftmz_test() ->
  F = fun(A, B, C) -> A + B + C end,
  ?assertEqual({just, 4},       liftmz(F, [?thunk({just, 1}), ?thunk({just, 2}), ?thunk({just, 1})])),
  ?assertError(function_clause, liftmz(F, [{just, 1}, ?thunk({just, 1})])).

bind_test() ->
  Fjust    = fun(A) -> {just, A + 1} end,
  Fnothing = fun(_) -> nothing end,
  ?assertEqual({just, 3}, bind({just, 2},Fjust)),
  ?assertEqual(nothing,   bind(nothing, Fjust)),
  ?assertEqual(nothing,   bind({just, 2}, Fnothing)),
  ?assertEqual(nothing,   bind(nothing, Fnothing)).

sequence_test() ->
  ?assertEqual({just, [1, 2, 3]},         sequence([{just, 1}, {just, 2}, {just, 3}])),
  ?assertEqual(nothing,                   sequence([{just, 1}, nothing, {just, 3}])),
  ?assertEqual({just, #{a => 1, b => 2}}, sequence(#{a => {just, 1}, b => {just, 2}})),
  ?assertEqual(nothing,                   sequence(#{a => {just, 1}, b => nothing})).

do_test() ->
  Fun0 = fun() -> ?pure(5) end,
  Fun = fun(A) -> ?pure(A + 1) end,
  ?assertEqual({just, 4},  do({just, 3}, [Fun])),
  ?assertEqual({just, 6},  do({just, 3}, [Fun0, Fun])),
  ?assertEqual(nothing,    do(nothing,   [Fun])).

maybe_test() ->
  F = fun(A) -> A + 1 end,
  ?assertEqual(1, maybe(1, F, nothing)),
  ?assertEqual(2, maybe(1, F, {just, 1})).

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
