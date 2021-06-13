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
-define(API, [ bind/2,
               do/2,
               fmap/2,
               lift/1,
               liftA2/2,
               liftm/2,
               liftmz/2,
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
-spec fmap(fn(A, B), maybe(A)) -> maybe(B).
fmap(F, {just, A}) when ?isF1(F) -> {just, F(A)};
fmap(F, nothing)   when ?isF1(F) -> nothing.

%%%_* applicative -------------------------------------------------------------
-spec liftA2(maybe(fn(A, B)), maybe(A)) -> maybe(B).
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

-spec liftmz(fun(), [fn(maybe(A))]) -> maybe(A).
liftmz(F, Maybes) -> do_monad:liftmz(F, Maybes, ?MODULE).

-spec then(maybe(_), fn(maybe(A))) -> maybe(A).
then(Maybe, F) -> do_monad:then(Maybe, F, ?MODULE).

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

-endif.
