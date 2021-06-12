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
fmap(F, {ok, A}) when ?isF1(F) -> {ok, F(A)};
fmap(F, error)   when ?isF1(F) -> error.

%%%_* applicative -------------------------------------------------------------
-spec liftA2(maybe(fn(A, B)), maybe(A)) -> maybe(B).
liftA2({ok, F}, Maybe) when ?isF1(F) -> fmap(F, Maybe);
liftA2(error, _)                     -> error.

-spec pure(A) -> maybe(A).
pure(A) -> {ok, A}.

-spec sequence(traversable(maybe(A))) -> maybe(traversable(A)).
sequence(Maybes) -> do_traversable:sequence(Maybes, ?MODULE).

%%%_* monad -------------------------------------------------------------------
-spec bind(fn(A, maybe(B)), maybe(A)) -> maybe(B).
bind(F, Maybe) when ?isF1(F) -> flat(fmap(F, Maybe)).

-spec do(maybe(A), [fn(A, maybe(B)) | fn(maybe(B))]) -> maybe(B).
do(Maybe, Fs) -> do_monad:do(Maybe, Fs, [?MODULE]).

-spec lift(fn(A, B)) -> fn(monad(A), monad(B)).
lift(F) -> do_monad:lift(F, ?MODULE).

-spec liftm(fun(), [maybe(A)]) -> maybe(A).
liftm(F, Maybes) -> do_monad:liftm(F, Maybes, ?MODULE).

-spec liftmz(fun(), [fn(maybe(A))]) -> maybe(A).
liftmz(F, Maybes) -> do_monad:liftmz(F, Maybes, ?MODULE).

-spec then(fn(maybe(A)), maybe(_)) -> maybe(A).
then(F, Maybe) -> do_monad:then(F, Maybe, ?MODULE).

%%%_* internal ----------------------------------------------------------------
flat({ok, error})   -> error;
flat(error)         -> error;
flat({ok, {ok, A}}) -> {ok, A}.

%%%_* Tests ===================================================================
-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

pure_test() ->
  ?assertEqual({ok, {ok, 3}}, pure({ok, 3})),
  ?assertEqual({ok, 3},       pure(3)).

lift_test() ->
  F      = fun(A) -> A + 1 end,
  Lifted = lift(F),
  ?assertEqual({ok, 2}, Lifted({ok, 1})),
  ?assertEqual(error,   Lifted(error)).

liftA2_test() ->
  F = fun(A) -> A + 1 end,
  ?assertEqual({ok, 3}, liftA2({ok, F}, {ok, 2})),
  ?assertEqual(error,   liftA2({ok, F}, error)),
  ?assertEqual(error,   liftA2(error,   {ok, 2})),
  ?assertEqual(error,   liftA2(error,   error)).

liftm_test() ->
  F = fun(A, B, C) -> A + B + C end,
  ?assertEqual({ok, 4}, liftm(F, [{ok, 1}, {ok, 2}, {ok, 1}])),
  ?assertEqual(error,   liftm(F, [{ok, 1}, error,   {ok, 1}])),
  ?assertEqual(error,   liftm(F, [error,   {ok, 2}, {ok, 1}])),
  ?assertEqual(error,   liftm(F, [error,   error,   {ok, 1}])).

liftmz_test() ->
  F = fun(A, B, C) -> A + B + C end,
  ?assertEqual({ok, 4},         liftmz(F, [?thunk({ok, 1}), ?thunk({ok, 2}), ?thunk({ok, 1})])),
  ?assertError(function_clause, liftmz(F, [{ok, 1}, ?thunk({ok, 1})])).

bind_test() ->
  FOk    = fun(A) -> {ok, A + 1} end,
  FError = fun(_) -> error end,
  ?assertEqual({ok, 3}, bind(FOk, {ok, 2})),
  ?assertEqual(error,   bind(FOk, error)),
  ?assertEqual(error,   bind(FError, {ok, 2})),
  ?assertEqual(error,   bind(FError, error)).

sequence_test() ->
  ?assertEqual({ok, [1, 2, 3]},         sequence([{ok, 1}, {ok, 2}, {ok, 3}])),
  ?assertEqual(error,                   sequence([{ok, 1}, error, {ok, 3}])),
  ?assertEqual({ok, #{a => 1, b => 2}}, sequence(#{a => {ok, 1}, b => {ok, 2}})),
  ?assertEqual(error,                   sequence(#{a => {ok, 1}, b => error})).

do_test() ->
  Fun0 = fun() -> ?pure(5) end,
  Fun = fun(A) -> ?pure(A + 1) end,
  ?assertEqual({ok, 4},  do({ok, 3}, [Fun])),
  ?assertEqual({ok, 6},  do({ok, 3}, [Fun0, Fun])),
  ?assertEqual(error,    do(error,   [Fun])).

-endif.
