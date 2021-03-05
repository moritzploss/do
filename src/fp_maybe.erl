%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% @doc The Maybe Type.
%%% @end
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%%_* Module declaration ======================================================
-module(fp_maybe).

-behaviour(fp_functor).
-behaviour(fp_applicative).
-behaviour(fp_monad).

%%%_* Exports =================================================================
-export([fmap/2]).
-export([pure/1]).
-export([liftA2/3]).
-export([bind/2]).

%%%_* Includes ================================================================
-include("include/fp_macros.hrl").
-include("include/fp_types.hrl").

%%%_* Code ====================================================================
-spec fmap(fn(A, B), maybe(A)) -> maybe(B).
fmap(F, {ok, A}) when ?isFn(F) -> {ok, F(A)};
fmap(F, error)   when ?isFn(F) -> error.

-spec pure(A) -> maybe(A).
pure(A) -> {ok, A}.

-spec liftA2(fn(A, B, C), maybe(A), maybe(B)) -> maybe(C).
liftA2(F, M1, M2) when is_function(F, 2) ->
  case fmap(fp:curry(F), M1) of
    {ok, F2} -> fmap(F2, M2);
    Error    -> Error
  end.

-spec bind(fn(A, maybe(B)), maybe(A)) -> maybe(B).
bind(F, E) when ?isFn(F) ->
  unlift(fmap(F, E)).

%%%_* Internal ----------------------------------------------------------------
unlift({ok, error})   -> error;
unlift(error)         -> error;
unlift({ok, {ok, A}}) -> {ok, A}.

%%%_* Tests ===================================================================
-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

liftA2_test() ->
  F = fun(A, B) -> A + B end,
  ?assertEqual({ok, 3}, liftA2(F, {ok, 1}, {ok, 2})),
  ?assertEqual(error,   liftA2(F, {ok, 1}, error)),
  ?assertEqual(error,   liftA2(F, error,   {ok, 2})),
  ?assertEqual(error,   liftA2(F, error,   error)).

bind_test() ->
  FOk    = fun(A) -> {ok, A + 1} end,
  FError = fun(_) -> error end,
  ?assertEqual({ok, 3}, bind(FOk, {ok, 2})),
  ?assertEqual(error,   bind(FOk, error)),
  ?assertEqual(error,   bind(FError, {ok, 2})),
  ?assertEqual(error,   bind(FError, error)).

-endif.
