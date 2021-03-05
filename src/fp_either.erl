%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% @doc The Either Type.
%%% @end
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%%_* Module declaration ======================================================
-module(fp_either).

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
%%%_* API ---------------------------------------------------------------------
-spec fmap(fn(B, C), either(A, B)) -> either(A, C).
fmap(F, {error, A}) when ?isFn(F) -> {error, A};
fmap(F, {ok, B})    when ?isFn(F) -> {ok, F(B)}.

-spec pure(B) -> either(_, B).
pure(B) -> {ok, B}.

-spec liftA2(fn(B1, B2, C), either(A1, B1), either(A2, B2)) -> either(A1 | A2, C).
liftA2(F, E1, E2) when is_function(F, 2) ->
  case fmap(fp:curry(F), E1) of
    {ok, F2} -> fmap(F2, E2);
    Error    -> Error
  end.

-spec bind(fn(A, either(B, C)), either(D, A)) -> either(B | D, C).
bind(F, E) when ?isFn(F) ->
  unlift(fmap(F, E)).

%%%_* Internal ----------------------------------------------------------------
unlift({ok, {error, A}}) -> {error, A};
unlift({error, A})       -> {error, A};
unlift({ok, {ok, B}})    -> {ok, B}.

%%%_* Tests ===================================================================
-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

liftA2_test() ->
  F = fun(A, B) -> A + B end,
  ?assertEqual({ok, 3},      liftA2(F, {ok, 1},      {ok, 2})),
  ?assertEqual({error, rsn}, liftA2(F, {ok, 1},      {error, rsn})),
  ?assertEqual({error, rsn}, liftA2(F, {error, rsn}, {ok, 2})),
  ?assertEqual({error, rsn}, liftA2(F, {error, rsn}, {error, rsn})).

bind_test() ->
  FOk    = fun(A) -> {ok, A + 1} end,
  FError = fun(_) -> {error, reason} end,
  ?assertEqual({ok, 3},          bind(FOk, {ok, 2})),
  ?assertEqual({error, reason},  bind(FOk, {error, reason})),
  ?assertEqual({error, reason},  bind(FError, {ok, 2})),
  ?assertEqual({error, reason1}, bind(FError, {error, reason1})).

-endif.
