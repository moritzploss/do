%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% @doc
%%% @end
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%%_* Module declaration ======================================================
-module(do_fn).

-behaviour(do_functor).
-behaviour(do_applicative).

%%%_* Exports =================================================================
-export([fmap/2]).
-export([liftA2/3]).
-export([pure/1]).

%%%_* Includes ================================================================
-include("do_macros.hrl").
-include("do_types.hrl").

%%%_* Code ====================================================================
-spec fmap(fn(B, C), fn(A, B)) -> fn(A, C).
fmap(F, Fun) when ?isF1(F), ?isF1(Fun) -> fun(Arg) -> F(Fun(Arg)) end.

-spec liftA2(fn(A, B, C), fn(D, A), fn(E, B)) -> fn(D, fn(E, C)).
liftA2(F1, F2, F3) ->
  fun(D) -> fun(E) -> F1(F2(D), F3(E)) end end.

-spec pure(A) -> fn(_, A).
pure(A) -> fun(_) -> A end.

%%%_* Tests ===================================================================
-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

pure_test() ->
  ?assertEqual(2, (pure(2))(foo)).

liftA2_test() ->
  F = fun(A, B) -> A + B end,
  G = fun(D) -> D end,
  H = fun(E) -> E end,
  I = liftA2(F, G, H),
  ?assertEqual(3, (I(1))(2)).

-endif.
