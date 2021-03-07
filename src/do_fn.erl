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
-export([sequence/1]).

%%%_* Includes ================================================================
-include("include/do_macros.hrl").
-include("include/do_types.hrl").

%%%_* Code ====================================================================
-spec fmap(fn(B, C), fn(A, B)) -> fn(A, C).
fmap(F, Fun) when ?isF1(F), ?isF1(Fun) -> fun(Arg) -> F(Fun(Arg)) end.

-spec liftA2(fn(A, B, C), fn(D, A), fn(E, B)) -> fn(D, fn(E, C)).
liftA2(F1, F2, F3) ->
  fun(D) -> fun(E) -> F1(F2(D), F3(E)) end end.

-spec pure(A) -> fn(_, A).
pure(A) -> fun(_) -> A end.

-spec sequence(iterable(fn(A, B))) -> fn(A, iterable(B)).
sequence(Iterable) when is_list(Iterable) or is_map(Iterable) ->
  fun(A) -> do_functor:fmap(fun(F) -> F(A) end, Iterable) end.

%%%_* Tests ===================================================================
-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

sequence_test() ->
  F1            = fun(X) -> X + 1 end,
  F2            = fun(X) -> X + 2 end,
  F3            = fun(X) -> X + 3 end,
  SequencedList = sequence([F1, F2, F3]),
  SequencedMap  = sequence(#{a => F1, b => F2, c => F3}),
  ?assertEqual([2, 3, 4], SequencedList(1)),
  ?assertEqual(#{a => 2, b => 3, c => 4}, SequencedMap(1)).

-endif.
