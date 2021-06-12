%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% @doc The Do Module.
%%% @end
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%%_* Module declaration ======================================================
-module(do).

%%%_* Exports =================================================================
-define(API, [ fmap/2,
               liftA2/2,
               pure/1,
               do/2,
               bind/2,
               then/2]).
-export(?API).
-ignore_xref(?API).

%%%_* Includes ================================================================
-include("do_types.hrl").
-include("do_macros.hrl").
-include("do.hrl").

%%%_* Macros ==================================================================
-define(MONADS, [do_either, do_list, do_maybe]).
-define(TRACE,  element(2, process_info(self(), current_stacktrace))).
-define(FMAP,   {_, _, 2, _}).
-define(TRY_,   {do_monad, try_, 4, _}).
-define(DO,     {do_monad, do, 3, _}).

%%%_* Code ====================================================================
%%%_* API ---------------------------------------------------------------------
-spec fmap(fn(A, B), functor(A)) -> functor(B).
fmap(F, List)           when ?isF1(F), is_list(List) -> do_list:fmap(F, List);
fmap(F, Map)            when ?isF1(F), is_map(Map)   -> do_map:fmap(F, Map);
fmap(F, Fn)             when ?isF1(F), ?isF1(Fn)     -> do_fn:fmap(F, Fn);
fmap(F, {error, _} = E) when ?isF1(F)                -> do_either:fmap(F, E);
fmap(F, {ok, _} = E)    when ?isF1(F)                -> do_either:fmap(F, E);
fmap(F, error = M)      when ?isF1(F)                -> do_maybe:fmap(F, M).

-spec liftA2(applicative(fn(A, B)), applicative(A)) -> applicative(B).
liftA2(A1, A2) when is_list(A1), is_list(A2) -> do_list:liftA2(A1, A2);
liftA2(error = A1, _ = A2)                   -> do_maybe:liftA2(A1, A2);
liftA2(_ = A1, error = A2)                   -> do_maybe:liftA2(A1, A2);
liftA2(A1, A2)                               -> do_either:liftA2(A1, A2).

-spec pure(A) -> monad(A) | A.
pure(A) ->
  case ?TRACE of
    [_, ?FMAP, {Monad, bind, 2, _}, ?TRY_, ?DO | _] -> Monad:pure(A);
    _Trace                                          -> A
  end.

-spec do(monad(A), [fn(A, monad(B)) | fn(monad(B))]) -> monad(B).
do(Monad, Funs) -> do_monad:do(Monad, Funs, ?MONADS).

-spec bind(fn(A, monad(B)), monad(A)) -> monad(B).
bind(F, Monad) when ?isF1(F) -> do(Monad, [F]).

-spec then(fn(monad(B)), monad(_)) -> monad(B).
then(F, Monad) when ?isF0(F) -> do(Monad, [F]).

%%%_* Tests ===================================================================
-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

fmap_macro_test() ->
  F = fun(A) -> A + 1 end,
  ?assertEqual({ok, 2}, ?fmap(F, {ok, 1})).

lift_macro_test() ->
  F      = fun(A) -> A + 1 end,
  Lifted = ?lift(F),
  ?assertEqual({ok, 2},      Lifted({ok, 1})),
  ?assertEqual({error, rsn}, Lifted({error, rsn})).

sequence_macro_test() ->
  ?assertEqual({ok, [1, 2]}, ?sequence([{ok, 1}, {ok, 2}])).

pure_test() ->
  F = fun(A) -> ?pure(A + 1) end,
  ?assertEqual(1,       ?pure(1)),
  ?assertEqual({ok, 2}, ?bind(F, {ok, 1})),
  ?assertEqual({ok, 2}, ?bind(F, {ok, 1})).

pure_nested_do_test() ->
  F     = fun(N) -> ?pure(N + 1) end,
  Outer = ?do(do_either:pure(1), [
              fun(M) ->
                Inner = ?do(do_list:pure(M), [F, F]),
                ?assertEqual([3], Inner),
                F(hd(Inner))
              end]),
  ?assertEqual({ok, 4}, Outer).

bind_macro_test() ->
  F = fun(A) -> {ok, A + 1} end,
  ?assertEqual({ok, 3},          ?bind(F, {ok, 2})),
  ?assertEqual({error, reason},  ?bind(F, {error, reason})).

then_macro_test() ->
  F = fun() -> {ok, 1} end,
  ?assertEqual({ok, 1},          ?then(F, {ok, 2})),
  ?assertEqual({error, reason},  ?then(F, {error, reason})).

do_macro_test() ->
  Fun0 = fun() -> ?pure(3) end,
  Fun = fun(A) -> ?pure(A + 1) end,
  ?assertEqual({ok, 4},                  ?do({ok, 3},         [Fun0, Fun])),
  ?assertEqual({error, reason},          ?do({error, reason}, [Fun])),
  ?assertEqual(error,                    ?do(error,           [Fun])),
  ?assertError({error, {no_monad, foo}}, ?do(foo,             [Fun])),
  ?assertEqual([2],                      ?do([1],             [Fun])).

liftm_macro_test() ->
  F = fun(A, B, C) -> A + B + C end,
  ?assertEqual({ok, 6},         ?liftm(F, {ok, 1}, {ok, 2}, {ok, 3})),
  ?assertError(function_clause, ?liftm(F, {ok, 1}, {ok, 2}, ?thunk({ok, 3}))),
  ?assertEqual({ok, 6},         ?liftm(F, {ok, 1}, {ok, 2}, {ok, 3})),
  ?assertEqual({error, 3},      ?liftm(F, {ok, 1}, {ok, 2}, {error, 3})).

liftA2_macro_test() ->
  F = fun(A) -> A + 1 end,
  ?assertEqual({ok, 3},    ?liftA2({ok, F}, {ok, 2})),
  ?assertEqual({error, 3}, ?liftA2({ok, F}, {error, 3})),
  ?assertEqual({error, 1}, ?liftA2({error, 1}, {ok, 3})),
  ?assertEqual(error,      ?liftA2(error, {ok, 1})),
  ?assertEqual(error,      ?liftA2({ok, F}, error)),
  ?assertEqual([2, 3],     ?liftA2([F], [1, 2])).

-endif.
