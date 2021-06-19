%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% @doc The Do Module.
%%% @end
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%%_* Module declaration ======================================================
-module(do).

%%%_* Exports =================================================================
-define(API, [ fmap/2,
               liftA2/2,
               sequence/1,
               traverse/2,
               pure/1,
               do/2,
               bind/2,
               then/2,
               mod/1]).
-export(?API).
-ignore_xref(?API).

%%%_* Includes ================================================================
-include("do_types.hrl").
-include("do_internal.hrl").
-include("do.hrl").

%%%_* Macros ==================================================================
-define(TRACE, element(2, process_info(self(), current_stacktrace))).
-define(FMAP,  {_, _, 2, _}).
-define(DO,    {do_monad, do, 3, _}).

%%%_* Code ====================================================================
%%%_* API ---------------------------------------------------------------------
-spec fmap(fn(A, B), functor(A)) -> functor(B).
fmap(F, Functor) -> do_functor:fmap(F, Functor).

-spec liftA2(applicative(fn(A, B)), applicative(A)) -> applicative(B).
liftA2(A1, A2) -> do_applicative:liftA2(A1, A2).

-spec sequence(traversable(applicative(A))) -> applicative(traversable(A)).
sequence(Traversable) -> do_traversable:sequence(Traversable).

-spec traverse(fn(A, applicative(B)), traversable(A)) ->
  applicative(traversable(B)).
traverse(F, Traversable) -> do_traversable:traverse(F, Traversable).

-spec do(monad(A), [fn(A, monad(B)) | fn(monad(B))]) -> monad(B).
do(Monad, Fs) -> do_monad:do(Monad, Fs).

-spec bind(monad(A), fn(A, monad(B))) -> monad(B).
bind(Monad, F) when ?isF1(F) -> do(Monad, [F]).

-spec then(monad(_), fn(monad(B))) -> monad(B).
then(Monad, F) when ?isF0(F) -> do(Monad, [F]).

-spec mod(Type :: term()) -> atom().
mod(Type) when is_list(Type) -> do_list;
mod(Type) when is_map(Type)  -> do_map;
mod(Type) when ?isF1(Type)   -> do_fn;
mod({error, _})              -> do_either;
mod({ok, _})                 -> do_either;
mod(nothing)                 -> do_maybe;
mod({just, _})               -> do_maybe.

-spec pure(A) -> monad(A) | A.
pure(A) ->
  case ?TRACE of
    [_, ?FMAP, {Monad, bind, 2, _}, ?DO | _] -> Monad:pure(A);
    _Trace                                   -> A
  end.

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
  ?assertEqual({ok, 2}, ?bind({ok, 1}, F)),
  ?assertEqual({ok, 2}, ?bind({ok, 1}, F)).

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
  ?assertEqual({ok, 3},          ?bind({ok, 2}, F)),
  ?assertEqual({error, reason},  ?bind({error, reason}, F)).

then_macro_test() ->
  ?assertEqual({ok, 1},          ?then({ok, 2}, {ok, 1})),
  ?assertEqual({ok, 1},          ?then({ok, 2}, {ok, 1})),
  ?assertEqual({error, reason},  ?then({error, reason}, {ok, 1})).

do_macro_test() ->
  Fun0 = fun()  -> ?pure(3) end,
  Fun1 = fun(A) -> ?pure(A + 1) end,
  Fun2 = fun(_) -> nothing end,
  ?assertEqual({ok, 4},         ?do({ok, 3},         [Fun0, Fun1])),
  ?assertEqual(nothing,         ?do({just, 3},       [Fun0, Fun2])),
  ?assertEqual({error, reason}, ?do({error, reason}, [Fun1])),
  ?assertEqual(nothing,         ?do(nothing,         [Fun1])),
  ?assertEqual([2],             ?do([1],             [Fun1])).

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
  ?assertEqual(nothing,    ?liftA2(nothing, {just, 1})),
  ?assertEqual(nothing,    ?liftA2({just, F}, nothing)),
  ?assertEqual([2, 3],     ?liftA2([F], [1, 2])).

-endif.
