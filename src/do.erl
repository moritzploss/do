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
-include("do_guards.hrl").
-include("do.hrl").

%%%_* Macros ==================================================================
-define(TRACE, element(2, process_info(self(), current_stacktrace))).
-define(FMAP,  {_, _, 2, _}).
-define(DO,    {do_monad, do, 3, _}).

%%%_* Code ====================================================================
%%%_* API ---------------------------------------------------------------------
-spec fmap(fn(A, B), functor(A)) -> functor(B).
fmap(F, Functor)        when ?isF1(F), is_list(Functor) -> do_list:fmap(F, Functor);
fmap(F, Functor)        when ?isF1(F), is_map(Functor)  -> do_map:fmap(F, Functor);
fmap(F, Functor)        when ?isF1(F), ?isF1(Functor)   -> do_fn:fmap(F, Functor);
fmap(F, {error, _} = E) when ?isF1(F)                   -> do_either:fmap(F, E);
fmap(F, {ok, _} = E)    when ?isF1(F)                   -> do_either:fmap(F, E);
fmap(F, nothing = M)    when ?isF1(F)                   -> do_maybe:fmap(F, M);
fmap(F, {just, _} = M)  when ?isF1(F)                   -> do_maybe:fmap(F, M).

-spec liftA2(applicative(fn(A, B)), applicative(A)) -> applicative(B).
liftA2(A1, A2) when is_list(A1) -> do_list:liftA2(A1, A2);
liftA2({ok, _} = A1, A2)        -> do_either:liftA2(A1, A2);
liftA2({error, _} = A1, A2)     -> do_either:liftA2(A1, A2);
liftA2({just, _} = A1, A2)      -> do_maybe:liftA2(A1, A2);
liftA2(nothing = A1, A2)        -> do_maybe:liftA2(A1, A2).

-spec pure(A) -> monad(A) | A.
pure(A) ->
  case ?TRACE of
    [_, ?FMAP, {Monad, bind, 2, _}, ?DO | _] -> Monad:pure(A);
    _Trace                                   -> A
  end.

-spec do(monad(A), [fn(A, monad(B)) | fn(monad(B))]) -> monad(B).
do(Monad, Fs) when is_list(Monad) -> do_list:do(Monad, Fs);
do({ok, _} = Monad, Fs)           -> do_either:do(Monad, Fs);
do({error, _} = Monad, Fs)        -> do_either:do(Monad, Fs);
do({just, _} = Monad, Fs)         -> do_maybe:do(Monad, Fs);
do(nothing = Monad, Fs)           -> do_maybe:do(Monad, Fs).

-spec bind(monad(A), fn(A, monad(B))) -> monad(B).
bind(Monad, F) when ?isF1(F) -> do(Monad, [F]).

-spec then(monad(_), fn(monad(B))) -> monad(B).
then(Monad, F) when ?isF0(F) -> do(Monad, [F]).

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
