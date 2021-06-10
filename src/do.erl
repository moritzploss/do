%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% @doc The Do Module.
%%% @end
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%%_* Module declaration ======================================================
-module(do).

%%%_* Exports =================================================================
-define(API, [ do/2,
               bind/2,
               then/2,
               register_monad/1,
               get_monads/0]).
-export(?API).
-ignore_xref(?API).

%%%_* Includes ================================================================
-include("do_types.hrl").
-include("do_macros.hrl").
-include("do.hrl").

%%%_* Macros ==================================================================
-define(MONADS, [do_either, do_maybe, do_list]).
-define(KEY,    {?MODULE, monads}).

%%%_* Code ====================================================================
%%%_* API ---------------------------------------------------------------------
-spec do(monad(A), list(fn(A, monad(B)) | fn(monad(B)))) -> monad(B).
do(Monad, Funs) -> do_monad:do(Monad, Funs, get_monads()).

-spec bind(fn(A, monad(B)), monad(A)) -> monad(B).
bind(F, Monad) when ?isF1(F) -> do(Monad, [F]).

-spec then(fn(monad(B)), monad(_)) -> monad(B).
then(F, Monad) when ?isF0(F) -> do(Monad, [F]).

-spec register_monad(atom()) -> either(duplicate_monad, ok).
register_monad(Mod) when is_atom(Mod) ->
  Monads = get_monads(),
  case lists:member(Mod, Monads) of
    true  -> {error, duplicate_monad};
    false -> {ok, put_monads(Monads ++ [Mod])}
  end.

-spec get_monads() -> [atom()].
get_monads() ->
  case persistent_term:get(?KEY, undefined) of
    undefined ->
      ok = put_monads(?MONADS),
      ?MONADS;
    Monads    ->
      Monads
  end.

%%%_* internal ----------------------------------------------------------------
put_monads(Monads) ->
  ok = persistent_term:put(?KEY, Monads).

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
  ?assertEqual({error, 1}, ?liftA2({error, 1}, {ok, 3})).

register_monad_test() ->
  ?assertNot(lists:member(test, get_monads())),
  ?assertMatch({ok, _}, register_monad(test)),
  ?assert(lists:member(test, get_monads())),
  ?assertMatch({error, _}, register_monad(test)).

-endif.
