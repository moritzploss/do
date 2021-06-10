%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% @doc
%%% @end
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%%_* Module declaration ======================================================
-module(do_monad).

%%%_* Exports =================================================================
-export([liftm/3]).
-export([do/3]).
-export([then/3]).
-export([lift/2]).

-export([pure_with_ctx/1]).
-ignore_xref([pure_with_ctx/1]).

%%%_* Includes ================================================================
-include("include/do_macros.hrl").
-include("include/do_types.hrl").
-include("include/do.hrl").

%%%_* Callbacks ===============================================================
-callback bind(fn(A, monad(B)), monad(A)) -> monad(B).
-callback do(monad(_), list(fn(_, monad(_)))) -> monad(_).
-callback then(fn(monad(A)), monad(_)) -> monad(A).
-callback lift(fn(A, B)) -> fn(monad(A), monad(B)).
-callback liftm(fun(), [monad(A)] | [fn(monad(A))]) -> monad(_).

%%%_* Code ====================================================================
%%%_* API ---------------------------------------------------------------------
pure_with_ctx(Ctx) ->
  Mod = get_module(Ctx),
  fun Mod:pure/1.

-spec lift(fn(A, B), atom()) -> fn(monad(A), monad(B)).
lift(F, Mod) when ?isF1(F) -> fun(Monad) -> Mod:liftm(F, [Monad]) end.

-spec liftm(fun(), [monad(A)] | [fn(monad(A))], atom()) -> monad(_).
liftm(F, Monads, Mod) when is_function(F, length(Monads)) andalso is_atom(Mod) ->
  Mod:fmap(fun(Args) -> apply(F, Args) end, do_traversable:sequence(Monads, Mod)).

-spec do(monad(A), list(fn(A, monad(B)) | fn(monad(B))), atom()) -> monad(B).
do(Monad, [], _Mod)                    -> Monad;
do(Monad, [F | Fs], Mod) when ?isF0(F) -> do(Mod:then(F, Monad), Fs, Mod);
do(Monad, [F | Fs], Mod) when ?isF1(F) -> do(Mod:bind(F, Monad), Fs, Mod).

-spec then(fn(monad(A)), monad(_), atom()) -> monad(A).
then(F, Monad, Mod) when ?isF0(F) -> Mod:bind(fun(_) -> F() end, Monad).

%%%_* Internal ----------------------------------------------------------------
is_monad({do_maybe, bind, 2, _File})  -> true;
is_monad({do_either, bind, 2, _File}) -> true;
is_monad(_Other)                      -> false.

get_module(Ctx) ->
  OnlyMonads            = lists:filter(fun is_monad/1, Ctx),
  {Mod, bind, 2, _File} = lists:keyfind(bind, 2, OnlyMonads),
  Mod.

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
  ?assertEqual({ok, 4},         ?do({ok, 3},           [Fun0, Fun])),
  ?assertEqual({error, reason}, ?do({error, reason},   [Fun])).

liftm_macro_test() ->
  F = fun(A, B, C) -> A + B + C end,
  ?assertEqual({ok, 6},    ?liftm(F, {ok, 1}, {ok, 2}, {ok, 3})),
  ?assertEqual({ok, 6},    ?liftm(F, {ok, 1}, {ok, 2}, ?thunk({ok, 3}))),
  ?assertEqual({error, 3}, ?liftm(F, {ok, 1}, {ok, 2}, {error, 3})).

liftA2_macro_test() ->
  F = fun(A, B) -> A + B end,
  ?assertEqual({ok, 3},    ?liftA2(F, {ok, 1}, {ok, 2})),
  ?assertEqual({error, 3}, ?liftA2(F, {ok, 1}, {error, 3})),
  ?assertEqual({error, 1}, ?liftA2(F, {error, 1}, {ok, 3})).

-endif.
