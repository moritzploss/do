%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% @doc The Either Type.
%%% @end
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%%_* Module declaration ======================================================
-module(do_either).

-behaviour(do_functor).
-behaviour(do_applicative).
-behaviour(do_monad).

%%%_* Exports =================================================================
-define(API, [ bind/2,
               do/2,
               fmap/2,
               lift/1,
               liftA2/3,
               liftm/2,
               pure/1,
               sequence/1,
               then/2]).
-export(?API).
-ignore_xref(?API).

%%%_* Includes ================================================================
-include("include/do_macros.hrl").
-include("include/do_types.hrl").
-include("include/do.hrl").

%%%_* Code ====================================================================
%%%_* functor -----------------------------------------------------------------
-spec fmap(fn(B, C), either(A, B)) -> either(A, C).
fmap(F, {error, A}) when ?isF1(F) -> {error, A};
fmap(F, {ok, B})    when ?isF1(F) -> {ok, F(B)}. 

%%%_* applicative -------------------------------------------------------------
-spec liftA2(fn(B, B, C), either(A1, B), either(A2, B)) -> either(A1 | A2, C).
liftA2(F, {ok, V1}, {ok, V2}) when ?isF2(F) -> {ok, F(V1, V2)};
liftA2(F, {error, Reason}, _) when ?isF2(F) -> {error, Reason};
liftA2(F, _, {error, Reason}) when ?isF2(F) -> {error, Reason}.

-spec pure(B) -> either(_, B).
pure(B) -> {ok, B}.

-spec sequence(traversable(either(A, B))) -> either(A, traversable(B)).
sequence(Eithers) -> do_traversable:sequence(Eithers, ?MODULE).

%%%_* monad -------------------------------------------------------------------
-spec bind(fn(A, either(B, C)), either(D, A)) -> either(B | D, C).
bind(F, Either) when ?isF1(F) -> flat(fmap(F, Either)).

-spec do(either(A, B), list(fn(B, either(C, D)) | fn(either(C, D)))) -> either(A | C, D).
do(Either, Fs) -> do_monad:do(Either, Fs, ?MODULE).

-spec lift(fn(A, B)) -> fn(monad(A), monad(B)).
lift(F) -> do_monad:lift(F, ?MODULE).

-spec liftm(fun(), [either(_, B)] | [fn(either(_, B))]) -> either(_, B).
liftm(F, Eithers) -> do_monad:liftm(F, Eithers, ?MODULE).

-spec then(fn(either(A, B)), either(_, _)) -> either(A, B).
then(F, Either) -> do_monad:then(F, Either, ?MODULE).

%%%_* internal ----------------------------------------------------------------
flat({ok, {error, A}}) -> {error, A};
flat({error, A})       -> {error, A};
flat({ok, {ok, B}})    -> {ok, B}.

%%%_* Tests ===================================================================
-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

pure_test() ->
  ?assertEqual({ok, {ok, 3}}, pure({ok, 3})),
  ?assertEqual({ok, 3},       pure(3)).

lift_test() ->
  F      = fun(A) -> A + 1 end,
  Lifted = lift(F),
  ?assertEqual({ok, 2},      Lifted({ok, 1})),
  ?assertEqual({error, rsn}, Lifted({error, rsn})).

liftA2_test() ->
  F = fun(A, B) -> A + B end,
  ?assertEqual({ok, 3},      liftA2(F, {ok, 1},      {ok, 2})),
  ?assertEqual({error, rsn}, liftA2(F, {ok, 1},      {error, rsn})),
  ?assertEqual({error, rsn}, liftA2(F, {error, rsn}, {ok, 2})),
  ?assertEqual({error, rsn}, liftA2(F, {error, rsn}, {error, rsn})).

liftm_test() ->
  F = fun(A, B, C) -> A + B + C end,
  ?assertEqual({ok, 4},      liftm(F, [{ok, 1},      {ok, 2},      {ok, 1}])),
  ?assertEqual({error, rsn}, liftm(F, [{ok, 1},      {error, rsn}, {ok, 1}])),
  ?assertEqual({error, rsn}, liftm(F, [{error, rsn}, {ok, 2},      {ok, 1}])),
  ?assertEqual({error, rsn}, liftm(F, [{error, rsn}, {error, rsn}, {ok, 1}])).

bind_test() ->
  FOk    = fun(A) -> {ok, A + 1} end,
  FError = fun(_) -> {error, reason} end,
  ?assertEqual({ok, 3},          bind(FOk, {ok, 2})),
  ?assertEqual({error, reason},  bind(FOk, {error, reason})),
  ?assertEqual({error, reason},  bind(FError, {ok, 2})),
  ?assertEqual({error, reason1}, bind(FError, {error, reason1})).

sequence_test() ->
  ?assertEqual({ok, [1, 2, 3]},         sequence([{ok, 1}, {ok, 2}, {ok, 3}])),
  ?assertEqual({ok, [1, 2, 3]},         sequence([{ok, 1}, ?thunk({ok, 2}), {ok, 3}])),
  ?assertEqual({error, reason},         sequence([{ok, 1}, {error, reason}, {ok, 3}])),
  ?assertEqual({ok, #{a => 1, b => 2}}, sequence(#{a => {ok, 1}, b => {ok, 2}})),
  ?assertEqual({error, reason},         sequence(#{a => {ok, 1}, b => {error, reason}})).

-endif.
