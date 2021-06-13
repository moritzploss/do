%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% @doc The Either Monad.
%%% @end
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%%_* Module declaration ======================================================
-module(do_either).

-behaviour(do_functor).
-behaviour(do_applicative).
-behaviour(do_monad).

%%%_* Exports =================================================================
-define(API, [ % functor
               fmap/2,
               % applicative
               liftA2/2,
               pure/1,
               sequence/1,
               % monad
               bind/2,
               do/2,
               lift/1,
               liftm/2,
               then/2,
               % either
               liftmz/2,
               either/3,
               errors/1,
               oks/1,
               is_error/1,
               is_ok/1,
               from_error/2,
               from_ok/2,
               partition/1]).

-export(?API).
-ignore_xref(?API).

%%%_* Includes ================================================================
-include("do_guards.hrl").
-include("do_types.hrl").
-include("do.hrl").

%%%_* Code ====================================================================
%%%_* functor -----------------------------------------------------------------
-spec fmap(fn(B, C), Either :: either(A, B)) -> either(A, C).
fmap(F, {error, A}) when ?isF1(F) -> {error, A};
fmap(F, {ok, B})    when ?isF1(F) -> {ok, F(B)}. 

%%%_* applicative -------------------------------------------------------------
-spec liftA2(Either :: either(A1, fn(B, C)), either(A2, B)) -> either(A1 | A2, C).
liftA2({ok, F}, Either) when ?isF1(F) -> fmap(F, Either);
liftA2({error, Reason}, _)            -> {error, Reason}.

-spec pure(B) -> either(_, B).
pure(B) -> {ok, B}.

-spec sequence(traversable(either(A, B))) -> either(A, traversable(B)).
sequence(Eithers) -> do_traversable:sequence(Eithers, ?MODULE).

%%%_* monad -------------------------------------------------------------------
-spec bind(either(D, A), fn(A, either(B, C))) -> either(B | D, C).
bind(Either, F) when ?isF1(F) -> flat(fmap(F, Either)).

-spec do(either(A, B), [fn(B, either(C, D)) | fn(either(C, D))]) -> either(A | C, D).
do(Either, Fs) -> do_monad:do(Either, Fs, ?MODULE).

-spec lift(fn(A, B)) -> fn(monad(A), monad(B)).
lift(F) -> do_monad:lift(F, ?MODULE).

-spec liftm(fun(), [either(_, B)]) -> either(_, B).
liftm(F, Eithers) -> do_monad:liftm(F, Eithers, ?MODULE).

-spec then(either(A, _), fn(either(B, C))) -> either(A | B, C).
then(Either, F) -> do_monad:then(Either, F, ?MODULE).

%%%_* either ------------------------------------------------------------------
-spec liftmz(fun(), [fn(either(_, B))]) -> either(_, B).
liftmz(F, Eithers) -> do_monad:liftmz(F, Eithers, ?MODULE).

-spec either(fn(A, C), fn(B, C), Either :: either(A, B)) -> C.
either(F1, F2, {error, A}) when ?isF1(F1), ?isF1(F2) -> F1(A);
either(F1, F2, {ok, B})    when ?isF1(F1), ?isF1(F2) -> F2(B).

-spec errors([either(A, _)]) -> [A].
errors(Eithers) when is_list(Eithers) ->
  lists:filtermap(fun({error, A}) -> {true, A};
                     ({ok, _B})   -> false end, Eithers).

-spec oks([either(_, B)]) -> [B].
oks(Eithers) when is_list(Eithers) ->
  lists:filtermap(fun({ok, B})     -> {true, B};
                     ({error, _A}) -> false end, Eithers).


-spec is_error(Either :: either(_, _)) -> boolean().
is_error({error, _}) -> true;
is_error({ok, _})    -> false.

-spec is_ok(either(_, _)) -> boolean().
is_ok(Either) -> not is_error(Either).

-spec from_error(A, Either :: either(A, _)) -> A.
from_error(_A, {error, A}) -> A;
from_error(A, {ok, _B})    -> A.

-spec from_ok(B, Either :: either(_, B)) -> B.
from_ok(_B, {ok, B})    -> B;
from_ok(B, {error, _A}) -> B.

-spec partition([either(A, B)]) -> {[A], [B]}.
partition(Eithers) when is_list(Eithers) ->
  lists:foldr(fun({error, A}, {As, Bs}) -> {[A | As], Bs};
                 ({ok, B},    {As, Bs}) -> {As, [B | Bs]} end,
              {[], []}, Eithers).

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
  F = fun(A) -> A + 1 end,
  ?assertEqual({ok, 3},      liftA2({ok, F},      {ok, 2})),
  ?assertEqual({error, rsn}, liftA2({ok, F},      {error, rsn})),
  ?assertEqual({error, rsn}, liftA2({error, rsn}, {ok, 2})),
  ?assertEqual({error, rsn}, liftA2({error, rsn}, {error, rsn})).

liftm_test() ->
  F = fun(A, B, C) -> A + B + C end,
  ?assertEqual({ok, 4},      liftm(F, [{ok, 1},      {ok, 2},      {ok, 1}])),
  ?assertEqual({error, rsn}, liftm(F, [{ok, 1},      {error, rsn}, {ok, 1}])),
  ?assertEqual({error, rsn}, liftm(F, [{error, rsn}, {ok, 2},      {ok, 1}])),
  ?assertEqual({error, rsn}, liftm(F, [{error, rsn}, {error, rsn}, {ok, 1}])).

bind_test() ->
  FOk    = fun(A) -> {ok, A + 1} end,
  FError = fun(_) -> {error, reason} end,
  ?assertEqual({ok, 3},          bind({ok, 2}, FOk)),
  ?assertEqual({error, reason},  bind({error, reason}, FOk)),
  ?assertEqual({error, reason},  bind({ok, 2}, FError)),
  ?assertEqual({error, reason1}, bind({error, reason1}, FError)).

do_test() ->
  Fun0 = fun() -> ?pure(3) end,
  Fun  = fun(A) -> ?pure(A + 1) end,
  ?assertEqual({ok, 3},         do({ok, 3},           [])),
  ?assertEqual({ok, 4},         do({ok, 3},           [Fun])),
  ?assertEqual({ok, 4},         do({ok, 3},           [Fun0, Fun])),
  ?assertEqual({error, reason}, do({error, reason},   [Fun])).

sequence_test() ->
  ?assertEqual({ok, [1, 2, 3]},         sequence([{ok, 1}, {ok, 2}, {ok, 3}])),
  ?assertEqual({error, reason},         sequence([{ok, 1}, {error, reason}, {ok, 3}])),
  ?assertEqual({ok, #{a => 1, b => 2}}, sequence(#{a => {ok, 1}, b => {ok, 2}})),
  ?assertEqual({error, reason},         sequence(#{a => {ok, 1}, b => {error, reason}})).

either_test() ->
  F1 = fun(X) -> X end,
  F2 = fun(_) -> 0 end,
  ?assertEqual(1, either(F2, F1, {ok, 1})),
  ?assertEqual(2, either(F1, F2, {error, 2})).

errors_test() ->
  ?assertEqual([],     errors([])),
  ?assertEqual([1, 2], errors([{ok, 3}, {error, 1}, {error, 2}, {ok, 4}])).

oks_test() ->
  ?assertEqual([],     oks([])),
  ?assertEqual([3, 4], oks([{ok, 3}, {error, 1}, {error, 2}, {ok, 4}])).

is_error_test() ->
  ?assertEqual(true,  is_error({error, 1})),
  ?assertEqual(false, is_error({ok, 1})).

is_ok_test() ->
  ?assertEqual(false, is_ok({error, 1})),
  ?assertEqual(true,  is_ok({ok, 1})).

from_error_test() ->
  ?assertEqual(1, from_error(2, {error, 1})),
  ?assertEqual(2, from_error(2, {ok, 1})).

from_ok_test() ->
  ?assertEqual(1, from_ok(2, {ok, 1})),
  ?assertEqual(2, from_ok(2, {error, 1})).

partition_test() ->
  ?assertEqual({[], []},         partition([])),
  ?assertEqual({[1, 2], [3, 4]}, partition([{ok, 3}, {error, 1}, {error, 2}, {ok, 4}])).

-endif.
