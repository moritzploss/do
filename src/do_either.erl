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
-export([bind/2]).
-export([do/2]).
-export([fmap/2]).
-export([lift/1]).
-export([liftA2/3]).
-export([pure/1]).
-export([sequence/1]).
-export([then/2]).

%%%_* Includes ================================================================
-include("include/do_macros.hrl").
-include("include/do_types.hrl").

%%%_* Code ====================================================================
%%%_* API ---------------------------------------------------------------------
-spec bind(fn(A, either(B, C)), either(D, A)) -> either(B | D, C).
bind(F, Either) when ?isF1(F) ->
  flat(fmap(F, Either)).

-spec do(either(A, B), list(fn(B, either(C, D)) | fn(either(C, D)))) -> either(A | C, D).
do(Either, [])                     -> Either;
do(Either, [F | Fs]) when ?isF0(F) -> do(then(F, Either), Fs);
do(Either, [F | Fs]) when ?isF1(F) -> do(bind(F, Either), Fs).

-spec fmap(fn(B, C), either(A, B)) -> either(A, C).
fmap(F, {error, A}) when ?isF1(F) -> {error, A};
fmap(F, {ok, B})    when ?isF1(F) -> {ok, F(B)}.

-spec lift(fn(A, B)) -> fn(A, either(_, B)).
lift(F) when ?isF1(F) -> fun(Arg) -> pure(F(Arg)) end.

-spec liftA2(fn(B1, B2, C), either(A1, B1), either(A2, B2)) -> either(A1 | A2, C).
liftA2(F, Either1, Either2) when is_function(F, 2) ->
  lift(F, [Either1, Either2]).

-spec pure(B) -> either(_, B).
pure({error, A}) -> {error, A};
pure({ok, B})    -> {ok, B};
pure(error)      -> {error, error};
pure(B)          -> {ok, B}.

-spec sequence(iterable(either(A, B))) -> either(A, iterable(B)).
sequence(List) when is_list(List) ->
  case lists:keyfind(error, 1, List) of
    {error, Reason} -> {error, Reason};
    false           -> pure(element(2, lists:unzip(List)))
  end;
sequence(Map) when is_map(Map) ->
  fmap( fun(Vals) -> maps:from_list(lists:zip(maps:keys(Map), Vals)) end
      , sequence(maps:values(Map))).

-spec then(fn(either(A, B)), either(_, _)) -> either(A, B).
then(F, Either) when ?isF0(F) -> bind(fun(_) -> F() end, Either).

%%%_* Internal ----------------------------------------------------------------
flat({ok, {error, A}}) -> {error, A};
flat({error, A})       -> {error, A};
flat({ok, {ok, B}})    -> {ok, B}.

lift(F, Eithers) when is_function(F) ->
  fmap(fun(Args) -> apply(F, Args) end, sequence(Eithers)).

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

sequence_test() ->
  ?assertEqual({ok, [1, 2, 3]},         sequence([{ok, 1}, {ok, 2}, {ok, 3}])),
  ?assertEqual({error, reason},         sequence([{ok, 1}, {error, reason}, {ok, 3}])),
  ?assertEqual({ok, #{a => 1, b => 2}}, sequence(#{a => {ok, 1}, b => {ok, 2}})),
  ?assertEqual({error, reason},         sequence(#{a => {ok, 1}, b => {error, reason}})).

-endif.
