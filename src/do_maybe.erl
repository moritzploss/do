%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% @doc The Maybe Type.
%%% @end
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%%_* Module declaration ======================================================
-module(do_maybe).

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
-spec bind(fn(A, maybe(B)), maybe(A)) -> maybe(B).
bind(F, Maybe) when ?isF1(F) -> flat(fmap(F, Maybe)).

-spec do(maybe(A), list(fn(A, maybe(B)) | fn(maybe(B)))) -> maybe(B).
do(Maybe, [])                     -> Maybe;
do(Maybe, [F | Fs]) when ?isF0(F) -> do(then(F, Maybe), Fs);
do(Maybe, [F | Fs]) when ?isF1(F) -> do(bind(F, Maybe), Fs).

-spec fmap(fn(A, B), maybe(A)) -> maybe(B).
fmap(F, {ok, A}) when ?isF1(F) -> {ok, F(A)};
fmap(F, error)   when ?isF1(F) -> error.

-spec lift(fn(A, B)) -> fn(A, maybe(B)).
lift(F) when ?isF1(F) -> fun(Arg) -> pure(F(Arg)) end.

-spec liftA2(fn(A, B, C), maybe(A), maybe(B)) -> maybe(C).
liftA2(F, Maybe1, Maybe2) when is_function(F, 2) -> lift(F, [Maybe1, Maybe2]).

-spec pure(A) -> maybe(A).
pure({ok, A})    -> {ok, A};
pure(error)      -> error;
pure({error, _}) -> error;
pure(A)          -> {ok, A}.

-spec sequence(iterable(maybe(A))) -> maybe(iterable(A)).
sequence(List) when is_list(List) ->
  case lists:member(error, List) of
    true  -> error;
    false -> pure(element(2, lists:unzip(List)))
  end;
sequence(Map) when is_map(Map) ->
  fmap( fun(Vals) -> maps:from_list(lists:zip(maps:keys(Map), Vals)) end
      , sequence(maps:values(Map))).

-spec then(fn(maybe(A)), maybe(_)) -> maybe(A).
then(F, Maybe) when ?isF0(F) -> bind(fun(_) -> F() end, Maybe).

%%%_* Internal ----------------------------------------------------------------
flat({ok, error})   -> error;
flat(error)         -> error;
flat({ok, {ok, A}}) -> {ok, A}.

lift(F, Maybes) when is_function(F) ->
  fmap(fun(Args) -> apply(F, Args) end, sequence(Maybes)).

%%%_* Tests ===================================================================
-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

liftA2_test() ->
  F = fun(A, B) -> A + B end,
  ?assertEqual({ok, 3}, liftA2(F, {ok, 1}, {ok, 2})),
  ?assertEqual(error,   liftA2(F, {ok, 1}, error)),
  ?assertEqual(error,   liftA2(F, error,   {ok, 2})),
  ?assertEqual(error,   liftA2(F, error,   error)).

bind_test() ->
  FOk    = fun(A) -> {ok, A + 1} end,
  FError = fun(_) -> error end,
  ?assertEqual({ok, 3}, bind(FOk, {ok, 2})),
  ?assertEqual(error,   bind(FOk, error)),
  ?assertEqual(error,   bind(FError, {ok, 2})),
  ?assertEqual(error,   bind(FError, error)).

sequence_test() ->
  ?assertEqual({ok, [1, 2, 3]},         sequence([{ok, 1}, {ok, 2}, {ok, 3}])),
  ?assertEqual(error,                   sequence([{ok, 1}, error, {ok, 3}])),
  ?assertEqual({ok, #{a => 1, b => 2}}, sequence(#{a => {ok, 1}, b => {ok, 2}})),
  ?assertEqual(error,                   sequence(#{a => {ok, 1}, b => error})).

-endif.
