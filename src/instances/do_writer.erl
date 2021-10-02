%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% @doc The Writer Monad.
%%% @end
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%%_* Module declaration ======================================================
-module(do_writer).

-behaviour(do_functor).
-behaviour(do_applicative).
-behaviour(do_monad).

%%%_* Exports =================================================================
-define(API, [ % functor
               fmap/2,
               % applicative
               liftA2/2,
               pure/1,
               % monad
               bind/2,
               do/2,
               lift/1,
               liftm/2,
               then/2,
               % writer
               writer/2,
               tell/1,
               listen/1,
               pass/1,
               censor/2]).

-export(?API).
-ignore_xref(?API).

%%%_* Includes ================================================================
-include("do_internal.hrl").
-include("do_types.hrl").
-include("do.hrl").

%%%_* Macros ==================================================================
-define(is_mempty(W), ?isF1(W)).

%%%_* Code ====================================================================
%%%_* functor -----------------------------------------------------------------
-spec fmap(fn(A, B), writer(W, A)) -> writer(W, B).
fmap(F, {writer, Monoid, W}) when ?isF1(F) -> {writer, Monoid, F(W)}.

%%%_* applicative -------------------------------------------------------------
-spec liftA2(writer(W, fn(A, B)), writer(W, A)) -> writer(W, B).
liftA2({writer, A1, F}, {writer, A2, A}) when ?isF1(F) ->
  {writer, append(A1, A2), F(A)}.

-spec pure(A) -> writer(_, A).
pure(A) -> {writer, fun(W) -> ?Mod(W):mempty() end, A}.

%%%_* monad -------------------------------------------------------------------
-spec bind(writer(W, A), fn(A, writer(W, B))) -> writer(W, B).
bind({writer, A1, _} = Writer, F) when ?isF1(F) ->
  {writer, A2, B2} = flat(fmap(F, Writer)),
  {writer, append(A1, A2), B2}.

-spec do(writer(W, A), [fn(A, writer(W, B)) | fn(writer(W, B))]) -> writer(W, B).
do(Writer, Fs) -> do_monad:do(Writer, Fs).

-spec lift(fn(A, B)) -> fn(writer(_, A), writer(_, B)).
lift(F) -> do_monad:lift(F, ?MODULE).

-spec liftm(fun(), [writer(_, A)]) -> writer(_, A).
liftm(F, Writers) -> do_monad:liftm(F, Writers).

-spec then(writer(_, _), fn(writer(W, A))) -> writer(W, A).
then(Writer, F) -> do_monad:then(Writer, F).

%%%_* writer ------------------------------------------------------------------
-spec writer(W, A) -> writer(W, A).
writer(W, A) -> {writer, W, A}.

-spec tell(W) -> fn(A, writer(W, A)).
tell(W) -> fun(A) -> writer(W, A) end.

-spec listen(writer(W, A)) -> writer(W, {A, W}).
listen({writer, W, A}) -> {writer, W, {A, W}}.

-spec pass(writer(W, {A, fn(W, W)})) -> writer(W, A).
pass({writer, W, {A, F}}) -> {writer, F(W), A}.

-spec censor(fn(W, W), writer(W, A)) -> writer(W, A).
censor(F, {writer, W, A}) -> {writer, F(W), A}.

%%%_* internal ----------------------------------------------------------------
flat({writer, _, {writer, _, _} = Writer}) -> Writer.

append(A1, A2) when ?is_mempty(A1), ?is_mempty(A2) -> A2;
append(A1, A2) when ?is_mempty(A1)                 -> A2;
append(A1, A2) when ?is_mempty(A2)                 -> A1;
append(A1, A2)                                     -> ?Mod(A1):append(A1, A2).

%%%_* Tests ===================================================================
-ifdef(TEST).

bind_test() ->
  F1 = fun(W) -> {writer, [bar], W + 1} end,
  F2 = fun(W) -> pure(W + 1) end,
  ?equals({writer, [foo, bar], 2}, bind({writer, [foo], 1}, F1)),
  ?equals({writer, [bar], 2},      bind(pure(1), F1)),
  ?equals({writer, [bar], 3},      bind(bind(pure(1), F2), F1)).

do_test() ->
  Fun  = fun(W) -> ?pure(W + 1) end,
  ?equals({writer, [foo], 4}, do({writer, [foo], 3}, [Fun])).

-endif.
