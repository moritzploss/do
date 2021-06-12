%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% @doc The Monad Type Class.
%%% @end
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%%_* Module declaration ======================================================
-module(do_monad).

%%%_* Exports =================================================================
-export([lift/2]).
-export([liftm/3]).
-export([liftmz/3]).
-export([do/3]).
-export([then/3]).

%%%_* Includes ================================================================
-include("do_macros.hrl").
-include("do_types.hrl").

%%%_* Callbacks ===============================================================
-callback bind(monad(A), fn(A, monad(B))) -> monad(B).
-callback do(monad(_), list(fn(_, monad(_)))) -> monad(_).
-callback then(monad(_), fn(monad(A))) -> monad(A).
-callback lift(fn(A, B)) -> fn(monad(A), monad(B)).
-callback liftm(fun(), [monad(A)] | [fn(monad(A))]) -> monad(_).

%%%_* Code ====================================================================
%%%_* API ---------------------------------------------------------------------
-spec lift(fn(A, B), atom()) -> fn(monad(A), monad(B)).
lift(F, Mod) when ?isF1(F) -> fun(Monad) -> Mod:liftm(F, [Monad]) end.

-spec liftm(fun(), [monad(_)], atom()) -> monad(_).
liftm(F, Monads, Mod) when ?isF(F, length(Monads)) andalso is_atom(Mod) ->
  do_liftm(F, Monads, Mod, fun do_traversable:sequence/2).

-spec liftmz(fun(), [fn(monad(_))], atom()) -> monad(_).
liftmz(F, Thunks, Mod) when ?isF(F, length(Thunks)) andalso is_atom(Mod) ->
  do_liftm(F, Thunks, Mod, fun do_traversable:sequence_lazy/2).

-spec do(monad(A), list(fn(A, monad(B)) | fn(monad(B))), atom()) -> monad(B).
do(Monad, [], _Mod)                    -> Monad;
do(Monad, [F | Fs], Mod) when ?isF0(F) -> do(Mod:then(Monad, F), Fs, Mod);
do(Monad, [F | Fs], Mod) when ?isF1(F) -> do(Mod:bind(Monad, F), Fs, Mod).

-spec then(monad(_), fn(monad(A)), atom()) -> monad(A).
then(Monad, F, Mod) when ?isF0(F) -> Mod:bind(Monad, fun(_) -> F() end).

%%%_* Internal ----------------------------------------------------------------
do_liftm(F, Monads, Mod, Sequence) ->
  Mod:fmap(fun(Args) -> apply(F, Args) end, Sequence(Monads, Mod)).
