%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% @doc The Monad Type Class.
%%% @end
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%%_* Module declaration ======================================================
-module(do_monad).

%%%_* Exports =================================================================
-define(API, [do/2, lift/2, liftm/2, liftmz/2, then/2]).
-export(?API).
-ignore_xref(?API).

%%%_* Includes ================================================================
-include("do_internal.hrl").
-include("do_types.hrl").

%%%_* Callbacks ===============================================================
-callback bind(monad(A), fn(A, monad(B))) -> monad(B).
-callback do(monad(_), list(fn(_, monad(_)))) -> monad(_).
-callback then(monad(_), fn(monad(A))) -> monad(A).
-callback lift(fn(A, B)) -> fn(monad(A), monad(B)).
-callback liftm(fun(), [monad(_)] | [fn(monad(_))]) -> monad(_).

%%%_* Code ====================================================================
%%%_* API ---------------------------------------------------------------------
-spec lift(fn(A, B), atom()) -> fn(monad(A), monad(B)).
lift(F, Mod) when ?isF1(F) -> fun(Monad) -> Mod:liftm(F, [Monad]) end.

-spec liftm(fun(), [monad(_)]) -> monad(_).
liftm(F, [M | _] = Monads) when ?isF(F, length(Monads)) ->
  do_liftm(F, Monads, ?Mod(M), fun do_traversable:sequence/1).

-spec liftmz(fun(), [fn(monad(_))]) -> monad(_).
liftmz(F, [T | Ts] = Thunks) when ?isF(F, length(Thunks)) ->
  V = T(),
  do_liftm(F, [?thunk(V) | Ts], ?Mod(V), fun do_list:sequencez/1).

-spec do(monad(A), [fn(A, monad(B)) | fn(monad(B))]) -> monad(B).
do(Monad, [])                     -> Monad;
do(Monad, [F | Fs]) when ?isF0(F) -> do(?Mod(Monad):then(Monad, F), Fs);
do(Monad, [F | Fs]) when ?isF1(F) -> do(?Mod(Monad):bind(Monad, F), Fs).

-spec then(monad(_), fn(monad(A))) -> monad(A).
then(Monad, F) when ?isF0(F) -> ?Mod(Monad):bind(Monad, fun(_) -> F() end).

%%%_* Internal ----------------------------------------------------------------
do_liftm(F, Monads, Mod, Sequence) ->
  Mod:fmap(fun(Args) -> apply(F, Args) end, Sequence(Monads)).

%%%_* Tests ===================================================================
-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

liftmz_test() ->
  F = fun(A, B) -> A + B end,
  ?equals({ok, 3}, liftmz(F, [?thunk({ok, 1}), ?thunk({ok, 2})])).

-endif.
