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
-callback bind(fn(A, monad(B)), monad(A)) -> monad(B).
-callback do(monad(_), list(fn(_, monad(_)))) -> monad(_).
-callback then(fn(monad(A)), monad(_)) -> monad(A).
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

-spec do(monad(A), list(fn(A, monad(B)) | fn(monad(B))), [atom()]) -> monad(B).
do(Monad, [], _Mods)                    -> Monad;
do(Monad, [F | Fs], Mods) when ?isF0(F) -> do(try_(F, Monad, then, Mods), Fs, Mods);
do(Monad, [F | Fs], Mods) when ?isF1(F) -> do(try_(F, Monad, bind, Mods), Fs, Mods).

-spec then(fn(monad(A)), monad(_), atom()) -> monad(A).
then(F, Monad, Mod) when ?isF0(F) -> Mod:bind(fun(_) -> F() end, Monad).

%%%_* Internal ----------------------------------------------------------------
do_liftm(F, Monads, Mod, Sequence) ->
  Mod:fmap(fun(Args) -> apply(F, Args) end, Sequence(Monads, Mod)).

try_(F, Monad, Fun, [Mod | Rest]) ->
  try
    apply(Mod, Fun, [F, Monad])
  catch
    error:function_clause:Trace ->
      case {Rest, Trace} of
        {[], [_, {Mod, bind, 2, _} | _]} -> error({error, {no_monad, Monad}});
        {_,  [_, {Mod, bind, 2, _} | _]} -> try_(F, Monad, Fun, Rest)
      end
  end.
