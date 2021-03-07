%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% @doc A Functional Programming Library
%%% @end
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%%_* Module declaration ======================================================
-module(do).

%%%_* Exports =================================================================
-export([arity/1]).
-export([curry/1]).
-export([partial/2]).

%%%_* Includes ================================================================
-include("include/do_macros.hrl").
-include("include/do_types.hrl").

%%%_* Macros ==================================================================

%%%_* Callbacks ===============================================================

%%%_* Code ====================================================================
%%%_* API ---------------------------------------------------------------------
arity(F) when ?isF(F) -> element(2, erlang:fun_info(F, arity)).

curry(F) when ?isF0(F) -> F;
curry(F) when ?isF1(F) -> F;
curry(F) when ?isF(F)  -> curry_(F, []).

partial(F, _Args) when ?isF0(F) -> F;
partial(F, Args)  when ?isF(F), is_list(Args) ->
  Arity  = arity(F),
  Length = length(Args),
  if Length < Arity   -> fun(Margs) when is_list(Margs) ->
                           partial(F, Args ++ Margs)
                         end;
     Arity =:= Length -> apply(F, Args)
  end.

%%%_* Internal ----------------------------------------------------------------
curry_(F, Args) ->
  Arity  = arity(F),
  Length = length(Args),
  if Length < Arity   -> fun(Arg) -> curry_(F, [Arg | Args]) end;
     Arity =:= Length -> apply(F, lists:reverse(Args))
  end.

%%%_* Tests ===================================================================
-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.
