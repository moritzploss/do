%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% @doc A Functional Programming Library
%%% @end
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%%_* Module declaration ======================================================
-module(fp).

%%%_* Exports =================================================================
-export([arity/1]).
-export([curry/1]).
-export([partial/2]).

%%%_* Includes ================================================================
-include("include/fp_macros.hrl").
-include("include/fp_types.hrl").

%%%_* Macros ==================================================================

%%%_* Callbacks ===============================================================

%%%_* Code ====================================================================
%%%_* API ---------------------------------------------------------------------
arity(F) -> element(2, erlang:fun_info(F, arity)).

curry(F) when is_function(F, 0) -> F;
curry(F) when is_function(F, 1) -> F;
curry(F) when is_function(F)    -> curry_(F, []).

partial(F, _Args) when is_function(F, 0) -> F;
partial(F, Args)  when is_function(F), is_list(Args) ->
  Arity  = arity(F),
  Length = length(Args),
  if Length < Arity   -> fun(Margs) when is_list(Args) ->
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
