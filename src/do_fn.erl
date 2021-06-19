%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% @doc The Fun Functor.
%%% @end
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%%_* Module declaration ======================================================
-module(do_fn).

-behaviour(do_functor).

%%%_* Exports =================================================================
-export([fmap/2]).

%%%_* Includes ================================================================
-include("do_internal.hrl").
-include("do_types.hrl").

%%%_* Code ====================================================================
-spec fmap(fn(B, C), fn(A, B)) -> fn(A, C).
fmap(F, Fun) when ?isF1(F), ?isF1(Fun) -> fun(Arg) -> F(Fun(Arg)) end.
