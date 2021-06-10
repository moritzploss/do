%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% @doc
%%% @end
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%%_* Module declaration ======================================================
-module(do_applicative).

%%%_* Includes ================================================================
-include("do_types.hrl").

%%%_* Callbacks ===============================================================
-callback pure(A) -> applicative(A).
-callback liftA2(fn(A, B, C), functor(A), functor(B)) -> functor(C).

%%%_* Tests ===================================================================
-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.
