%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% @doc The Applicative Type Class.
%%% @end
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%%_* Module declaration ======================================================
-module(do_applicative).

%%%_* Includes ================================================================
-include("do_types.hrl").

%%%_* Callbacks ===============================================================
-callback pure(A) -> applicative(A).
-callback liftA2(applicative(fn(A, B)), applicative(A)) -> applicative(B).
