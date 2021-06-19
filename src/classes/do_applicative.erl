%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% @doc The Applicative Type Class.
%%% @end
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%%_* Module declaration ======================================================
-module(do_applicative).

%%%_* Exports =================================================================
-define(API, [liftA2/2]).
-export(?API).
-ignore_xref(?API).

%%%_* Includes ================================================================
-include("do_types.hrl").
-include("do_internal.hrl").

%%%_* Callbacks ===============================================================
-callback pure(A) -> applicative(A).
-callback liftA2(applicative(fn(A, B)), applicative(A)) -> applicative(B).

%%%_* Code ====================================================================
%%%_* API ---------------------------------------------------------------------
-spec liftA2(applicative(fn(A, B)), applicative(A)) -> applicative(B).
liftA2(A1, A2) -> ?Mod(A1):liftA2(A1, A2).
