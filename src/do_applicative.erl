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

%%%_* Callbacks ===============================================================
-callback pure(A) -> applicative(A).
-callback liftA2(applicative(fn(A, B)), applicative(A)) -> applicative(B).

%%%_* Code ====================================================================
%%%_* API ---------------------------------------------------------------------
-spec liftA2(applicative(fn(A, B)), applicative(A)) -> applicative(B).
liftA2(A1, A2) when is_list(A1) -> do_list:liftA2(A1, A2);
liftA2({ok, _} = A1, A2)        -> do_either:liftA2(A1, A2);
liftA2({error, _} = A1, A2)     -> do_either:liftA2(A1, A2);
liftA2({just, _} = A1, A2)      -> do_maybe:liftA2(A1, A2);
liftA2(nothing = A1, A2)        -> do_maybe:liftA2(A1, A2).
