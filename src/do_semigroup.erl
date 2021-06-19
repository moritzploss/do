%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% @doc The Semigroup Type Class.
%%% @end
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%%_* Module declaration ======================================================
-module(do_semigroup).

%%%_* Exports =================================================================
-define(API, [append/2]).
-export(?API).
-ignore_xref(?API).

%%%_* Includes ================================================================
-include("do_types.hrl").

%%%_* Callbacks ===============================================================
-callback append(semigroup(A), semigroup(A)) -> semigroup(A).

%%%_* Code ====================================================================
%%%_* API ---------------------------------------------------------------------
append(List1, List2) when is_list(List1) -> do_list:append(List1, List2);
append(nothing = M1, M2)                 -> do_maybe:append(M1, M2);
append({just, _} = M1, M2)               -> do_maybe:append(M1, M2);
append({ok, _} = M1, M2)                 -> do_either:append(M1, M2);
append({error, _} = M1, M2)              -> do_either:append(M1, M2).
