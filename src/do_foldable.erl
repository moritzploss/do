%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% @doc The Foldable Type Class.
%%% @end
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%%_* Module declaration ======================================================
-module(do_foldable).

%%%_* Includes ================================================================
-include("do_types.hrl").

%%%_* Callbacks ===============================================================
-callback fold_map(fn(A, monoid()), foldable(A)) -> monoid().
-callback foldr(fn(A, B, B), B, foldable(A)) -> B.

-optional_callbacks([fold_map/2]).
