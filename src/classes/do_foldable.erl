%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% @doc The Foldable Type Class.
%%% @end
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%%_* Module declaration ======================================================
-module(do_foldable).

%%%_* Exports =================================================================
-define(API, [foldr/3]).
-export(?API).
-ignore_xref(?API).

%%%_* Includes ================================================================
-include("do_types.hrl").
-include("do_internal.hrl").

%%%_* Callbacks ===============================================================
-callback fold_map(fn(A, monoid()), foldable(A)) -> monoid().
-callback foldr(fn(A, B, B), B, foldable(A)) -> B.

-optional_callbacks([fold_map/2]).

%%%_* Code ====================================================================
%%%_* API ---------------------------------------------------------------------
-spec foldr(fn(A, B, B), B, foldable(A)) -> B.
foldr(F, B, Foldable) -> ?Mod(Foldable):foldr(F, B, Foldable).
