%%%_* Includes ================================================================
-include("fp_types.hrl").

%%%_* Macros ==================================================================
-define(fmap(F, Functor), fp_functor:fmap(F, Functor)).

-define(do, fun fp_either:do/2).
-define(doEither, fun fp_either:do/2).
-define(doMaybe, fun fp_maybe:do/2).

-define(liftA2, (fp_monad:liftA2_with_ctx(?fp_internal_ctx))).
-define(pure,   (fp_monad:pure_with_ctx(?fp_internal_ctx))).
-define(lift(F), fun(A) -> ?pure(F(A)) end).

%%%_* Macros Internal - May Change Any Time ===================================
-define(fp_internal_ctx, element(2, process_info(self(), current_stacktrace))).
