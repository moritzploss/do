%%%_* Includes ================================================================
-include("do_types.hrl").

%%%_* Macros ==================================================================
-define(fmap(F, Functor), do_functor:fmap(F, Functor)).

-define(do, fun do_either:do/2).
-define(doEither, fun do_either:do/2).
-define(doMaybe, fun do_maybe:do/2).

-define(liftA2, (do_monad:liftA2_with_ctx(?do_internal_ctx))).
-define(pure,   (do_monad:pure_with_ctx(?do_internal_ctx))).
-define(lift(F), fun(A) -> ?pure(F(A)) end).

%%%_* Macros Internal - May Change Any Time ===================================
-define(do_internal_ctx, element(2, process_info(self(), current_stacktrace))).
