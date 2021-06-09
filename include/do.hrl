%%%_* Macros ==================================================================
-define(fmap(F, Functor),   do_functor:fmap(F, Functor)).

-define(do(Val, Funs),      do_either:do(Val, Funs)).
-define(do(Mod, Val, Funs), do_monad:do(Val, Funs, Mod)).

-define(pure(A),            (do_monad:pure_with_ctx(?do_internal_ctx))(A)).

%%%_* Macros Internal - May Change Any Time ===================================
-define(do_internal_ctx, element(2, process_info(self(), current_stacktrace))).
