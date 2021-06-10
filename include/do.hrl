%%%_* Macros ==================================================================
-define(fmap(F, Functor),      do_functor:fmap(F, Functor)).
-define(pure(A),               (do_monad:pure_with_ctx(?do_internal_ctx))(A)).
-define(sequence(Traversable), do_either:sequence(Traversable)).
-define(lift(F),               do_either:lift(F)).
-define(liftA2(F, A1, A2),     do_either:liftA2(F, A1, A2)).
-define(do(Either, Funs),      do_either:do(Either, Funs)).
-define(bind(F, Either),       do_either:bind(F, Either)).
-define(then(F, Either),       do_either:then(F, Either)).

-define(liftm(F, A1),
        do_either:liftm(F, [?f(A1)])).
-define(liftm(F, A1, A2),
        do_either:liftm(F, [?f(A1), ?f(A2)])).
-define(liftm(F, A1, A2, A3),
        do_either:liftm(F, [?f(A1), ?f(A2), ?f(A3)])).
-define(liftm(F, A1, A2, A3, A4),
        do_either:liftm(F, [?f(A1), ?f(A2), ?f(A3), ?f(A4)])).
-define(liftm(F, A1, A2, A3, A4, A5),
        do_either:liftm(F, [?f(A1), ?f(A2), ?f(A3), ?f(A4), ?f(A5)])).
-define(liftm(F, A1, A2, A3, A4, A5, A6),
        do_either:liftm(F, [?f(A1), ?f(A2), ?f(A3), ?f(A4), ?f(A5), ?f(A6)])).
-define(liftm(F, A1, A2, A3, A4, A5, A6, A7),
        do_either:liftm(F, [?f(A1), ?f(A2), ?f(A3), ?f(A4), ?f(A5), ?f(A6), ?f(A7)])).
-define(liftm(F, A1, A2, A3, A4, A5, A6, A7, A8),
        do_either:liftm(F, [?f(A1), ?f(A2), ?f(A3), ?f(A4), ?f(A5), ?f(A6), ?f(A7), ?f(A8)])).
-define(liftm(F, A1, A2, A3, A4, A5, A6, A7, A8, A9),
        do_either:liftm(F, [?f(A1), ?f(A2), ?f(A3), ?f(A4), ?f(A5), ?f(A6), ?f(A7), ?f(A8), ?f(A9)])).

-define(thunk(A), fun() -> A end).

%%%_* Macros Internal - May Change Any Time ===================================
-define(do_internal_ctx, element(2, process_info(self(), current_stacktrace))).
-define(f(F),            ?thunk(F)).
