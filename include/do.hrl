%%%_* Macros ==================================================================
-define(thunk(A),              fun() -> A end).
-define(f(F),                  ?thunk(F)).

-define(fmap(F, Functor),      do:fmap(F, Functor)).
-define(liftA2(A1, A2),        do:liftA2(A1, A2)).
-define(pure(A),               do:pure(A)).
-define(do(Monad, Fs),         do:do(Monad, Fs)).
-define(bind(Monad, F),        do:bind(Monad, F)).
-define(then(Monad1, Monad2),  do:then(Monad1, ?f(Monad2))).

-define(sequence(Traversable), do_either:sequence(Traversable)).
-define(lift(F),               do_either:lift(F)).
-define(liftm(F, A1),
        do_either:liftmz(F, [?f(A1)])).
-define(liftm(F, A1, A2),
        do_either:liftmz(F, [?f(A1), ?f(A2)])).
-define(liftm(F, A1, A2, A3),
        do_either:liftmz(F, [?f(A1), ?f(A2), ?f(A3)])).
-define(liftm(F, A1, A2, A3, A4),
        do_either:liftmz(F, [?f(A1), ?f(A2), ?f(A3), ?f(A4)])).
-define(liftm(F, A1, A2, A3, A4, A5),
        do_either:liftmz(F, [?f(A1), ?f(A2), ?f(A3), ?f(A4), ?f(A5)])).
-define(liftm(F, A1, A2, A3, A4, A5, A6),
        do_either:liftmz(F, [?f(A1), ?f(A2), ?f(A3), ?f(A4), ?f(A5), ?f(A6)])).
-define(liftm(F, A1, A2, A3, A4, A5, A6, A7),
        do_either:liftmz(F, [?f(A1), ?f(A2), ?f(A3), ?f(A4), ?f(A5), ?f(A6), ?f(A7)])).
-define(liftm(F, A1, A2, A3, A4, A5, A6, A7, A8),
        do_either:liftmz(F, [?f(A1), ?f(A2), ?f(A3), ?f(A4), ?f(A5), ?f(A6), ?f(A7), ?f(A8)])).
-define(liftm(F, A1, A2, A3, A4, A5, A6, A7, A8, A9),
        do_either:liftmz(F, [?f(A1), ?f(A2), ?f(A3), ?f(A4), ?f(A5), ?f(A6), ?f(A7), ?f(A8), ?f(A9)])).
