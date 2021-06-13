-define(sequence(Traversable), do_either:sequence(Traversable)).
-define(lift(F),               do_either:lift(F)).

-ifndef(thunk).
  -define(thunk(A), fun() -> A end).
-endif.

-define(liftm(F, A1),
        do_either:liftmz(F, [?thunk(A1)])).
-define(liftm(F, A1, A2),
        do_either:liftmz(F, [?thunk(A1), ?thunk(A2)])).
-define(liftm(F, A1, A2, A3),
        do_either:liftmz(F, [?thunk(A1), ?thunk(A2), ?thunk(A3)])).
-define(liftm(F, A1, A2, A3, A4),
        do_either:liftmz(F, [?thunk(A1), ?thunk(A2), ?thunk(A3), ?thunk(A4)])).
-define(liftm(F, A1, A2, A3, A4, A5),
        do_either:liftmz(F, [?thunk(A1), ?thunk(A2), ?thunk(A3), ?thunk(A4), ?thunk(A5)])).
-define(liftm(F, A1, A2, A3, A4, A5, A6),
        do_either:liftmz(F, [?thunk(A1), ?thunk(A2), ?thunk(A3), ?thunk(A4), ?thunk(A5), ?thunk(A6)])).
-define(liftm(F, A1, A2, A3, A4, A5, A6, A7),
        do_either:liftmz(F, [?thunk(A1), ?thunk(A2), ?thunk(A3), ?thunk(A4), ?thunk(A5), ?thunk(A6), ?thunk(A7)])).
-define(liftm(F, A1, A2, A3, A4, A5, A6, A7, A8),
        do_either:liftmz(F, [?thunk(A1), ?thunk(A2), ?thunk(A3), ?thunk(A4), ?thunk(A5), ?thunk(A6), ?thunk(A7), ?thunk(A8)])).
-define(liftm(F, A1, A2, A3, A4, A5, A6, A7, A8, A9),
        do_either:liftmz(F, [?thunk(A1), ?thunk(A2), ?thunk(A3), ?thunk(A4), ?thunk(A5), ?thunk(A6), ?thunk(A7), ?thunk(A8), ?thunk(A9)])).
