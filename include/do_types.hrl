%%%_* types -------------------------------------------------------------------
-type either(A, B)   :: {error, A} | {ok, B}.

-type fn(A, B)       :: fun((A) -> B).

-type map(A)         :: #{_ := A}.

-type maybe(A)       :: {just, A} | nothing.

%%%_* type classes ------------------------------------------------------------
-type applicative(A) :: [A]
                        | fn(_, A)
                        | either(_, A)
                        | maybe(A).

-type foldable(A)    :: [A]
                        | maybe(A).

-type functor(A)     :: [A]
                        | map(A)
                        | fn(_, A)
                        | either(_, A)
                        | maybe(A).

-type monad(A)       :: [A]
                        | either(_, A)
                        | maybe(A).

-type monoid()       :: [monoid()]
                        | maybe(monoid()).

-type semigroup(A)   :: [A]
                        | maybe(A).

-type traversable(A) :: [A] | map(A).

%%%_* other -------------------------------------------------------------------
-type fn(A)          :: fun(() -> A).

-type fn(A, B, C)    :: fun((A, B) -> C).
