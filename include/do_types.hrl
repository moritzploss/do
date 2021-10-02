%%%_* types -------------------------------------------------------------------
-type either(A, B)   :: {error, A} | {ok, B}.

-type fn(A, B)       :: fun((A) -> B).

-type map(A)         :: #{_ := A}.

-type maybe(A)       :: {just, A} | nothing.

-type writer(A, B)   :: {writer, A, B}.

%%%_* type classes ------------------------------------------------------------
-type applicative(A) :: [A]
                        | fn(_, A)
                        | either(_, A)
                        | maybe(A)
                        | writer(_, A).

-type foldable(A)    :: [A]
                        | maybe(A)
                        | either(_, A).

-type functor(A)     :: [A]
                        | map(A)
                        | fn(_, A)
                        | either(_, A)
                        | maybe(A)
                        | writer(_, A).

-type monad(A)       :: [A]
                        | either(_, A)
                        | maybe(A)
                        | writer(_, A).

-type monoid()       :: [monoid()]
                        | maybe(monoid())
                        | either(_, monoid()).

-type semigroup(A)   :: [A]
                        | map(A)
                        | either(_, A)
                        | maybe(A).

-type traversable(A) :: [A]
                        | either(_, A)
                        | maybe(A).

%%%_* other -------------------------------------------------------------------
-type fn(A)          :: fun(() -> A).

-type fn(A, B, C)    :: fun((A, B) -> C).
