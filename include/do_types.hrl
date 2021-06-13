%%%_* Types ===================================================================
-type fn(A)          :: fun(() -> A).

-type fn(A, B)       :: fun((A) -> B).

-type fn(A, B, C)    :: fun((A, B) -> C).

-type map(A)         :: #{_ := A}.

-type either(A, B)   :: {error, A} | {ok, B}.

-type maybe(A)       :: {just, A} | nothing.

-type traversable(A) :: [A] | map(A).

-type applicative(A) :: [A]
                        | fn(_, A)
                        | either(_, A)
                        | maybe(A).

-type functor(A)     :: [A]
                        | map(A)
                        | fn(_, A)
                        | either(_, A)
                        | maybe(A).

-type monad(A)       :: [A]
                        | either(_, A)
                        | maybe(A).
