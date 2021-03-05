%%%_* Types ===================================================================
-type fn(A)          :: fun(() -> A).

-type fn(A, B)       :: fun((A) -> B).

-type fn(A, B, C)    :: fun((A, B) -> C).

-type either(A, B)   :: {error, A} | {ok, B}.

-type maybe(A)       :: {ok, A} | error.

-type iterable(A)    :: [A] | #{_ := A}.

-type applicative(A) :: [A]
                        | fn(_, A)
                        | either(_, A)
                        | maybe(A).

-type functor(A)     :: [A]
                        | #{_ := A}
                        | fn(_, A)
                        | either(_, A)
                        | maybe(A).

-type monad(A)       :: [A]
                        | either(_, A)
                        | maybe(A).
