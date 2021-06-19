-define(isF(F),     is_function(F)).
-define(isF(F, A),  is_function(F, A)).

-define(isF0(F),    is_function(F, 0)).
-define(isF1(F),    is_function(F, 1)).
-define(isF2(F),    is_function(F, 2)).
-define(isF3(F),    is_function(F, 3)).

-define(thunk(A),   fun() -> A end).
-define(isThunk(A), ?isF0(A)).

-define(Mod(A),     (do:mod(A))).