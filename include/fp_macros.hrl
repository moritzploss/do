%%%_* Macros ==================================================================
-define(isF(F),  is_function(F)).
-define(isF0(F), is_function(F, 0)).
-define(isF1(F), is_function(F, 1)).
-define(isF2(F), is_function(F, 2)).
-define(isF3(F), is_function(F, 3)).

-define(doEither, fun fp_either:do/2).
-define(doMaybe, fun fp_maybe:do/2).

-define(ctx, element(2, process_info(self(), current_stacktrace))).
-define(liftA2, (fp_monad:liftA2_with_ctx(?ctx))).
-define(pure,   (fp_monad:pure_with_ctx(?ctx))).
-define(lift(F), fun(A) -> ?pure(F(A)) end).

%%%_* Emacs ===================================================================
%%% Local Variables:
%%% allout-layout: t
%%% erlang-indent-level: 2
%%% End:
