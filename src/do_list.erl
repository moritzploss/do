%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% @doc The List Monad.
%%% @end
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%%_* Module declaration ======================================================
-module(do_list).

-behaviour(do_functor).
-behaviour(do_applicative).
-behaviour(do_monad).

%%%_* Exports =================================================================
-define(API, [ bind/2,
               do/2,
               fmap/2,
               lift/1,
               liftA2/2,
               liftm/2,
               pure/1,
               sequence/1,
               then/2]).
-export(?API).
-ignore_xref(?API).

%%%_* Includes ================================================================
-include("do_macros.hrl").
-include("do_types.hrl").

%%%_* Code ====================================================================
%%%_* functor -----------------------------------------------------------------
-spec fmap(fn(A, B), [A]) -> [B].
fmap(F, List) when ?isF1(F) -> lists:map(F, List).

%%%_* applicative -------------------------------------------------------------
-spec liftA2([fn(A, B)], [A]) -> [B].
liftA2(List1, List2) ->
  flat(fmap(fun(E2) -> fmap(fun(F) -> F(E2) end, List1) end, List2)).

-spec pure(A) -> [A].
pure(A) -> [A].

-spec sequence(traversable([A])) -> [traversable(A)].
sequence(Lists) -> do_traversable:sequence(Lists, ?MODULE).

%%%_* monad -------------------------------------------------------------------
-spec bind(fn(A, [B]), [A]) -> [B].
bind(F, List) when ?isF1(F) -> flat(fmap(F, List)).

-spec do([A], list(fn(A, [B]) | fn([B]))) -> [B].
do(List, Fs) -> do_monad:do(List, Fs, [?MODULE]).

-spec lift(fn(A, B)) -> fn(monad(A), monad(B)).
lift(F) -> do_monad:lift(F, ?MODULE).

-spec liftm(fun(), [[_]] | [fn([_])]) -> [_].
liftm(F, Lists) -> do_monad:liftm(F, Lists, ?MODULE).

-spec then(fn([A]), list()) -> [A].
then(F, List) -> do_monad:then(F, List, ?MODULE).

%%%_* internal ----------------------------------------------------------------
flat(List) -> lists:concat(List).

%%%_* Tests ===================================================================
-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

liftA2_test() ->
  Applicative = fmap(fun(E1) -> fun(E2) -> E1 + E2 end end, [-1, 0, 1]),
  ?assertEqual([0, 1, 2, 1, 2, 3, 2, 3, 4], liftA2(Applicative, [1, 2, 3])).

-endif.
