%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% @doc The Functor Type Class.
%%% @end
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%%_* Module declaration ======================================================
-module(do_functor).

%%%_* Exports =================================================================
-define(API, [fmap/2]).
-export(?API).
-ignore_xref(?API).


%%%_* Includes ================================================================
-include("do_guards.hrl").
-include("do_types.hrl").

%%%_* Callbacks ===============================================================
-callback fmap(fn(A, B), functor(A)) -> functor(B).

%%%_* Code ====================================================================
%%%_* API ---------------------------------------------------------------------
-spec fmap(fn(A, B), functor(A)) -> functor(B).
fmap(F, Functor)        when ?isF1(F), is_list(Functor) -> do_list:fmap(F, Functor);
fmap(F, Functor)        when ?isF1(F), is_map(Functor)  -> do_map:fmap(F, Functor);
fmap(F, Functor)        when ?isF1(F), ?isF1(Functor)   -> do_fn:fmap(F, Functor);
fmap(F, {error, _} = E) when ?isF1(F)                   -> do_either:fmap(F, E);
fmap(F, {ok, _} = E)    when ?isF1(F)                   -> do_either:fmap(F, E);
fmap(F, nothing = M)    when ?isF1(F)                   -> do_maybe:fmap(F, M);
fmap(F, {just, _} = M)  when ?isF1(F)                   -> do_maybe:fmap(F, M).

%%%_* Tests ===================================================================
-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

functors(A) ->
  [ [A]
  , #{key => A}
  , fun(_) -> A end
  , {ok, A}
  , {error, reason}
  , {just, A}
  , nothing].
  
preserve_identity_morphism_test() ->
  Id       = fun(Term) -> Term end,
  CheckLaw = fun(Functor) -> assertEqual(Functor, fmap(Id, Functor)) end,
  lists:foreach(CheckLaw, functors({ok, 2})).

preserve_composition_of_morphisms_test() ->
  F        = fun({ok, Value}) -> {ok, Value - 1}                              end,
  G        = fun({ok, Value}) -> {ok, Value * 2}                              end,
  Composed = fun(Functor) -> fmap(fun(X) -> F(G(X)) end, Functor)             end,
  Chained  = fun(Functor) -> fmap(F, fmap(G, Functor))                        end,
  CheckLaw = fun(Functor) -> assertEqual(Chained(Functor), Composed(Functor)) end,
  lists:foreach(CheckLaw, functors({ok, 2})).
  
basic_test() ->
  F = fun(X) -> X + 1 end,
  ?assertEqual([2, 3],            fmap(F, [1, 2])),
  ?assertEqual(#{a => 2, b => 3}, fmap(F, #{a => 1, b => 2})),
  ?assertEqual(2,                 (fmap(F, fun(_) -> 1 end))(arg)).

assertEqual(Expected, Actual) when ?isF(Expected), ?isF(Actual) ->
  assertEqual(Expected(arg), Actual(arg));
assertEqual(Expected, Actual) ->
  ?assertEqual(Expected, Actual).  

-endif.
