%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% @doc The Functor Type Class.
%%% @end
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%%_* Module declaration ======================================================
-module(do_functor).

%%%_* Exports =================================================================
-export([fmap/2]).

%%%_* Includes ================================================================
-include("include/do_macros.hrl").
-include("include/do_types.hrl").

%%%_* Callbacks ===============================================================
-callback fmap(fn(A, B), functor(A)) -> functor(B).

%%%_* Code ====================================================================
-spec fmap(fn(A, B), functor(A)) -> functor(B).
%%@doc fmap(F, Functor) is the result of calling F on every value in Functor.
fmap(F, List)                when ?isF1(F), is_list(List) -> lists:map(F, List);
fmap(F, Map)                 when ?isF1(F), is_map(Map)   -> maps:map(fun(_, V) -> F(V) end, Map);
fmap(F, Fn)                  when ?isF1(F), ?isF1(Fn)     -> do_fn:fmap(F, Fn);
fmap(F, {error, _} = Either) when ?isF1(F)                -> do_either:fmap(F, Either);
fmap(F, {ok, _} = Either)    when ?isF1(F)                -> do_either:fmap(F, Either);
fmap(F, error)               when ?isF1(F)                -> do_maybe:fmap(F, error).

%%%_* Tests ===================================================================
-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

functors(A) ->
  [ [A]
  , #{key => A}
  , fun(_) -> A end
  , {ok, A}
  , {error, reason}
  , error].
  
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

assertEqual(Expected, Actual) when is_function(Expected), is_function(Actual) ->
  assertEqual(Expected(arg), Actual(arg));
assertEqual(Expected, Actual) ->
  ?assertEqual(Expected, Actual).  

-endif.
