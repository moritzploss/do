%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% @doc The Map Functor.
%%% @end
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%%_* Module declaration ======================================================
-module(do_map).

-behaviour(do_semigroup).
-behaviour(do_monoid).
-behaviour(do_functor).

%%%_* Exports =================================================================
-define(API, [ % semigroup
               append/2,
               % monoid
               mempty/0,
               % functor
               fmap/2,
               % map
               sequence/1
               ]).
-export(?API).
-ignore_xref(?API).

%%%_* Includes ================================================================
-include("do_internal.hrl").
-include("do_types.hrl").

%%%_* Code ====================================================================
%%%_* functor -----------------------------------------------------------------
%%%_* semigroup ---------------------------------------------------------------
-spec append(map(A), map(A)) -> map(A).
append(Map1, Map2) when is_map(Map1), is_map(Map2) -> maps:merge(Map1, Map2).

%%%_* monoid ------------------------------------------------------------------
-spec mempty() -> map(_).
mempty() -> #{}.

%%%_* functor -----------------------------------------------------------------
-spec fmap(fn(A, B), map(A)) -> map(B).
fmap(F, Map) when ?isF1(F) -> maps:map(fun(_, V) -> F(V) end, Map).

%%%_* map ---------------------------------------------------------------------
%% maps aren't traversable in two directions since they arent't instances of
%% the applicative type class (no implementation for pure/1). however, it's
%% possible to implement a one-directional sequence function
%% map(applicative) -> applicative(map)
-spec sequence(map(applicative(A))) -> applicative(map(A)).
sequence(Map) when is_map(Map) ->
  {Keys, [Val | _] = Vals} = lists:unzip(maps:to_list(Map)),
  ?Mod(Val):fmap(fun(Sequenced) ->
                    maps:from_list(lists:zip(Keys, Sequenced))
                 end,
                 do_list:sequence(Vals)).

%%%_* Tests ===================================================================
-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

sequence_map_test() ->
  F = fun(A) -> A + 1 end,
  ?assertEqual(#{a => 2, b => 3}, fmap(F, #{a => 1, b => 2})).

sequence_test() ->
  ?assertEqual({ok, #{a => 1, b => 2}}, sequence(#{a => {ok, 1}, b => {ok, 2}})),
  ?assertEqual(nothing,                 sequence(#{a => nothing, b => {ok, 2}})),
  ?assertEqual([#{a => 1, b => 2}],     sequence(#{a => [1], b => [2]})).

-endif.
