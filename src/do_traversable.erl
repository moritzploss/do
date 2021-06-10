%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% @doc The Traversable Type Class.
%%% @end
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%%_* Module declaration ======================================================
-module(do_traversable).

%%%_* Exports =================================================================
-define(API, [ sequence/2,
               sequence_lazy/2,
               traverse/2]).
-export(?API).
-ignore_xref(?API).

%%%_* Includes ================================================================
-include("do_macros.hrl").
-include("do_types.hrl").

%%%_* Macros ==================================================================
-define(LAZY,    lazy).
-define(THUNK,   thunk).
-define(DEFAULT, default).

%%%_* Code ====================================================================
%%%_* API ---------------------------------------------------------------------
-spec sequence(traversable(applicative(A)), atom()) ->
  applicative(traversable(A)).
sequence(Traversable, Mod) ->
  sequence(Traversable, Mod, ?DEFAULT).

-spec sequence_lazy(traversable(fn(applicative(A))), atom()) ->
  applicative(traversable(A)).
sequence_lazy(Traversable, Mod) ->
  sequence(Traversable, Mod, ?LAZY).

-spec traverse(fn(A, applicative(B)), traversable(A)) ->
  applicative(traversable(B)).
traverse(F, Traversable) when ?isF1(F) -> do_functor:fmap(F, Traversable).

%%%_* internal ----------------------------------------------------------------
sequence([F | Rest], Mod, ?LAZY) when ?isF0(F) ->
  sequence([F() | Rest], Mod, ?THUNK);
sequence([Elm | Rest], Mod, Mode) ->
  Applicative = Mod:fmap(fun(Val) -> fun(Vals) -> [Val | Vals] end end, Elm),
  Mod:liftA2(Applicative, sequence(Rest, Mod, reset(Mode)));
sequence([], Mod, _Mode) ->
  Mod:pure([]);
sequence(Map, Mod, Mode) when is_map(Map) ->
  {Keys, Vals} = lists:unzip(maps:to_list(Map)),
  Mod:fmap(fun(Sequenced) -> maps:from_list(lists:zip(Keys, Sequenced)) end,
           sequence(Vals, Mod, Mode)).

reset(?LAZY)    -> ?LAZY;
reset(?THUNK)   -> ?LAZY;
reset(?DEFAULT) -> ?DEFAULT.

%%%_* Tests ===================================================================
-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

traverse_test() ->
  F = fun(A) -> A + 1 end,
  ?assertEqual([1, 2, 3], traverse(F, [0, 1, 2])),
  ?assertEqual(#{a => 1, b => 2, c => 3}, traverse(F, #{a => 0, b => 1, c => 2})).

-endif.
