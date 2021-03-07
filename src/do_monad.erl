%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% @doc
%%% @end
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%%_* Module declaration ======================================================
-module(do_monad).

%%%_* Exports =================================================================
-export([pure_with_ctx/1]).
-export([lift_with_ctx/1]).
-export([liftA2_with_ctx/1]).

%%%_* Includes ================================================================
-include("include/do_macros.hrl").
-include("include/do.hrl").

%%%_* Callbacks ===============================================================
-callback bind(fn(A, monad(B)), monad(A)) -> monad(B).
-callback do(monad(_), list(fn(_, monad(_)))) -> monad(_).
-callback then(fn(monad(A)), monad(_)) -> monad(A).
-callback lift(fn(A, B)) -> fn(A, monad(B)).

%%%_* Code ====================================================================
%%%_* API ---------------------------------------------------------------------
pure_with_ctx(Ctx) ->
  Mod = get_module(Ctx),
  fun Mod:pure/1.

lift_with_ctx(Ctx) ->
  Mod = get_module(Ctx),
  fun Mod:lift/1.

liftA2_with_ctx(Ctx) ->
  Mod = get_module(Ctx),
  fun Mod:liftA2/3.

%%%_* Internal ----------------------------------------------------------------
is_monad({do_maybe, bind, 2, _File})  -> true;
is_monad({do_either, bind, 2, _File}) -> true;
is_monad(_Other)                      -> false.

get_module(Ctx) ->
  OnlyMonads            = lists:filter(fun is_monad/1, Ctx),
  {Mod, bind, 2, _File} = lists:keyfind(bind, 2, OnlyMonads),
  Mod.

%%%_* Tests ===================================================================
-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

do_pure_test() ->
  Fun0 = fun() -> ?pure(3) end,
  Fun = fun(A) -> ?pure(A + 1) end,
  ?assertEqual({ok, 4},         ?doEither({ok, 3},         [Fun])),
  ?assertEqual({ok, 4},         ?doEither({ok, 3},         [Fun0, Fun])),
  ?assertEqual({error, reason}, ?doEither({error, reason}, [Fun])),
  ?assertEqual({ok, 4},         ?doMaybe({ok, 3},          [Fun])),
  ?assertEqual(error,           ?doMaybe(error,            [Fun])).

do_lift_test() ->
  Fun = fun(A) -> A + 1 end,
  ?assertEqual({ok, 4},         ?doEither({ok, 3},         [?lift(Fun)])),
  ?assertEqual({error, reason}, ?doEither({error, reason}, [?lift(Fun)])),
  ?assertEqual({ok, 4},         ?doMaybe({ok, 3},          [?lift(Fun)])),
  ?assertEqual(error,           ?doMaybe(error,            [?lift(Fun)])).

-endif.
