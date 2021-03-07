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
-endif.
