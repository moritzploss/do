%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% @doc The Functor Type Class.
%%% @end
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%%_* Module declaration ======================================================
-module(fp_functor).

%%%_* Exports =================================================================
-export([fmap/2]).

%%%_* Includes ================================================================
-include("include/fp_macros.hrl").
-include("include/fp_types.hrl").

%%%_* Callbacks ===============================================================
-callback fmap(fn(A, B), functor(A)) -> functor(B).

%%%_* Code ====================================================================
-spec fmap(fn(A, B), functor(A)) -> functor(B).
%%@doc fmap(F, Functor) is the result of calling F on every value in Functor.
fmap(F, List)                when ?isF1(F), is_list(List) -> lists:map(F, List);
fmap(F, Map)                 when ?isF1(F), is_map(Map)   -> maps:map(fun(_, V) -> F(V) end, Map);
fmap(F, Fn)                  when ?isF1(F), ?isF1(Fn)     -> fp_fn:fmap(F, Fn);
fmap(F, {error, _} = Either) when ?isF1(F)                -> fp_either:fmap(F, Either);
fmap(F, {ok, _} = Either)    when ?isF1(F)                -> fp_either:fmap(F, Either);
fmap(F, error)               when ?isF1(F)                -> fp_maybe:fmap(F, error).

%%%_* Tests ===================================================================
-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.
