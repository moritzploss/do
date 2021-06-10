[![Tests](https://github.com/moritzploss/do/actions/workflows/tests.yml/badge.svg)](https://github.com/moritzploss/do/actions/workflows/tests.yml)

## do

This package brings `do`-notation and related monads to Erlang.

## Quick Start

The `either` type is defined as a tuple of either `{ok, Value}` or
`{error, Reason}`, and is a member of the functor and monad type classes.
The `?fmap` macro can be used to map functions over functors:

```erlang
-include_lib("do/include/do.hrl").

add1(Num) -> Num + 1.

fmap_example() ->
  {ok, 2}      = ?fmap(fun add1/1, {ok, 1}),
  {error, rsn} = ?fmap(fun add1/1, {error, rsn}),
  [2, 3, 4]    = ?fmap(fun add1/1, [1, 2, 3]),
  #{a => 2}    = ?fmap(fun add1/1, #{a => 1}).
```

The `?do` macro consecutively executes functions inside the `either` monad.
The macro takes a start value, and a list of functions. The functions must
each take either 0 or 1 argument(s) and must return an `either` monad. On
execution, the start value is passed to the first function in the provided
list. If the function returns `{ok, Val}`, the second function is called with
`Val`, and so on:

```erlang
-include_lib("do/include/do.hrl").

maybe_add1(N) when N > 5 -> {error, greater_five};
maybe_add1(N)            -> {ok, N + 1}.

do_example1() ->
  {ok, 3} = ?do(0, [ fun maybe_add1/1,
                     fun maybe_add1/1,
                     fun maybe_add1/1]).
```

If a function returns an error tuple `{error, Reason}`, the error is returned
immediately.

```erlang
do_example2() ->
  {error, greater_five} = ?do(0, [ fun maybe_add1/1,
                                   fun maybe_add1/1,
                                   fun maybe_add1/1,
                                   fun maybe_add1/1,
                                   fun maybe_add1/1,
                                   fun maybe_add1/1]).
```


