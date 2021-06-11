[![Tests](https://github.com/moritzploss/do/actions/workflows/tests.yml/badge.svg)](https://github.com/moritzploss/do/actions/workflows/tests.yml)

## do

This package brings monads, applicatives, functors, and `do`-notation to
Erlang. It provides behaviours for related type classes, as well as
implementations for commonly used class instances.

### Installation

To install the latest version of `do` from [`hex`](https://hex.pm/packages/do),
put the following in your rebar config file:

    {do, "1.2.1"}

### What's in the box

By default, `do` provides implementations for `either`, `list` and `maybe`
monads, as well as a range of useful functors. See
[do_types.hrl](./include/do_types.hrl) for type definitions. 

### The fmap macro

The `?fmap` macro can be used to map functions over functors:

```erlang
-include_lib("do/include/do.hrl").

add1(N) -> N + 1.

fmap_example() ->
  {ok, 2}      = ?fmap(fun add1/1, {ok, 1}),
  {error, rsn} = ?fmap(fun add1/1, {error, rsn}),
  [2, 3, 4]    = ?fmap(fun add1/1, [1, 2, 3]),
  #{a => 2}    = ?fmap(fun add1/1, #{a => 1}).
```

### The do macro

The `?do` macro consecutively executes functions inside monads. The macro
takes a start value (a monad), and a list of functions. The functions must
each take either 0 or 1 argument(s) and must return a monad. On execution,
the start value is passed to the first function in the provided list, and
then piped through consecutive functions using `bind`.

```erlang
-include_lib("do/include/do.hrl").

maybe_add1(N) when N > 2 -> {error, N};
maybe_add1(N)            -> {ok, N + 1}.

do_either() ->
  {error, 3} = ?do({ok, 1}, [ fun maybe_add1/1,
                              fun maybe_add1/1,
                              fun maybe_add1/1,
                              fun maybe_add1/1]).
```
