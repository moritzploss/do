[![Tests](https://github.com/moritzploss/do/actions/workflows/tests.yml/badge.svg)](https://github.com/moritzploss/do/actions/workflows/tests.yml)

## do

This package brings Haskell-style type classes to Erlang, including
monads, applicatives, functors, and traversables. It provides implementations
of commonly used class instances, as well as useful utility functions.

### Installation

To install the latest release from [`hex`](https://hex.pm/packages/do), add
`do` to the `deps` in your rebar3 config file:

    {do, "1.10.1"}

### What's in the box

The `do` package implements [`either`](./src/do_either.erl), [`list`](./src/do_list.erl) and [`maybe`](./src/do_maybe.erl) monads. For a complete overview
of types and functions, refer to [`do_types.hrl`](./include/do_types.hrl) and [`do`'s docs on hex](https://hexdocs.pm/do/).

### fmap

Use the `?fmap` macro or `do:fmap/2` to map functions over functors:

```erlang
-include_lib("do/include/do.hrl").

add1(N) -> N + 1.

fmap_example() ->
  {ok, 2}         = ?fmap(fun add1/1, {ok, 1}),
  {error, reason} = ?fmap(fun add1/1, {error, reason}),
  {just, 2}       = ?fmap(fun add1/1, {just, 1}),
  nothing         = ?fmap(fun add1/1, nothing),
  [2, 3, 4]       = ?fmap(fun add1/1, [1, 2, 3]),
  #{a => 2}       = ?fmap(fun add1/1, #{a => 1}).
```

### bind

Use the `?bind` macro or `do:bind/2` to bind (`>>=`) a function that returns a
monad to a monad of the same type. For example (with
[`either`](./src/do_either.erl) monad):

```erlang
-include_lib("do/include/do.hrl").

maybe_add1(1) -> {ok, 2};
maybe_add1(_) -> {error, not_1}.

bind_example() ->
  {ok, 2}        = ?bind({ok, 1}, fun maybe_add1/1),
  {error, not_1} = ?bind({ok, 2}, fun maybe_add1/1).
```

### then

Use the `?then` macro or `do:then/2` to chain (`>>`) monadic expressions of the
same type. The second argument to `?then` is wrapped in a thunk that will only
be executed if the first argument indicates success. For example (with
[`list`](./src/do_list.erl) monad):

```erlang
-include_lib("do/include/do.hrl").

maybe_add1(1) -> [2];
maybe_add1(_) -> [].

then_example() ->
  []  = ?then([],  maybe_add1(1)),
  []  = ?then([5], maybe_add1(2)),
  [2] = ?then([5], maybe_add1(1)).
```

### do

Use the `?do` macro or `do:do/2` to consecutively bind (`>>=` or `>>`) monads
and functions. The macro takes a start value (a monad), and a list of functions.
The functions must each take either 0 or 1 argument(s) and must return a monad.
On execution, the start value is passed to the first function, and is then
piped through consecutive functions using `bind` or `then`. For example (with
[`maybe`](./src/do_maybe.erl) monad):

```erlang
-include_lib("do/include/do.hrl").

maybe_add1(N) when N > 2 -> nothing;
maybe_add1(N)            -> {just, N + 1}.

do_example() ->
  nothing = ?do({just, 1}, [ fun maybe_add1/1,
                             fun maybe_add1/1,
                             fun maybe_add1/1,
                             fun maybe_add1/1]).
```
