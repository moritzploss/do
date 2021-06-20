[![Tests](https://github.com/moritzploss/do/actions/workflows/tests.yml/badge.svg)](https://github.com/moritzploss/do/actions/workflows/tests.yml)

## do

This package brings monads, applicatives, functors, and `do`-notation to
Erlang. Inspired by Haskell's type system, it provides behaviours for a range
of useful type classes, as well as implementations for commonly used class
instances.

### Installation

To install the latest release from [`hex`](https://hex.pm/packages/do),
add `do` to the `deps` in your rebar config file:

    {do, "1.10.0"}

### What's in the box

The `do` package provides implementations for [`either`](./src/do_either.erl),
[`list`](./src/do_list.erl) and [`maybe`](./src/do_maybe.erl) monads, as well
as a range of useful functors. See [do_types.hrl](./include/do_types.hrl) for
type definitions, and [hex docs](https://hexdocs.pm/do/) for a complete
overview of functions and types.

### The fmap macro

The `?fmap` macro can be used to map functions over functors:

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

### The bind macro

The `?bind` macro can be used to bind (`>>=`) a function that returns
a monad to a monad of the same type. For example (with
[`either`](./src/do_either.erl) monad):

```erlang
-include_lib("do/include/do.hrl").

maybe_add1(1) -> {ok, 2};
maybe_add1(_) -> {error, not_1}.

bind_example() ->
  {ok, 2}        = ?bind({ok, 1}, fun maybe_add1/1),
  {error, not_1} = ?bind({ok, 2}, fun maybe_add1/1).
```

### The then macro

The `?then` macro can be used to chain (`>>`) monadic expressions of the same
type. The second argument to `?then` is automatically wrapped in a thunk that
will only be executed if the first argument indicates success. For example
(with [`list`](./src/do_list.erl) monad):

```erlang
-include_lib("do/include/do.hrl").

maybe_add1(1) -> [2];
maybe_add1(_) -> [].

then_example() ->
  []  = ?then([],  maybe_add1(1)),
  []  = ?then([5], maybe_add1(2)),
  [2] = ?then([5], maybe_add1(1)).
```

### The do macro

The `?do` macro consecutively binds (`>>=` or `>>`) monads and functions. The
macro takes a start value (a monad), and a list of functions. The functions
must each take either 0 or 1 argument(s) and must return a monad. On execution,
the start value is passed to the first function, and is then piped through
consecutive functions using `bind` or `then`. For example (with
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
