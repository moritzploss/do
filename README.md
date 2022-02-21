[![Tests](https://github.com/moritzploss/do/actions/workflows/tests.yml/badge.svg)](https://github.com/moritzploss/do/actions/workflows/tests.yml)

## do

This package brings Haskell-style type classes to Erlang, including
monads, applicatives, functors, and traversables. It provides implementations
of commonly used class instances, as well as useful utility functions.

### Installation

To install the latest release from [`hex`](https://hex.pm/packages/do), add
`do` to the `deps` in your rebar3 config file:

    {do, "1.10.2"}

### What's in the box

The `do` package implements [`either`](./src/instances/do_either.erl),
[`list`](./src/instances/do_list.erl), and
[`maybe`](./src/instances/do_maybe.erl) monads. For a complete overview
of types and functions, refer to [`do_types.hrl`](./include/do_types.hrl) and
[`do`'s docs on hex](https://hexdocs.pm/do/).

### fmap

Use the `?fmap` macro or `do:fmap/2` to map functions over functors:

```erlang
-include_lib("do/include/do.hrl").

increment(N) ->
  N + 1.

fmap_either() ->
  {ok, 2}         = ?fmap(fun increment/1, {ok, 1}),
  {error, reason} = ?fmap(fun increment/1, {error, reason}).

fmap_maybe() ->
  {just, 2} = ?fmap(fun increment/1, {just, 1}),
  nothing   = ?fmap(fun increment/1, nothing).

fmap_list() ->
  [2, 3, 4] = ?fmap(fun increment/1, [1, 2, 3]).

fmap_map() ->
  #{a => 2} = ?fmap(fun increment/1, #{a => 1}).
```

### sequence

Use the `?sequence` macro or `do:sequence/1` to sequence a traversable of
applicatives. For example:

```erlang
-include_lib("do/include/do.hrl").

sequence_list() ->
  {ok, [1, 2, 3]} = ?sequence([{ok, 1}, {ok, 2}, {ok, 3}]),
  {error, reason} = ?sequence([{ok, 1}, {error, reason}, {ok, 3}]).

sequence_map() ->
  {just, #{a => 1, b => 2}} = ?sequence(#{a => {just, 1}, b => {just, 2}}),
  nothing                   = ?sequence(#{a => {just, 1}, b => nothing}).

sequence_either() ->
  [{ok, 1}] = ?sequence({ok, [1]}),
  []        = ?sequence({ok, []}).

```

### bind

Use the `?bind` macro or `do:bind/2` to bind (`>>=`) a function that returns a
monad to a monad of the same type. In case of the
[`either`](./src/instances/do_either.erl) monad:

```erlang
-include_lib("do/include/do.hrl").

increment_either(N) when is_integer(N) ->
  {ok, N + 1};
increment_either(_) ->
  {error, no_int}.

bind_either() ->
  {ok, 2}         = ?bind({ok, 1}, fun increment_either/1),
  {error, no_int} = ?bind({ok, foo}, fun increment_either/1),
  {error, reason} = ?bind({error, reason}, fun increment_either/1).
```

For the [`maybe`](./src/instances/do_maybe.erl) monad:

```erlang
increment_maybe(N) when is_integer(N) ->
  {just, N + 1};
increment_maybe(_) ->
  nothing.

bind_maybe() ->
  {just, 2} = ?bind({just, 1}, fun increment_maybe/1),
  nothing   = ?bind({just, foo}, fun increment_maybe/1),
  nothing   = ?bind(nothing, fun increment_maybe/1).
```

For the [`list`](./src/instances/do_list.erl) monad:

```erlang
increment_list(N) when is_integer(N) ->
  [N + 1];
increment_maybe(_) ->
  [].

bind_list() ->
  [2] = ?bind([1], fun increment_list/1),
  []  = ?bind([foo], fun increment_list/1),
  []  = ?bind([], fun increment_maybe/1).
```

### then

Use the `?then` macro or `do:then/2` to chain (`>>`) monadic expressions of the
same type. The second argument to `?then` is wrapped in a thunk that will only
be executed if the first argument indicates success. For example:

```erlang
-include_lib("do/include/do.hrl").

increment(N) when is_integer(N) ->
  [N + 1];
increment(_) ->
  [].

then_list() ->
  [2] = ?then([5], increment(1)),
  []  = ?then([5], increment(foo)),
  []  = ?then([],  increment(1)).
```

### liftm

Use the `?liftm` macro to lift a function into a monad. For example:

```erlang
-include_lib("do/include/do.hrl").

liftm_either() ->
  {ok, 3}         = ?liftm(fun erlang:'+'/2, {ok, 1}, {ok, 2}),
  {error, reason} = ?liftm(fun erlang:'+'/2, {ok, 1}, {error, reason}).
```

Arguments to `?liftm` are evaluated lazily. In the following example
`1 + 1` will never be evaluated:

```erlang
-include_lib("do/include/do.hrl").

liftm_either() ->
  {error, reason} = ?liftm(fun erlang:'+'/2, {error, reason}, {ok, 1 + 1}).
```

### do

Use the `?do` macro or `do:do/2` to consecutively bind (`>>=` or `>>`) monads
and functions. The macro takes a start value (a monad), and a list of functions.
The functions must each take either 0 or 1 argument(s) and must return a monad.
On execution, the start value is passed to the first function, and is then
piped through consecutive functions using `bind` or `then`. For example (with
[`either`](./src/instances/do_either.erl) monad):

```erlang
-include_lib("do/include/do.hrl").

increment(N) when is_integer(N) ->
  {ok, N + 1};
increment(_) ->
  {error, no_int}.

int_to_bin(N) when is_integer(N) ->
  {ok, integer_to_binary(N)};
int_to_bin(_) ->
  {error, no_int}.

do_either() ->
  {ok, 4}         = ?do({ok, 1}, [ fun increment/1,
                                   fun increment/1,
                                   fun increment/1 ]),

  {error, no_int} = ?do({ok, 1}, [ fun increment/1,
                                   fun int_to_bin/1,
                                   fun increment/1 ]).
```
