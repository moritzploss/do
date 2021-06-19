-include("do_traversable.hrl").

-define(bind(Monad, F),       do:bind(Monad, F)).
-define(then(Monad1, Monad2), do:then(Monad1, fun() -> Monad2 end)).
-define(do(Monad, Fs),        do:do(Monad, Fs)).
