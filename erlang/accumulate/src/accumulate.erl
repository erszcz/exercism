-module(accumulate).

-export([accumulate/2]).

accumulate(_Fn, []) -> [];
accumulate(Fn, [H|T]) -> [Fn(H) | accumulate(Fn, T)].
