-module(test).

-export([testfun/1]).

testfun(X) ->
    Z = abs(X),
    Y = X + Z,
    case X of
      0 -> 0;
      Y -> false;
      _ -> true
    end.
