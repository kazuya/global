-module(test).

-export([testfun/1]).

-define(MACRO0, "Macro0").
-define(MACRO1(Q, R), Q + R).

-record(person, {name, phone, address}).

testfun(X) ->
    Z = abs(X),
    Y = ?MACRO1(X, Z),
    case X of
      0 -> 0;
      Y -> #person{name="Foo", phone=123, address="Nowhere"};
      _ -> true
    end.
