-module(example_test).

-include_lib("eunit/include/eunit.hrl").

example_hello_test() -> ?assert('Elixir.Example':hello() =:= <<"Hello World!">>).
