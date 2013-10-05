-module (basic_test).

-include_lib("eunit/include/eunit.hrl").

basic_test() ->
  gen_driver_test:start_link(),
  timer:sleep(500),
  ?assertEqual(100,gen_driver_test:sum(1,2,97)),
  gen_driver_test:stop().


