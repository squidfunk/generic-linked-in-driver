-module (basic_test).

-include_lib("eunit/include/eunit.hrl").

setup_and_teardown_test()->
  {ok, Drv} = gen_driver_test:start_link(),
  gen_driver_test:stop(Drv).

basic_test() ->
  {ok, Drv} = gen_driver_test:start_link(),
  timer:sleep(500),
  {ok, Result} = gen_driver_test:sum(Drv,[1,2,97]), 
  ?assertEqual(100.0, Result),
  gen_driver_test:stop(Drv).


