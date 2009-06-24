-module(test_suite).

-author("alex.borovsky@gmail.com").

-include_lib("eunit.hrl").

all_test_() ->
  [{module, element_br_test},
   {module, element_link_test}
  ].
