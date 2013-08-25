-module(tnetstring_tests).
-include_lib("eunit/include/eunit.hrl").

encode_null_test() ->
    Payload = tnetstring:encode(null),
    ?assertEqual(<<"0:~">>, Payload).

