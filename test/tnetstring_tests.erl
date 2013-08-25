-module(tnetstring_tests).
-include_lib("eunit/include/eunit.hrl").

encode_null_test() ->
    Payload = tnetstring:encode(null),
    ?assertEqual(<<"0:~">>, Payload).

encode_true_test() ->
    Payload = tnetstring:encode(true),
    ?assertEqual(<<"4:true!">>, Payload).

encode_false_test() ->
    Payload = tnetstring:encode(false),
    ?assertEqual(<<"5:false!">>, Payload).

