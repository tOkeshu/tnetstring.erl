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

encode_int_test() ->
    Payload = tnetstring:encode(123),
    ?assertEqual(<<"3:123#">>, Payload).

encode_float_test() ->
    Payload = tnetstring:encode(3.141592653589793),
    ?assertEqual(<<"17:3.141592653589793^">>, Payload).

encode_binary_test() ->
    Payload = tnetstring:encode(<<"Back to the Future">>),
    ?assertEqual(<<"18:Back to the Future,">>, Payload).

