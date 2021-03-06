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

encode_list_test() ->
    Payload = tnetstring:encode([<<"hello">>, 12345, 3.14, false]),
    ?assertEqual(<<"31:5:hello,5:12345#4:3.14^5:false!]">>, Payload).

encode_objects_test() ->
    Payload = tnetstring:encode([{<<"a">>, <<"hello">>},
                                 {<<"b">>, 12345},
                                 {<<"c">>, 3.14},
                                 {<<"d">>, false}]),
    ?assertEqual(<<"47:1:a,5:hello,1:b,5:12345#1:c,4:3.14^1:d,5:false!}">>, Payload).

encode_empty_object_test() ->
    Payload = tnetstring:encode([{}]),
    ?assertEqual(<<"0:}">>, Payload).

decode_null_test() ->
    {Term, Remain} = tnetstring:decode(<<"0:~">>),
    ?assertEqual(null, Term),
    ?assertEqual(<<>>, Remain).

decode_true_test() ->
    {Term, Remain} = tnetstring:decode(<<"4:true!">>),
    ?assertEqual(true, Term),
    ?assertEqual(<<>>, Remain).

decode_false_test() ->
    {Term, Remain} = tnetstring:decode(<<"5:false!">>),
    ?assertEqual(false, Term),
    ?assertEqual(<<>>, Remain).

decode_int_test() ->
    {Term, Remain} = tnetstring:decode(<<"11:12345678901#">>),
    ?assertEqual(12345678901, Term),
    ?assertEqual(<<>>, Remain).

decode_float_test() ->
    {Term, Remain} = tnetstring:decode(<<"17:3.141592653589793^">>),
    ?assertEqual(3.141592653589793, Term),
    ?assertEqual(<<>>, Remain).

decode_string_test() ->
    {Term, Remain} = tnetstring:decode(<<"18:Back to the Future,">>),
    ?assertEqual(<<"Back to the Future">>, Term),
    ?assertEqual(<<>>, Remain).

decode_list_test() ->
    {Term, Remain} = tnetstring:decode(<<"31:5:hello,5:12345#4:3.14^5:false!]">>),
    ?assertEqual([<<"hello">>, 12345, 3.14, false], Term),
    ?assertEqual(<<>>, Remain).

decode_object_test() ->
    {Term, Remain} = tnetstring:decode(<<"47:1:a,5:hello,1:b,5:12345#1:c,4:3.14^1:d,5:false!}">>),
    ?assertEqual([{<<"a">>, <<"hello">>},
                  {<<"b">>, 12345},
                  {<<"c">>, 3.14},
                  {<<"d">>, false}], Term),
    ?assertEqual(<<>>, Remain).

decode_empty_list_test() ->
    {Term, Remain} = tnetstring:decode(<<"0:]">>),
    ?assertEqual([], Term),
    ?assertEqual(<<>>, Remain).

decode_empty_object_test() ->
    {Term, Remain} = tnetstring:decode(<<"0:}">>),
    ?assertEqual([{}], Term),
    ?assertEqual(<<>>, Remain).

remain_test() ->
    {Term, Remain} = tnetstring:decode(<<"12:1:1#1:2#1:3#]1:4#">>),
    ?assertEqual([1, 2, 3], Term),
    ?assertEqual(<<"1:4#">>, Remain),
    {Term2, Remain2} = tnetstring:decode(Remain),
    ?assertEqual(4, Term2),
    ?assertEqual(<<>>, Remain2).

