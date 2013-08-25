-module(tnetstring).

-export([encode/1, decode/1]).

encode(null) ->
    <<"0:~">>;
encode(true) ->
    <<"4:true!">>;
encode(false) ->
    <<"5:false!">>;
encode(Number) when is_integer(Number) ->
    Payload = list_to_binary(integer_to_list(Number)),
    Size = list_to_binary(integer_to_list(size(Payload))),
    <<Size/binary, <<":">>/binary, Payload/binary, <<"#">>/binary>>;
encode(Number) when is_float(Number) ->
    Payload = list_to_binary(io_lib:format("~p", [Number])),
    Size = list_to_binary(integer_to_list(size(Payload))),
    <<Size/binary, <<":">>/binary, Payload/binary, <<"^">>/binary>>;
encode(ByteString) when is_binary(ByteString) ->
    Size = list_to_binary(integer_to_list(size(ByteString))),
    <<Size/binary, <<":">>/binary, ByteString/binary, <<",">>/binary>>;
encode(Object) when is_tuple(hd(Object)) ->
    Payload = << <<Bin/binary>> || Bin <- encode_o(Object, []) >>,
    Size = list_to_binary(integer_to_list(size(Payload))),
    <<Size/binary, <<":">>/binary, Payload/binary, <<"}">>/binary>>;
encode(List) when is_list(List) ->
    Payload = << <<Bin/binary>> || Bin <- encode_l(List, []) >>,
    Size = list_to_binary(integer_to_list(size(Payload))),
    <<Size/binary, <<":">>/binary, Payload/binary, <<"]">>/binary>>.

encode_l([], Acc) ->
    lists:reverse(Acc);
encode_l([Head|Tail], Acc) ->
    encode_l(Tail, [encode(Head)|Acc]).

encode_o([], Acc) ->
    lists:reverse(Acc);
encode_o([{Key, Value}|Tail], Acc) ->
    encode_o(Tail, [encode(Value), encode(Key)|Acc]).

decode(<<Null:3/binary, Remain/binary>>) when Null =:= <<"0:~">> ->
    {null, Remain};
decode(<<True:7/binary, Remain/binary>>) when True =:= <<"4:true!">> ->
    {true, Remain}.

