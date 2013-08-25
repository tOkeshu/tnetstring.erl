-module(tnetstring).

-export([encode/1]).

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
encode(List) when is_list(List) ->
    Payload = << <<Bin/binary>> || Bin <- encode_l(List, []) >>,
    Size = list_to_binary(integer_to_list(size(Payload))),
    <<Size/binary, <<":">>/binary, Payload/binary, <<"]">>/binary>>.

encode_l([], Acc) ->
    lists:reverse(Acc);
encode_l([Head|Tail], Acc) ->
    encode_l(Tail, [encode(Head)|Acc]).

