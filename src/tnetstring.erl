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
    <<Size/binary, <<":">>/binary, Payload/binary, <<"#">>/binary>>.

