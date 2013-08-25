-module(tnetstring).

-export([encode/1]).

encode(null) ->
    <<"0:~">>;
encode(true) ->
    <<"4:true!">>;
encode(false) ->
    <<"5:false!">>.

