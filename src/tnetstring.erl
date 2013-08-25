-module(tnetstring).

-export([encode/1]).

encode(null) ->
    <<"0:~">>.

