%% @author Tom Burdick <thomas.burdick@gmail.com>
%% @copyright 2011 Tom Burdick 
%% @doc Uuid functions.

-module(bf_uuid).

%% api
-export([v4/0, to_formatted/1, from_formatted/1]).

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.


%% ------------------------------------------------------------------
%% api
%% ------------------------------------------------------------------

%% @doc Generate a cryptographically random UUID version 4 (random) binary.
-spec v4() -> Uuid::binary().
v4() ->
    v4(crypto:rand_uniform(0, (1 bsl 48 - 1)),
       crypto:rand_uniform(0, (1 bsl 12 - 1)),
       crypto:rand_uniform(0, (1 bsl 32 - 1)),
       crypto:rand_uniform(0, (1 bsl 30 - 1))).
v4(R1, R2, R3, R4) ->
    <<R1:48, 4:4, R2:12, 2:2, R3:32, R4: 30>>.

%% @doc Converts a binary uuid to a binary formatted with dashes.
-spec to_formatted(Uuid::binary()) -> Uuid::binary().
to_formatted(U) ->
    list_to_binary(lists:flatten(io_lib:format("~8.16.0b-~4.16.0b-~4.16.0b-~2.16.0b~2.16.0b-~12.16.0b", get_parts(U)))).

% %% @doc Converts a UUID binary in the format of 550e8400-e29b-41d4-a716-446655440000
% (with or without the dashes) to binary.
-spec from_formatted(Uuid::binary()) -> Uuid::binary().
from_formatted(UBin)->
    U = binary_to_list(UBin),
    convert(lists:filter(fun(Elem) -> Elem /= $- end, U), []).


%% ------------------------------------------------------------------
%% private api
%% ------------------------------------------------------------------

% Converts a list of pairs of hex characters (00-ff) to bytes.
convert([], Acc)->
    list_to_binary(lists:reverse(Acc));
convert([X, Y | Tail], Acc)->
    {ok, [Byte], _} = io_lib:fread("~16u", [X, Y]),
    convert(Tail, [Byte | Acc]).

get_parts(<<TL:32, TM:16, THV:16, CSR:8, CSL:8, N:48>>) ->
    [TL, TM, THV, CSR, CSL, N].


%% ------------------------------------------------------------------
%% unit tests
%% ------------------------------------------------------------------

-ifdef(TEST).

uuid_test() ->
    Uuid = v4(),
    UuidFormatted = to_formatted(Uuid),
    UuidUnformatted = from_formatted(UuidFormatted),
    ?assertEqual(Uuid, UuidUnformatted).

-endif.
