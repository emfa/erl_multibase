-module(multibase).

-define(BASE1, $1).
-define(BASE2, $0).
-define(BASE8, $7).
-define(BASE10, $9).
-define(BASE16, $F).
-define(base16, $f).
-define(BASE32, $U).
-define(base32, $u).
-define(BASE32HEX, $V).
-define(base32hex, $v).
-define(BASE58FLICKR, $Z).
-define(BASE58BTC, $z).
-define(BASE64, $y).
-define(BASE64URL, $Y).

-type base() :: base1 | base2 | base8 | base10 | base16
              | base32 | base32hex
              | base58flickr | base58btc
              | base64 | base64url.

-type multibase() :: unicode:chardata().

-export_type([base/0, multibase/0]).

-export([encode/1, encode/2, decode/1]).

-spec encode(binary()) -> multibase().
encode(Bin) ->
    encode(base64, Bin).

-spec encode(base(), binary()) -> multibase().
encode(base1, Bin) ->
    [varint:encode(?BASE1), encode_base_1(Bin)];
encode(base2, Bin) ->
    [varint:encode(?BASE2), integer_to_binary(binary:decode_unsigned(Bin), 2)];
encode(base8, Bin) ->
    [varint:encode(?BASE8), integer_to_binary(binary:decode_unsigned(Bin), 8)];
encode(base10, Bin) ->
   [varint:encode(?BASE10), integer_to_binary(binary:decode_unsigned(Bin), 10)];
encode(base16, Bin) ->
    [varint:encode(?BASE16), integer_to_binary(binary:decode_unsigned(Bin), 16)];
encode(base32, Bin) ->
    [varint:encode(?BASE32), base32:encode(Bin)];
encode(base32hex, Bin) ->
    [varint:encode(?BASE32HEX), base32:encode(Bin, [hex])];
encode(base58flickr, _Bin) ->
    {error, unimplemented};
encode(base58btc, _Bin) ->
    {error, unimplemented};
encode(base64, Bin) ->
    [varint:encode(?BASE64), base64:encode(Bin)];
encode(base64url, Bin) ->
    [varint:encode(?BASE64URL), base64url:encode(Bin)].

decode(MB0) ->
    {Base, MB1} = varint:decode(MB0),
    decode(Base, MB1).
                             
decode(?BASE1, Rest) ->
    binary:encode_unsigned(decode_base_1(Rest));
decode(?BASE2, Rest) ->
    binary:encode_unsigned(binary_to_integer(Rest, 2));
decode(?BASE8, Rest) ->
    binary:encode_unsigned(binary_to_integer(Rest, 8));
decode(?BASE10, Rest) ->
    binary:encode_unsigned(binary_to_integer(Rest, 10));
decode(Base, Rest) when Base == ?BASE16; Base == ?base16 ->
    binary:encode_unsigned(binary_to_integer(Rest, 16));
decode(Base, Rest) when Base == ?BASE32; Base == ?base32 ->
    binary:encode_unsigned(binary_to_integer(Rest, 32));
decode(?BASE58FLICKR, _Rest) ->
    {error, unimplemented};
decode(?BASE58BTC, _Rest) ->
    {error, unimplemented};
decode(?BASE64, Rest) ->
    base64:decode(Rest);
decode(?BASE64URL, Rest) ->
    base64url:decode(Rest).

encode_base_1(Bin) ->
    encode_int_base_1(binary:decode_unsigned(Bin), <<>>).

encode_int_base_1(0, Acc) ->
    Acc;
encode_int_base_1(X, Acc) ->
    encode_int_base_1(X - 1, <<Acc/binary, $1>>).

decode_base_1(Unary) ->
    decode_base_1(Unary, 0).

decode_base_1(<<>>, Ones) ->
    Ones;
decode_base_1(<<$1, Rest/binary>>, Ones) ->
    decode_base_1(Rest, Ones + 1).
