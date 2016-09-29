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

-export([encode/2, decode/1]).

encode(base1, Bin) ->
    <<?BASE1, (encode_base_1(Bin))/binary>>;
encode(base2, Bin) ->
    <<?BASE2, (integer_to_binary(binary:decode_unsigned(Bin), 2))/binary>>;
encode(base8, Bin) ->
    <<?BASE8, (integer_to_binary(binary:decode_unsigned(Bin), 8))/binary>>;
encode(base10, Bin) ->
    <<?BASE10, (integer_to_binary(binary:decode_unsigned(Bin), 10))/binary>>;
encode(base16, Bin) ->
    <<?BASE16, (integer_to_binary(binary:decode_unsigned(Bin), 16))/binary>>;
encode(base32, Bin) ->
    <<?BASE32, (base32:encode(Bin))/binary>>;
encode(base32hex, Bin) ->
    <<?BASE32HEX, (base32:encode(Bin, [hex]))/binary>>;
encode(base58flickr, _Bin) ->
    {error, unimplemented};
encode(base58btc, _Bin) ->
    {error, unimplemented};
encode({base, 64}, Bin) ->
    <<?BASE64, (base64:encode(Bin))/binary>>;
encode(base64url, Bin) ->
    <<?BASE64URL, (base64url:encode(Bin))/binary>>.
                             
decode(<<?BASE1, Rest/binary>>) ->
    binary:encode_unsigned(decode_base_1(Rest));
decode(<<?BASE2, Rest/binary>>) ->
    binary:encode_unsigned(binary_to_integer(Rest, 2));
decode(<<?BASE8, Rest/binary>>) ->
    binary:encode_unsigned(binary_to_integer(Rest, 8));
decode(<<?BASE10, Rest/binary>>) ->
    binary:encode_unsigned(binary_to_integer(Rest, 10));
decode(<<Base, Rest/binary>>) when Base == ?BASE16; Base == ?base16 ->
    binary:encode_unsigned(binary_to_integer(Rest, 16));
decode(<<Base, Rest/binary>>) when Base == ?BASE32; Base == ?base32 ->
    binary:encode_unsigned(binary_to_integer(Rest, 32));
decode(<<?BASE58FLICKR, _Rest/binary>>) ->
    {error, unimplemented};
decode(<<?BASE58BTC, _Rest/binary>>) ->
    {error, unimplemented};
decode(<<?BASE64, Rest/binary>>) ->
    base64:decode(Rest);
decode(<<?BASE64URL, Rest/binary>>) ->
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

