module Avro.Internal.Result exposing (recover, toJsonDecoder)

import Json.Decode as Decode exposing (Decoder)


recover : (e -> Result ee a) -> Result e a -> Result ee a
recover f result =
    case result of
        Ok v ->
            Ok v

        Err e ->
            f e


toJsonDecoder : Result Decode.Error a -> Decoder a
toJsonDecoder result =
    case result of
        Ok value ->
            Decode.succeed value

        Err msg ->
            Decode.fail <| Decode.errorToString msg
