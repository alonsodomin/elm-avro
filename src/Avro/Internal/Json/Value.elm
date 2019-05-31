module Avro.Internal.Json.Value exposing (encodeValue, fieldDefaultDecoder)

import Avro.Internal.Result exposing (recover)
import Avro.Types as Type exposing (Type)
import Avro.Types.Value as AvroV
import Json.Decode as Decode exposing (Decoder)
import Json.Encode as Json


encodeValue : AvroV.Value t -> Json.Value
encodeValue avroV =
    case avroV of
        AvroV.Null ->
            Json.string "null"

        AvroV.Boolean v ->
            Json.bool v

        AvroV.Int v ->
            Json.int v

        AvroV.Long v ->
            Json.int v

        AvroV.Float v ->
            Json.float v

        AvroV.Double v ->
            Json.float v


fieldDefaultDecoder : Type -> Decoder (AvroV.Value Type)
fieldDefaultDecoder typ =
    Decode.value
        |> Decode.andThen
            (\json ->
                case decodeFieldDefault typ json of
                    Ok value ->
                        Decode.succeed value

                    Err msg ->
                        Decode.fail msg
            )


typeMismatch actual expected =
    Err ("Found value '" ++ Debug.toString actual ++ "' when expecting type: " ++ Debug.toString expected)


tryDecode : Decoder a -> Json.Value -> (a -> Result String (AvroV.Value Type)) -> Result String (AvroV.Value Type)
tryDecode decoder json handler =
    case Decode.decodeValue decoder json of
        Ok value ->
            handler value

        Err err ->
            Err (Decode.errorToString err)


decodeFieldDefault : Type -> Json.Value -> Result String (AvroV.Value Type)
decodeFieldDefault fieldType json =
    let
        decodeNull =
            tryDecode (Decode.null "null") json <|
                \_ ->
                    case fieldType of
                        Type.Null ->
                            Ok AvroV.Null

                        _ ->
                            typeMismatch "null" fieldType

        decodeBoolean =
            tryDecode Decode.bool json <|
                \b ->
                    case fieldType of
                        Type.Boolean ->
                            Ok (AvroV.Boolean b)

                        _ ->
                            typeMismatch b fieldType

        decodeInteger =
            tryDecode Decode.int json <|
                \num ->
                    case fieldType of
                        Type.Int ->
                            Ok (AvroV.Int num)

                        Type.Long ->
                            Ok (AvroV.Long num)

                        _ ->
                            typeMismatch num fieldType

        decodeFloatingPoint =
            tryDecode Decode.float json <|
                \num ->
                    case fieldType of
                        Type.Float ->
                            Ok (AvroV.Float num)

                        Type.Double ->
                            Ok (AvroV.Double num)

                        _ ->
                            typeMismatch num fieldType
    in
    decodeNull
        |> recover (\_ -> decodeBoolean)
        |> recover (\_ -> decodeInteger)
        |> recover (\_ -> decodeFloatingPoint)
