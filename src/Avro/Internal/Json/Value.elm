module Avro.Internal.Json.Value exposing (encodeValue, fieldDefaultDecoder)

import Avro.Internal.Result as ResultExtra exposing (recover)
import Avro.Types as Type exposing (Type)
import Avro.Types.Value as AvroV
import Base64
import Bytes
import Dict
import Json.Decode as Decode exposing (Decoder)
import Json.Encode as Json
import List.Nonempty as NEL


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

        AvroV.Bytes v ->
            Json.string <| Maybe.withDefault "" (Base64.fromBytes v)

        AvroV.Fixed _ v ->
            Json.string <| Maybe.withDefault "" (Base64.fromBytes v)

        AvroV.String v ->
            Json.string v

        AvroV.Union _ _ v ->
            encodeValue v

        AvroV.Array vs ->
            Json.list encodeValue vs

        AvroV.Map vs ->
            Json.dict identity encodeValue vs

        AvroV.Record _ vs ->
            Json.dict identity encodeValue vs

        AvroV.Enum _ v ->
            Json.string v


parseBytes str =
    case Base64.toBytes str of
        Just b ->
            Decode.succeed b

        Nothing ->
            Decode.fail ("Not valid binary data: " ++ str)


fieldDefaultDecoder : Type -> Decoder (AvroV.Value Type)
fieldDefaultDecoder typ =
    case typ of
        Type.Null ->
            Decode.map (\_ -> AvroV.Null) <| Decode.null "null"

        Type.Boolean ->
            Decode.map AvroV.Boolean Decode.bool

        Type.Int ->
            Decode.map AvroV.Int Decode.int

        Type.Long ->
            Decode.map AvroV.Long Decode.int

        Type.Float ->
            Decode.map AvroV.Float Decode.float

        Type.Double ->
            Decode.map AvroV.Double Decode.float

        Type.String ->
            Decode.map AvroV.String Decode.string

        Type.Bytes ->
            Decode.string
                |> Decode.andThen parseBytes
                |> Decode.map AvroV.Bytes

        Type.Fixed { size } ->
            Decode.string
                |> Decode.andThen parseBytes
                |> Decode.andThen
                    (\bytes ->
                        if Bytes.width bytes /= size then
                            Decode.fail
                                ("Fixed string wrong size. Expected "
                                    ++ String.fromInt size
                                    ++ " but got "
                                    ++ (String.fromInt <| Bytes.width bytes)
                                )

                        else
                            Decode.succeed bytes
                    )
                |> Decode.map (AvroV.Fixed typ)

        Type.Array { items } ->
            Decode.map AvroV.Array <| Decode.list (fieldDefaultDecoder items)

        Type.Map { values } ->
            Decode.map AvroV.Map <| Decode.dict (fieldDefaultDecoder values)

        Type.Union { options } ->
            Decode.oneOf (NEL.toList <| NEL.map fieldDefaultDecoder options)
                |> Decode.map (AvroV.Union options typ)

        Type.Record { fields } ->
            let
                lookupAndParseField values fld =
                    case Dict.get fld.name values of
                        Just v ->
                            case Decode.decodeValue (fieldDefaultDecoder fld.fieldType) v of
                                Ok fieldValue ->
                                    Decode.succeed ( fld.name, fieldValue )

                                Err err ->
                                    Decode.fail <| Decode.errorToString err

                        Nothing ->
                            Decode.fail ("Field not found: " ++ fld.name)

                sequenceDecoder : List (Decoder a) -> Decoder (List a)
                sequenceDecoder list =
                    let
                        parseJson json =
                            List.foldr
                                (\dec prev ->
                                    case Decode.decodeValue dec json of
                                        Ok elem ->
                                            Result.map (\ls -> elem :: ls) prev

                                        Err err ->
                                            Err err
                                )
                                (Ok [])
                                list
                    in
                    Decode.value |> Decode.andThen (\json -> ResultExtra.toJsonDecoder <| parseJson json)

                recordValue pairs =
                    AvroV.Record typ (Dict.fromList pairs)
            in
            Decode.dict Decode.value
                |> Decode.andThen
                    (\d -> sequenceDecoder <| List.map (lookupAndParseField d) fields)
                |> Decode.map recordValue

        Type.Enum { symbols } ->
            Decode.string
                |> Decode.andThen
                    (\sym ->
                        if not (List.member sym symbols) then
                            Decode.succeed <| AvroV.Enum typ sym

                        else
                            Decode.fail ("Value '" ++ sym ++ "' is not one of the expected symbols.")
                    )
