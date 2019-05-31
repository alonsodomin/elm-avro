module Avro.Internal.Json.Types exposing (decodeType, encodeType)

import Array
import Avro.Internal.Json.Value exposing (..)
import Avro.Internal.Maybe exposing (catMaybes)
import Avro.Types as Type exposing (Field, Order(..), Type)
import Json.Decode as Decode exposing (Decoder)
import Json.Encode as Json
import List.Nonempty as NEL exposing (Nonempty)



-- Encoding operations


encodeType : Type -> Json.Value
encodeType typ =
    case typ of
        Type.Null ->
            Json.string "null"

        Type.Boolean ->
            Json.string "boolean"

        Type.Int ->
            Json.string "int"

        Type.Long ->
            Json.string "long"

        Type.Float ->
            Json.string "float"

        Type.Double ->
            Json.string "double"

        Type.Bytes ->
            Json.string "bytes"

        Type.String ->
            Json.string "string"

        Type.Array { items } ->
            Json.object [ ( "type", Json.string "array" ), ( "items", encodeType items ) ]

        Type.Map { values } ->
            Json.object [ ( "type", Json.string "map" ), ( "values", encodeType values ) ]

        Type.Union { options } ->
            Json.array encodeType (Array.fromList (NEL.toList options))

        Type.Record rec ->
            let
                opts =
                    catMaybes
                        [ Maybe.map (\x -> ( "order", encodeOrder x )) rec.order
                        , Maybe.map (\x -> ( "namespace", Json.string x )) rec.namespace
                        , Maybe.map (\x -> ( "doc", Json.string x )) rec.doc
                        ]
            in
            Json.object <|
                [ ( "type", Json.string "record" )
                , ( "name", Json.string rec.name )
                , ( "aliases", Json.list Json.string rec.aliases )
                , ( "fields", Json.list encodeField rec.fields )
                ]
                    ++ opts

        Type.Fixed fixed ->
            let
                opts =
                    catMaybes
                        [ Maybe.map (\x -> ( "namespace", Json.string x )) fixed.namespace ]
            in
            Json.object <|
                [ ( "type", Json.string "fixed" )
                , ( "name", Json.string fixed.name )
                , ( "aliases", Json.list Json.string fixed.aliases )
                , ( "size", Json.int fixed.size )
                ]
                    ++ opts

        Type.Enum enum ->
            let
                opts =
                    catMaybes
                        [ Maybe.map (\x -> ( "namespace", Json.string x )) enum.namespace
                        , Maybe.map (\x -> ( "doc", Json.string x )) enum.doc
                        ]
            in
            Json.object <|
                [ ( "type", Json.string "enum" )
                , ( "name", Json.string enum.name )
                , ( "aliases", Json.list Json.string enum.aliases )
                , ( "symbols", Json.list Json.string enum.symbols )
                ]
                    ++ opts


encodeOrder : Order -> Json.Value
encodeOrder order =
    case order of
        Ascending ->
            Json.string "ascending"

        Descending ->
            Json.string "descending"

        Ignore ->
            Json.string "ignore"


encodeField : Field -> Json.Value
encodeField field =
    let
        opts =
            catMaybes
                [ Maybe.map (\x -> ( "order", encodeOrder x )) field.order
                , Maybe.map (\x -> ( "doc", Json.string x )) field.doc
                , Maybe.map (\x -> ( "default", encodeValue x )) field.default
                ]
    in
    Json.object <|
        [ ( "name", Json.string field.name )
        , ( "type", encodeType field.fieldType )
        , ( "aliases", Json.list Json.string field.aliases )
        ]
            ++ opts



-- Decoding operations


decodeType : Decoder Type
decodeType =
    Decode.oneOf
        [ decodeNull
        , decodeBoolean
        , decodeInt
        , decodeLong
        , decodeFloat
        , decodeDouble
        , decodeBytes
        , decodeString
        , decodeArray
        , decodeMap
        , Decode.lazy (\_ -> decodeUnion)
        , Decode.lazy (\_ -> decodeRecord)
        , decodeFixed
        , decodeEnum
        ]


match : a -> ( String, t ) -> a -> Decoder t
match toMatch ( err, res ) value =
    if value == toMatch then
        Decode.succeed res

    else
        Decode.fail err


decodeFromString : (String -> Decoder t) -> Decoder t
decodeFromString f =
    Decode.string |> Decode.andThen f


decodeNull : Decoder Type
decodeNull =
    decodeFromString <| match "null" ( "Not a null", Type.Null )


decodeBoolean : Decoder Type
decodeBoolean =
    decodeFromString <| match "boolean" ( "Not a boolean", Type.Boolean )


decodeInt : Decoder Type
decodeInt =
    decodeFromString <| match "int" ( "Not an int", Type.Int )


decodeLong : Decoder Type
decodeLong =
    decodeFromString <| match "long" ( "Not a long", Type.Long )


decodeFloat : Decoder Type
decodeFloat =
    decodeFromString <| match "float" ( "Not a float", Type.Float )


decodeDouble : Decoder Type
decodeDouble =
    decodeFromString <| match "double" ( "Not a double", Type.Double )


decodeBytes : Decoder Type
decodeBytes =
    decodeFromString <| match "bytes" ( "Not bytes", Type.Bytes )


decodeString : Decoder Type
decodeString =
    decodeFromString <| match "string" ( "Not a string", Type.String )


decodeComplex : String -> String -> Decoder Type -> Decoder Type
decodeComplex typeName errMsg decoder =
    let
        handleTypeName typ =
            if typ == typeName then
                decoder

            else
                Decode.fail errMsg
    in
    Decode.field "type" Decode.string |> Decode.andThen handleTypeName


decodeArray : Decoder Type
decodeArray =
    decodeComplex "array" "Not an array" <|
        Decode.lazy (\_ -> Decode.field "items" decodeType |> Decode.map (\x -> Type.Array { items = x }))


decodeMap : Decoder Type
decodeMap =
    decodeComplex "map" "Not a map" <|
        Decode.lazy (\_ -> Decode.field "values" decodeType |> Decode.map (\x -> Type.Map { values = x }))


decodeUnion : Decoder Type
decodeUnion =
    let
        handleParsed parsed =
            case parsed of
                Just opts ->
                    Decode.succeed <| Type.Union { options = opts }

                Nothing ->
                    Decode.fail "Empty union!"
    in
    Decode.list decodeType
        |> Decode.map NEL.fromList
        |> Decode.andThen handleParsed


decodeRecord : Decoder Type
decodeRecord =
    let
        createRecord name namespace aliases doc order fields =
            Type.Record
                { name = name
                , namespace = namespace
                , aliases = aliases
                , doc = doc
                , order = order
                , fields = fields
                }
    in
    decodeComplex "record" "Not a record" <|
        Decode.map6 createRecord
            (Decode.field "name" Decode.string)
            (Decode.maybe <| Decode.field "namespace" Decode.string)
            (Decode.field "aliases" <| Decode.list Decode.string)
            (Decode.maybe <| Decode.field "doc" Decode.string)
            (Decode.maybe <| Decode.field "order" decodeOrder)
            (Decode.field "fields" <| Decode.list decodeField)


decodeFixed : Decoder Type
decodeFixed =
    let
        createFixed name namespace aliases size =
            Type.Fixed { name = name, namespace = namespace, aliases = aliases, size = size }
    in
    decodeComplex "fixed" "Not a fixed type" <|
        Decode.map4 createFixed
            (Decode.field "name" Decode.string)
            (Decode.maybe <| Decode.field "namespace" Decode.string)
            (Decode.field "aliases" <| Decode.list Decode.string)
            (Decode.field "size" Decode.int)


decodeEnum : Decoder Type
decodeEnum =
    let
        createEnum name namespace aliases doc symbols =
            Type.Enum { name = name, namespace = namespace, aliases = aliases, doc = doc, symbols = symbols }
    in
    decodeComplex "enum" "Not an enum" <|
        Decode.map5 createEnum
            (Decode.field "name" Decode.string)
            (Decode.maybe <| Decode.field "namespace" Decode.string)
            (Decode.field "aliases" <| Decode.list Decode.string)
            (Decode.maybe <| Decode.field "doc" Decode.string)
            (Decode.field "symbols" <| Decode.list Decode.string)


decodeField : Decoder Field
decodeField =
    let
        readFieldDefault typ =
            Decode.maybe <| Decode.field "default" (fieldDefaultDecoder typ)

        createField name aliases doc order typ =
            Decode.map (Field name aliases doc order typ) (readFieldDefault typ)
    in
    Decode.map5 createField
        (Decode.field "name" Decode.string)
        (Decode.field "aliases" <| Decode.list Decode.string)
        (Decode.maybe <| Decode.field "doc" Decode.string)
        (Decode.maybe <| Decode.field "order" decodeOrder)
        (Decode.field "type" decodeType)
        |> Decode.andThen identity


decodeOrder : Decoder Order
decodeOrder =
    let
        handleOrderString str =
            case str of
                "ascending" ->
                    Decode.succeed Ascending

                "descending" ->
                    Decode.succeed Descending

                "ignore" ->
                    Decode.succeed Ignore

                _ ->
                    Decode.fail <| "Invalid order value: " ++ str
    in
    Decode.string |> Decode.andThen handleOrderString
