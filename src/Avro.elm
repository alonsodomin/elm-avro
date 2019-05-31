module Avro exposing (Schema, prettyPrint, toString)

import Avro.Json exposing (..)
import Avro.Types exposing (Type)
import Json.Decode exposing (Decoder)
import Json.Encode as Json


type alias Schema =
    Type


toString : Schema -> String
toString schema =
    Json.encode 0 (encodeType schema)


prettyPrint : Schema -> String
prettyPrint schema =
    Json.encode 4 (encodeType schema)
