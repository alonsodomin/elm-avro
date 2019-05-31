module Avro.Json exposing (decodeType, encodeType)

import Avro.Internal.Json.Types as Types
import Avro.Types as Type exposing (Type)
import Json.Decode exposing (Decoder)
import Json.Encode as Json


encodeType : Type -> Json.Value
encodeType =
    Types.encodeType


decodeType : Decoder Type
decodeType =
    Types.decodeType
