module Avro.Types.Value exposing (Value(..))

import Bytes exposing (Bytes)


type Value t
    = Null
    | Boolean Bool
    | Int Int
    | Long Int
    | Float Float
    | Double Float



-- | Bytes Bytes
