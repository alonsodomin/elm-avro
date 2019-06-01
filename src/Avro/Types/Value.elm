module Avro.Types.Value exposing (Value(..))

import Bytes exposing (Bytes)
import Dict exposing (Dict)
import List.Nonempty exposing (Nonempty)


type Value t
    = Null
    | Boolean Bool
    | Int Int
    | Long Int
    | Float Float
    | Double Float
    | Bytes Bytes
    | String String
    | Array (List (Value t))
    | Map (Dict String (Value t))
    | Record t (Dict String (Value t))
    | Union (Nonempty t) t (Value t)
    | Fixed t Bytes
    | Enum t String
