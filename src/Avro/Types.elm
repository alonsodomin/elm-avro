module Avro.Types exposing (Field, Order(..), Type(..))

import Avro.Types.Value exposing (Value)
import List.Nonempty as NEL exposing (Nonempty)


type alias TypeName =
    String


type Type
    = Null
    | Boolean
    | Int
    | Long
    | Float
    | Double
    | Bytes
    | String
    | Array { items : Type }
    | Map { values : Type }
    | Union { options : Nonempty Type }
    | Record
        { name : TypeName
        , namespace : Maybe String
        , aliases : List TypeName
        , doc : Maybe String
        , order : Maybe Order
        , fields : List Field
        }
    | Fixed
        { name : TypeName
        , namespace : Maybe String
        , aliases : List TypeName
        , size : Int
        }
    | Enum
        { name : TypeName
        , namespace : Maybe String
        , aliases : List TypeName
        , doc : Maybe String
        , symbols : List TypeName
        }


type alias Field =
    { name : String
    , aliases : List String
    , doc : Maybe String
    , order : Maybe Order
    , fieldType : Type
    , default : Maybe (Value Type)
    }


type Order
    = Ascending
    | Descending
    | Ignore
