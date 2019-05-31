module Avro.Internal.Maybe exposing (catMaybes, toList)


toList : Maybe a -> List a
toList maybe =
    case maybe of
        Just a ->
            [ a ]

        Nothing ->
            []


catMaybes : List (Maybe a) -> List a
catMaybes =
    List.foldl (\e prev -> toList e ++ prev) []
