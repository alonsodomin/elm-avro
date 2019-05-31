module Avro.Internal.Result exposing (recover)


recover : (e -> Result ee a) -> Result e a -> Result ee a
recover f result =
    case result of
        Ok v ->
            Ok v

        Err e ->
            f e
