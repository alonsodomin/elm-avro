module Avro.Encode exposing (..)

import Bytes
import Bytes.Encode as Enc

type alias Encoder a = a -> Enc.Encoder

int : Encoder Int
int = Enc.signedInt32 Bytes.BE

float : Encoder Float
float = Enc.float32 Bytes.BE

double : Encoder Float
double = Enc.float64 Bytes.BE

string : Encoder String
string = Enc.string

bool : Encoder Bool
bool = \b -> if b then Enc.signedInt8 1 else Enc.signedInt8 0

contramap : (b -> a) -> Encoder a -> Encoder b
contramap f enc = \b -> enc (f b)