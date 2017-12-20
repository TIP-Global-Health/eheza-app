module ZScore.Decoder exposing (..)

import IntDict exposing (IntDict)
import Json.Decode exposing (..)
import Json.Decode.Pipeline exposing (..)
import ZScore.Model exposing (..)


decodeZScoreEntry : Decoder ZScoreEntry
decodeZScoreEntry =
    -- TODO: This is fragile, since it depends on the order
    -- of the fields in ZScoreEntry. So, it would be nice
    -- to make the fields more explicit here. However, it
    -- would be more verbose!
    decode ZScoreEntry
        |> required "SD0" float
        |> required "SD1" float
        |> required "SD2" float
        |> required "SD3" float
        |> required "SD1neg" float
        |> required "SD2neg" float
        |> required "SD3neg" float


decodeZScoreEntriesByDay : Decoder (IntDict ZScoreEntry)
decodeZScoreEntriesByDay =
    map2 (,) (field "Day" int) decodeZScoreEntry
        |> list
        |> map IntDict.fromList


{-| This is a bit different because the height in JSON is a float in cm,
and we want to convert to integer millimetres.
-}
decodeZScoreEntriesByHeight : Decoder (IntDict ZScoreEntry)
decodeZScoreEntriesByHeight =
    let
        decodeIntegerMillimetres =
            field "Length" float
                |> map ((*) 10 >> round)
    in
        map2 (,) decodeIntegerMillimetres decodeZScoreEntry
            |> list
            |> map IntDict.fromList
