module ZScore.Decoder exposing (decodeForAge, decodeForCentimetres)

import Backend.Measurement.Model exposing (Gender(..))
import Json.Decode exposing (Decoder, andThen, fail, float, int, list, map, succeed)
import Json.Decode.Pipeline exposing (required)
import Utils.AllDict as AllDict exposing (AllDict)
import ZScore.Model exposing (Kilograms(..), MaleAndFemale, ZScoreEntry)


{-| This is a type which matches the structure of our JSON. We initially
succeed this, and then turn it into a type that is more oriented to our
app's needs.
-}
type alias JsonByAge key value =
    { gender : Gender
    , age : key
    , l : Float
    , m : value
    , s : Float
    }


{-| Like JsonByAge, but where the key is in centimetres.
-}
type alias JsonKgByCm key =
    { gender : Gender
    , centimetres : key
    , l : Float
    , m : Kilograms
    , s : Float
    }


{-| The way that gender is encoded in our ZScore tables.
-}
decodeGender : Decoder Gender
decodeGender =
    andThen
        (\gender ->
            case gender of
                1 ->
                    succeed Male

                2 ->
                    succeed Female

                _ ->
                    fail <| "Not a recognized code for a gender: " ++ String.fromInt gender
        )
        int


{-| Decodes our intermediate format.
-}
decodeJsonByAge : (Int -> key) -> (Float -> value) -> Decoder (JsonByAge key value)
decodeJsonByAge tagKey tagValue =
    succeed JsonByAge
        |> required "sex" decodeGender
        |> required "age" (map tagKey int)
        |> required "l" float
        |> required "m" (map tagValue float)
        |> required "s" float


decodeJsonKgByCm : String -> (Float -> key) -> Decoder (JsonKgByCm key)
decodeJsonKgByCm label tagKey =
    succeed JsonKgByCm
        |> required "sex" decodeGender
        |> required label (map tagKey float)
        |> required "l" float
        |> required "m" (map Kilograms float)
        |> required "s" float


{-| Decodes a JSON table organized by age.
-}
decodeForAge : (Int -> key) -> (key -> Int) -> (Float -> value) -> Decoder (MaleAndFemale (AllDict key (ZScoreEntry value) Int))
decodeForAge tagKey untagKey tagValue =
    let
        initial =
            { male = AllDict.empty untagKey
            , female = AllDict.empty untagKey
            }

        eachEntry entry accum =
            let
                value =
                    { l = entry.l
                    , m = entry.m
                    , s = entry.s
                    }
            in
            case entry.gender of
                Male ->
                    { accum | male = AllDict.insert entry.age value accum.male }

                Female ->
                    { accum | female = AllDict.insert entry.age value accum.female }
    in
    decodeJsonByAge tagKey tagValue
        |> list
        |> map (List.foldl eachEntry initial)


decodeForCentimetres : String -> (Float -> key) -> (key -> Float) -> Decoder (MaleAndFemale (AllDict key (ZScoreEntry Kilograms) Int))
decodeForCentimetres label tagKey untagKey =
    let
        -- We index by the millimetre, so we multiple the centimetres by 10 and
        -- round. I suppose float itself is comparable, but comparing floats
        -- for equality can be tricky, so it's probably better to round to the
        -- precision we know we're using.
        ord key =
            round <| untagKey key * 10

        initial =
            { male = AllDict.empty ord
            , female = AllDict.empty ord
            }

        eachEntry entry accum =
            let
                value =
                    { l = entry.l
                    , m = entry.m
                    , s = entry.s
                    }
            in
            case entry.gender of
                Male ->
                    { accum | male = AllDict.insert entry.centimetres value accum.male }

                Female ->
                    { accum | female = AllDict.insert entry.centimetres value accum.female }
    in
    decodeJsonKgByCm label tagKey
        |> list
        |> map (List.foldl eachEntry initial)
