module Backend.FamilyActivity.Utils exposing (..)

import Backend.FamilyActivity.Model exposing (..)


encodeActivityAsString : FamilyActivity -> String
encodeActivityAsString activity =
    case activity of
        FBFMother ->
            "fbf-mother"

        FBFChild ->
            "fbf-child"


decodeActivityFromString : String -> Maybe FamilyActivity
decodeActivityFromString activity =
    case activity of
        "fbf-mother" ->
            Just FBFMother

        "fbf-child" ->
            Just FBFChild

        _ ->
            Nothing
