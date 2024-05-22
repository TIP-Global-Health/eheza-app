module Backend.Reports.Utils exposing (..)

import Backend.Reports.Model exposing (..)


genderFromString : String -> Maybe Gender
genderFromString s =
    case s of
        "female" ->
            Just Female

        "male" ->
            Just Male

        _ ->
            Nothing
