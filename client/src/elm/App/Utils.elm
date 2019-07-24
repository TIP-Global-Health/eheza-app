module App.Utils exposing (getLoggedInData)

import App.Model exposing (..)
import Backend.Entities exposing (HealthCenterId)
import RemoteData


{-| Returns the logged in model and selected health center, if we're logged in.
-}
getLoggedInData : Model -> Maybe ( HealthCenterId, LoggedInModel )
getLoggedInData model =
    model.configuration
        |> RemoteData.toMaybe
        |> Maybe.andThen (.loggedIn >> RemoteData.toMaybe)
        |> Maybe.map2 (\healthCenterId loggedIn -> ( healthCenterId, loggedIn )) model.healthCenterId
