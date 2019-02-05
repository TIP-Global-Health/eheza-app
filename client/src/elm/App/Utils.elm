module App.Utils exposing (getLoggedInModel)

import App.Model exposing (..)
import Backend.Entities exposing (..)
import RemoteData


{-| Returns the logged in model if we're logged in.
-}
getLoggedInModel : Model -> Maybe LoggedInModel
getLoggedInModel model =
    model.configuration
        |> RemoteData.toMaybe
        |> Maybe.andThen (.loggedIn >> RemoteData.toMaybe)
