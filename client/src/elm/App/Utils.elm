module App.Utils exposing (..)

import App.Model exposing (..)
import RemoteData
import Restful.Login exposing (maybeData)


{-| Returns the logged in model if we're logged in.
-}
getLoggedInModel : Model -> Maybe LoggedInModel
getLoggedInModel model =
    model.configuration
        |> RemoteData.toMaybe
        |> Maybe.andThen (.login >> maybeData)
