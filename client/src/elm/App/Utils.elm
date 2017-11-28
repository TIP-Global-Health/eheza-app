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


{-| Do we think we have a valid access token?
-}
hasValidAccessToken : Model -> Bool
hasValidAccessToken model =
    model.configuration
        |> RemoteData.map (.login >> Restful.Login.hasValidAccessToken)
        |> RemoteData.withDefault False


{-| Do we have an access token at all?
-}
hasAccessToken : Model -> Bool
hasAccessToken model =
    model.configuration
        |> RemoteData.map (.login >> Restful.Login.hasAccessToken)
        |> RemoteData.withDefault False
