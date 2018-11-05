module App.Utils exposing (getLoggedInModel, getUserId, hasAccessToken, hasValidAccessToken)

import App.Model exposing (..)
import Backend.Entities exposing (..)
import RemoteData
import Restful.Login exposing (getUser, maybeAuthenticatedData)


{-| Returns the logged in model if we're logged in.
-}
getLoggedInModel : Model -> Maybe LoggedInModel
getLoggedInModel model =
    model.configuration
        |> RemoteData.toMaybe
        |> Maybe.andThen (.login >> maybeAuthenticatedData)


getUserId : Model -> Maybe UserId
getUserId model =
    model.configuration
        |> RemoteData.toMaybe
        |> Maybe.andThen (.login >> getUser)
        |> Maybe.map .id


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
