module Device.View exposing (view)

import Device.Model exposing (..)
import Html exposing (..)
import RemoteData exposing (RemoteData(..), WebData)
import Translate exposing (Language)
import Utils.WebData exposing (viewError)


{-| We call this if we have an active service worker. If the device is authorized,
we show its status. Otherwise, we show a UI that allows for authorization.
-}
view : Language -> WebData Device -> Html msg
view language device =
    case device of
        NotAsked ->
            text "NotAsked"

        Loading ->
            text "Loading"

        Failure err ->
            viewError language err

        Success device ->
            text "Success"
