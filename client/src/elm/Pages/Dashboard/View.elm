module Pages.Dashboard.View exposing (view)

import Backend.Entities exposing (..)
import Backend.Model exposing (ModelIndexedDb)
import Gizra.NominalDate exposing (NominalDate)
import Html exposing (..)
import Pages.Dashboard.Model exposing (..)
import Translate exposing (Language)


{-| Shows a dashboard page.
-}
view : Language -> NominalDate -> HealthCenterId -> Model -> ModelIndexedDb -> Html Msg
view language currentDate healthCenterId model db =
    div
        []
        [ text "@todo: Dashboard" ]
