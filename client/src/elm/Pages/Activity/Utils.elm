module Pages.Activity.Utils exposing (viewChildMeasurements, viewMotherMeasurements)

import Activity.Model exposing (ChildActivity, MotherActivity)
import Backend.Entities exposing (..)
import Backend.Model exposing (ModelIndexedDb)
import Backend.NutritionEncounter.Utils exposing (resolvePreviousValuesSetForChild)
import Backend.Session.Model exposing (EditableSession)
import Backend.Session.Utils exposing (getChild, getChildMeasurementData, getMother, getMotherMeasurementData)
import Gizra.Html exposing (emptyNode)
import Gizra.NominalDate exposing (NominalDate)
import Html exposing (Html)
import LocalData
import Measurement.Model
import Measurement.Utils exposing (getChildForm, getMotherForm)
import Measurement.View
import Pages.Activity.Model exposing (Msg(..))
import Pages.Session.Model
import SyncManager.Model exposing (Site)
import Translate exposing (Language)
import ZScore.Model


{-| These are conveniences for the way the code was structured.
Ideally, we'd have smaller capabilities in `Participant` that
this could be built on more generically, but this will do for now.
-}
viewChildMeasurements :
    Language
    -> NominalDate
    -> Site
    -> ZScore.Model.Model
    -> Bool
    -> ModelIndexedDb
    -> PersonId
    -> ChildActivity
    -> Pages.Session.Model.Model
    -> EditableSession
    -> Html (Msg PersonId Measurement.Model.MsgChild)
viewChildMeasurements language currentDate site zscores isChw db childId activity pages session =
    let
        form =
            getChildForm site childId pages session
    in
    getChildMeasurementData childId session
        |> LocalData.unwrap
            emptyNode
            (\measurements ->
                getChild childId session.offlineSession
                    |> Maybe.map
                        (\child ->
                            resolvePreviousValuesSetForChild currentDate site childId db
                                |> Measurement.View.viewChild language currentDate site isChw ( childId, child ) activity measurements zscores session db form
                                |> Html.map MsgMeasurement
                        )
                    |> Maybe.withDefault emptyNode
            )


viewMotherMeasurements : Language -> PersonId -> MotherActivity -> Pages.Session.Model.Model -> EditableSession -> Html (Msg PersonId Measurement.Model.MsgMother)
viewMotherMeasurements language motherId activity pages session =
    let
        form =
            getMotherForm motherId pages session
    in
    getMotherMeasurementData motherId session
        |> LocalData.unwrap
            emptyNode
            (\measurements ->
                getMother motherId session.offlineSession
                    |> Maybe.map
                        (\_ ->
                            Measurement.View.viewMother language activity session.offlineSession.session.clinicType measurements form
                                |> Html.map MsgMeasurement
                        )
                    |> Maybe.withDefault emptyNode
            )
