module Pages.Activity.Utils exposing (..)

import Activity.Model exposing (ChildActivityType(..), MotherActivityType(..))
import Activity.Utils exposing (onlyCheckedIn)
import Backend.Entities exposing (..)
import Backend.Session.Model exposing (EditableSession)
import Backend.Session.Utils exposing (getChild, getChildMeasurementData, getMother, getMotherMeasurementData)
import EveryDict
import Gizra.Html exposing (emptyNode)
import Gizra.NominalDate exposing (NominalDate)
import Html exposing (Html)
import Maybe.Extra
import Measurement.Model
import Measurement.Utils exposing (getChildForm, getMotherForm)
import Measurement.View
import Pages.Activity.Model exposing (..)
import Participant.Model exposing (Participant)
import Translate exposing (Language)
import ZScore.Model


{-| These are conveniences for the way the code was structured.
Ideally, we'd have smaller capabilities in `Participant` that
this could be built on more generically, but this will do for now.
-}
viewChildMeasurements : Language -> NominalDate -> ZScore.Model.Model -> ChildId -> ChildActivityType -> EditableSession -> Html (Msg ChildId Measurement.Model.MsgChild)
viewChildMeasurements language currentDate zscores childId activity session =
    let
        measurements =
            getChildMeasurementData childId session

        form =
            getChildForm childId session
    in
    getChild childId session.offlineSession
        |> Maybe.map
            (\child ->
                Measurement.View.viewChild language currentDate child activity measurements zscores form
                    |> Html.map MsgMeasurement
            )
        |> Maybe.withDefault emptyNode


viewMotherMeasurements : Language -> NominalDate -> MotherId -> MotherActivityType -> EditableSession -> Html (Msg MotherId Measurement.Model.MsgMother)
viewMotherMeasurements language currentDate motherId activity session =
    let
        measurements =
            getMotherMeasurementData motherId session

        form =
            getMotherForm motherId session
    in
    Measurement.View.viewMother language activity measurements form
        |> Html.map MsgMeasurement


{-| This chooses an appropriate participant, given the participant the user selected
and the tab we're on. So, we automatically select a participant when we can, and we
don't show a "completed" participant for an activity if we're on the "pending"
tab (and vice-versa).
-}
selectParticipantForTab : Participant id value activity msg -> Tab -> activity -> EditableSession -> Maybe id -> Maybe id
selectParticipantForTab config tab activity session userSelection =
    let
        checkedIn =
            onlyCheckedIn session

        ( pendingParticipants, completedParticipants ) =
            config.getParticipants checkedIn
                |> EveryDict.toList
                |> List.sortBy (Tuple.second >> config.getName)
                |> List.map Tuple.first
                |> List.partition (\id -> config.hasPendingActivity id activity checkedIn)
    in
    case tab of
        Completed ->
            userSelection
                |> Maybe.andThen
                    (\id ->
                        if config.hasPendingActivity id activity session then
                            Nothing
                        else
                            Just id
                    )
                |> Maybe.Extra.orElse (List.head completedParticipants)

        Pending ->
            userSelection
                |> Maybe.andThen
                    (\id ->
                        if config.hasPendingActivity id activity session then
                            Just id
                        else
                            Nothing
                    )
                |> Maybe.Extra.orElse (List.head pendingParticipants)
