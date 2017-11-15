module Pages.View exposing (..)

import Backend.Session.Model exposing (EditableSession)
import EveryDict
import Gizra.NominalDate exposing (NominalDate)
import Html exposing (..)
import Pages.Model exposing (..)
import Pages.Activities.View
import Pages.Activity.Model
import Pages.Activity.View
import Pages.Attendance.View
import Pages.Page exposing (SessionPage(..))
import Pages.Participant.Model
import Pages.Participant.View
import Pages.Participants.View
import Translate exposing (Language)


{-| This is a view function specialized for session pages, which require an editable session.
-}
viewSessionPage : Language -> NominalDate -> SessionPage -> EditableSession -> SessionPages -> Html MsgSession
viewSessionPage language currentDate page session model =
    case page of
        ActivitiesPage ->
            model.activitiesPage
                |> Pages.Activities.View.view language session
                |> Html.map MsgActivities

        ActivityPage activityType ->
            EveryDict.get activityType model.activityPages
                |> Maybe.withDefault Pages.Activity.Model.emptyModel
                |> Pages.Activity.View.view language activityType session
                |> Html.map (MsgActivity activityType)

        AttendancePage ->
            Pages.Attendance.View.view

        ParticipantsPage ->
            model.participantsPage
                |> Pages.Participants.View.view language session
                |> Html.map MsgParticipants

        ChildPage childId ->
            EveryDict.get childId model.childPages
                |> Maybe.withDefault Pages.Participant.Model.emptyModel
                |> Pages.Participant.View.viewChild language currentDate childId session
                |> Html.map (MsgChild childId)

        MotherPage motherId ->
            EveryDict.get motherId model.motherPages
                |> Maybe.withDefault Pages.Participant.Model.emptyModel
                |> Pages.Participant.View.viewMother language motherId session
                |> Html.map (MsgMother motherId)
