module Pages.View exposing (viewClosedSession, viewFoundSession, viewUnauthorizedSession)

import Activity.Model exposing (ActivityType(..))
import App.Model exposing (Model)
import Backend.Entities exposing (..)
import Backend.Session.Model exposing (EditableSession)
import Backend.Session.Utils exposing (activeClinicName, isAuthorized, isClosed)
import Date
import EveryDict
import Gizra.Html exposing (showMaybe)
import Gizra.NominalDate exposing (fromLocalDateTime)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Pages.Activities.View
import Pages.Activity.Model
import Pages.Activity.View
import Pages.Attendance.View
import Pages.Model exposing (..)
import Pages.Page exposing (Page(..), SessionPage(..), UserPage(..))
import Pages.Participant.Model
import Pages.Participant.View
import Pages.Participants.View
import Pages.ProgressReport.View
import Participant.Utils exposing (childParticipant, motherParticipant)
import Translate exposing (Language, translate)
import User.Model exposing (User)


{-| This is a view function specialized for session pages, which require an editable session.
-}
viewFoundSession : User -> SessionPage -> ( SessionId, EditableSession ) -> Model -> Html MsgSession
viewFoundSession user page ( sessionId, session ) model =
    let
        language =
            model.language

        currentDate =
            fromLocalDateTime <| Date.fromTime model.currentTime

        zscores =
            model.zscores
    in
    if isClosed currentDate session then
        viewClosedSession language sessionId session

    else if not (isAuthorized user session) then
        viewUnauthorizedSession language session

    else
        case page of
            ActivitiesPage ->
                model.sessionPages.activitiesPage
                    |> Pages.Activities.View.view language session
                    |> Html.map MsgActivities

            ActivityPage activityType ->
                case activityType of
                    ChildActivity activity ->
                        EveryDict.get activity model.sessionPages.childActivityPages
                            |> Maybe.withDefault Pages.Activity.Model.emptyModel
                            |> Pages.Activity.View.view childParticipant language currentDate zscores activity session
                            |> Html.map (MsgChildActivity activity)

                    MotherActivity activity ->
                        EveryDict.get activity model.sessionPages.motherActivityPages
                            |> Maybe.withDefault Pages.Activity.Model.emptyModel
                            |> Pages.Activity.View.view motherParticipant language currentDate zscores activity session
                            |> Html.map (MsgMotherActivity activity)

            AttendancePage ->
                Pages.Attendance.View.view language session

            ParticipantsPage ->
                model.sessionPages.participantsPage
                    |> Pages.Participants.View.view language session
                    |> Html.map MsgParticipants

            ProgressReportPage childId ->
                Pages.ProgressReport.View.view language zscores childId ( sessionId, session )

            ChildPage childId ->
                EveryDict.get childId model.sessionPages.childPages
                    |> Maybe.withDefault Pages.Participant.Model.emptyModel
                    |> Pages.Participant.View.viewChild language currentDate zscores childId session
                    |> Html.map (MsgChild childId)

            MotherPage motherId ->
                EveryDict.get motherId model.sessionPages.motherPages
                    |> Maybe.withDefault Pages.Participant.Model.emptyModel
                    |> Pages.Participant.View.viewMother language motherId session
                    |> Html.map (MsgMother motherId)


viewClosedSession : Language -> SessionId -> EditableSession -> Html MsgSession
viewClosedSession language sessionId session =
    div
        [ class "wrap wrap-alt-2" ]
        [ div
            [ class "ui basic head segment" ]
            [ h1
                [ class "ui header" ]
                [ activeClinicName session
                    |> Maybe.map text
                    |> showMaybe
                ]
            , a
                [ class "link-back"
                , onClick <| SetActivePage <| UserPage <| ClinicsPage <| Just session.offlineSession.session.clinicId
                ]
                [ span [ class "icon-back" ] []
                , span [] []
                ]
            ]
        , div
            [ class "ui error message" ]
            [ h1 [] [ text <| translate language Translate.SessionClosed ]
            , p [] [ text <| translate language <| Translate.SessionClosed2 sessionId ]
            ]
        ]


viewUnauthorizedSession : Language -> EditableSession -> Html MsgSession
viewUnauthorizedSession language session =
    div
        [ class "wrap wrap-alt-2" ]
        [ div
            [ class "ui basic head segment" ]
            [ h1
                [ class "ui header" ]
                [ activeClinicName session
                    |> Maybe.map text
                    |> showMaybe
                ]
            , a
                [ class "link-back"
                , onClick <| SetActivePage <| UserPage <| ClinicsPage <| Just session.offlineSession.session.clinicId
                ]
                [ span [ class "icon-back" ] []
                , span [] []
                ]
            ]
        , div
            [ class "ui error message" ]
            [ h1 [] [ text <| translate language Translate.SessionUnauthorized ]
            , p [] [ text <| translate language Translate.SessionUnauthorized2 ]
            ]
        ]
