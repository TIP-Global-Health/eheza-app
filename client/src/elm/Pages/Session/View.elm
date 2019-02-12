module Pages.Session.View exposing (view)

import Activity.Model exposing (Activity(..))
import App.Model
import Backend.Entities exposing (..)
import Backend.Model exposing (ModelIndexedDb)
import Backend.Nurse.Model exposing (Nurse)
import Backend.Session.Model exposing (EditableSession, Session)
import Backend.Session.Utils exposing (isAuthorized, isClosed)
import Date
import EveryDict
import EveryDictList
import Gizra.Html exposing (showMaybe)
import Gizra.NominalDate exposing (NominalDate, fromLocalDateTime)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Pages.Activities.View
import Pages.Activity.Model
import Pages.Activity.View
import Pages.Attendance.View
import Pages.Page exposing (Page(..), SessionPage(..), UserPage(..))
import Pages.Participant.Model
import Pages.Participant.View
import Pages.Participants.View
import Pages.ProgressReport.View
import Pages.Session.Model exposing (..)
import Participant.Utils exposing (childParticipant, motherParticipant)
import RemoteData
import Translate exposing (Language, translate)
import User.Model exposing (User)
import Utils.WebData exposing (viewWebData)
import ZScore.Model


view : Language -> NominalDate -> ZScore.Model.Model -> Nurse -> SessionId -> SessionPage -> Model -> ModelIndexedDb -> Html Msg
view language currentDate zscores nurse sessionId page model db =
    Debug.crash "todo"


viewFoundSession : Language -> NominalDate -> ZScore.Model.Model -> Nurse -> ( SessionId, Session ) -> SessionPage -> Model -> ModelIndexedDb -> Html Msg
viewFoundSession language currentDate zscores nurse ( sessionId, session ) page model db =
    if isClosed currentDate session then
        viewClosedSession language sessionId session db

    else if not (isAuthorized nurse session) then
        viewUnauthorizedSession language sessionId session db

    else
        let
            editableSession =
                Debug.crash "todo"
        in
        viewWebData language
            (viewEditableSession language currentDate zscores nurse sessionId page model db)
            identity
            editableSession


viewEditableSession : Language -> NominalDate -> ZScore.Model.Model -> Nurse -> SessionId -> SessionPage -> Model -> ModelIndexedDb -> EditableSession -> Html Msg
viewEditableSession language currentDate zscores nurse sessionId page model db session =
    case page of
        ActivitiesPage ->
            model.activitiesPage
                |> Pages.Activities.View.view language ( sessionId, session )
                |> Html.map MsgActivities

        ActivityPage activityType ->
            case activityType of
                ChildActivity activity ->
                    EveryDict.get activity model.childActivityPages
                        |> Maybe.withDefault Pages.Activity.Model.emptyModel
                        |> Pages.Activity.View.view childParticipant language currentDate zscores activity ( sessionId, session ) model
                        |> (\( html, maybeChildId ) -> Html.map (MsgChildActivity activity maybeChildId) html)

                MotherActivity activity ->
                    EveryDict.get activity model.motherActivityPages
                        |> Maybe.withDefault Pages.Activity.Model.emptyModel
                        |> Pages.Activity.View.view motherParticipant language currentDate zscores activity ( sessionId, session ) model
                        |> (\( html, maybeMotherId ) -> Html.map (MsgMotherActivity activity maybeMotherId) html)

        AttendancePage ->
            Pages.Attendance.View.view language ( sessionId, session )

        ParticipantsPage ->
            model.participantsPage
                |> Pages.Participants.View.view language ( sessionId, session )
                |> Html.map MsgParticipants

        ProgressReportPage childId ->
            Pages.ProgressReport.View.view language zscores childId ( sessionId, session ) db

        ChildPage childId ->
            EveryDict.get childId model.childPages
                |> Maybe.withDefault Pages.Participant.Model.emptyModel
                |> Pages.Participant.View.viewChild language currentDate zscores childId ( sessionId, session ) model
                |> Html.map (MsgChild childId)

        MotherPage motherId ->
            EveryDict.get motherId model.motherPages
                |> Maybe.withDefault Pages.Participant.Model.emptyModel
                |> Pages.Participant.View.viewMother language motherId ( sessionId, session ) model
                |> Html.map (MsgMother motherId)


viewClosedSession : Language -> SessionId -> Session -> ModelIndexedDb -> Html Msg
viewClosedSession language sessionId session db =
    div
        [ class "wrap wrap-alt-2" ]
        [ div
            [ class "ui basic head segment" ]
            [ h1
                [ class "ui header" ]
                [ db.clinics
                    |> RemoteData.toMaybe
                    |> Maybe.andThen (\clinics -> EveryDictList.get session.clinicId clinics)
                    |> Maybe.map (.name >> text)
                    |> showMaybe
                ]
            , a
                [ class "link-back"
                , onClick <| SetActivePage <| UserPage <| ClinicsPage <| Just session.clinicId
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


viewUnauthorizedSession : Language -> SessionId -> Session -> ModelIndexedDb -> Html Msg
viewUnauthorizedSession language sessionId session db =
    div
        [ class "wrap wrap-alt-2" ]
        [ div
            [ class "ui basic head segment" ]
            [ h1
                [ class "ui header" ]
                [ db.clinics
                    |> RemoteData.toMaybe
                    |> Maybe.andThen (\clinics -> EveryDictList.get session.clinicId clinics)
                    |> Maybe.map (.name >> text)
                    |> showMaybe
                ]
            , a
                [ class "link-back"
                , onClick <| SetActivePage <| UserPage <| ClinicsPage <| Just session.clinicId
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
