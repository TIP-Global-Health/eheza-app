module Pages.Session.View exposing (view)

import Activity.Model exposing (Activity(..))
import AssocList as Dict
import Backend.Entities exposing (..)
import Backend.Model exposing (ModelIndexedDb)
import Backend.Nurse.Model exposing (Nurse)
import Backend.Nurse.Utils exposing (isAuthorithedNurse)
import Backend.Session.Model exposing (EditableSession, Session)
import Backend.Session.Utils exposing (isClosed)
import EverySet exposing (EverySet)
import Gizra.Html exposing (showMaybe)
import Gizra.NominalDate exposing (NominalDate)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Pages.Activities.View
import Pages.Activity.Model
import Pages.Activity.View
import Pages.Attendance.View
import Pages.NextSteps.Model
import Pages.NextSteps.View
import Pages.Page exposing (Page(..), SessionPage(..), UserPage(..))
import Pages.Participant.Model
import Pages.Participant.View
import Pages.Participants.View
import Pages.ProgressReport.Model
import Pages.ProgressReport.View
import Pages.Session.Model exposing (Model, Msg(..))
import Participant.Utils exposing (childParticipant, motherParticipant)
import RemoteData exposing (RemoteData(..))
import SyncManager.Model exposing (Site, SiteFeature)
import Translate exposing (Language, translate)
import Utils.WebData exposing (viewWebData)
import ZScore.Model


view :
    Language
    -> NominalDate
    -> ZScore.Model.Model
    -> Site
    -> EverySet SiteFeature
    -> Bool
    -> Nurse
    -> SessionId
    -> SessionPage
    -> Model
    -> ModelIndexedDb
    -> Html Msg
view language currentDate zscores site features isChw nurse sessionId page model db =
    let
        sessionData =
            Dict.get sessionId db.sessions
                |> Maybe.withDefault NotAsked
    in
    viewWebData language
        (\session -> viewFoundSession language currentDate zscores site features isChw nurse ( sessionId, session ) page model db)
        (wrapError language sessionId)
        sessionData


wrapError : Language -> SessionId -> Html Msg -> Html Msg
wrapError language sessionId errorHtml =
    div
        [ class "wrap wrap-alt-2" ]
        [ div
            [ class "ui basic head segment" ]
            [ h1
                [ class "ui header" ]
                [ text <| translate language Translate.GroupEncounterLoading ]
            , span
                [ class "link-back"
                , onClick <| SetActivePage <| UserPage ClinicsPage
                ]
                [ span [ class "icon-back" ] []
                ]
            ]
        , errorHtml
        ]


viewFoundSession :
    Language
    -> NominalDate
    -> ZScore.Model.Model
    -> Site
    -> EverySet SiteFeature
    -> Bool
    -> Nurse
    -> ( SessionId, Session )
    -> SessionPage
    -> Model
    -> ModelIndexedDb
    -> Html Msg
viewFoundSession language currentDate zscores site features isChw nurse ( sessionId, session ) page model db =
    if isClosed currentDate session then
        viewClosedSession language sessionId session db

    else
        let
            authorized =
                RemoteData.toMaybe db.clinics
                    |> Maybe.andThen (Dict.get session.clinicId)
                    |> Maybe.map (\clinic -> isAuthorithedNurse clinic nurse)
                    |> Maybe.withDefault False
        in
        if authorized then
            let
                editableSession =
                    Dict.get sessionId db.editableSessions
                        |> Maybe.withDefault NotAsked
            in
            viewWebData language
                (viewEditableSession language currentDate zscores site features isChw nurse sessionId page model db)
                (wrapError language sessionId)
                editableSession

        else
            viewUnauthorizedSession language sessionId session db


viewEditableSession :
    Language
    -> NominalDate
    -> ZScore.Model.Model
    -> Site
    -> EverySet SiteFeature
    -> Bool
    -> Nurse
    -> SessionId
    -> SessionPage
    -> Model
    -> ModelIndexedDb
    -> EditableSession
    -> Html Msg
viewEditableSession language currentDate zscores site features isChw nurse sessionId page model db session =
    case page of
        ActivitiesPage ->
            model.activitiesPage
                |> Pages.Activities.View.view language isChw ( sessionId, session )
                |> Html.map MsgActivities

        ActivityPage activityType ->
            case activityType of
                ChildActivity activity ->
                    Dict.get activity model.childActivityPages
                        |> Maybe.withDefault Pages.Activity.Model.emptyModel
                        |> Pages.Activity.View.view childParticipant
                            language
                            currentDate
                            zscores
                            site
                            features
                            isChw
                            activity
                            ( sessionId, session )
                            model
                            db
                        |> (\( html, maybeChildId ) -> Html.map (MsgChildActivity activity maybeChildId) html)

                MotherActivity activity ->
                    Dict.get activity model.motherActivityPages
                        |> Maybe.withDefault Pages.Activity.Model.emptyModel
                        |> Pages.Activity.View.view motherParticipant
                            language
                            currentDate
                            zscores
                            site
                            features
                            isChw
                            activity
                            ( sessionId, session )
                            model
                            db
                        |> (\( html, maybeMotherId ) -> Html.map (MsgMotherActivity activity maybeMotherId) html)

        AttendancePage ->
            Pages.Attendance.View.view language isChw ( sessionId, session ) model.attendancePage
                |> Html.map MsgAttendance

        ParticipantsPage ->
            model.participantsPage
                |> Pages.Participants.View.view language isChw ( sessionId, session )
                |> Html.map MsgParticipants

        ProgressReportPage childId ->
            Dict.get childId model.progressReportPages
                |> Maybe.withDefault Pages.ProgressReport.Model.emptyModel
                |> Pages.ProgressReport.View.view language currentDate zscores site features isChw childId ( sessionId, session ) db
                |> Html.map (MsgProgressReport childId)

        ChildPage childId ->
            Dict.get childId model.childPages
                |> Maybe.withDefault Pages.Participant.Model.emptyModel
                |> Pages.Participant.View.viewChild language currentDate zscores site features isChw childId ( sessionId, session ) model db
                |> Html.map (MsgChild childId)

        MotherPage motherId ->
            Dict.get motherId model.motherPages
                |> Maybe.withDefault Pages.Participant.Model.emptyModel
                |> Pages.Participant.View.viewMother language currentDate zscores site features isChw motherId ( sessionId, session ) model db
                |> Html.map (MsgMother motherId)

        NextStepsPage childId activity ->
            Dict.get childId model.nextStepsPages
                |> Maybe.withDefault Pages.NextSteps.Model.emptyModel
                |> Pages.NextSteps.View.view language currentDate zscores childId activity ( sessionId, session ) db
                |> Html.map (MsgNextSteps childId activity)


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
                    |> Maybe.andThen (\clinics -> Dict.get session.clinicId clinics)
                    |> Maybe.map (.name >> text)
                    |> showMaybe
                ]
            , span
                [ class "link-back"
                , onClick <| SetActivePage <| UserPage ClinicsPage
                ]
                [ span [ class "icon-back" ] []
                ]
            ]
        , div
            [ class "ui error message" ]
            [ h1 [] [ text <| translate language Translate.GroupEncounterClosed ]
            , p [] [ text <| translate language <| Translate.GroupEncounterClosed2 sessionId ]
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
                    |> Maybe.andThen (\clinics -> Dict.get session.clinicId clinics)
                    |> Maybe.map (.name >> text)
                    |> showMaybe
                ]
            , span
                [ class "link-back"
                , onClick <| SetActivePage <| UserPage ClinicsPage
                ]
                [ span [ class "icon-back" ] []
                ]
            ]
        , div
            [ class "ui error message" ]
            [ h1 [] [ text <| translate language Translate.GroupEncounterUnauthorized ]
            , p [] [ text <| translate language Translate.GroupEncounterUnauthorized2 ]
            ]
        ]
