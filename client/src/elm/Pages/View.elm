module Pages.View exposing (..)

import Activity.Model exposing (ActivityType(..))
import Backend.Session.Model exposing (EditableSession)
import Backend.Session.Utils exposing (isClosed, activeClinicName)
import EveryDict
import Gizra.Html exposing (showMaybe)
import Gizra.NominalDate exposing (NominalDate)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Pages.Model exposing (..)
import Pages.Activities.View
import Pages.Activity.Model
import Pages.Activity.View
import Pages.Attendance.View
import Pages.Page exposing (SessionPage(..), Page(..), UserPage(..))
import Pages.Participant.Model
import Pages.Participant.View
import Pages.Participants.View
import Participant.Utils exposing (childParticipant, motherParticipant)
import Translate exposing (translate, Language)
import ZScore.Model


{-| This is a view function specialized for session pages, which require an editable session.
-}
viewFoundSession : Language -> NominalDate -> ZScore.Model.Model -> SessionPage -> EditableSession -> SessionPages -> Html MsgSession
viewFoundSession language currentDate zscores page session model =
    if isClosed currentDate session then
        viewClosedSession language session
    else
        case page of
            ActivitiesPage ->
                model.activitiesPage
                    |> Pages.Activities.View.view language session
                    |> Html.map MsgActivities

            ActivityPage activityType ->
                case activityType of
                    ChildActivity activity ->
                        EveryDict.get activity model.childActivityPages
                            |> Maybe.withDefault Pages.Activity.Model.emptyModel
                            |> Pages.Activity.View.view childParticipant language currentDate zscores activity session
                            |> Html.map (MsgChildActivity activity)

                    MotherActivity activity ->
                        EveryDict.get activity model.motherActivityPages
                            |> Maybe.withDefault Pages.Activity.Model.emptyModel
                            |> Pages.Activity.View.view motherParticipant language currentDate zscores activity session
                            |> Html.map (MsgMotherActivity activity)

            AttendancePage ->
                Pages.Attendance.View.view language session

            ParticipantsPage ->
                model.participantsPage
                    |> Pages.Participants.View.view language session
                    |> Html.map MsgParticipants

            ChildPage childId ->
                EveryDict.get childId model.childPages
                    |> Maybe.withDefault Pages.Participant.Model.emptyModel
                    |> Pages.Participant.View.viewChild language currentDate zscores childId session
                    |> Html.map (MsgChild childId)

            MotherPage motherId ->
                EveryDict.get motherId model.motherPages
                    |> Maybe.withDefault Pages.Participant.Model.emptyModel
                    |> Pages.Participant.View.viewMother language motherId session
                    |> Html.map (MsgMother motherId)


viewClosedSession : Language -> EditableSession -> Html MsgSession
viewClosedSession language session =
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
            , p [] [ text <| translate language Translate.SessionClosed2 ]
            ]
        ]
