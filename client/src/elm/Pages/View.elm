module Pages.View exposing (..)

import Backend.Entities exposing (..)
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
import RemoteData exposing (RemoteData(..), WebData)
import Translate exposing (translate, Language)
import Utils.Html exposing (spinner, wrapPage)
import Utils.WebData exposing (viewError)


{-| This is a view function specialized for session pages, which require an editable session.

We handle the possible presence or absence of the session here.

-}
viewSessionPage : Language -> NominalDate -> SessionPage -> WebData (Maybe ( SessionId, EditableSession )) -> SessionPages -> Html MsgSession
viewSessionPage language currentDate page session model =
    -- TODO: The error messages should have a link to somewhere possibly useful ...
    -- probably the home page?
    case session of
        NotAsked ->
            wrapPage [ spinner ]

        Loading ->
            wrapPage [ spinner ]

        Failure err ->
            wrapPage
                [ h4 [] [ text <| translate language Translate.ErrorFetchingCachedSession ]
                , viewError language err
                ]

        Success fetched ->
            case fetched of
                Just ( sessionId, session ) ->
                    viewFoundSession language currentDate page session model

                Nothing ->
                    wrapPage
                        [ h4 [] [ text <| translate language Translate.NoCachedSession ]
                        ]


viewFoundSession : Language -> NominalDate -> SessionPage -> EditableSession -> SessionPages -> Html MsgSession
viewFoundSession language currentDate page session model =
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
            Pages.Attendance.View.view language session

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
