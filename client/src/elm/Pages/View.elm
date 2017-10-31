module Pages.View exposing (..)

import Backend.Session.Model exposing (EditableSession)
import Date exposing (Date)
import EveryDict
import Html exposing (..)
import Pages.Model exposing (..)
import Pages.Activities.View
import Pages.Activity.Model
import Pages.Activity.View
import Pages.Page exposing (Page(..))
import Pages.Participants.View
import Translate exposing (Language)


view : Language -> Date -> EditableSession -> Model -> Html Msg
view language currentDate session model =
    case model.userAttention of
        ActivitiesPage ->
            model.activitiesPage
                |> Pages.Activities.View.view language session
                |> Html.map MsgActivities

        ActivityPage activityType ->
            EveryDict.get activityType model.activityPages
                |> Maybe.withDefault Pages.Activity.Model.emptyModel
                |> Pages.Activity.View.view language currentDate activityType session
                |> Html.map (MsgActivity activityType)

        ParticipantsPage ->
            model.participantsPage
                |> Pages.Participants.View.view language session
                |> Html.map MsgParticipants
