module Pages.Update exposing (..)

import EveryDict
import Pages.Activity.Model
import Pages.Activity.Update
import Pages.Activities.Update
import Pages.Model exposing (..)
import Pages.Page exposing (Page(..), SessionPage)
import Pages.Participant.Model
import Pages.Participant.Update
import Pages.Participants.Update


{-| The third return parameter, if `Just`, indicates our desire
to redirect the user's attention to the given page.

This is specialized to our `SessionPages` model.

-}
updateSession : MsgSession -> SessionPages -> ( SessionPages, Cmd MsgSession, Maybe Page )
updateSession msg model =
    case msg of
        MsgActivities subMsg ->
            let
                ( subModel, subCmd, subPage ) =
                    Pages.Activities.Update.update subMsg model.activitiesPage
            in
                ( { model | activitiesPage = subModel }
                , Cmd.map MsgActivities subCmd
                , Maybe.map SessionPage subPage
                )

        MsgActivity activityType subMsg ->
            let
                ( subModel, subCmd ) =
                    EveryDict.get activityType model.activityPages
                        |> Maybe.withDefault Pages.Activity.Model.emptyModel
                        |> Pages.Activity.Update.update subMsg
            in
                ( { model | activityPages = EveryDict.insert activityType subModel model.activityPages }
                , Cmd.map (MsgActivity activityType) subCmd
                , Nothing
                )

        MsgChild childId subMsg ->
            let
                ( subModel, subCmd ) =
                    EveryDict.get childId model.childPages
                        |> Maybe.withDefault Pages.Participant.Model.emptyModel
                        |> Pages.Participant.Update.updateChild subMsg
            in
                ( { model | childPages = EveryDict.insert childId subModel model.childPages }
                , Cmd.map (MsgChild childId) subCmd
                , Nothing
                )

        MsgMother motherId subMsg ->
            let
                ( subModel, subCmd ) =
                    EveryDict.get motherId model.motherPages
                        |> Maybe.withDefault Pages.Participant.Model.emptyModel
                        |> Pages.Participant.Update.updateMother subMsg
            in
                ( { model | motherPages = EveryDict.insert motherId subModel model.motherPages }
                , Cmd.map (MsgMother motherId) subCmd
                , Nothing
                )

        MsgParticipants subMsg ->
            let
                ( subModel, subCmd, subPage ) =
                    Pages.Participants.Update.update subMsg model.participantsPage
            in
                ( { model | participantsPage = subModel }
                , Cmd.map MsgParticipants subCmd
                , Maybe.map SessionPage subPage
                )

        SetActivePage page ->
            ( model, Cmd.none, Just page )
