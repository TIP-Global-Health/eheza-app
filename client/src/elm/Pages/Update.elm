module Pages.Update exposing (..)

import App.Model
import Backend.Model
import EveryDict
import Maybe.Extra
import Pages.Activity.Model
import Pages.Activity.Update
import Pages.Activities.Update
import Pages.Model exposing (..)
import Pages.Participant.Model
import Pages.Participant.Update
import Pages.Participants.Update


{-| The third return parameter, if `Just`, indicates our desire
to redirect the user's attention to the given page.

This is specialized to our `SessionPages` model.

-}
updateSession : MsgSession -> SessionPages -> ( SessionPages, Cmd MsgSession, List App.Model.Msg )
updateSession msg model =
    case msg of
        MsgActivities subMsg ->
            let
                ( subModel, subCmd, subPage ) =
                    Pages.Activities.Update.update subMsg model.activitiesPage
            in
                ( { model | activitiesPage = subModel }
                , Cmd.map MsgActivities subCmd
                , Maybe.map App.Model.SetActivePage subPage
                    |> Maybe.Extra.toList
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
                , []
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
                , []
                )

        MsgEditableSession subMsg ->
            -- Just route it over to the backend ...
            ( model
            , Cmd.none
            , [ App.Model.MsgCache <|
                    Backend.Model.MsgEditableSession subMsg
              ]
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
                , []
                )

        MsgParticipants subMsg ->
            let
                ( subModel, subCmd, subPage ) =
                    Pages.Participants.Update.update subMsg model.participantsPage
            in
                ( { model | participantsPage = subModel }
                , Cmd.map MsgParticipants subCmd
                , Maybe.map App.Model.SetActivePage subPage
                    |> Maybe.Extra.toList
                )

        SetActivePage page ->
            ( model, Cmd.none, [ App.Model.SetActivePage page ] )
