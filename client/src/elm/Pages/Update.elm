module Pages.Update exposing (..)

import App.Model
import Backend.Model
import Backend.Session.Model exposing (EditableSession, MsgEditableSession(..))
import EveryDict
import Maybe.Extra
import Measurement.Utils exposing (getMotherForm, getChildForm)
import Pages.Activity.Model
import Pages.Activity.Update
import Pages.Activities.Update
import Pages.Model exposing (..)
import Pages.Participant.Model
import Pages.Participant.Update
import Pages.Participants.Update


{-| We need the editableSession in order to pass on some needed data. But we
don't modify it directly ... instead, we return messages to do so.
-}
updateSession : EditableSession -> MsgSession -> SessionPages -> ( SessionPages, Cmd MsgSession, List App.Model.Msg )
updateSession session msg model =
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

        MsgChildActivity activityType subMsg ->
            let
                activityPage =
                    EveryDict.get activityType model.childActivityPages
                        |> Maybe.withDefault Pages.Activity.Model.emptyModel

                childForm =
                    -- This isn't ideal ... should let Pages.Activity.Update
                    -- manage this, ideally. Note that this means that we would
                    -- need to be careful about how we use an alternate selected
                    -- participant, since we're using it in the routing. It might
                    -- be better to embed an ID in the messages, so the view can
                    -- explicitly route messages to the ID it actually used.
                    activityPage.selectedParticipant
                        |> Maybe.map (\childId -> getChildForm childId session)

                ( subModel, subCmd, subForm, outMsg, page ) =
                    Pages.Activity.Update.updateChild subMsg activityPage childForm

                sessionMsgs =
                    -- Again, this isn't ideal ... should rethink this structure eventually.
                    activityPage.selectedParticipant
                        |> Maybe.map
                            (\childId ->
                                [ Maybe.map (Backend.Session.Model.SetChildForm childId) subForm
                                , Maybe.map (MeasurementOutMsgChild childId) outMsg
                                ]
                                    |> List.filterMap identity
                                    |> List.map (App.Model.MsgCache << Backend.Model.MsgEditableSession)
                            )
                        |> Maybe.withDefault []

                redirectMsgs =
                    Maybe.map App.Model.SetActivePage page
                        |> Maybe.Extra.toList
            in
                -- So, to summarize
                --
                -- - we own the subModel and subCmd, so we handle them normally
                -- - the EditableSession owns the subForm, so we send a message to update that
                -- - we turn the redirect page into a message, if provided
                -- - we send a message to implement the OutMsg, if provided
                ( { model | childActivityPages = EveryDict.insert activityType subModel model.childActivityPages }
                , Cmd.map (MsgChildActivity activityType) subCmd
                , redirectMsgs ++ sessionMsgs
                )

        MsgMotherActivity activityType subMsg ->
            let
                activityPage =
                    EveryDict.get activityType model.motherActivityPages
                        |> Maybe.withDefault Pages.Activity.Model.emptyModel

                motherForm =
                    -- This isn't ideal ... should let Pages.Activity.Update
                    -- manage this, ideally. Note that this means that we would
                    -- need to be careful about how we use an alternate selected
                    -- participant, since we're using it in the routing. It might
                    -- be better to embed an ID in the messages, so the view can
                    -- explicitly route messages to the ID it actually used.
                    activityPage.selectedParticipant
                        |> Maybe.map (\motherId -> getMotherForm motherId session)

                ( subModel, subCmd, subForm, outMsg, page ) =
                    Pages.Activity.Update.updateMother subMsg activityPage motherForm

                sessionMsgs =
                    -- Again, this isn't ideal ... should rethink this structure eventually.
                    activityPage.selectedParticipant
                        |> Maybe.map
                            (\motherId ->
                                [ Maybe.map (Backend.Session.Model.SetMotherForm motherId) subForm
                                , Maybe.map (MeasurementOutMsgMother motherId) outMsg
                                ]
                                    |> List.filterMap identity
                                    |> List.map (App.Model.MsgCache << Backend.Model.MsgEditableSession)
                            )
                        |> Maybe.withDefault []

                redirectMsgs =
                    Maybe.map App.Model.SetActivePage page
                        |> Maybe.Extra.toList
            in
                -- So, to summarize
                --
                -- - we own the subModel and subCmd, so we handle them normally
                -- - the EditableSession owns the subForm, so we send a message to update that
                -- - we turn the redirect page into a message, if provided
                -- - we send a message to implement the OutMsg, if provided
                ( { model | motherActivityPages = EveryDict.insert activityType subModel model.motherActivityPages }
                , Cmd.map (MsgMotherActivity activityType) subCmd
                , redirectMsgs ++ sessionMsgs
                )

        MsgChild childId subMsg ->
            let
                childForm =
                    getChildForm childId session

                childPage =
                    EveryDict.get childId model.childPages
                        |> Maybe.withDefault Pages.Participant.Model.emptyModel

                ( subModel, subCmd, subForm, outMsg, page ) =
                    Pages.Participant.Update.updateChild subMsg childPage childForm

                sessionMsgs =
                    List.map (App.Model.MsgCache << Backend.Model.MsgEditableSession)
                        (Backend.Session.Model.SetChildForm childId subForm
                            :: (Maybe.Extra.toList (Maybe.map (MeasurementOutMsgChild childId) outMsg))
                        )

                redirectMsgs =
                    Maybe.map App.Model.SetActivePage page
                        |> Maybe.Extra.toList
            in
                -- So, to summarize
                --
                -- - we own the subModel and subCmd, so we handle them normally
                -- - the EditableSession owns the subForm, so we send a message to update that
                -- - we turn the redirect page into a message, if provided
                -- - we send a message to implement the OutMsg, if provided
                ( { model | childPages = EveryDict.insert childId subModel model.childPages }
                , Cmd.map (MsgChild childId) subCmd
                , redirectMsgs ++ sessionMsgs
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
                motherForm =
                    getMotherForm motherId session

                motherPage =
                    EveryDict.get motherId model.motherPages
                        |> Maybe.withDefault Pages.Participant.Model.emptyModel

                ( subModel, subCmd, subForm, outMsg, page ) =
                    Pages.Participant.Update.updateMother subMsg motherPage motherForm

                sessionMsgs =
                    List.map (App.Model.MsgCache << Backend.Model.MsgEditableSession)
                        (Backend.Session.Model.SetMotherForm motherId subForm
                            :: Maybe.Extra.toList (Maybe.map (MeasurementOutMsgMother motherId) outMsg)
                        )

                redirectMsgs =
                    Maybe.map App.Model.SetActivePage page
                        |> Maybe.Extra.toList
            in
                -- So, to summarize
                --
                -- - we own the subModel and subCmd, so we handle them normally
                -- - the EditableSession owns the subForm, so we send a message to update that
                -- - we turn the redirect page into a message, if provided
                -- - we send a message to implement the OutMsg, if provided
                ( { model | motherPages = EveryDict.insert motherId subModel model.motherPages }
                , Cmd.map (MsgMother motherId) subCmd
                , redirectMsgs ++ sessionMsgs
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
