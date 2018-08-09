module Pages.Update exposing (..)

import App.Model exposing (Msg(MsgSession))
import Backend.Model
import Backend.Session.Model exposing (EditableSession, MsgEditableSession(..))
import EveryDict
import Maybe.Extra
import Measurement.Utils exposing (getChildForm, getMotherForm)
import Pages.Activities.Update
import Pages.Activity.Model
import Pages.Activity.Update
import Pages.Model exposing (..)
import Pages.Participant.Model
import Pages.Participant.Update
import Pages.Participants.Update
import Participant.Utils exposing (childParticipant, motherParticipant)


{-| We need the editableSession in order to pass on some needed data. But we
don't modify it directly ... instead, we return messages to do so.
-}
updateSession : EditableSession -> MsgSession -> SessionPages -> ( SessionPages, Cmd MsgSession, List App.Model.Msg )
updateSession session msg model =
    case msg of
        MsgActivities subMsg ->
            let
                ( subModel, subCmd, extraMsgs ) =
                    Pages.Activities.Update.update session subMsg model.activitiesPage
            in
            ( { model | activitiesPage = subModel }
            , Cmd.map MsgActivities subCmd
            , List.map MsgSession extraMsgs
            )

        MsgChildActivity activityType maybeChildId subMsg ->
            let
                activityPage =
                    EveryDict.get activityType model.childActivityPages
                        |> Maybe.withDefault Pages.Activity.Model.emptyModel

                childForm =
                    Maybe.map (\childId -> getChildForm childId session) maybeChildId

                ( subModel, subCmd, subForm, outMsg, page ) =
                    Pages.Activity.Update.updateChild subMsg activityPage childForm

                sessionMsgs =
                    maybeChildId
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
            , Cmd.map (MsgChildActivity activityType maybeChildId) subCmd
            , redirectMsgs ++ sessionMsgs
            )

        MsgMotherActivity activityType maybeMotherId subMsg ->
            let
                activityPage =
                    EveryDict.get activityType model.motherActivityPages
                        |> Maybe.withDefault Pages.Activity.Model.emptyModel

                motherForm =
                    Maybe.map (\motherId -> getMotherForm motherId session) maybeMotherId

                ( subModel, subCmd, subForm, outMsg, page ) =
                    Pages.Activity.Update.updateMother subMsg activityPage motherForm

                sessionMsgs =
                    maybeMotherId
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
            , Cmd.map (MsgMotherActivity activityType maybeMotherId) subCmd
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
                            :: Maybe.Extra.toList (Maybe.map (MeasurementOutMsgChild childId) outMsg)
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
                ( subModel, subCmd, extraMsgs ) =
                    Pages.Participants.Update.update session subMsg model.participantsPage
            in
            ( { model | participantsPage = subModel }
            , Cmd.map MsgParticipants subCmd
            , List.map MsgSession extraMsgs
            )

        SetActivePage page ->
            ( model, Cmd.none, [ App.Model.SetActivePage page ] )
