module Pages.Activity.Update exposing (nextParticipant, update, subscriptions)

import Activity.Model exposing (ActivityType(..), ChildActivityType(..))
import App.PageType exposing (Page(..))
import Config.Model exposing (BackendUrl)
import EveryDict
import FilePicker.Model
import FilePicker.Update
import Maybe.Extra exposing (isNothing)
import Measurement.Model exposing (saveMeasurementMessage)
import Measurement.Update
import Participant.Utils exposing (getParticipantName)
import User.Model exposing (..)
import Pages.Activity.Model exposing (Model, Msg(..))
import Pages.Participant.Utils exposing (sequenceExtra)
import Participant.Model exposing (Participant(..), ParticipantId, ParticipantTypeFilter(..), ParticipantsDict)
import Participant.Utils exposing (getExamination, setExamination)
import Translate as Trans exposing (Language)


update :
    BackendUrl
    -> String
    -> User
    -> Language
    -> ParticipantsDict
    -> Msg
    -> Model
    -> ( Maybe ( ParticipantId, Participant, Measurement.Model.Model ), Model, Cmd Msg, Maybe Page )
update backendUrl accessToken user language participants msg model =
    case msg of
        MsgFilePicker subMsg ->
            let
                ( subModel, cmd ) =
                    FilePicker.Update.update backendUrl language subMsg model.filePicker
            in
                ( Nothing
                , { model | filePicker = subModel }
                , Cmd.map MsgFilePicker cmd
                , Nothing
                )

        MsgMeasurement ( participantId, participant ) subMsg ->
            let
                ( measurementsUpdated, examinationUpdated, cmds, maybeActivityTypeCompleted ) =
                    Measurement.Update.update backendUrl accessToken participantId subMsg model.measurements (getExamination participant)

                additionalMsgs =
                    case maybeActivityTypeCompleted of
                        Nothing ->
                            []

                        Just _ ->
                            case model.selectedActivity of
                                Child ChildPicture ->
                                    [ SetSelectedParticipant Nothing
                                    , SetSelectedParticipant <| nextParticipant participants model
                                    ]

                                _ ->
                                    [ SetSelectedParticipant <| nextParticipant participants model ]

                updatedModel =
                    if saveMeasurementMessage subMsg then
                        { model | measurements = Measurement.Model.emptyModel }
                    else
                        { model | measurements = measurementsUpdated }
            in
                sequenceExtra (update backendUrl accessToken user language participants)
                    additionalMsgs
                    ( Just
                        ( participantId
                        , setExamination examinationUpdated participant
                        , measurementsUpdated
                        )
                    , updatedModel
                    , Cmd.map (MsgMeasurement ( participantId, participant )) cmds
                    , Nothing
                    )

        SetRedirectPage page ->
            sequenceExtra (update backendUrl accessToken user language participants)
                [ SetSelectedParticipant Nothing ]
                ( Nothing, model, Cmd.none, Just page )

        SetSelectedParticipant maybeParticipant ->
            let
                measurements =
                    -- We're loading them from our single, mocked examintion, for the moment ...
                    -- this will need to change.
                    maybeParticipant
                        |> Maybe.map (\( participantId, participant ) -> toMeasurements (getExamination participant))
                        |> Maybe.withDefault Measurement.Model.emptyModel

                ( updatedModel, additionaMsgs ) =
                    if maybeParticipant /= model.selectedParticipant then
                        ( { model
                            | selectedParticipant = maybeParticipant
                            , measurements = measurements
                          }
                        , if isNothing maybeParticipant then
                            [ MsgFilePicker <| FilePicker.Model.Unbind ]
                          else
                            case model.selectedActivity of
                                Child ChildPicture ->
                                    [ MsgFilePicker <| FilePicker.Model.Bind ]

                                _ ->
                                    []
                        )
                    else
                        ( model, [] )
            in
                sequenceExtra (update backendUrl accessToken user language participants)
                    additionaMsgs
                    ( Nothing, updatedModel, Cmd.none, Nothing )

        SetSelectedTab tab ->
            let
                updatedModel =
                    { model | selectedTab = tab }

                additionalMsgs =
                    case model.selectedActivity of
                        Child ChildPicture ->
                            [ SetSelectedParticipant Nothing ]

                        _ ->
                            []
            in
                sequenceExtra (update backendUrl accessToken user language participants)
                    additionalMsgs
                    ( Nothing, updatedModel, Cmd.none, Nothing )


{-| Eventually, this will need to be parameterized to deal with multiple examinations.
-}
nextParticipant : ParticipantsDict -> Model -> Maybe ( ParticipantId, Participant )
nextParticipant participants model =
    let
        pendingParticipants =
            participantsWithPendingActivity participants model
                |> EveryDict.toList
                |> List.filter (\participant -> (Just <| participant) /= model.selectedParticipant)
                |> List.sortBy (\( _, participant ) -> getParticipantName participant)
    in
        -- At this point, the just completed form is still in pendingActivities.
        List.head pendingParticipants


subscriptions : Model -> ( ParticipantId, Participant ) -> Sub Pages.Activity.Model.Msg
subscriptions model ( participantId, participant ) =
    Sub.map (MsgMeasurement ( participantId, participant )) <| Measurement.Update.subscriptions model.measurements
