module Pages.Activity.Update exposing (update, subscriptions)

import Activity.Model exposing (ActivityType(..), ChildActivityType(..))
import App.PageType exposing (Page(..))
import Config.Model exposing (BackendUrl)
import Date exposing (Date)
import FilePicker.Model
import FilePicker.Update
import Maybe.Extra exposing (isNothing)
import Measurement.Model exposing (saveMeasurementMessage)
import Measurement.Update
import User.Model exposing (..)
import Pages.Activity.Model exposing (Model, Msg(..))
import Pages.Participant.Utils exposing (updateActivityDate, sequenceExtra)
import Participant.Model exposing (Participant, ParticipantId, ParticipantType(..), ParticipantTypeFilter(..), ParticipantsDict)
import Translate as Trans exposing (Language)


update :
    Date
    -> BackendUrl
    -> String
    -> User
    -> Language
    -> Msg
    -> Model
    -> ( Maybe ( ParticipantId, Participant, Measurement.Model.Model ), Model, Cmd Msg, Maybe Page )
update currentDate backendUrl accessToken user language msg model =
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
                ( measurementsUpdated, cmds, maybeActivityTypeCompleted ) =
                    Measurement.Update.update backendUrl accessToken user ( participantId, participant ) subMsg model.measurements

                newDate =
                    (Date.toTime currentDate) + (24 * 60 * 60 * 1000) |> Date.fromTime

                -- Hard-wiring the period of one day, while we consider
                -- the Activity completed.
                ( participantUpdated, additionalMsgs ) =
                    case maybeActivityTypeCompleted of
                        Nothing ->
                            ( participant, [] )

                        Just ( activtyTypeCompleted, activityToRedirect ) ->
                            ( updateActivityDate newDate activtyTypeCompleted participant
                            , [ MsgFilePicker <| FilePicker.Model.Unbind ]
                            )

                updatedModel =
                    if saveMeasurementMessage subMsg then
                        { model | measurements = Measurement.Model.emptyModel, selectedParticipant = Nothing }
                    else
                        { model | measurements = measurementsUpdated }
            in
                sequenceExtra (update currentDate backendUrl accessToken user language)
                    additionalMsgs
                    ( Just ( participantId, participantUpdated, measurementsUpdated )
                    , updatedModel
                    , Cmd.map (MsgMeasurement ( participantId, participantUpdated )) cmds
                    , Nothing
                    )

        SetRedirectPage page ->
            ( Nothing, model, Cmd.none, Just page )

        SetSelectedParticipant maybeParticipant ->
            let
                ( updatedModel, additionaMsgs ) =
                    if maybeParticipant /= model.selectedParticipant then
                        ( { model
                            | selectedParticipant = maybeParticipant
                            , measurements = Measurement.Model.emptyModel
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
                sequenceExtra (update currentDate backendUrl accessToken user language)
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
                sequenceExtra (update currentDate backendUrl accessToken user language)
                    additionalMsgs
                    ( Nothing, updatedModel, Cmd.none, Nothing )


subscriptions : Model -> ( ParticipantId, Participant ) -> Sub Pages.Activity.Model.Msg
subscriptions model ( participantId, participant ) =
    Sub.map (MsgMeasurement ( participantId, participant )) <| Measurement.Update.subscriptions model.measurements
