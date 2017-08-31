module Pages.Activity.Update exposing (update)

import App.PageType exposing (Page(..))
import Config.Model exposing (BackendUrl)
import Date exposing (Date)
import Measurement.Model exposing (saveMeasurementMessage)
import Measurement.Update
import User.Model exposing (..)
import Pages.Activity.Model exposing (Model, Msg(..))
import Pages.Participant.Utils exposing (updateActivityDate)
import Participant.Model exposing (Participant, ParticipantId, ParticipantType(..), ParticipantTypeFilter(..), ParticipantsDict)


update :
    Date
    -> BackendUrl
    -> String
    -> User
    -> Msg
    -> Model
    -> ( Maybe ( ParticipantId, Participant, Measurement.Model.Model ), Model, Cmd Msg, Maybe Page )
update currentDate backendUrl accessToken user msg model =
    case msg of
        MsgMeasurement ( participantId, participant ) subMsg ->
            let
                ( measurementsUpdated, cmds, maybeActivityTypeCompleted ) =
                    Measurement.Update.update backendUrl accessToken user ( participantId, participant ) subMsg model.measurements

                newDate =
                    (Date.toTime currentDate) + (24 * 60 * 60 * 1000) |> Date.fromTime

                -- Hard-wiring the period of one day, while we consider
                -- the Activity completed.
                participantUpdated =
                    case maybeActivityTypeCompleted of
                        Nothing ->
                            participant

                        Just ( activtyTypeCompleted, activityToRedirect ) ->
                            updateActivityDate newDate activtyTypeCompleted participant

                updatedModel =
                    if saveMeasurementMessage subMsg then
                        { model | measurements = Measurement.Model.emptyModel, selectedParticipant = Nothing }
                    else
                        { model | measurements = measurementsUpdated }
            in
                ( Just ( participantId, participantUpdated, measurementsUpdated )
                , updatedModel
                , Cmd.map (MsgMeasurement ( participantId, participantUpdated )) cmds
                , Nothing
                )

        SetRedirectPage page ->
            ( Nothing, model, Cmd.none, Just page )

        SetSelectedParticipant maybeParticipant ->
            let
                updatedModel =
                    if maybeParticipant /= model.selectedParticipant then
                        { model | selectedParticipant = maybeParticipant, measurements = Measurement.Model.emptyModel }
                    else
                        model
            in
                ( Nothing, updatedModel, Cmd.none, Nothing )

        SetSelectedTab tab ->
            ( Nothing, { model | selectedTab = tab, selectedParticipant = Nothing }, Cmd.none, Nothing )
