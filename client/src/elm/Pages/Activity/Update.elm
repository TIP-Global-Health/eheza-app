module Pages.Activity.Update exposing (update)

import App.PageType exposing (Page(..))
import Config.Model exposing (BackendUrl)
import Date exposing (Date)
import Maybe.Extra exposing (isJust)
import Measurement.Update
import User.Model exposing (..)
import Pages.Activity.Model exposing (Model, Msg(..))
import Pages.Participant.Utils exposing (updateActivityDate)
import Participant.Model exposing (Participant, ParticipantId, ParticipantType(..), ParticipantTypeFilter(..), ParticipantsDict)


update : Date -> BackendUrl -> String -> User -> Msg -> Model -> ( Maybe ( ParticipantId, Participant ), Model, Cmd Msg, Maybe Page )
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

                modelWithMeasurements =
                    { model | measurements = measurementsUpdated }
            in
                ( Just ( participantId, participantUpdated )
                , modelWithMeasurements
                , Cmd.map (MsgMeasurement ( participantId, participantUpdated )) cmds
                , Nothing
                )

        SetRedirectPage page ->
            ( Nothing, model, Cmd.none, Just page )

        SetSelectedParticipant maybeParticipant ->
            ( Nothing, { model | selectedParticipant = maybeParticipant }, Cmd.none, Nothing )

        SetSelectedTab tab ->
            ( Nothing, { model | selectedTab = tab, selectedParticipant = Nothing }, Cmd.none, Nothing )
