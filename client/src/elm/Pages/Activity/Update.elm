module Pages.Activity.Update exposing (update)

import App.PageType exposing (Page(..))
import Config.Model exposing (BackendUrl)
import Date
import Maybe.Extra exposing (isJust)
import Measurement.Update
import User.Model exposing (..)
import Pages.Activity.Model exposing (Model, Msg(..))
import Participant.Model exposing (Participant, ParticipantId, ParticipantType(..), ParticipantTypeFilter(..), ParticipantsDict)


update : BackendUrl -> String -> User -> Msg -> Model -> ( Model, Cmd Msg, Maybe Page )
update backendUrl accessToken user msg model =
    case msg of
        MsgMeasurement ( participantId, participant ) subMsg ->
            let
                ( measurementsUpdated, cmds, maybeActivityTypeCompleted ) =
                    Measurement.Update.update backendUrl accessToken user ( participantId, participant ) subMsg model.measurements

                -- newDate =
                --     (Date.toTime currentDate) + (24 * 60 * 60 * 1000) |> Date.fromTime
                --
                -- -- Hard-wiring the period of one day, while we consider
                -- -- the Activity completed.
                -- participantUpdated =
                --     case maybeActivityTypeCompleted of
                --         Nothing ->
                --             participant
                --
                --         Just ( activtyTypeCompleted, activityToRedirect ) ->
                --             updateActivityDate newDate activtyTypeCompleted participant
                modelWithMeasurements =
                    { model | measurements = measurementsUpdated }

                -- selectedActivity =
                --     if isJust maybeActivityTypeCompleted then
                --         Maybe.map (\( _, redirectToActivity ) -> Just redirectToActivity) maybeActivityTypeCompleted
                --             |> Maybe.withDefault Nothing
                --     else
                --         model.selectedActivity
            in
                ( model
                , Cmd.map (MsgMeasurement ( participantId, participant )) cmds
                , Nothing
                )

        SetRedirectPage page ->
            ( model, Cmd.none, Just page )

        SetSelectedParticipant maybeParticipant ->
            ( { model | selectedParticipant = maybeParticipant }, Cmd.none, Nothing )

        SetSelectedTab tab ->
            ( { model | selectedTab = tab, selectedParticipant = Nothing }, Cmd.none, Nothing )
