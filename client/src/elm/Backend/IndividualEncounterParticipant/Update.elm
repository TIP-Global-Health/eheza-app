module Backend.IndividualEncounterParticipant.Update exposing (update)

import App.Model
import App.Utils exposing (triggerRollbarOnFailure)
import Backend.Endpoints exposing (individualEncounterParticipantEndpoint)
import Backend.Entities exposing (IndividualEncounterParticipantId)
import Backend.IndividualEncounterParticipant.Model exposing (IndividualEncounterParticipant, IndividualEncounterParticipantOutcome(..), Model, Msg(..))
import Backend.Utils exposing (sw)
import Gizra.NominalDate exposing (NominalDate)
import Maybe.Extra exposing (unwrap)
import RemoteData exposing (RemoteData(..))
import Restful.Endpoint exposing (toCmd, withoutDecoder)


update :
    NominalDate
    -> IndividualEncounterParticipantId
    -> Maybe IndividualEncounterParticipant
    -> Msg
    -> Model
    -> ( Model, Cmd Msg, List App.Model.Msg )
update currentDate participantId maybeParticipant msg model =
    case msg of
        ClosePrenatalSession concludedDate outcome deliveryLocation ->
            updateIndividualEncounterParticipant currentDate
                participantId
                maybeParticipant
                (\participant ->
                    { participant
                        | endDate = Just currentDate
                        , dateConcluded = Just concludedDate
                        , outcome = Just (Pregnancy outcome)
                        , deliveryLocation = Just deliveryLocation
                    }
                )
                model

        CloseAcuteIllnessSession outcome ->
            updateIndividualEncounterParticipant currentDate
                participantId
                maybeParticipant
                (\participant -> { participant | endDate = Just currentDate, outcome = Just (AcuteIllness outcome) })
                model

        CloseTuberculosisSession outcome ->
            updateIndividualEncounterParticipant currentDate
                participantId
                maybeParticipant
                (\participant ->
                    { participant
                        | endDate = Just currentDate
                        , dateConcluded = Just currentDate
                        , outcome = Just (Tuberculosis outcome)
                    }
                )
                model

        CloseHIVSession outcome ->
            updateIndividualEncounterParticipant currentDate
                participantId
                maybeParticipant
                (\participant ->
                    { participant
                        | endDate = Just currentDate
                        , dateConcluded = Just currentDate
                        , outcome = Just (HIV outcome)
                    }
                )
                model

        SetEddDate eddDate ->
            updateIndividualEncounterParticipant currentDate
                participantId
                maybeParticipant
                (\participant -> { participant | eddDate = Just eddDate })
                model

        SetNewborn personId ->
            updateIndividualEncounterParticipant currentDate
                participantId
                maybeParticipant
                (\participant -> { participant | newborn = Just personId })
                model

        HandleUpdatedIndividualEncounterParticipant data ->
            ( { model | updateIndividualEncounterParticipant = data }
            , Cmd.none
            , triggerRollbarOnFailure data
            )


updateIndividualEncounterParticipant :
    NominalDate
    -> IndividualEncounterParticipantId
    -> Maybe IndividualEncounterParticipant
    -> (IndividualEncounterParticipant -> IndividualEncounterParticipant)
    -> Model
    -> ( Model, Cmd Msg, List App.Model.Msg )
updateIndividualEncounterParticipant currentDate individualEncounterParticipantId maybeIndividualEncounterParticipant updateFunc model =
    maybeIndividualEncounterParticipant
        |> unwrap ( model, Cmd.none, [] )
            (\individualEncounterParticipant ->
                ( { model | updateIndividualEncounterParticipant = Loading }
                , updateFunc individualEncounterParticipant
                    |> sw.patchFull individualEncounterParticipantEndpoint individualEncounterParticipantId
                    |> withoutDecoder
                    |> toCmd (RemoteData.fromResult >> HandleUpdatedIndividualEncounterParticipant)
                , []
                )
            )
