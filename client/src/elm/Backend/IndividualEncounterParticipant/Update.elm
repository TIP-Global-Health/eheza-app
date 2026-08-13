module Backend.IndividualEncounterParticipant.Update exposing (update)

import App.Model
import App.Utils exposing (triggerRollbarOnFailure)
import Backend.Endpoints exposing (individualEncounterParticipantEndpoint)
import Backend.Entities exposing (IndividualEncounterParticipantId)
import Backend.IndividualEncounterParticipant.Model exposing (IndividualEncounterParticipant, IndividualEncounterParticipantOutcome(..), Model, Msg(..))
import Backend.Utils exposing (sw)
import Error.Model exposing (ErrorType(..))
import Gizra.NominalDate exposing (NominalDate)
import Maybe.Extra exposing (unwrap)
import RemoteData exposing (RemoteData(..), WebData)
import Restful.Endpoint exposing (fromEntityUuid, toCmd, withoutDecoder)
import Utils.WebData exposing (viewErrorForRollbar)


update :
    NominalDate
    -> IndividualEncounterParticipantId
    -> Maybe (WebData IndividualEncounterParticipant)
    -> Msg
    -> Model
    -> ( Model, Cmd Msg, List App.Model.Msg )
update currentDate participantId participantState msg model =
    case msg of
        ClosePrenatalSession concludedDate outcome deliveryLocation ->
            updateIndividualEncounterParticipant
                participantId
                participantState
                "close-prenatal-session"
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
            updateIndividualEncounterParticipant
                participantId
                participantState
                "close-acute-illness-session"
                (\participant -> { participant | endDate = Just currentDate, outcome = Just (AcuteIllness outcome) })
                model

        CloseTuberculosisSession outcome ->
            updateIndividualEncounterParticipant
                participantId
                participantState
                "close-tuberculosis-session"
                (\participant ->
                    { participant
                        | endDate = Just currentDate
                        , dateConcluded = Just currentDate
                        , outcome = Just (Tuberculosis outcome)
                    }
                )
                model

        CloseHIVSession outcome ->
            updateIndividualEncounterParticipant
                participantId
                participantState
                "close-hiv-session"
                (\participant ->
                    { participant
                        | endDate = Just currentDate
                        , dateConcluded = Just currentDate
                        , outcome = Just (HIV outcome)
                    }
                )
                model

        SetEddDate eddDate ->
            updateIndividualEncounterParticipant
                participantId
                participantState
                "set-edd-date"
                (\participant -> { participant | eddDate = Just eddDate })
                model

        SetNewborn personId ->
            updateIndividualEncounterParticipant
                participantId
                participantState
                "set-newborn"
                (\participant -> { participant | newborn = Just personId })
                model

        HandleUpdatedIndividualEncounterParticipant data ->
            ( { model | updateIndividualEncounterParticipant = data }
            , Cmd.none
            , triggerRollbarOnFailure data
            )


updateIndividualEncounterParticipant :
    IndividualEncounterParticipantId
    -> Maybe (WebData IndividualEncounterParticipant)
    -> String
    -> (IndividualEncounterParticipant -> IndividualEncounterParticipant)
    -> Model
    -> ( Model, Cmd Msg, List App.Model.Msg )
updateIndividualEncounterParticipant individualEncounterParticipantId participantState action updateFunc model =
    participantState
        |> Maybe.andThen RemoteData.toMaybe
        |> unwrap
            -- The participant is not loaded, so there is nothing to apply the
            -- update to, and it is discarded. Discarding used to be silent,
            -- which made field occurrences (pregnancies left without EDD)
            -- undiagnosable - so report it, with the cache state that explains
            -- why the participant was not available.
            ( model
            , Cmd.none
            , [ ("Dropped '"
                    ++ action
                    ++ "' for individual participant "
                    ++ fromEntityUuid individualEncounterParticipantId
                    ++ ": participant not loaded (cache state: "
                    ++ describeParticipantState participantState
                    ++ ")"
                )
                    |> Plain
                    |> App.Model.TriggerRollbar App.Model.IndexedDB
              ]
            )
            (\individualEncounterParticipant ->
                ( { model | updateIndividualEncounterParticipant = Loading }
                , updateFunc individualEncounterParticipant
                    |> sw.patchFull individualEncounterParticipantEndpoint individualEncounterParticipantId
                    |> withoutDecoder
                    |> toCmd (RemoteData.fromResult >> HandleUpdatedIndividualEncounterParticipant)
                , []
                )
            )


describeParticipantState : Maybe (WebData IndividualEncounterParticipant) -> String
describeParticipantState participantState =
    case participantState of
        Nothing ->
            "missing"

        Just NotAsked ->
            "not-asked"

        Just Loading ->
            "loading"

        Just (Failure error) ->
            "failure - " ++ viewErrorForRollbar error

        Just (Success _) ->
            "success"
