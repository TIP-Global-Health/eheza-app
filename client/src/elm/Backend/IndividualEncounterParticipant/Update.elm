module Backend.IndividualEncounterParticipant.Update exposing (update)

import App.Model
import App.Utils exposing (triggerRollbarOnFailure)
import Backend.Endpoints exposing (individualEncounterParticipantEndpoint)
import Backend.Entities exposing (IndividualEncounterParticipantId)
import Backend.IndividualEncounterParticipant.Model exposing (IndividualEncounterParticipant, IndividualEncounterParticipantOutcome(..), Model, Msg(..))
import Backend.Utils exposing (sw)
import Gizra.NominalDate exposing (NominalDate, encodeYYYYMMDD)
import Json.Encode exposing (object)
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
            updateIndividualEncounterParticipant
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
            updateIndividualEncounterParticipant
                participantId
                maybeParticipant
                (\participant -> { participant | endDate = Just currentDate, outcome = Just (AcuteIllness outcome) })
                model

        CloseTuberculosisSession outcome ->
            updateIndividualEncounterParticipant
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
            updateIndividualEncounterParticipant
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
            -- Patch the EDD field on its own, independent of whether the
            -- participant is currently loaded in the local cache. Routing this
            -- through updateIndividualEncounterParticipant silently dropped the
            -- EDD whenever the participant was not loaded yet (e.g. right after a
            -- pregnancy is created, before its fetch resolves) - which is why
            -- some pregnancies ended up with no EDD and needed the daily backfill.
            ( { model | updateIndividualEncounterParticipant = Loading }
            , object [ ( "expected_date_concluded", encodeYYYYMMDD eddDate ) ]
                |> sw.patchAny individualEncounterParticipantEndpoint participantId
                |> withoutDecoder
                |> toCmd (RemoteData.fromResult >> HandleUpdatedIndividualEncounterParticipant)
            , []
            )

        SetNewborn personId ->
            updateIndividualEncounterParticipant
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
    IndividualEncounterParticipantId
    -> Maybe IndividualEncounterParticipant
    -> (IndividualEncounterParticipant -> IndividualEncounterParticipant)
    -> Model
    -> ( Model, Cmd Msg, List App.Model.Msg )
updateIndividualEncounterParticipant individualEncounterParticipantId maybeIndividualEncounterParticipant updateFunc model =
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
