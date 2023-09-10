module Backend.IndividualEncounterParticipant.Encoder exposing (acuteIllnessOutcomeToString, encodeDeliveryLocation, encodeIndividualEncounterParticipant, encodeIndividualEncounterParticipantOutcome, pregnancyOutcomeToString)

import Backend.IndividualEncounterParticipant.Model exposing (..)
import Backend.IndividualEncounterParticipant.Utils exposing (individualEncounterTypeToString)
import Gizra.NominalDate exposing (encodeYYYYMMDD)
import Json.Encode exposing (..)
import Json.Encode.Extra exposing (maybe)
import Restful.Endpoint exposing (encodeEntityUuid)
import Utils.Json exposing (encodeIfExists)


encodeIndividualEncounterParticipant : IndividualEncounterParticipant -> List ( String, Value )
encodeIndividualEncounterParticipant data =
    [ ( "person", encodeEntityUuid data.person )
    , ( "encounter_type", encodeIndividualEncounterType data.encounterType )
    , ( "expected"
      , object
            [ ( "value", encodeYYYYMMDD data.startDate )
            , ( "value2", maybe encodeYYYYMMDD data.endDate )
            ]
      )
    , ( "expected_date_concluded", maybe encodeYYYYMMDD data.eddDate )
    , ( "date_concluded", maybe encodeYYYYMMDD data.dateConcluded )
    , ( "outcome", maybe encodeIndividualEncounterParticipantOutcome data.outcome )
    , ( "outcome_location", maybe encodeDeliveryLocation data.deliveryLocation )
    , ( "newborn", maybe encodeEntityUuid data.newborn )
    , ( "deleted", bool data.deleted )
    , ( "type", string "individual_participant" )
    ]
        ++ encodeIfExists "shard" data.shard encodeEntityUuid


encodeIndividualEncounterType : IndividualEncounterType -> Value
encodeIndividualEncounterType type_ =
    individualEncounterTypeToString type_ |> string


encodeIndividualEncounterParticipantOutcome : IndividualEncounterParticipantOutcome -> Value
encodeIndividualEncounterParticipantOutcome participantOutcome =
    case participantOutcome of
        Pregnancy outcome ->
            encodePregnancyOutcome outcome

        AcuteIllness outcome ->
            encodeAcuteIllnessOutcome outcome


encodePregnancyOutcome : PregnancyOutcome -> Value
encodePregnancyOutcome outcome =
    pregnancyOutcomeToString outcome |> string


pregnancyOutcomeToString : PregnancyOutcome -> String
pregnancyOutcomeToString outcome =
    case outcome of
        OutcomeLiveAtTerm ->
            "live-at-term"

        OutcomeLivePreTerm ->
            "live-pre-term"

        OutcomeStillAtTerm ->
            "still-at-term"

        OutcomeStillPreTerm ->
            "still-pre-term"

        OutcomeAbortions ->
            "abortions"


encodeDeliveryLocation : DeliveryLocation -> Value
encodeDeliveryLocation location =
    deliveryLocationToString location |> string


deliveryLocationToString : DeliveryLocation -> String
deliveryLocationToString location =
    case location of
        FacilityDelivery ->
            "facility"

        HomeDelivery ->
            "home"


acuteIllnessOutcomeToString : AcuteIllnessOutcome -> String
acuteIllnessOutcomeToString outcome =
    case outcome of
        OutcomeIllnessResolved ->
            "illness-resolved"

        OutcomeLostToFollowUp ->
            "lost-to-follow-up"

        OutcomeMovedOutsideCA ->
            "moved-out-of-ca"

        OutcomePatientDied ->
            "patient-died"

        OutcomeReferredToHC ->
            "referred-to-hc"

        OutcomeOther ->
            "other"


encodeAcuteIllnessOutcome : AcuteIllnessOutcome -> Value
encodeAcuteIllnessOutcome outcome =
    acuteIllnessOutcomeToString outcome |> string
