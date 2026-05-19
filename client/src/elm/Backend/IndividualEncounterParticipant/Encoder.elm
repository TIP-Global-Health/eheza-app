module Backend.IndividualEncounterParticipant.Encoder exposing (encodeDeliveryLocation, encodeIndividualEncounterParticipant, encodeIndividualEncounterParticipantOutcome)

import Backend.IndividualEncounterParticipant.Model exposing (AcuteIllnessOutcome, DeliveryLocation, HIVOutcome, IndividualEncounterParticipant, IndividualEncounterParticipantOutcome(..), IndividualEncounterType, PregnancyOutcome, TuberculosisOutcome)
import Backend.IndividualEncounterParticipant.Utils exposing (acuteIllnessOutcomeToString, deliveryLocationToString, hivOutcomeToString, individualEncounterTypeToString, pregnancyOutcomeToString, tuberculosisOutcomeToString)
import Gizra.NominalDate exposing (encodeYYYYMMDD)
import Json.Encode exposing (Value, bool, object, string)
import Json.Encode.Extra exposing (maybe)
import Restful.Endpoint exposing (encodeEntityUuid)
import Utils.Json exposing (encodeIfSet)


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
        ++ encodeIfSet "shard" data.shard encodeEntityUuid


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

        Tuberculosis outcome ->
            encodeTuberculosisOutcome outcome

        HIV outcome ->
            encodeHIVOutcome outcome


encodePregnancyOutcome : PregnancyOutcome -> Value
encodePregnancyOutcome outcome =
    pregnancyOutcomeToString outcome |> string


encodeDeliveryLocation : DeliveryLocation -> Value
encodeDeliveryLocation location =
    deliveryLocationToString location |> string


encodeAcuteIllnessOutcome : AcuteIllnessOutcome -> Value
encodeAcuteIllnessOutcome outcome =
    acuteIllnessOutcomeToString outcome |> string


encodeTuberculosisOutcome : TuberculosisOutcome -> Value
encodeTuberculosisOutcome outcome =
    tuberculosisOutcomeToString outcome |> string


encodeHIVOutcome : HIVOutcome -> Value
encodeHIVOutcome outcome =
    hivOutcomeToString outcome |> string
