module Backend.IndividualEncounterParticipant.Encoder exposing
    ( encodeDeliveryLocation
    , encodeIndividualEncounterParticipant
    , encodePregnancyOutcome
    , pregnancyOutcomeToString
    )

import Backend.IndividualEncounterParticipant.Model exposing (..)
import Backend.IndividualEncounterParticipant.Utils exposing (encoudeIndividualEncounterTypeAsString)
import Gizra.NominalDate exposing (encodeYYYYMMDD)
import Json.Encode exposing (..)
import Json.Encode.Extra exposing (maybe)
import Restful.Endpoint exposing (encodeEntityUuid)


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
    , ( "outcome", maybe encodePregnancyOutcome data.outcome )
    , ( "outcome_location", maybe encodeDeliveryLocation data.deliveryLocation )
    , ( "shard", maybe encodeEntityUuid data.shard )
    , ( "type", string "individual_participant" )
    ]


encodeIndividualEncounterType : IndividualEncounterType -> Value
encodeIndividualEncounterType type_ =
    encoudeIndividualEncounterTypeAsString type_ |> string


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
