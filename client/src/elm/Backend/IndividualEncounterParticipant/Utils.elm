module Backend.IndividualEncounterParticipant.Utils exposing (..)

import Backend.IndividualEncounterParticipant.Model exposing (..)
import Backend.PatientRecord.Utils
import Gizra.NominalDate exposing (NominalDate)
import Maybe.Extra exposing (isNothing)
import Restful.Endpoint exposing (fromEntityUuid, toEntityUuid)


individualEncounterTypeToString : IndividualEncounterType -> String
individualEncounterTypeToString encounterType =
    case encounterType of
        AcuteIllnessEncounter ->
            "acute-illness"

        AntenatalEncounter ->
            "antenatal"

        ChildScoreboardEncounter ->
            "child-scoreboard"

        HealthyStartEncounter ->
            "healthy-start"

        HIVEncounter ->
            "hiv"

        HomeVisitEncounter ->
            "home-visit"

        InmmunizationEncounter ->
            "inmmunization"

        NCDEncounter ->
            "ncd"

        NutritionEncounter ->
            "nutrition"

        TuberculosisEncounter ->
            "tuberculosis"

        WellChildEncounter ->
            "well-child"


individualEncounterTypeFromString : String -> Maybe IndividualEncounterType
individualEncounterTypeFromString string =
    case string of
        "acute-illness" ->
            Just AcuteIllnessEncounter

        "antenatal" ->
            Just AntenatalEncounter

        "child-scoreboard" ->
            Just ChildScoreboardEncounter

        "healthy-start" ->
            Just HealthyStartEncounter

        "hiv" ->
            Just HIVEncounter

        "home-visit" ->
            Just HomeVisitEncounter

        "inmmunization" ->
            Just InmmunizationEncounter

        "ncd" ->
            Just NCDEncounter

        "nutrition" ->
            Just NutritionEncounter

        "tuberculosis" ->
            Just TuberculosisEncounter

        "well-child" ->
            Just WellChildEncounter

        _ ->
            Nothing


isDailyEncounterActive : NominalDate -> { a | startDate : NominalDate, endDate : Maybe NominalDate } -> Bool
isDailyEncounterActive currentDate encounter =
    encounter.startDate == currentDate && isNothing encounter.endDate


initiatorToUrlFragment : IndividualParticipantInitiator -> String
initiatorToUrlFragment initiator =
    case initiator of
        InitiatorParticipantsPage ->
            "participants-page"

        InitiatorPatientRecord patientRecordInitiator personId ->
            "patient-record-"
                ++ fromEntityUuid personId
                ++ "+++"
                ++ Backend.PatientRecord.Utils.progressReportInitiatorToUrlFragment patientRecordInitiator


initiatorFromUrlFragment : String -> Maybe IndividualParticipantInitiator
initiatorFromUrlFragment s =
    case s of
        "participants-page" ->
            Just InitiatorParticipantsPage

        _ ->
            if String.startsWith "patient-record-" s then
                let
                    fragments =
                        String.dropLeft 15 s
                            |> String.split "+++"
                in
                if List.length fragments /= 2 then
                    Nothing

                else
                    Maybe.map2
                        (\personId patientRecordInitiator ->
                            Just <| InitiatorPatientRecord patientRecordInitiator (toEntityUuid personId)
                        )
                        (List.head fragments)
                        (List.drop 1 fragments
                            |> List.head
                            |> Maybe.andThen Backend.PatientRecord.Utils.progressReportInitiatorFromUrlFragment
                        )
                        |> Maybe.Extra.join

            else
                Nothing


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


pregnancyOutcomeFromString : String -> Maybe PregnancyOutcome
pregnancyOutcomeFromString outcome =
    case outcome of
        "live-at-term" ->
            Just OutcomeLiveAtTerm

        "live-pre-term" ->
            Just OutcomeLivePreTerm

        "still-at-term" ->
            Just OutcomeStillAtTerm

        "still-pre-term" ->
            Just OutcomeStillPreTerm

        "abortions" ->
            Just OutcomeAbortions

        _ ->
            Nothing


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


acuteIllnessOutcomeFromString : String -> Maybe AcuteIllnessOutcome
acuteIllnessOutcomeFromString outcome =
    case outcome of
        "illness-resolved" ->
            Just OutcomeIllnessResolved

        "lost-to-follow-up" ->
            Just OutcomeLostToFollowUp

        "moved-out-of-ca" ->
            Just OutcomeMovedOutsideCA

        "patient-died" ->
            Just OutcomePatientDied

        "referred-to-hc" ->
            Just OutcomeReferredToHC

        "other" ->
            Just OutcomeOther

        _ ->
            Nothing


deliveryLocationToString : DeliveryLocation -> String
deliveryLocationToString location =
    case location of
        FacilityDelivery ->
            "facility"

        HomeDelivery ->
            "home"


deliveryLocationFromString : String -> Maybe DeliveryLocation
deliveryLocationFromString location =
    case location of
        "facility" ->
            Just FacilityDelivery

        "home" ->
            Just HomeDelivery

        _ ->
            Nothing


tuberculosisOutcomeToString : TuberculosisOutcome -> String
tuberculosisOutcomeToString outcome =
    case outcome of
        TuberculosisOutcomeNotDiagnosed ->
            "not-dignosed"


tuberculosisOutcomeFromString : String -> Maybe TuberculosisOutcome
tuberculosisOutcomeFromString outcome =
    case outcome of
        "not-dignosed" ->
            Just TuberculosisOutcomeNotDiagnosed

        _ ->
            Nothing


hivOutcomeToString : HIVOutcome -> String
hivOutcomeToString outcome =
    case outcome of
        HIVOutcomeNotDiagnosed ->
            "hiv-not-dignosed"


hivOutcomeFromString : String -> Maybe HIVOutcome
hivOutcomeFromString outcome =
    case outcome of
        "hiv-not-dignosed" ->
            Just HIVOutcomeNotDiagnosed

        _ ->
            Nothing
