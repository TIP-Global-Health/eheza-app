module Backend.Reports.Decoder exposing (decodeReportsData, decodeSyncResponse)

import AssocList as Dict exposing (Dict)
import Backend.Components.Decoder exposing (decodeReportParams, decodeSelectedEntity)
import Backend.Decoder exposing (decodeSite, decodeSiteFeatures, decodeWithFallback)
import Backend.Reports.Model exposing (AcuteIllnessDiagnosis(..), AcuteIllnessEncounterData, AcuteIllnessEncounterType(..), BackendGeneratedNutritionReportTableDate, DeliveryLocation(..), FamilyNutritionEncounterData, FamilyNutritionMotherEncounterData, Gender(..), MotherFbfEncounterData, NutritionData, NutritionEncounterData, NutritionReportTableType(..), PatientData, PregnancyOutcome(..), PrenatalDiagnosis(..), PrenatalEncounterData, PrenatalEncounterType(..), PrenatalIndicator(..), PrenatalParticipantData, ReportsData, SyncResponse, WellChildEncounterData)
import Backend.Reports.Utils exposing (genderFromString)
import Backend.Scoreboard.Model exposing (VaccineType(..))
import Date
import EverySet exposing (EverySet)
import Gizra.Json exposing (decodeInt)
import Gizra.NominalDate exposing (NominalDate, decodeYYYYMMDD)
import Json.Decode exposing (Decoder, andThen, fail, field, list, nullable, string, succeed)
import Json.Decode.Pipeline exposing (hardcoded, optional, optionalAt, required)
import Maybe.Extra


decodeReportsData : Decoder ReportsData
decodeReportsData =
    succeed ReportsData
        |> required "site" decodeSite
        |> required "features" decodeSiteFeatures
        |> required "entity_name" string
        |> required "entity_type" decodeSelectedEntity
        |> required "params" decodeReportParams
        |> hardcoded []
        |> optionalAt [ "additional", "nutrition_report_data" ] (nullable (list decodeBackendGeneratedNutritionReportTableDate)) Nothing
        |> hardcoded Nothing


decodeSyncResponse : Decoder SyncResponse
decodeSyncResponse =
    field "data"
        (succeed SyncResponse
            |> required "batch" (list decodePatientData)
            |> required "total_remaining" decodeInt
            |> required "last" decodeInt
        )


decodePatientData : Decoder PatientData
decodePatientData =
    succeed PatientData
        |> required "id" decodeInt
        |> required "created" decodeYYYYMMDD
        |> required "birth_date" decodeYYYYMMDD
        |> required "gender" (decodeWithFallback Female decodeGender)
        |> optionalAt [ "individual", "acute-illness" ] (nullable (list (list decodeAcuteIllnessEncounterData))) Nothing
        |> optionalAt [ "individual", "antenatal" ] (nullable (list decodePrenatalParticipantData)) Nothing
        |> optionalAt [ "individual", "family-nutrition" ] (nullable (list (list decodeFamilyNutritionMotherEncounterData))) Nothing
        |> optionalAt [ "individual", "family-nutrition-muac" ] (nullable (list (list decodeFamilyNutritionEncounterData))) Nothing
        |> optionalAt [ "individual", "home-visit" ] (nullable (list (list decodeYYYYMMDD))) Nothing
        |> optionalAt [ "individual", "well-child" ] (nullable (list (list decodeWellChildEncounterData))) Nothing
        |> optionalAt [ "individual", "child-scoreboard" ] (nullable (list (list decodeYYYYMMDD))) Nothing
        |> optionalAt [ "individual", "ncd" ] (nullable (list (list decodeYYYYMMDD))) Nothing
        |> optionalAt [ "individual", "hiv" ] (nullable (list (list decodeYYYYMMDD))) Nothing
        |> optionalAt [ "individual", "tuberculosis" ] (nullable (list (list decodeYYYYMMDD))) Nothing
        |> optionalAt [ "individual", "nutrition" ] (nullable (list (list decodeNutritionEncounterData))) Nothing
        |> optionalAt [ "group_nutrition", "pmtct" ] (nullable (list decodeNutritionEncounterData)) Nothing
        |> optionalAt [ "group_nutrition", "fbf" ] (nullable (list decodeNutritionEncounterData)) Nothing
        |> optionalAt [ "group_nutrition", "sorwathe" ] (nullable (list decodeNutritionEncounterData)) Nothing
        |> optionalAt [ "group_nutrition", "chw" ] (nullable (list decodeNutritionEncounterData)) Nothing
        |> optionalAt [ "group_nutrition", "achi" ] (nullable (list decodeNutritionEncounterData)) Nothing
        |> optionalAt [ "group_nutrition_mother", "fbf" ] (nullable (list decodeMotherFbfEncounterData)) Nothing


decodeGender : Decoder Gender
decodeGender =
    string
        |> andThen
            (\gender ->
                genderFromString gender
                    |> Maybe.map succeed
                    |> Maybe.withDefault (fail <| gender ++ " is not a recognized Gender.")
            )


decodeAcuteIllnessEncounterData : Decoder AcuteIllnessEncounterData
decodeAcuteIllnessEncounterData =
    string
        |> andThen
            (\s ->
                case String.split "|" (String.trim s) of
                    [ first, second, third ] ->
                        Date.fromIsoString first
                            |> Result.toMaybe
                            |> Maybe.map
                                (\startDate ->
                                    let
                                        encounterType =
                                            acuteIllnessEncounterTypeFromString second
                                                |> Maybe.withDefault AcuteIllnessEncounterCHW

                                        diagnosis =
                                            acuteIllnessDiagnosisFromMapping third
                                    in
                                    succeed (AcuteIllnessEncounterData startDate encounterType diagnosis)
                                )
                            |> Maybe.withDefault (fail "Failed to decode AcuteIllnessEncounterData")

                    _ ->
                        fail "Failed to decode AcuteIllnessEncounterData"
            )


acuteIllnessEncounterTypeFromString : String -> Maybe AcuteIllnessEncounterType
acuteIllnessEncounterTypeFromString encounterType =
    case encounterType of
        "nurse-encounter" ->
            Just AcuteIllnessEncounterNurse

        "nurse-encounter-subsequent" ->
            Just AcuteIllnessEncounterNurseSubsequent

        "chw-encounter" ->
            Just AcuteIllnessEncounterCHW

        _ ->
            Nothing


acuteIllnessDiagnosisFromMapping : String -> Maybe AcuteIllnessDiagnosis
acuteIllnessDiagnosisFromMapping mapping =
    case mapping of
        "a" ->
            Just DiagnosisCovid19Suspect

        "b" ->
            Just DiagnosisSevereCovid19

        "c" ->
            Just DiagnosisPneuminialCovid19

        "d" ->
            Just DiagnosisLowRiskCovid19

        "e" ->
            Just DiagnosisMalariaComplicated

        "f" ->
            Just DiagnosisMalariaUncomplicated

        "g" ->
            Just DiagnosisMalariaUncomplicatedAndPregnant

        "h" ->
            Just DiagnosisGastrointestinalInfectionComplicated

        "i" ->
            Just DiagnosisGastrointestinalInfectionUncomplicated

        "j" ->
            Just DiagnosisSimpleColdAndCough

        "k" ->
            Just DiagnosisRespiratoryInfectionComplicated

        "l" ->
            Just DiagnosisRespiratoryInfectionUncomplicated

        "m" ->
            Just DiagnosisFeverOfUnknownOrigin

        "n" ->
            Just DiagnosisUndeterminedMoreEvaluationNeeded

        "o" ->
            Just DiagnosisTuberculosisSuspect

        _ ->
            Nothing


decodePrenatalParticipantData : Decoder PrenatalParticipantData
decodePrenatalParticipantData =
    succeed PrenatalParticipantData
        |> required "created" decodeYYYYMMDD
        |> optional "edd" (nullable decodeYYYYMMDD) Nothing
        |> optional "dc" (nullable decodeYYYYMMDD) Nothing
        |> optional "o" (nullable decodePregnancyOutcome) Nothing
        |> optional "ol" (nullable decodeDeliveryLocation) Nothing
        |> required "encounters" (list decodePrenatalEncounterData)


decodeDeliveryLocation : Decoder DeliveryLocation
decodeDeliveryLocation =
    string
        |> andThen
            (\s ->
                deliveryLocationFromString s
                    |> Maybe.map succeed
                    |> Maybe.withDefault (s ++ " is not a recognized DeliveryLocation" |> fail)
            )


deliveryLocationFromString : String -> Maybe DeliveryLocation
deliveryLocationFromString location =
    case location of
        "f" ->
            Just FacilityDelivery

        "h" ->
            Just HomeDelivery

        _ ->
            Nothing


decodePregnancyOutcome : Decoder PregnancyOutcome
decodePregnancyOutcome =
    string
        |> andThen
            (\s ->
                pregnancyOutcomeFromString s
                    |> Maybe.map succeed
                    |> Maybe.withDefault (s ++ " is not a recognized PregnancyOutcome" |> fail)
            )


pregnancyOutcomeFromString : String -> Maybe PregnancyOutcome
pregnancyOutcomeFromString outcome =
    case outcome of
        "a" ->
            Just OutcomeLiveAtTerm

        "b" ->
            Just OutcomeLivePreTerm

        "c" ->
            Just OutcomeStillAtTerm

        "d" ->
            Just OutcomeStillPreTerm

        "e" ->
            Just OutcomeAbortions

        _ ->
            Nothing


decodePrenatalEncounterData : Decoder PrenatalEncounterData
decodePrenatalEncounterData =
    string
        |> andThen
            (\s ->
                case String.split "|" (String.trim s) of
                    [ first, second, third, fourth ] ->
                        Date.fromIsoString first
                            |> Result.toMaybe
                            |> Maybe.map
                                (\startDate ->
                                    let
                                        encounterType =
                                            prenatalEncounterTypeFromString second

                                        diagnoses =
                                            if String.isEmpty third then
                                                []

                                            else
                                                String.split "," third
                                                    |> List.map prenatalDiagnosisFromMapping
                                                    |> Maybe.Extra.values

                                        indicators =
                                            if String.isEmpty fourth then
                                                []

                                            else
                                                String.split "," fourth
                                                    |> List.map prenatalIndicatorFromMapping
                                                    |> Maybe.Extra.values
                                    in
                                    succeed (PrenatalEncounterData startDate encounterType diagnoses indicators)
                                )
                            |> Maybe.withDefault (fail "Failed to decode PrenatalEncounterData")

                    _ ->
                        fail "Failed to decode PrenatalEncounterData"
            )


prenatalEncounterTypeFromString : String -> PrenatalEncounterType
prenatalEncounterTypeFromString encounterType =
    case encounterType of
        "nurse" ->
            NurseEncounter

        "nurse-postpartum" ->
            NursePostpartumEncounter

        "chw-1" ->
            ChwFirstEncounter

        "chw-2" ->
            ChwSecondEncounter

        "chw-3" ->
            ChwThirdPlusEncounter

        "chw-postpartum" ->
            ChwPostpartumEncounter

        _ ->
            -- Fallback.
            NurseEncounter


prenatalDiagnosisFromMapping : String -> Maybe PrenatalDiagnosis
prenatalDiagnosisFromMapping s =
    case s of
        "a" ->
            Just DiagnosisChronicHypertension

        "b" ->
            Just DiagnosisGestationalHypertension

        "c" ->
            Just DiagnosisModeratePreeclampsia

        "d" ->
            Just DiagnosisSeverePreeclampsia

        "g" ->
            Just DiagnosisEclampsia

        "h" ->
            Just DiagnosisHIV

        "i" ->
            Just DiagnosisHIVDetectableViralLoad

        "j" ->
            Just DiagnosisDiscordantPartnership

        "k" ->
            Just DiagnosisSyphilis

        "l" ->
            Just DiagnosisSyphilisWithComplications

        "m" ->
            Just DiagnosisNeurosyphilis

        "n" ->
            Just DiagnosisHepatitisB

        "o" ->
            Just DiagnosisMalaria

        "p" ->
            Just DiagnosisMalariaWithAnemia

        "q" ->
            Just DiagnosisMalariaWithSevereAnemia

        "r" ->
            Just DiagnosisModerateAnemia

        "s" ->
            Just DiagnosisSevereAnemia

        "t" ->
            Just DiagnosisSevereAnemiaWithComplications

        "u" ->
            Just DiagnosisMiscarriage

        "v" ->
            Just DiagnosisMolarPregnancy

        "w" ->
            Just DiagnosisPlacentaPrevia

        "x" ->
            Just DiagnosisPlacentalAbruption

        "y" ->
            Just DiagnosisUterineRupture

        "z" ->
            Just DiagnosisObstructedLabor

        "a1" ->
            Just DiagnosisPostAbortionSepsis

        "b1" ->
            Just DiagnosisEctopicPregnancy

        "c1" ->
            Just DiagnosisPROM

        "d1" ->
            Just DiagnosisPPROM

        "e1" ->
            Just DiagnosisHyperemesisGravidum

        "f1" ->
            Just DiagnosisSevereVomiting

        "g1" ->
            Just DiagnosisMaternalComplications

        "h1" ->
            Just DiagnosisInfection

        "i1" ->
            Just DiagnosisImminentDelivery

        "j1" ->
            Just DiagnosisLaborAndDelivery

        "k1" ->
            Just DiagnosisHeartburn

        "l1" ->
            Just DiagnosisDeepVeinThrombosis

        "m1" ->
            Just DiagnosisPelvicPainIntense

        "n1" ->
            Just DiagnosisUrinaryTractInfection

        "o1" ->
            Just DiagnosisPyelonephritis

        "p1" ->
            Just DiagnosisCandidiasis

        "q1" ->
            Just DiagnosisGonorrhea

        "r1" ->
            Just DiagnosisTrichomonasOrBacterialVaginosis

        "s1" ->
            Just DiagnosisTuberculosis

        "t1" ->
            Just DiagnosisDiabetes

        "u1" ->
            Just DiagnosisGestationalDiabetes

        "v1" ->
            Just DiagnosisRhesusNegative

        "w1" ->
            Just DiagnosisDepressionNotLikely

        "x1" ->
            Just DiagnosisDepressionPossible

        "y1" ->
            Just DiagnosisDepressionHighlyPossible

        "z1" ->
            Just DiagnosisDepressionProbable

        "a2" ->
            Just DiagnosisSuicideRisk

        "b2" ->
            Just DiagnosisOther

        "c2" ->
            Just DiagnosisPostpartumAbdominalPain

        "d2" ->
            Just DiagnosisPostpartumUrinaryIncontinence

        "e2" ->
            Just DiagnosisPostpartumHeadache

        "f2" ->
            Just DiagnosisPostpartumFatigue

        "g2" ->
            Just DiagnosisPostpartumFever

        "h2" ->
            Just DiagnosisPostpartumPerinealPainOrDischarge

        "i2" ->
            Just DiagnosisPostpartumInfection

        "j2" ->
            Just DiagnosisPostpartumExcessiveBleeding

        "k2" ->
            Just DiagnosisPostpartumEarlyMastitisOrEngorgment

        "l2" ->
            Just DiagnosisPostpartumMastitis

        _ ->
            Nothing


prenatalIndicatorFromMapping : String -> Maybe PrenatalIndicator
prenatalIndicatorFromMapping s =
    case s of
        "a" ->
            Just IndicatorAdequateGWG

        "b" ->
            Just IndicatorReceivedMMS

        "c" ->
            Just IndicatorReferredToUltrasound

        "d" ->
            Just IndicatorReceivedAspirin

        "e" ->
            Just IndicatorReceivedCalcium

        "f" ->
            Just IndicatorPretermBirth

        "g" ->
            Just IndicatorAbortion

        "h" ->
            Just IndicatorIntrauterineDeath

        "i" ->
            Just IndicatorStillbirth

        "j" ->
            Just IndicatorReceivedAzithromycin

        "k" ->
            Just IndicatorAnemiaTest

        "l" ->
            Just IndicatorBreastfedFirstHour

        "m" ->
            Just IndicatorPrematureOnsetContractions

        _ ->
            Nothing


{-| Decoder for child entries on `individual.family-nutrition-muac`.

Wire shapes emitted by hedley\_reports\_calculate\_aggregated\_data\_for\_person:

  - "YYYY-MM-DD" (legacy date-only, kept for backward compatibility)
  - "YYYY-MM-DD <muac>" (legacy MUAC-only, kept for backward compatibility)
  - "YYYY-MM-DD <muac>|<aheza\_amount>" (MUAC + AHEZA recorded together)
  - "YYYY-MM-DD |<aheza\_amount>" (AHEZA only -- empty MUAC chunk)

The optional "|<amount>" tail chunk inside the second space-segment is the
same convention used by decodeNutritionEncounterData below for FBF amounts.
Stays in sync with the family-nutrition-muac emission in
server/hedley/modules/custom/hedley\_reports/hedley\_reports.module; update
both together.

-}
decodeFamilyNutritionEncounterData : Decoder FamilyNutritionEncounterData
decodeFamilyNutritionEncounterData =
    string
        |> andThen
            (\s ->
                case String.split " " (String.trim s) of
                    [ first ] ->
                        Date.fromIsoString first
                            |> Result.toMaybe
                            |> Maybe.map
                                (\startDate ->
                                    succeed { startDate = startDate, muacCm = Nothing, ahezaAmount = Nothing }
                                )
                            |> Maybe.withDefault (fail "Failed to decode FamilyNutritionEncounterData")

                    [ first, second ] ->
                        Date.fromIsoString first
                            |> Result.toMaybe
                            |> Maybe.map
                                (\startDate ->
                                    let
                                        ( muacCm, ahezaAmount ) =
                                            parseFamilyNutritionPayload second
                                    in
                                    succeed { startDate = startDate, muacCm = muacCm, ahezaAmount = ahezaAmount }
                                )
                            |> Maybe.withDefault (fail "Failed to decode FamilyNutritionEncounterData")

                    _ ->
                        fail "Failed to decode FamilyNutritionEncounterData"
            )


parseFamilyNutritionPayload : String -> ( Maybe Float, Maybe Float )
parseFamilyNutritionPayload s =
    case String.split "|" s of
        [ muacPart ] ->
            ( String.toFloat muacPart, Nothing )

        [ muacPart, ahezaPart ] ->
            ( String.toFloat muacPart, String.toFloat ahezaPart )

        _ ->
            ( Nothing, Nothing )


{-| Decoder for mother entries on `individual.family-nutrition`.

Wire shapes:

  - "YYYY-MM-DD" (legacy date-only entry, kept for backward compatibility)
  - "YYYY-MM-DD <aheza\_amount>" (AHEZA mother distribution recorded)

Stays in sync with the family-nutrition mother emission in
server/hedley/modules/custom/hedley\_reports/hedley\_reports.module; update
both together.

-}
decodeFamilyNutritionMotherEncounterData : Decoder FamilyNutritionMotherEncounterData
decodeFamilyNutritionMotherEncounterData =
    string
        |> andThen
            (\s ->
                case String.split " " (String.trim s) of
                    [ first ] ->
                        Date.fromIsoString first
                            |> Result.toMaybe
                            |> Maybe.map
                                (\startDate ->
                                    succeed { startDate = startDate, ahezaAmount = Nothing }
                                )
                            |> Maybe.withDefault (fail "Failed to decode FamilyNutritionMotherEncounterData")

                    [ first, second ] ->
                        Date.fromIsoString first
                            |> Result.toMaybe
                            |> Maybe.map
                                (\startDate ->
                                    succeed { startDate = startDate, ahezaAmount = String.toFloat second }
                                )
                            |> Maybe.withDefault (fail "Failed to decode FamilyNutritionMotherEncounterData")

                    _ ->
                        fail "Failed to decode FamilyNutritionMotherEncounterData"
            )


{-| Decoder for `group_nutrition_mother.fbf`.

Wire shape: "YYYY-MM-DD <amount>". Mother FBF distribution records have no
anthropometry to embed alongside, so the wire is always exactly two
space-separated segments. Stays in sync with the mother FBF emission in
server/hedley/modules/custom/hedley\_reports/hedley\_reports.module.

-}
decodeMotherFbfEncounterData : Decoder MotherFbfEncounterData
decodeMotherFbfEncounterData =
    string
        |> andThen
            (\s ->
                case String.split " " (String.trim s) of
                    [ first, second ] ->
                        case ( Date.fromIsoString first |> Result.toMaybe, String.toFloat second ) of
                            ( Just startDate, Just fbfAmount ) ->
                                succeed { startDate = startDate, fbfAmount = fbfAmount }

                            _ ->
                                fail "Failed to decode MotherFbfEncounterData"

                    _ ->
                        fail "Failed to decode MotherFbfEncounterData"
            )


{-| Decoder for child entries on `individual.nutrition` and on
`group_nutrition.{pmtct,fbf,sorwathe,chw,achi}`.

Wire shapes emitted by hedley\_reports\_calculate\_aggregated\_data\_for\_person:

  - "YYYY-MM-DD" (legacy date-only)
  - "YYYY-MM-DD <anth>" (anthropometry, where <anth> is the comma-separated
    payload from hedley\_reports\_nutrition\_metrics\_to\_string)
  - "YYYY-MM-DD <anth>|<fbf\_amount>" (anthropometry + FBF distribution)
  - "YYYY-MM-DD ,,,,|<fbf\_amount>" (FBF only -- empty anthropometry chunk;
    the PHP emitter always produces ",,,," for empty anthropometry, never
    an empty string)

The optional "|<amount>" tail chunk is appended to the existing
space-delimited second segment. Stays in sync with the group\_nutrition
emission in
server/hedley/modules/custom/hedley\_reports/hedley\_reports.module; update
both together.

-}
decodeNutritionEncounterData : Decoder NutritionEncounterData
decodeNutritionEncounterData =
    string
        |> andThen
            (\s ->
                case String.split " " (String.trim s) of
                    [ first ] ->
                        Date.fromIsoString first
                            |> Result.toMaybe
                            |> Maybe.map
                                (\startDate ->
                                    succeed
                                        { startDate = startDate
                                        , nutritionData = Nothing
                                        , muacCm = Nothing
                                        , hasEdema = False
                                        , fbfAmount = Nothing
                                        }
                                )
                            |> Maybe.withDefault (fail "Failed to decode NutritionEncounterData")

                    [ first, second ] ->
                        Date.fromIsoString first
                            |> Result.toMaybe
                            |> Maybe.map
                                (\startDate ->
                                    let
                                        ( anthropometryPart, fbfAmount ) =
                                            case String.split "|" second of
                                                [ anth, fbf ] ->
                                                    ( anth, String.toFloat fbf )

                                                _ ->
                                                    ( second, Nothing )

                                        ( nutritionData, muacCm, hasEdema ) =
                                            parseAnthropometryPayload anthropometryPart
                                    in
                                    succeed
                                        { startDate = startDate
                                        , nutritionData = nutritionData
                                        , muacCm = muacCm
                                        , hasEdema = hasEdema
                                        , fbfAmount = fbfAmount
                                        }
                                )
                            |> Maybe.withDefault (fail "Failed to decode NutritionEncounterData")

                    _ ->
                        fail "Failed to decode NutritionEncounterData"
            )


decodeWellChildEncounterData : Decoder WellChildEncounterData
decodeWellChildEncounterData =
    string
        |> andThen
            (\s ->
                case String.split "|" (String.trim s) of
                    [ first, second, third ] ->
                        (Date.fromIsoString first |> Result.toMaybe)
                            |> Maybe.map
                                (\startDate ->
                                    let
                                        ( nutritionData, muacCm ) =
                                            nutritionDataFromString second
                                    in
                                    succeed
                                        (WellChildEncounterData startDate
                                            nutritionData
                                            muacCm
                                            (immunisationDataFromString third)
                                        )
                                )
                            |> Maybe.withDefault (fail "Failed to decode WellChildEncounterData")

                    _ ->
                        fail "Failed to decode WellChildEncounterData"
            )



-- Wire format from hedley_reports_nutrition_metrics_to_string is
-- "<stunting>,<underweight>,<wasting>,<muac>,<edema>" (PRs #1479/#1481
-- established this order to fix issue 3199; do not reorder without
-- updating the PHP encoder/decoder in lockstep). NutritionData carries
-- the three z-scores; MUAC is returned alongside as the tuple's second
-- component so callers can store it on their encounter type (mirroring
-- parseNutritionEncounterPayload below). The edema token is discarded
-- because WellChildEncounterData doesn't carry edema today.


nutritionDataFromString : String -> ( Maybe NutritionData, Maybe Float )
nutritionDataFromString s =
    case String.split "," s of
        [ stunting, underweight, wasting, muac, _ ] ->
            ( Just (NutritionData (String.toFloat stunting) (String.toFloat underweight) (String.toFloat wasting))
            , String.toFloat muac
            )

        _ ->
            ( Nothing, Nothing )


immunisationDataFromString : String -> Maybe (Dict VaccineType (EverySet NominalDate))
immunisationDataFromString s =
    let
        tuples =
            String.split "," s
                |> List.filterMap
                    (\item ->
                        case String.split ":" item of
                            [ mappedVaccineType, administrationDates ] ->
                                vaccineTypeFromMapping mappedVaccineType
                                    |> Maybe.andThen
                                        (\vaccineType ->
                                            let
                                                dates =
                                                    String.split "+" administrationDates
                                                        |> List.map (Date.fromIsoString >> Result.toMaybe)
                                                        |> Maybe.Extra.values
                                                        |> EverySet.fromList
                                            in
                                            if EverySet.isEmpty dates then
                                                Nothing

                                            else
                                                Just ( vaccineType, dates )
                                        )

                            _ ->
                                Nothing
                    )
    in
    if List.isEmpty tuples then
        Nothing

    else
        Just <| Dict.fromList tuples


vaccineTypeFromMapping : String -> Maybe VaccineType
vaccineTypeFromMapping s =
    case s of
        "a" ->
            Just VaccineBCG

        "b" ->
            Just VaccineOPV

        "c" ->
            Just VaccineDTP

        "d" ->
            Just VaccineDTPStandalone

        "e" ->
            Just VaccinePCV13

        "f" ->
            Just VaccineRotarix

        "g" ->
            Just VaccineIPV

        "h" ->
            Just VaccineMR

        "i" ->
            Just VaccineHPV

        _ ->
            Nothing



-- Wire format from hedley_reports_nutrition_metrics_to_string is
-- "<stunting>,<underweight>,<wasting>,<muac>,<edema>" (PRs #1479/#1481
-- established this order to fix issue 3199). NutritionData carries the
-- three z-scores; MUAC and edema flow alongside in the tuple so they
-- can be stored on NutritionEncounterData's top-level muacCm/hasEdema
-- fields without duplicating data inside the nested NutritionData.


parseAnthropometryPayload : String -> ( Maybe NutritionData, Maybe Float, Bool )
parseAnthropometryPayload payload =
    case String.split "," payload of
        [ stunting, underweight, wasting ] ->
            ( Just (NutritionData (String.toFloat stunting) (String.toFloat underweight) (String.toFloat wasting))
            , Nothing
            , False
            )

        [ stunting, underweight, wasting, muac, edema ] ->
            ( Just (NutritionData (String.toFloat stunting) (String.toFloat underweight) (String.toFloat wasting))
            , String.toFloat muac
            , edema == "1"
            )

        _ ->
            ( Nothing, Nothing, False )


decodeBackendGeneratedNutritionReportTableDate : Decoder BackendGeneratedNutritionReportTableDate
decodeBackendGeneratedNutritionReportTableDate =
    succeed BackendGeneratedNutritionReportTableDate
        |> required "type" decodeNutritionReportTableType
        |> required "period" (list string)
        |> required "stunting_moderate" (list string)
        |> required "stunting_severe" (list string)
        |> required "wasting_moderate" (list string)
        |> required "wasting_severe" (list string)
        |> required "underweight_moderate" (list string)
        |> required "underweight_severe" (list string)
        |> required "acute_malnutrition_mam" (list string)
        |> required "acute_malnutrition_sam" (list string)


decodeNutritionReportTableType : Decoder NutritionReportTableType
decodeNutritionReportTableType =
    string
        |> andThen
            (\tableType ->
                case tableType of
                    "prevalence-1" ->
                        succeed NutritionTablePrevalanceOneOrMore

                    "prevalence-2" ->
                        succeed NutritionTablePrevalanceTwoOrMore

                    "incidence-month-1" ->
                        succeed NutritionTableIncidenceMonthOneOrMore

                    "incidence-month-2" ->
                        succeed NutritionTableIncidenceMonthTwoOrMore

                    "incidence-quarter-1" ->
                        succeed NutritionTableIncidenceQuarterOneOrMore

                    "incidence-quarter-2" ->
                        succeed NutritionTableIncidenceQuarterTwoOrMore

                    "incidence-year-1" ->
                        succeed NutritionTableIncidenceYearOneOrMore

                    "incidence-year-2" ->
                        succeed NutritionTableIncidenceYearTwoOrMore

                    _ ->
                        fail <| tableType ++ " is unknown NutritionReportTableType type"
            )
