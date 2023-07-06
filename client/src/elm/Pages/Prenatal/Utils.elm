module Pages.Prenatal.Utils exposing (..)

import AssocList as Dict exposing (Dict)
import Backend.Measurement.Model exposing (..)
import Backend.Measurement.Utils exposing (getCurrentReasonForNonReferral, getMeasurementValueFunc)
import Backend.Model exposing (ModelIndexedDb)
import Backend.Person.Model exposing (Person)
import Backend.PrenatalEncounter.Model exposing (PrenatalEncounterType(..))
import Backend.PrenatalEncounter.Types exposing (PrenatalDiagnosis(..))
import Date exposing (Unit(..))
import EverySet exposing (EverySet)
import Gizra.Html exposing (emptyNode)
import Gizra.NominalDate exposing (NominalDate, diffDays, diffWeeks)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Maybe.Extra exposing (andMap, isJust, isNothing, or, unwrap)
import Measurement.Model exposing (SendToHCForm)
import Measurement.Utils exposing (generateVaccinationProgressForVaccine, sendToHCFormWithDefault, toEverySet, vitalsFormWithDefault)
import Measurement.View exposing (viewActionTakenLabel, viewMultipleTreatmentWithDosage, viewTreatmentOptionWithDosage, viewTreatmentWithDosage)
import Pages.AcuteIllness.Activity.View exposing (viewAdministeredMedicationCustomLabel, viewAdministeredMedicationLabel, viewAdministeredMedicationQuestion)
import Pages.Prenatal.Model exposing (..)
import Pages.Utils
    exposing
        ( getCurrentReasonForMedicationNonAdministration
        , ifEverySetEmpty
        , ifNullableTrue
        , ifTrue
        , maybeToBoolTask
        , maybeValueConsideringIsDirtyField
        , nonAdministrationReasonToSign
        , taskAllCompleted
        , taskCompleted
        , valueConsideringIsDirtyField
        , viewBoolInput
        , viewCheckBoxSelectCustomInput
        , viewCheckBoxSelectInput
        , viewCheckBoxSelectInputWithRecommendation
        , viewCustomLabel
        , viewInstructionsLabel
        , viewQuestionLabel
        )
import Translate exposing (Language, TranslationId, translate)


nurseEncounterNotPerformed : AssembledData -> Bool
nurseEncounterNotPerformed assembled =
    List.isEmpty assembled.nursePreviousEncountersData


nurseEncounterPerformed : AssembledData -> Bool
nurseEncounterPerformed =
    nurseEncounterNotPerformed >> not


calculateEGAWeeks : NominalDate -> NominalDate -> Int
calculateEGAWeeks currentDate lmpDate =
    calculateEGADays currentDate lmpDate // 7


calculateEGADays : NominalDate -> NominalDate -> Int
calculateEGADays currentDate lmpDate =
    diffDays lmpDate currentDate


diagnosed : PrenatalDiagnosis -> AssembledData -> Bool
diagnosed diagnosis assembled =
    EverySet.member diagnosis assembled.encounter.diagnoses


diagnosedAnyOf : List PrenatalDiagnosis -> AssembledData -> Bool
diagnosedAnyOf diagnoses assembled =
    List.any
        (\diagnosis -> diagnosed diagnosis assembled)
        diagnoses


diagnosedNoneOf : List PrenatalDiagnosis -> AssembledData -> Bool
diagnosedNoneOf diagnoses assembled =
    List.all
        (\diagnosis -> not <| diagnosed diagnosis assembled)
        diagnoses


diagnosedPreviously : PrenatalDiagnosis -> AssembledData -> Bool
diagnosedPreviously diagnosis assembled =
    diagnosedPreviouslyAnyOf [ diagnosis ] assembled


diagnosedPreviouslyAnyOf : List PrenatalDiagnosis -> AssembledData -> Bool
diagnosedPreviouslyAnyOf diagnoses assembled =
    assembled.nursePreviousEncountersData
        |> List.filter
            (\data ->
                List.any (\diagnosis -> EverySet.member diagnosis data.diagnoses) diagnoses
            )
        |> List.isEmpty
        |> not


filterNonUrgentDiagnoses : List PrenatalDiagnosis -> List PrenatalDiagnosis
filterNonUrgentDiagnoses diagnoses =
    let
        exclusions =
            NoPrenatalDiagnosis :: emergencyReferralDiagnoses
    in
    List.filter (\diagnosis -> not <| List.member diagnosis exclusions) diagnoses


diagnosesCausingHospitalReferralByPhase : PrenatalEncounterPhase -> AssembledData -> EverySet PrenatalDiagnosis
diagnosesCausingHospitalReferralByPhase phase assembled =
    applyDiagnosesHierarchy <|
        case phase of
            PrenatalEncounterPhaseInitial ->
                let
                    general =
                        diagnosesCausingHospitalReferralByImmediateDiagnoses PrenatalEncounterPhaseInitial assembled
                            ++ diagnosesCausingHospitalReferralByMentalHealth assembled
                            ++ diagnosesCausingHospitalReferralByOtherReasons assembled

                    byAdverseEvent =
                        diagnosesCausingHospitalReferralByAdverseEventForTreatment assembled

                    byPastDiagnoses =
                        diagnosesCausingHospitalReferralByPastDiagnoses assembled
                in
                general
                    ++ byAdverseEvent
                    ++ byPastDiagnoses
                    |> EverySet.fromList

            PrenatalEncounterPhaseRecurrent ->
                diagnosesCausingHospitalReferralByImmediateDiagnoses PrenatalEncounterPhaseRecurrent assembled
                    |> EverySet.fromList


diagnosesCausingHospitalReferralByMentalHealth : AssembledData -> List PrenatalDiagnosis
diagnosesCausingHospitalReferralByMentalHealth assembled =
    if mentalHealthSpecialistAtHC assembled then
        []

    else
        List.filter (\diagnosis -> diagnosed diagnosis assembled) mentalHealthDiagnosesRequiringTreatment


mentalHealthSpecialistAtHC : AssembledData -> Bool
mentalHealthSpecialistAtHC assembled =
    getMeasurementValueFunc assembled.measurements.mentalHealth
        |> Maybe.map .specialistAtHC
        |> Maybe.withDefault False


mentalHealthDiagnosesRequiringTreatment : List PrenatalDiagnosis
mentalHealthDiagnosesRequiringTreatment =
    [ DiagnosisDepressionPossible
    , DiagnosisDepressionHighlyPossible
    , DiagnosisDepressionProbable
    , DiagnosisSuicideRisk
    ]


diagnosesCausingHospitalReferralByOtherReasons : AssembledData -> List PrenatalDiagnosis
diagnosesCausingHospitalReferralByOtherReasons assembled =
    let
        malaria =
            if diagnosedMalaria assembled && severeMalariaTreatment then
                [ DiagnosisMalaria ]

            else
                []

        severeMalariaTreatment =
            getMeasurementValueFunc assembled.measurements.medicationDistribution
                |> Maybe.andThen (.recommendedTreatmentSigns >> Maybe.map (EverySet.member TreatmentReferToHospital))
                |> Maybe.withDefault False

        hypertension =
            let
                moderatePreeclampsiaAsCurrent =
                    moderatePreeclampsiaAsPreviousHypertensionlikeDiagnosis assembled
            in
            if updateHypertensionTreatmentWithHospitalization assembled then
                List.singleton <|
                    if moderatePreeclampsiaAsCurrent then
                        DiagnosisModeratePreeclampsiaInitialPhase

                    else
                        DiagnosisChronicHypertensionImmediate

            else
                let
                    bloodPressureRequiresHospitalization =
                        bloodPressureAtHypertensionTreatmentRequiresHospitalization assembled
                in
                if moderatePreeclampsiaAsCurrent && bloodPressureRequiresHospitalization then
                    [ DiagnosisModeratePreeclampsiaInitialPhase ]

                else
                    []
    in
    malaria ++ hypertension


moderatePreeclampsiaAsPreviousHypertensionlikeDiagnosis : AssembledData -> Bool
moderatePreeclampsiaAsPreviousHypertensionlikeDiagnosis assembled =
    resolvePreviousHypertensionlikeDiagnosis assembled.nursePreviousEncountersData
        |> Maybe.map
            (\diagnosis ->
                List.member diagnosis
                    [ DiagnosisModeratePreeclampsiaInitialPhase
                    , DiagnosisModeratePreeclampsiaRecurrentPhase
                    ]
            )
        |> Maybe.withDefault False


bloodPressureAtHypertensionTreatmentRequiresHospitalization : AssembledData -> Bool
bloodPressureAtHypertensionTreatmentRequiresHospitalization assembled =
    getMeasurementValueFunc assembled.measurements.vitals
        |> Maybe.andThen
            (\value ->
                Maybe.map2
                    (\sys dia ->
                        sys >= 180 || dia >= 110
                    )
                    value.sys
                    value.dia
            )
        |> Maybe.withDefault False


diagnosesCausingHospitalReferralByImmediateDiagnoses : PrenatalEncounterPhase -> AssembledData -> List PrenatalDiagnosis
diagnosesCausingHospitalReferralByImmediateDiagnoses phase assembled =
    let
        immediateReferralDiagnoses =
            case phase of
                PrenatalEncounterPhaseInitial ->
                    emergencyReferralDiagnosesInitial
                        ++ [ DiagnosisModeratePreeclampsiaInitialPhase
                           , DiagnosisSeverePreeclampsiaInitialPhase
                           , DiagnosisHeartburnPersistent
                           , DiagnosisDeepVeinThrombosis
                           , DiagnosisPelvicPainIntense
                           , DiagnosisPelvicPainContinued
                           , DiagnosisPyelonephritis
                           , DiagnosisMalariaMedicatedContinued
                           , DiagnosisMalariaWithAnemiaMedicatedContinued
                           , DiagnosisUrinaryTractInfectionContinued
                           , DiagnosisCandidiasisContinued
                           , DiagnosisGonorrheaContinued
                           , DiagnosisTrichomonasOrBacterialVaginosisContinued
                           , DiagnosisPostpartumUrinaryIncontinence
                           , DiagnosisPostpartumInfection
                           , DiagnosisPostpartumExcessiveBleeding
                           ]

                PrenatalEncounterPhaseRecurrent ->
                    emergencyReferralDiagnosesRecurrent
                        ++ [ DiagnosisHepatitisB
                           , DiagnosisNeurosyphilis
                           , DiagnosisMalariaWithSevereAnemia
                           , DiagnosisSevereAnemia
                           , DiagnosisModeratePreeclampsiaRecurrentPhase
                           , DiagnosisSeverePreeclampsiaRecurrentPhase
                           , Backend.PrenatalEncounter.Types.DiagnosisDiabetes
                           , Backend.PrenatalEncounter.Types.DiagnosisGestationalDiabetes
                           , DiagnosisRhesusNegative
                           ]
    in
    List.filter (\diagnosis -> diagnosed diagnosis assembled)
        immediateReferralDiagnoses


emergencyReferralDiagnoses : List PrenatalDiagnosis
emergencyReferralDiagnoses =
    emergencyReferralDiagnosesInitial ++ emergencyReferralDiagnosesRecurrent


emergencyReferralDiagnosesInitial : List PrenatalDiagnosis
emergencyReferralDiagnosesInitial =
    [ DiagnosisModeratePreeclampsiaInitialPhaseEGA37Plus
    , DiagnosisSeverePreeclampsiaInitialPhaseEGA37Plus
    , DiagnosisEclampsia
    , DiagnosisMiscarriage
    , DiagnosisMolarPregnancy
    , DiagnosisPlacentaPrevia
    , DiagnosisPlacentalAbruption
    , DiagnosisUterineRupture
    , DiagnosisObstructedLabor
    , DiagnosisPostAbortionSepsis
    , DiagnosisEctopicPregnancy
    , DiagnosisPROM
    , DiagnosisPPROM
    , DiagnosisMaternalComplications
    , DiagnosisImminentDelivery
    , DiagnosisLaborAndDelivery
    , DiagnosisHyperemesisGravidum
    , DiagnosisSevereVomiting

    -- Infection diagnosis will be available at latter phase.
    -- , DiagnosisInfection
    ]


emergencyReferralDiagnosesRecurrent : List PrenatalDiagnosis
emergencyReferralDiagnosesRecurrent =
    [ DiagnosisModeratePreeclampsiaRecurrentPhaseEGA37Plus
    , DiagnosisSeverePreeclampsiaRecurrentPhaseEGA37Plus
    , DiagnosisSevereAnemiaWithComplications
    ]


hierarchalBloodPressureDiagnoses : List PrenatalDiagnosis
hierarchalBloodPressureDiagnoses =
    [ -- Emergency diagnoses.
      DiagnosisEclampsia
    , DiagnosisSeverePreeclampsiaInitialPhaseEGA37Plus
    , DiagnosisSeverePreeclampsiaRecurrentPhaseEGA37Plus
    , DiagnosisModeratePreeclampsiaInitialPhaseEGA37Plus
    , DiagnosisModeratePreeclampsiaRecurrentPhaseEGA37Plus

    -- Non emergency diagnoses.
    , DiagnosisSeverePreeclampsiaInitialPhase
    , DiagnosisSeverePreeclampsiaRecurrentPhase
    , DiagnosisModeratePreeclampsiaInitialPhase
    , DiagnosisModeratePreeclampsiaRecurrentPhase
    , DiagnosisChronicHypertensionImmediate
    , DiagnosisChronicHypertensionAfterRecheck
    , DiagnosisGestationalHypertensionImmediate
    , DiagnosisGestationalHypertensionAfterRecheck
    ]


hierarchalHypertensionlikeDiagnosisToNumber : PrenatalDiagnosis -> Maybe Int
hierarchalHypertensionlikeDiagnosisToNumber diagnosis =
    case diagnosis of
        DiagnosisEclampsia ->
            Just 50

        DiagnosisSeverePreeclampsiaInitialPhaseEGA37Plus ->
            Just 42

        DiagnosisSeverePreeclampsiaRecurrentPhaseEGA37Plus ->
            Just 41

        DiagnosisModeratePreeclampsiaInitialPhaseEGA37Plus ->
            Just 32

        DiagnosisModeratePreeclampsiaRecurrentPhaseEGA37Plus ->
            Just 31

        DiagnosisSeverePreeclampsiaInitialPhase ->
            Just 22

        DiagnosisSeverePreeclampsiaRecurrentPhase ->
            Just 21

        DiagnosisModeratePreeclampsiaInitialPhase ->
            Just 12

        DiagnosisModeratePreeclampsiaRecurrentPhase ->
            Just 11

        DiagnosisChronicHypertensionImmediate ->
            Just 4

        DiagnosisChronicHypertensionAfterRecheck ->
            Just 3

        DiagnosisGestationalHypertensionImmediate ->
            Just 2

        DiagnosisGestationalHypertensionAfterRecheck ->
            Just 1

        _ ->
            Nothing


hierarchalHypertensionlikeDiagnosisFromNumber : Int -> Maybe PrenatalDiagnosis
hierarchalHypertensionlikeDiagnosisFromNumber number =
    case number of
        50 ->
            Just DiagnosisEclampsia

        42 ->
            Just DiagnosisSeverePreeclampsiaInitialPhaseEGA37Plus

        41 ->
            Just DiagnosisSeverePreeclampsiaRecurrentPhaseEGA37Plus

        32 ->
            Just DiagnosisModeratePreeclampsiaInitialPhaseEGA37Plus

        31 ->
            Just DiagnosisModeratePreeclampsiaRecurrentPhaseEGA37Plus

        22 ->
            Just DiagnosisSeverePreeclampsiaInitialPhase

        21 ->
            Just DiagnosisSeverePreeclampsiaRecurrentPhase

        12 ->
            Just DiagnosisModeratePreeclampsiaInitialPhase

        11 ->
            Just DiagnosisModeratePreeclampsiaRecurrentPhase

        4 ->
            Just DiagnosisChronicHypertensionImmediate

        3 ->
            Just DiagnosisChronicHypertensionAfterRecheck

        2 ->
            Just DiagnosisGestationalHypertensionImmediate

        1 ->
            Just DiagnosisGestationalHypertensionAfterRecheck

        _ ->
            Nothing


hierarchalMastitisDiagnoses : List PrenatalDiagnosis
hierarchalMastitisDiagnoses =
    [ DiagnosisPostpartumMastitis, DiagnosisPostpartumEarlyMastitisOrEngorgment ]


hierarchalMastitisDiagnosisToNumber : PrenatalDiagnosis -> Maybe Int
hierarchalMastitisDiagnosisToNumber diagnosis =
    case diagnosis of
        DiagnosisPostpartumMastitis ->
            Just 2

        DiagnosisPostpartumEarlyMastitisOrEngorgment ->
            Just 1

        _ ->
            Nothing


hierarchalMastitisDiagnosisFromNumber : Int -> Maybe PrenatalDiagnosis
hierarchalMastitisDiagnosisFromNumber number =
    case number of
        2 ->
            Just DiagnosisPostpartumMastitis

        1 ->
            Just DiagnosisPostpartumEarlyMastitisOrEngorgment

        _ ->
            Nothing


medicationDistributionFormWithDefault : MedicationDistributionSign -> MedicationDistributionForm -> Maybe PrenatalMedicationDistributionValue -> MedicationDistributionForm
medicationDistributionFormWithDefault valueForNone form saved =
    case valueForNone of
        NoMedicationDistributionSignsInitialPhase ->
            medicationDistributionFormWithDefaultInitialPhase form saved

        NoMedicationDistributionSignsRecurrentPhase ->
            medicationDistributionFormWithDefaultRecurrentPhase form saved

        -- We should never get here.
        _ ->
            form


medicationDistributionFormWithDefaultInitialPhase : MedicationDistributionForm -> Maybe PrenatalMedicationDistributionValue -> MedicationDistributionForm
medicationDistributionFormWithDefaultInitialPhase form saved =
    saved
        |> unwrap
            form
            (\value ->
                let
                    allowedSigns =
                        NoMedicationDistributionSignsInitialPhase :: medicationsInitialPhase

                    hypertensionAvoidingGuidanceReason =
                        Maybe.andThen (EverySet.toList >> List.head) value.avoidingGuidanceReason
                in
                { mebendezole = or form.mebendezole (medicationDistributionResolveFromValue allowedSigns value Mebendezole)
                , tenofovir = or form.tenofovir (medicationDistributionResolveFromValue allowedSigns value Tenofovir)
                , lamivudine = or form.lamivudine (medicationDistributionResolveFromValue allowedSigns value Lamivudine)
                , dolutegravir = or form.dolutegravir (medicationDistributionResolveFromValue allowedSigns value Dolutegravir)
                , tdf3tc = or form.tdf3tc (medicationDistributionResolveFromValue allowedSigns value TDF3TC)
                , ceftriaxone = or form.ceftriaxone (medicationDistributionResolveFromValue allowedSigns value Ceftriaxone)
                , azithromycin = or form.azithromycin (medicationDistributionResolveFromValue allowedSigns value Azithromycin)
                , metronidazole = or form.metronidazole (medicationDistributionResolveFromValue allowedSigns value Metronidazole)
                , vitaminA = or form.vitaminA (medicationDistributionResolveFromValue allowedSigns value VitaminA)
                , paracetamol = or form.paracetamol (medicationDistributionResolveFromValue allowedSigns value Paracetamol)

                -- Following 2 do not participate at initial phase, therefore,
                -- resolved directly from value.
                , iron = EverySet.member Iron value.distributionSigns |> Just
                , folicAcid = EverySet.member FolicAcid value.distributionSigns |> Just
                , nonAdministrationSigns = or form.nonAdministrationSigns (Just value.nonAdministrationSigns)
                , recommendedTreatmentSigns = or form.recommendedTreatmentSigns (Maybe.map EverySet.toList value.recommendedTreatmentSigns)
                , hypertensionAvoidingGuidanceReason = maybeValueConsideringIsDirtyField form.hypertensionAvoidingGuidanceReasonDirty form.hypertensionAvoidingGuidanceReason hypertensionAvoidingGuidanceReason
                , hypertensionAvoidingGuidanceReasonDirty = form.hypertensionAvoidingGuidanceReasonDirty
                }
            )


medicationDistributionFormWithDefaultRecurrentPhase : MedicationDistributionForm -> Maybe PrenatalMedicationDistributionValue -> MedicationDistributionForm
medicationDistributionFormWithDefaultRecurrentPhase form saved =
    saved
        |> unwrap
            form
            (\value ->
                let
                    allowedSigns =
                        NoMedicationDistributionSignsRecurrentPhase :: medicationsRecurrentPhase

                    hypertensionAvoidingGuidanceReason =
                        Maybe.andThen (EverySet.toList >> List.head) value.avoidingGuidanceReason
                in
                { iron = or form.iron (medicationDistributionResolveFromValue allowedSigns value Iron)
                , folicAcid = or form.folicAcid (medicationDistributionResolveFromValue allowedSigns value FolicAcid)

                -- Following 9 do not participate at recurrent phase, therefore,
                -- resolved directly from value.
                , mebendezole = EverySet.member Mebendezole value.distributionSigns |> Just
                , tenofovir = EverySet.member Tenofovir value.distributionSigns |> Just
                , lamivudine = EverySet.member Lamivudine value.distributionSigns |> Just
                , dolutegravir = EverySet.member Dolutegravir value.distributionSigns |> Just
                , tdf3tc = EverySet.member TDF3TC value.distributionSigns |> Just
                , ceftriaxone = EverySet.member Ceftriaxone value.distributionSigns |> Just
                , azithromycin = EverySet.member Azithromycin value.distributionSigns |> Just
                , metronidazole = EverySet.member Metronidazole value.distributionSigns |> Just
                , vitaminA = EverySet.member VitaminA value.distributionSigns |> Just
                , paracetamol = EverySet.member Paracetamol value.distributionSigns |> Just
                , nonAdministrationSigns = or form.nonAdministrationSigns (Just value.nonAdministrationSigns)
                , recommendedTreatmentSigns = or form.recommendedTreatmentSigns (Maybe.map EverySet.toList value.recommendedTreatmentSigns)
                , hypertensionAvoidingGuidanceReason = maybeValueConsideringIsDirtyField form.hypertensionAvoidingGuidanceReasonDirty form.hypertensionAvoidingGuidanceReason hypertensionAvoidingGuidanceReason
                , hypertensionAvoidingGuidanceReasonDirty = form.hypertensionAvoidingGuidanceReasonDirty
                }
            )


medicationDistributionResolveFromValue :
    List MedicationDistributionSign
    -> PrenatalMedicationDistributionValue
    -> MedicationDistributionSign
    -> Maybe Bool
medicationDistributionResolveFromValue allowedSigns value sign =
    let
        valueSetForSign =
            EverySet.member sign value.distributionSigns

        nonAdministrationNoteSetForSign =
            Measurement.Utils.resolveMedicationsNonAdministrationReasons value
                |> Dict.filter (\medicationDistributionSign _ -> medicationDistributionSign == sign)
                |> Dict.isEmpty
                |> not
    in
    if valueSetForSign then
        Just True

    else if nonAdministrationNoteSetForSign then
        Just False

    else
        Nothing


toMedicationDistributionValueWithDefaultInitialPhase :
    Maybe PrenatalMedicationDistributionValue
    -> MedicationDistributionForm
    -> Maybe PrenatalMedicationDistributionValue
toMedicationDistributionValueWithDefaultInitialPhase =
    toMedicationDistributionValueWithDefault NoMedicationDistributionSignsInitialPhase


toMedicationDistributionValueWithDefaultRecurrentPhase :
    Maybe PrenatalMedicationDistributionValue
    -> MedicationDistributionForm
    -> Maybe PrenatalMedicationDistributionValue
toMedicationDistributionValueWithDefaultRecurrentPhase =
    toMedicationDistributionValueWithDefault NoMedicationDistributionSignsRecurrentPhase


toMedicationDistributionValueWithDefault :
    MedicationDistributionSign
    -> Maybe PrenatalMedicationDistributionValue
    -> MedicationDistributionForm
    -> Maybe PrenatalMedicationDistributionValue
toMedicationDistributionValueWithDefault valueForNone saved form =
    medicationDistributionFormWithDefault valueForNone form saved
        |> toMedicationDistributionValue valueForNone


toMedicationDistributionValue : MedicationDistributionSign -> MedicationDistributionForm -> Maybe PrenatalMedicationDistributionValue
toMedicationDistributionValue valueForNone form =
    let
        distributionSigns =
            [ ifNullableTrue Mebendezole form.mebendezole
            , ifNullableTrue Tenofovir form.tenofovir
            , ifNullableTrue Lamivudine form.lamivudine
            , ifNullableTrue Dolutegravir form.dolutegravir
            , ifNullableTrue TDF3TC form.tdf3tc
            , ifNullableTrue Iron form.iron
            , ifNullableTrue FolicAcid form.folicAcid
            , ifNullableTrue Ceftriaxone form.ceftriaxone
            , ifNullableTrue Azithromycin form.azithromycin
            , ifNullableTrue Metronidazole form.metronidazole
            , ifNullableTrue VitaminA form.vitaminA
            , ifNullableTrue Paracetamol form.paracetamol
            ]
                |> Maybe.Extra.combine
                |> Maybe.map (List.foldl EverySet.union EverySet.empty >> ifEverySetEmpty valueForNone)

        nonAdministrationSigns =
            form.nonAdministrationSigns
                |> Maybe.withDefault EverySet.empty
                |> ifEverySetEmpty NoMedicationNonAdministrationSigns
                |> Just

        recommendedTreatmentSigns =
            Maybe.map EverySet.fromList form.recommendedTreatmentSigns
                |> Just

        avoidingGuidanceReason =
            Maybe.map EverySet.singleton form.hypertensionAvoidingGuidanceReason
                |> Just
    in
    Maybe.map PrenatalMedicationDistributionValue distributionSigns
        |> andMap nonAdministrationSigns
        |> andMap recommendedTreatmentSigns
        |> andMap avoidingGuidanceReason


resolveMedicationDistributionInputsAndTasks :
    Language
    -> NominalDate
    -> PrenatalEncounterPhase
    -> AssembledData
    -> ((Bool -> MedicationDistributionForm -> MedicationDistributionForm) -> Bool -> msg)
    -> (Maybe AdministrationNote -> MedicationDistributionSign -> AdministrationNote -> msg)
    -> (List RecommendedTreatmentSign -> RecommendedTreatmentSign -> msg)
    -> (AvoidingGuidanceReason -> msg)
    -> MedicationDistributionForm
    -> ( List (Html msg), Int, Int )
resolveMedicationDistributionInputsAndTasks language currentDate phase assembled setMedicationDistributionBoolInputMsg setMedicationDistributionAdministrationNoteMsg setRecommendedTreatmentSignMsg avoidingGuidanceReasonMsg form =
    let
        foldResults =
            List.foldr
                (\( inputs, completed, active ) ( accumInputs, accumCompleted, accumActive ) ->
                    ( inputs ++ accumInputs, completed + accumCompleted, active + accumActive )
                )
                ( [], 0, 0 )

        ( inputsByMedications, completedByMedications, activeByMedications ) =
            resolveRequiredMedicationsSet language currentDate phase assembled
                |> List.map
                    (\( helper, medications, footer ) ->
                        let
                            ( inputs, completed, active ) =
                                List.map
                                    (resolveMedicationDistributionInputsAndTasksForMedication language
                                        currentDate
                                        assembled.person
                                        setMedicationDistributionBoolInputMsg
                                        setMedicationDistributionAdministrationNoteMsg
                                        form
                                    )
                                    medications
                                    |> foldResults
                        in
                        ( [ viewCustomLabel language helper "." "instructions"
                          , h2 [] [ text <| translate language Translate.ActionsToTake ++ ":" ]
                          ]
                            ++ inputs
                            ++ footer
                            ++ [ div [ class "separator" ] [] ]
                        , completed
                        , active
                        )
                    )
                |> foldResults

        ( inputsByDiagnoses, completedByDiagnoses, activeByDiagnoses ) =
            let
                ( hypertensionInputs, hypertensionCompleted, hypertensionActive ) =
                    if diagnosedHypertension phase assembled then
                        resolveRecommendedTreatmentForDiagnosedHypertensionInputsAndTasks language
                            currentDate
                            (setRecommendedTreatmentSignMsg recommendedTreatmentSignsForHypertension)
                            assembled
                            form

                    else if diagnosedHypertensionPrevoiusly assembled || diagnosedModeratePreeclampsiaPrevoiusly assembled then
                        resolveRecommendedTreatmentForPrevoiuslyDiagnosedHypertensionInputsAndTasks language
                            currentDate
                            (setRecommendedTreatmentSignMsg recommendedTreatmentSignsForHypertension)
                            avoidingGuidanceReasonMsg
                            assembled
                            form

                    else
                        ( [], 0, 0 )
            in
            case phase of
                PrenatalEncounterPhaseInitial ->
                    let
                        ( malariaInputs, malariaCompleted, malariaActive ) =
                            if diagnosedMalaria assembled then
                                resolveRecommendedTreatmentForMalariaInputsAndTasks language
                                    currentDate
                                    setRecommendedTreatmentSignMsg
                                    assembled
                                    form

                            else
                                ( [], 0, 0 )

                        ( heartburnInputs, heartburnCompleted, heartburnActive ) =
                            if diagnosed DiagnosisHeartburn assembled then
                                resolveRecommendedTreatmentForHeartburnInputsAndTasks language
                                    currentDate
                                    setRecommendedTreatmentSignMsg
                                    assembled
                                    form

                            else
                                ( [], 0, 0 )

                        ( urinaryTractInfectionInputs, urinaryTractInfectionCompleted, urinaryTractInfectionActive ) =
                            if diagnosed DiagnosisUrinaryTractInfection assembled then
                                resolveRecommendedTreatmentForUrinaryTractInfectionInputsAndTasks language
                                    currentDate
                                    setRecommendedTreatmentSignMsg
                                    assembled
                                    form

                            else
                                ( [], 0, 0 )

                        ( candidiasisInputs, candidiasisCompleted, candidiasisActive ) =
                            if diagnosed DiagnosisCandidiasis assembled then
                                resolveRecommendedTreatmentForCandidiasisInputsAndTasks language
                                    currentDate
                                    setRecommendedTreatmentSignMsg
                                    assembled
                                    form

                            else
                                ( [], 0, 0 )

                        ( mastitisInputs, mastitisCompleted, mastitisActive ) =
                            if
                                diagnosedAnyOf
                                    [ DiagnosisPostpartumEarlyMastitisOrEngorgment
                                    , DiagnosisPostpartumMastitis
                                    ]
                                    assembled
                            then
                                resolveRecommendedTreatmentForMastitisInputsAndTasks language
                                    currentDate
                                    setRecommendedTreatmentSignMsg
                                    assembled
                                    form

                            else
                                ( [], 0, 0 )
                    in
                    ( malariaInputs
                        ++ heartburnInputs
                        ++ hypertensionInputs
                        ++ urinaryTractInfectionInputs
                        ++ candidiasisInputs
                        ++ mastitisInputs
                    , malariaCompleted
                        + heartburnCompleted
                        + hypertensionCompleted
                        + urinaryTractInfectionCompleted
                        + candidiasisCompleted
                        + mastitisCompleted
                    , malariaActive
                        + heartburnActive
                        + hypertensionActive
                        + urinaryTractInfectionActive
                        + candidiasisActive
                        + mastitisActive
                    )

                PrenatalEncounterPhaseRecurrent ->
                    let
                        ( syphilisInputs, syphilisCompleted, syphilisActive ) =
                            if diagnosedSyphilis assembled then
                                resolveRecommendedTreatmentForSyphilisInputsAndTasks language
                                    currentDate
                                    setRecommendedTreatmentSignMsg
                                    recommendedTreatmentSignsForSyphilis
                                    assembled
                                    form

                            else
                                ( [], 0, 0 )
                    in
                    ( syphilisInputs ++ hypertensionInputs
                    , syphilisCompleted + hypertensionCompleted
                    , syphilisActive + hypertensionActive
                    )
    in
    ( inputsByMedications ++ inputsByDiagnoses
    , completedByMedications + completedByDiagnoses
    , activeByMedications + activeByDiagnoses
    )


resolveRecommendedTreatmentForDiagnosedHypertensionInputsAndTasks :
    Language
    -> NominalDate
    -> (RecommendedTreatmentSign -> msg)
    -> AssembledData
    -> MedicationDistributionForm
    -> ( List (Html msg), Int, Int )
resolveRecommendedTreatmentForDiagnosedHypertensionInputsAndTasks language currentDate setRecommendedTreatmentSignMsg assembled form =
    let
        ( input, completed, active ) =
            recommendedTreatmentForHypertensionInputAndTask language
                currentDate
                recommendedTreatmentSignsForHypertensionInitial
                setRecommendedTreatmentSignMsg
                assembled
                form

        isChronic =
            diagnosedAnyOf [ DiagnosisChronicHypertensionImmediate, DiagnosisChronicHypertensionAfterRecheck ] assembled
    in
    ( viewCustomLabel language (Translate.HypertensionRecommendedTreatmentHeader isChronic) "." "instructions"
        :: input
        ++ [ div [ class "separator" ] [] ]
    , completed
    , active
    )


resolveRecommendedTreatmentForPrevoiuslyDiagnosedHypertensionInputsAndTasks :
    Language
    -> NominalDate
    -> (RecommendedTreatmentSign -> msg)
    -> (AvoidingGuidanceReason -> msg)
    -> AssembledData
    -> MedicationDistributionForm
    -> ( List (Html msg), Int, Int )
resolveRecommendedTreatmentForPrevoiuslyDiagnosedHypertensionInputsAndTasks language currentDate setRecommendedTreatmentSignMsg setAvoidingGuidanceReasonMsg assembled form =
    let
        diagnosisHeader =
            viewCustomLabel language (Translate.HypertensionRecommendedTreatmentUpdateHeader forModeratePreeclamsia) "." "label"

        forModeratePreeclamsia =
            diagnosedModeratePreeclampsiaPrevoiusly assembled

        currentBPLabel =
            getMeasurementValueFunc assembled.measurements.vitals
                |> Maybe.andThen
                    (\value ->
                        Maybe.map2
                            (\sys dia ->
                                let
                                    floatToString =
                                        round >> String.fromInt
                                in
                                div [ class "label overview" ]
                                    [ text <| translate language Translate.HypertensionRecommendedTreatmentUpdateBPLabel
                                    , text " "
                                    , span [ class "highlight" ] [ text <| floatToString sys ++ "/" ++ floatToString dia ]
                                    , text "."
                                    ]
                            )
                            value.sys
                            value.dia
                    )
                |> Maybe.withDefault emptyNode

        ( input, completed, active ) =
            recommendedTreatmentForHypertensionInputAndTask language
                currentDate
                recommendedTreatmentSignsForHypertension
                setRecommendedTreatmentSignMsg
                assembled
                form
    in
    -- This usecase is when we already provide treatment and want
    -- update it.
    Maybe.map2
        (\recommendationDosageUpdate recommendedMedication ->
            let
                ( currentTreatmentLabel, newTreatmentLabel ) =
                    getLatestTreatmentByTreatmentOptions recommendedTreatmentSignsForHypertension assembled
                        |> Maybe.map
                            (\currentTreatment ->
                                let
                                    recommendationDosageUpdateLabel =
                                        if
                                            -- This is a special usecase, where previously no treatment is given, and
                                            -- currently BP is normal. Nevertheless, we still recommend starting tratement
                                            -- sinve Hypertension was diagnosed previously.
                                            (currentTreatment == NoTreatmentForHypertension)
                                                && (recommendationDosageUpdate == TreatementUpdateMaintainCurrentDoasage)
                                        then
                                            Translate.HypertensionRecommendedTreatmentUpdateStartTreatment

                                        else
                                            Translate.HypertensionRecommendedTreatmentUpdateNewTreatment recommendationDosageUpdate
                                in
                                ( div [ class "label overview" ] <|
                                    if currentTreatment == NoTreatmentForHypertension then
                                        [ text <| translate language Translate.HypertensionRecommendedTreatmentUpdateNoCurrentTreatment
                                        , text "."
                                        ]

                                    else
                                        [ text <| translate language Translate.HypertensionRecommendedTreatmentUpdateCurrentTreatment
                                        , text " "
                                        , text <| translate language <| Translate.RecommendedTreatmentSignLabel currentTreatment
                                        , text " "
                                        , text <| translate language <| Translate.RecommendedTreatmentSignDosage currentTreatment
                                        , text "."
                                        ]
                                , div [ class "label overview" ]
                                    [ text <| translate language recommendationDosageUpdateLabel
                                    , text " "
                                    , viewTreatmentOptionForHypertension language recommendedMedication
                                    , text "."
                                    ]
                                )
                            )
                        |> Maybe.withDefault ( emptyNode, emptyNode )

                ( derivedInput, derivedCompleted, derivedActive ) =
                    Maybe.map
                        (\recommendedTreatmentSigns ->
                            if not <| List.member recommendedMedication recommendedTreatmentSigns then
                                ( [ viewQuestionLabel language Translate.NotFollowingRecommendationQuestion
                                  , viewCheckBoxSelectInput language
                                        [ AvoidingGuidanceHypertensionLackOfStock
                                        , AvoidingGuidanceHypertensionKnownAllergy
                                        , AvoidingGuidanceHypertensionPatientDeclined
                                        ]
                                        [ AvoidingGuidanceHypertensionPatientUnableToAfford
                                        , AvoidingGuidanceHypertensionOther
                                        , AvoidingGuidanceHypertensionReinforceAdherence
                                        ]
                                        form.hypertensionAvoidingGuidanceReason
                                        setAvoidingGuidanceReasonMsg
                                        Translate.AvoidingGuidanceReason
                                  ]
                                , taskCompleted form.hypertensionAvoidingGuidanceReason
                                , 1
                                )

                            else
                                ( [], 0, 0 )
                        )
                        form.recommendedTreatmentSigns
                        |> Maybe.withDefault ( [], 0, 0 )
            in
            ( [ diagnosisHeader
              , currentBPLabel
              , currentTreatmentLabel
              , newTreatmentLabel
              ]
                ++ input
                ++ derivedInput
                ++ [ div [ class "separator" ] [] ]
            , completed + derivedCompleted
            , active + derivedActive
            )
        )
        (hypertensionTreatementUpdateRecommendationByBP assembled)
        (resolveHypertensionTreatementUpdateMedication assembled)
        |> -- There's no treatment, and only case when this is possible,
           -- is when Moderate Preeclamsia was diagnosed at previous encounter,
           -- and patient was referred to hospital.
           Maybe.withDefault
            ( [ diagnosisHeader
              , currentBPLabel
              ]
                ++ input
                ++ [ div [ class "separator" ] [] ]
            , completed
            , active
            )


recommendedTreatmentForHypertensionInputAndTask :
    Language
    -> NominalDate
    -> List RecommendedTreatmentSign
    -> (RecommendedTreatmentSign -> msg)
    -> AssembledData
    -> MedicationDistributionForm
    -> ( List (Html msg), Int, Int )
recommendedTreatmentForHypertensionInputAndTask language currentDate options setRecommendedTreatmentSignMsg assembled form =
    let
        -- Since we may have values set for another diagnosis, or from
        -- the other phase of encounter, we need to filter them out,
        -- to be able to determine the current value.
        currentValue =
            Maybe.andThen
                (List.filter (\sign -> List.member sign recommendedTreatmentSignsForHypertension)
                    >> List.head
                )
                form.recommendedTreatmentSigns
    in
    ( [ h2 [] [ text <| translate language Translate.ActionsToTake ++ ":" ]
      , div [ class "instructions" ]
            [ viewInstructionsLabel
                "icon-pills"
                (text <| translate language Translate.HypertensionRecommendedTreatmentHelper ++ ":")
            ]
      , viewCheckBoxSelectCustomInput language
            options
            []
            currentValue
            setRecommendedTreatmentSignMsg
            (viewTreatmentOptionForHypertension language)
      ]
    , taskCompleted currentValue
    , 1
    )


{-| Note: Even though name says Hypertension, it includes Moderate Preeclamsia as well.
-}
viewTreatmentOptionForHypertension : Language -> RecommendedTreatmentSign -> Html any
viewTreatmentOptionForHypertension language sign =
    case sign of
        TreatmentHypertensionAddCarvedilol ->
            viewMultipleTreatmentWithDosage language
                [ TreatmentMethyldopa4
                , TreatmentHypertensionAddCarvedilol
                ]

        TreatmentHypertensionAddAmlodipine ->
            viewMultipleTreatmentWithDosage language
                [ TreatmentMethyldopa4
                , TreatmentHypertensionAddCarvedilol
                , TreatmentHypertensionAddAmlodipine
                ]

        _ ->
            viewTreatmentOptionWithDosage language sign


{-| Note: Even though name says Hypertension, it includes Moderate Preeclamsia as well.
-}
updateHypertensionTreatmentWithMedication : AssembledData -> Bool
updateHypertensionTreatmentWithMedication assembled =
    resolveHypertensionTreatementUpdateMedication assembled
        |> isJust


{-| Note: Even though name says Hypertension, it includes Moderate Preeclamsia as well.
-}
resolveHypertensionTreatementUpdateMedication : AssembledData -> Maybe RecommendedTreatmentSign
resolveHypertensionTreatementUpdateMedication assembled =
    resolveHypertensionTreatementUpdateRecommendation assembled
        |> Maybe.andThen
            (\( medication, hospitalizationRequired ) ->
                if hospitalizationRequired || medication == NoTreatmentForHypertension then
                    Nothing

                else
                    Just medication
            )


{-| Note: Even though name says Hypertension, it includes Moderate Preeclamsia as well.
-}
updateHypertensionTreatmentWithHospitalization : AssembledData -> Bool
updateHypertensionTreatmentWithHospitalization assembled =
    resolveHypertensionTreatementUpdateRecommendation assembled
        |> Maybe.map ((==) hypertensionTreatementHospitalizationOption)
        |> Maybe.withDefault False


{-| Note: Even though name says Hypertension, it includes Moderate Preeclamsia as well.
-}
hypertensionTreatementHospitalizationOption : ( RecommendedTreatmentSign, Bool )
hypertensionTreatementHospitalizationOption =
    ( NoTreatmentForHypertension, True )


{-| Note: Even though name says Hypertension, it includes Moderate Preeclamsia as well.
-}
resolveHypertensionTreatementUpdateRecommendation : AssembledData -> Maybe ( RecommendedTreatmentSign, Bool )
resolveHypertensionTreatementUpdateRecommendation assembled =
    Maybe.map2
        (\currentTreatment treatementUpdateByBP ->
            let
                medicationOption medication =
                    ( medication, False )
            in
            case currentTreatment of
                NoTreatmentForHypertension ->
                    case treatementUpdateByBP of
                        TreatementUpdateMaintainCurrentDoasage ->
                            -- No treatment was given and BP seems normal, however,
                            -- we still recommend taking Methyldopa because Hypertension
                            -- was diagnosed previously.
                            medicationOption TreatmentMethyldopa2

                        TreatementUpdateIncreaseOneDose ->
                            medicationOption TreatmentMethyldopa2

                        TreatementUpdateIncreaseTwoDoses ->
                            medicationOption TreatmentMethyldopa3

                        TreatementUpdateHospitalize ->
                            hypertensionTreatementHospitalizationOption

                TreatmentMethyldopa2 ->
                    case treatementUpdateByBP of
                        TreatementUpdateMaintainCurrentDoasage ->
                            medicationOption TreatmentMethyldopa2

                        TreatementUpdateIncreaseOneDose ->
                            medicationOption TreatmentMethyldopa3

                        TreatementUpdateIncreaseTwoDoses ->
                            medicationOption TreatmentMethyldopa4

                        TreatementUpdateHospitalize ->
                            hypertensionTreatementHospitalizationOption

                TreatmentMethyldopa3 ->
                    case treatementUpdateByBP of
                        TreatementUpdateMaintainCurrentDoasage ->
                            medicationOption TreatmentMethyldopa3

                        TreatementUpdateIncreaseOneDose ->
                            medicationOption TreatmentMethyldopa4

                        TreatementUpdateIncreaseTwoDoses ->
                            medicationOption TreatmentHypertensionAddCarvedilol

                        TreatementUpdateHospitalize ->
                            hypertensionTreatementHospitalizationOption

                TreatmentMethyldopa4 ->
                    case treatementUpdateByBP of
                        TreatementUpdateMaintainCurrentDoasage ->
                            medicationOption TreatmentMethyldopa4

                        TreatementUpdateIncreaseOneDose ->
                            medicationOption TreatmentHypertensionAddCarvedilol

                        TreatementUpdateIncreaseTwoDoses ->
                            medicationOption TreatmentHypertensionAddAmlodipine

                        TreatementUpdateHospitalize ->
                            hypertensionTreatementHospitalizationOption

                TreatmentHypertensionAddCarvedilol ->
                    case treatementUpdateByBP of
                        TreatementUpdateMaintainCurrentDoasage ->
                            medicationOption TreatmentHypertensionAddCarvedilol

                        TreatementUpdateIncreaseOneDose ->
                            medicationOption TreatmentHypertensionAddAmlodipine

                        TreatementUpdateIncreaseTwoDoses ->
                            hypertensionTreatementHospitalizationOption

                        TreatementUpdateHospitalize ->
                            hypertensionTreatementHospitalizationOption

                TreatmentHypertensionAddAmlodipine ->
                    case treatementUpdateByBP of
                        TreatementUpdateMaintainCurrentDoasage ->
                            medicationOption TreatmentHypertensionAddAmlodipine

                        TreatementUpdateIncreaseOneDose ->
                            hypertensionTreatementHospitalizationOption

                        TreatementUpdateIncreaseTwoDoses ->
                            hypertensionTreatementHospitalizationOption

                        TreatementUpdateHospitalize ->
                            hypertensionTreatementHospitalizationOption

                -- We should never get here, since these are not
                -- Hypertension options.
                _ ->
                    ( NoTreatmentForHypertension, False )
        )
        (getLatestTreatmentByTreatmentOptions recommendedTreatmentSignsForHypertension assembled)
        (hypertensionTreatementUpdateRecommendationByBP assembled)


{-| Note: Even though name says Hypertension, it includes Moderate Preeclamsia as well.
-}
hypertensionTreatementUpdateRecommendationByBP : AssembledData -> Maybe HypertensionTreatementUpdateOption
hypertensionTreatementUpdateRecommendationByBP assembled =
    if diagnosedHypertensionPrevoiusly assembled || diagnosedModeratePreeclampsiaPrevoiusly assembled then
        getMeasurementValueFunc assembled.measurements.vitals
            |> Maybe.andThen
                (\value ->
                    Maybe.map2
                        (\sys dia ->
                            let
                                bySys =
                                    hypertensionTreatementUpdateRecommendationBySys sys

                                byDia =
                                    hypertensionTreatementUpdateRecommendationByDia dia
                            in
                            if hypertensionTreatementUpdateToNumber bySys < hypertensionTreatementUpdateToNumber byDia then
                                byDia

                            else
                                bySys
                        )
                        value.sys
                        value.dia
                )

    else
        Nothing


{-| Note: Even though name says Hypertension, it includes Moderate Preeclamsia as well.
-}
hypertensionTreatementUpdateRecommendationBySys : Float -> HypertensionTreatementUpdateOption
hypertensionTreatementUpdateRecommendationBySys value =
    if value < 140 then
        TreatementUpdateMaintainCurrentDoasage

    else if value < 160 then
        TreatementUpdateIncreaseOneDose

    else if value < 180 then
        TreatementUpdateIncreaseTwoDoses

    else
        TreatementUpdateHospitalize


{-| Note: Even though name says Hypertension, it includes Moderate Preeclamsia as well.
-}
hypertensionTreatementUpdateRecommendationByDia : Float -> HypertensionTreatementUpdateOption
hypertensionTreatementUpdateRecommendationByDia value =
    if value < 90 then
        TreatementUpdateMaintainCurrentDoasage

    else if value < 100 then
        TreatementUpdateIncreaseOneDose

    else if value < 110 then
        TreatementUpdateIncreaseTwoDoses

    else
        TreatementUpdateHospitalize


hypertensionTreatementUpdateToNumber : HypertensionTreatementUpdateOption -> Int
hypertensionTreatementUpdateToNumber value =
    case value of
        TreatementUpdateMaintainCurrentDoasage ->
            0

        TreatementUpdateIncreaseOneDose ->
            1

        TreatementUpdateIncreaseTwoDoses ->
            2

        TreatementUpdateHospitalize ->
            3


getLatestTreatmentByTreatmentOptions : List RecommendedTreatmentSign -> AssembledData -> Maybe RecommendedTreatmentSign
getLatestTreatmentByTreatmentOptions treatmentOptions assembled =
    List.reverse assembled.nursePreviousEncountersData
        |> List.filterMap
            (.measurements
                >> .medicationDistribution
                >> getMeasurementValueFunc
                >> Maybe.andThen
                    (\value ->
                        Maybe.map
                            (EverySet.toList
                                >> List.filter (\sign -> List.member sign treatmentOptions)
                                >> List.head
                            )
                            value.recommendedTreatmentSigns
                    )
            )
        |> Maybe.Extra.values
        |> List.head


resolveRecommendedTreatmentForMalariaInputsAndTasks :
    Language
    -> NominalDate
    -> (List RecommendedTreatmentSign -> RecommendedTreatmentSign -> msg)
    -> AssembledData
    -> MedicationDistributionForm
    -> ( List (Html msg), Int, Int )
resolveRecommendedTreatmentForMalariaInputsAndTasks language currentDate setRecommendedTreatmentSignMsg assembled form =
    let
        egaInWeeks =
            Maybe.map
                (calculateEGAWeeks currentDate)
                assembled.globalLmpDate

        allowedSigns =
            recommendedTreatmentSignsForMalaria

        medicationTreatment =
            Maybe.map
                (\egaWeeks ->
                    if egaWeeks <= 14 then
                        TreatmentQuinineSulphate

                    else
                        TreatmentCoartem
                )
                egaInWeeks
                |> Maybe.withDefault TreatmentQuinineSulphate

        -- Since we may have values set for another diagnosis, or from
        -- the other phase of encounter, we need to filter them out,
        -- to be able to determine the current value.
        currentValue =
            Maybe.andThen
                (List.filter (\sign -> List.member sign allowedSigns)
                    >> List.head
                )
                form.recommendedTreatmentSigns
    in
    ( [ viewCustomLabel language Translate.MalariaRecommendedTreatmentHeader "." "instructions"
      , h2 []
            [ text <| translate language Translate.ActionsToTake ++ ":" ]
      , div [ class "instructions" ]
            [ viewInstructionsLabel "icon-pills" (text <| translate language Translate.MalariaRecommendedTreatmentHelper ++ ":") ]
      , viewCheckBoxSelectInput language
            [ medicationTreatment
            , TreatmentWrittenProtocols
            , TreatmentReferToHospital
            , NoTreatmentForMalaria
            ]
            []
            currentValue
            (setRecommendedTreatmentSignMsg allowedSigns)
            Translate.RecommendedTreatmentSignLabel
      , div [ class "separator" ] []
      ]
    , taskCompleted currentValue
    , 1
    )


recommendedTreatmentSignsForMalaria : List RecommendedTreatmentSign
recommendedTreatmentSignsForMalaria =
    [ TreatmentQuinineSulphate
    , TreatmentCoartem
    , TreatmentWrittenProtocols
    , TreatmentReferToHospital
    , NoTreatmentForMalaria
    ]


resolveRecommendedTreatmentForSyphilisInputsAndTasks :
    Language
    -> NominalDate
    -> (List RecommendedTreatmentSign -> RecommendedTreatmentSign -> msg)
    -> List RecommendedTreatmentSign
    -> AssembledData
    -> MedicationDistributionForm
    -> ( List (Html msg), Int, Int )
resolveRecommendedTreatmentForSyphilisInputsAndTasks language currentDate setRecommendedTreatmentSignMsg allowedSigns assembled form =
    let
        -- Since we may have values set for another diagnosis, or from
        -- inital phase of encounter, we need to filter them out,
        -- to be able to determine the current value.
        currentValue =
            Maybe.andThen
                (List.filter (\sign -> List.member sign recommendedTreatmentSignsForSyphilis)
                    >> List.head
                )
                form.recommendedTreatmentSigns

        warning =
            Maybe.map
                (\signs ->
                    if
                        List.any (\sign -> List.member sign signs)
                            [ TreatmentErythromycin, TreatmentAzithromycin ]
                    then
                        div [ class "warning" ]
                            [ img [ src "assets/images/exclamation-red.png" ] []
                            , text <| translate language Translate.SyphilisRecommendedTreatmentWarning
                            ]

                    else
                        emptyNode
                )
                form.recommendedTreatmentSigns
                |> Maybe.withDefault emptyNode
    in
    ( [ viewCustomLabel language Translate.SyphilisRecommendedTreatmentHeader "." "instructions"
      , h2 [] [ text <| translate language Translate.ActionsToTake ++ ":" ]
      , div [ class "instructions" ]
            [ viewInstructionsLabel "icon-pills" (text <| translate language Translate.SyphilisRecommendedTreatmentHelper ++ ".")
            , p [ class "instructions-warning" ] [ text <| translate language Translate.SyphilisRecommendedTreatmentInstructions ++ "." ]
            ]
      , viewCheckBoxSelectCustomInput language
            recommendedTreatmentSignsForSyphilis
            []
            currentValue
            (setRecommendedTreatmentSignMsg allowedSigns)
            (viewTreatmentOptionWithDosage language)
      , warning
      , div [ class "separator" ] []
      ]
    , taskCompleted currentValue
    , 1
    )


recommendedTreatmentSignsForSyphilis : List RecommendedTreatmentSign
recommendedTreatmentSignsForSyphilis =
    [ TreatmentPenecilin1
    , TreatmentPenecilin3
    , TreatmentErythromycin
    , TreatmentAzithromycin
    , TreatmentCeftriaxon
    , NoTreatmentForSyphilis
    ]


resolveRecommendedTreatmentForHeartburnInputsAndTasks :
    Language
    -> NominalDate
    -> (List RecommendedTreatmentSign -> RecommendedTreatmentSign -> msg)
    -> AssembledData
    -> MedicationDistributionForm
    -> ( List (Html msg), Int, Int )
resolveRecommendedTreatmentForHeartburnInputsAndTasks language currentDate setRecommendedTreatmentSignMsg assembled form =
    let
        allowedSigns =
            recommendedTreatmentSignsForHeartburn

        -- Since we may have values set for another diagnosis,
        -- we need to filter them out, to be able to determine the current value.
        currentValue =
            Maybe.andThen
                (List.filter (\sign -> List.member sign allowedSigns)
                    >> List.head
                )
                form.recommendedTreatmentSigns
    in
    ( [ viewCustomLabel language Translate.HeartburnRecommendedTreatmentHeader "." "instructions"
      , h2 [] [ text <| translate language Translate.ActionsToTake ++ ":" ]
      , div [ class "instructions" ]
            [ viewInstructionsLabel "icon-pills" (text <| translate language Translate.HeartburnRecommendedTreatmentHelper ++ ".") ]
      , viewCheckBoxSelectCustomInput language
            allowedSigns
            []
            currentValue
            (setRecommendedTreatmentSignMsg allowedSigns)
            (viewTreatmentOptionForHeartburn language)
      , div [ class "separator" ] []
      ]
    , taskCompleted currentValue
    , 1
    )


viewTreatmentOptionForHeartburn : Language -> RecommendedTreatmentSign -> Html any
viewTreatmentOptionForHeartburn language sign =
    case sign of
        TreatmentAluminiumHydroxide ->
            viewTreatmentOptionWithDosage language sign

        _ ->
            label [] [ text <| translate language <| Translate.RecommendedTreatmentSignLabel sign ]


recommendedTreatmentSignsForHeartburn : List RecommendedTreatmentSign
recommendedTreatmentSignsForHeartburn =
    [ TreatmentAluminiumHydroxide
    , TreatmentHealthEducationForHeartburn
    ]


resolveRecommendedTreatmentForUrinaryTractInfectionInputsAndTasks :
    Language
    -> NominalDate
    -> (List RecommendedTreatmentSign -> RecommendedTreatmentSign -> msg)
    -> AssembledData
    -> MedicationDistributionForm
    -> ( List (Html msg), Int, Int )
resolveRecommendedTreatmentForUrinaryTractInfectionInputsAndTasks language currentDate setRecommendedTreatmentSignMsg assembled form =
    let
        allowedSigns =
            recommendedTreatmentSignsForUrinaryTractInfection

        -- Since we may have values set for another diagnosis,
        -- we need to filter them out, to be able to determine the current value.
        currentValue =
            Maybe.andThen
                (List.filter (\sign -> List.member sign allowedSigns)
                    >> List.head
                )
                form.recommendedTreatmentSigns
    in
    ( [ viewCustomLabel language Translate.UrinaryTractInfectionRecommendedTreatmentHeader "." "instructions"
      , h2 [] [ text <| translate language Translate.ActionsToTake ++ ":" ]
      , div [ class "instructions" ]
            [ viewInstructionsLabel "icon-pills" (text <| translate language Translate.UrinaryTractInfectionRecommendedTreatmentHelper ++ ".")
            , p [ class "instructions-warning" ] [ text <| translate language Translate.UrinaryTractInfectionRecommendedTreatmentInstructions ++ "." ]
            ]
      , viewCheckBoxSelectCustomInput language
            allowedSigns
            []
            currentValue
            (setRecommendedTreatmentSignMsg allowedSigns)
            (viewTreatmentOptionWithDosage language)
      , div [ class "separator" ] []
      ]
    , taskCompleted currentValue
    , 1
    )


recommendedTreatmentSignsForUrinaryTractInfection : List RecommendedTreatmentSign
recommendedTreatmentSignsForUrinaryTractInfection =
    [ TreatmentNitrofurantoin
    , TreatmentAmoxicillin
    ]


resolveRecommendedTreatmentForCandidiasisInputsAndTasks :
    Language
    -> NominalDate
    -> (List RecommendedTreatmentSign -> RecommendedTreatmentSign -> msg)
    -> AssembledData
    -> MedicationDistributionForm
    -> ( List (Html msg), Int, Int )
resolveRecommendedTreatmentForCandidiasisInputsAndTasks language currentDate setRecommendedTreatmentSignMsg assembled form =
    let
        allowedSigns =
            recommendedTreatmentSignsForCandidiasis

        -- Since we may have values set for another diagnosis,
        -- we need to filter them out, to be able to determine the current value.
        currentValue =
            Maybe.andThen
                (List.filter (\sign -> List.member sign allowedSigns)
                    >> List.head
                )
                form.recommendedTreatmentSigns
    in
    ( [ viewCustomLabel language Translate.CandidiasisRecommendedTreatmentHeader "." "instructions"
      , h2 [] [ text <| translate language Translate.ActionsToTake ++ ":" ]
      , div [ class "instructions" ]
            [ viewInstructionsLabel "icon-pills" (text <| translate language Translate.CandidiasisRecommendedTreatmentHelper ++ ":")
            ]
      , viewCheckBoxSelectCustomInput language
            allowedSigns
            []
            currentValue
            (setRecommendedTreatmentSignMsg allowedSigns)
            (viewTreatmentOptionWithDosage language)
      , div [ class "separator" ] []
      ]
    , taskCompleted currentValue
    , 1
    )


recommendedTreatmentSignsForCandidiasis : List RecommendedTreatmentSign
recommendedTreatmentSignsForCandidiasis =
    [ TreatmentClotrimaxazole200
    , TreatmentClotrimaxazole500
    ]


resolveRecommendedTreatmentForMastitisInputsAndTasks :
    Language
    -> NominalDate
    -> (List RecommendedTreatmentSign -> RecommendedTreatmentSign -> msg)
    -> AssembledData
    -> MedicationDistributionForm
    -> ( List (Html msg), Int, Int )
resolveRecommendedTreatmentForMastitisInputsAndTasks language currentDate setRecommendedTreatmentSignMsg assembled form =
    let
        allowedSigns =
            recommendedTreatmentSignsForMastitis

        forEarlyMastitisOrEngorgment =
            diagnosed DiagnosisPostpartumEarlyMastitisOrEngorgment assembled

        -- Since we may have values set for another diagnosis,
        -- we need to filter them out, to be able to determine the current value.
        currentValue =
            Maybe.andThen
                (List.filter (\sign -> List.member sign allowedSigns)
                    >> List.head
                )
                form.recommendedTreatmentSigns
    in
    ( [ viewCustomLabel language (Translate.MastitisRecommendedTreatmentHeader forEarlyMastitisOrEngorgment) "." "instructions"
      , h2 [] [ text <| translate language Translate.ActionsToTake ++ ":" ]
      , div [ class "instructions" ]
            [ viewInstructionsLabel "icon-pills" (text <| translate language Translate.MastitisRecommendedTreatmentHelper ++ ".") ]
      , viewCheckBoxSelectCustomInput language
            allowedSigns
            []
            currentValue
            (setRecommendedTreatmentSignMsg allowedSigns)
            (viewTreatmentOptionWithDosage language)
      , div [ class "separator" ] []
      ]
    , taskCompleted currentValue
    , 1
    )


recommendedTreatmentSignsForMastitis : List RecommendedTreatmentSign
recommendedTreatmentSignsForMastitis =
    [ TreatmentCloxacillin
    , TreatmentMastitisAmoxicillin
    , TreatmentPenecilinV
    , TreatmentParacetamol
    , TreatmentIbuprofen
    , NoTreatmentForMastitis
    ]


{-| Medication Distribution activity appears on both initial and recurrent encounters.
Each one of them got unique set of signs that can be used.
In order to know if activity was completed or not, we check if at least one
of those signs was set.
-}
medicationDistributionMeasurementTaken : List MedicationDistributionSign -> PrenatalMeasurements -> Bool
medicationDistributionMeasurementTaken allowedSigns measurements =
    getMeasurementValueFunc measurements.medicationDistribution
        |> Maybe.map
            (\value ->
                List.any (\sign -> EverySet.member sign value.distributionSigns)
                    allowedSigns
            )
        |> Maybe.withDefault False


resolveRequiredMedicationsSet :
    Language
    -> NominalDate
    -> PrenatalEncounterPhase
    -> AssembledData
    -> List ( TranslationId, List MedicationDistributionSign, List (Html any) )
resolveRequiredMedicationsSet language currentDate phase assembled =
    case phase of
        -- Not for Postpartum encounter.
        PrenatalEncounterPhaseInitial ->
            let
                mebendazoleSet =
                    let
                        prescribeMebendazole =
                            (assembled.encounter.encounterType == NurseEncounter)
                                && showMebendazoleQuestion currentDate assembled
                                && (getMeasurementValueFunc assembled.measurements.medication
                                        |> Maybe.andThen .signs
                                        |> Maybe.map (EverySet.member Mebendazole >> not)
                                        |> Maybe.withDefault False
                                   )
                    in
                    if prescribeMebendazole then
                        Just
                            ( Translate.MedicationDistributionHelperMebendazole
                            , [ Mebendezole ]
                            , []
                            )

                    else
                        Nothing

                hivPositiveSet =
                    let
                        hivDiagnosed =
                            diagnosed DiagnosisHIV assembled

                        hivProgramHC =
                            hivProgramAtHC assembled.measurements
                    in
                    if
                        (hivDiagnosed && not hivProgramHC)
                            || patientReportedNoMedicineRecievedFromPMTCT assembled.measurements
                    then
                        Just
                            ( Translate.MedicationDistributionHelperHIV
                            , [ TDF3TC, Dolutegravir ]
                            , []
                            )

                    else
                        Nothing

                discordantPartnershipSet =
                    if diagnosed DiagnosisDiscordantPartnership assembled then
                        let
                            partnerTakingARVs =
                                getMeasurementValueFunc assembled.measurements.hivTest
                                    |> Maybe.andThen .hivSigns
                                    |> Maybe.map (EverySet.member PartnerTakingARV)
                                    |> Maybe.withDefault False

                            helper =
                                if partnerTakingARVs then
                                    Translate.MedicationDistributionHelperDiscordantPartnership

                                else
                                    Translate.MedicationDistributionHelperDiscordantPartnershipNoARVs
                        in
                        Just
                            ( helper
                            , [ TDF3TC ]
                            , []
                            )

                    else
                        Nothing

                gonorheaSet =
                    if diagnosed DiagnosisGonorrhea assembled then
                        Just
                            ( Translate.MedicationDistributionHelperGonorrhea
                            , [ Ceftriaxone, Azithromycin ]
                            , [ viewCustomLabel language Translate.MedicationDistributionNoticeGonorrhea "," "label footer"
                              , ul []
                                    [ li [] [ text <| translate language Translate.MedicationDistributionNoticeGonorrheaPartnerMed1 ]
                                    , li [] [ text <| translate language Translate.MedicationDistributionNoticeGonorrheaPartnerMed2 ]
                                    ]
                              ]
                            )

                    else
                        Nothing

                trichomonasOrBVSet =
                    if diagnosed DiagnosisTrichomonasOrBacterialVaginosis assembled then
                        Just
                            ( Translate.MedicationDistributionHelperTrichomonasOrBacterialVaginosis
                            , [ Metronidazole ]
                            , []
                            )

                    else
                        Nothing

                -- Only for Postpartum encounter.
                vitaminASet =
                    let
                        prescribeVitaminA =
                            (assembled.encounter.encounterType == NursePostpartumEncounter)
                                && (getMeasurementValueFunc assembled.measurements.medication
                                        |> Maybe.andThen .signs
                                        |> Maybe.map (EverySet.member PostpartumVitaminA >> not)
                                        |> Maybe.withDefault False
                                   )
                    in
                    if prescribeVitaminA then
                        Just
                            ( Translate.MedicationDistributionHelperVitaminA
                            , [ VitaminA ]
                            , []
                            )

                    else
                        Nothing

                -- Used to be prescribed for Early Mastitis / Engorgment
                -- diagnosis. Currently not in use.
                -- Keeping it's infrastructure for possible future applications.
                paracetamolSet =
                    ( Translate.MedicationDistributionHelperEarlyMastitisOrEngorgment
                    , [ Paracetamol ]
                    , []
                    )
            in
            Maybe.Extra.values
                [ mebendazoleSet
                , hivPositiveSet
                , discordantPartnershipSet
                , gonorheaSet
                , trichomonasOrBVSet
                , vitaminASet
                ]

        PrenatalEncounterPhaseRecurrent ->
            if
                diagnosed DiagnosisModerateAnemia assembled
                    && (not <| referToHospitalDueToAdverseEventForAnemiaTreatment assembled)
            then
                [ ( Translate.MedicationDistributionHelperAnemia
                  , [ Iron, FolicAcid ]
                  , []
                  )
                ]

            else
                []


diagnosesCausingHospitalReferralByAdverseEventForTreatment : AssembledData -> List PrenatalDiagnosis
diagnosesCausingHospitalReferralByAdverseEventForTreatment assembled =
    filterDiagnosesCausingHospitalReferralByAdverseEventForTreatment
        [ DiagnosisHIV
        , -- Since treatment for Hypertension and Moderate Preeclampsia
          -- is identical, we use this indicator for both.
          DiagnosisChronicHypertensionImmediate
        , DiagnosisMalaria
        , DiagnosisModerateAnemia
        , DiagnosisSyphilis
        ]
        assembled


{-| Note: Even though name says Hypertension, it includes Moderate Preeclamsia as well.
Moderate Preeclamsia logic is a higher lavel condition for Hypertension within
identical treatement, and therefore, we do not differentiate between the 2.
-}
referToHospitalDueToAdverseEventForHypertensionTreatment : AssembledData -> Bool
referToHospitalDueToAdverseEventForHypertensionTreatment =
    filterDiagnosesCausingHospitalReferralByAdverseEventForTreatment
        [ -- Since treatment for Hypertension and Moderate Preeclampsia
          -- is identical, we use this indicator for both.
          DiagnosisChronicHypertensionImmediate
        ]
        >> List.isEmpty
        >> not


referToHospitalDueToAdverseEventForAnemiaTreatment : AssembledData -> Bool
referToHospitalDueToAdverseEventForAnemiaTreatment =
    filterDiagnosesCausingHospitalReferralByAdverseEventForTreatment
        [ DiagnosisModerateAnemia ]
        >> List.isEmpty
        >> not


referToHospitalDueToAdverseEventForMalariaTreatment : AssembledData -> Bool
referToHospitalDueToAdverseEventForMalariaTreatment =
    filterDiagnosesCausingHospitalReferralByAdverseEventForTreatment
        [ DiagnosisMalaria ]
        >> List.isEmpty
        >> not


filterDiagnosesCausingHospitalReferralByAdverseEventForTreatment : List PrenatalDiagnosis -> AssembledData -> List PrenatalDiagnosis
filterDiagnosesCausingHospitalReferralByAdverseEventForTreatment diagnoses assembled =
    getMeasurementValueFunc assembled.measurements.medication
        |> Maybe.map
            (\value ->
                let
                    conditionByDiagnosis diagnosis =
                        case diagnosis of
                            DiagnosisHIV ->
                                Maybe.map (EverySet.member HIVTreatmentAdverseEventsHospitalization) value.hivTreatment
                                    |> Maybe.withDefault False

                            DiagnosisChronicHypertensionImmediate ->
                                referByTreatment value.hypertensionTreatment

                            DiagnosisMalaria ->
                                referByTreatment value.malariaTreatment

                            DiagnosisModerateAnemia ->
                                referByTreatment value.anemiaTreatment

                            DiagnosisSyphilis ->
                                referByTreatment value.syphilisTreatment

                            -- There's no other diagnosis treatment we revise
                            -- at Treatment Review activity.
                            _ ->
                                False

                    referByTreatment =
                        Maybe.map (EverySet.member MedicationTreatmentAdverseEventsHospitalization)
                            >> Maybe.withDefault False
                in
                List.filter conditionByDiagnosis diagnoses
            )
        |> Maybe.withDefault []


diagnosesCausingHospitalReferralByPastDiagnoses : AssembledData -> List PrenatalDiagnosis
diagnosesCausingHospitalReferralByPastDiagnoses assembled =
    let
        allowedPastDiagnoses =
            syphilisDiagnosesIncludingNeurosyphilis
                ++ diabetesDiagnoses
                ++ [ DiagnosisHepatitisB, DiagnosisRhesusNegative ]
    in
    EverySet.toList assembled.encounter.pastDiagnoses
        |> List.filter (\diagnosis -> List.member diagnosis allowedPastDiagnoses)


showMebendazoleQuestion : NominalDate -> AssembledData -> Bool
showMebendazoleQuestion currentDate assembled =
    assembled.globalLmpDate
        |> Maybe.map
            (\lmpDate ->
                let
                    egaInWeeks =
                        calculateEGAWeeks currentDate lmpDate

                    dewormingPillNotGiven =
                        List.filter
                            (.measurements
                                >> .medication
                                >> getMeasurementValueFunc
                                >> Maybe.andThen .signs
                                >> Maybe.map (EverySet.member DewormingPill)
                                >> Maybe.withDefault False
                            )
                            assembled.nursePreviousEncountersData
                            |> List.isEmpty

                    mebenadazoleNotPrescribed =
                        List.filter
                            (.measurements
                                >> .medicationDistribution
                                >> Maybe.map (Tuple.second >> .value >> .distributionSigns >> EverySet.member Mebendezole)
                                >> Maybe.withDefault False
                            )
                            assembled.nursePreviousEncountersData
                            |> List.isEmpty
                in
                -- Starting EGA week 24.
                (egaInWeeks >= 24)
                    && -- Previous variation had a question about deworming pill,
                       -- which is actually Menendazole, or something similar.
                       -- If somewhere during previous encounters patient stated that
                       -- deworming pill was given, we do not ask about Mebendazole.
                       dewormingPillNotGiven
                    && -- Mebendazole was not prescribed during the current pregnancy.
                       mebenadazoleNotPrescribed
            )
        |> Maybe.withDefault False


patientReportedNoMedicineRecievedFromPMTCT : PrenatalMeasurements -> Bool
patientReportedNoMedicineRecievedFromPMTCT measurements =
    getMeasurementValueFunc measurements.medication
        |> Maybe.andThen .hivTreatment
        |> Maybe.map
            (\hivTreatment ->
                List.any (\sign -> EverySet.member sign hivTreatment)
                    [ HIVTreatmentNoMedicineNotSeenAtPMTCT
                    , HIVTreatmentNoMedicineOutOfStock
                    , HIVTreatmentNoMedicinePatientRefused
                    , HIVTreatmentNoMedicineOther
                    ]
            )
        |> Maybe.withDefault False


resolveMedicationDistributionInputsAndTasksForMedication :
    Language
    -> NominalDate
    -> Person
    -> ((Bool -> MedicationDistributionForm -> MedicationDistributionForm) -> Bool -> msg)
    -> (Maybe AdministrationNote -> MedicationDistributionSign -> AdministrationNote -> msg)
    -> MedicationDistributionForm
    -> MedicationDistributionSign
    -> ( List (Html msg), Int, Int )
resolveMedicationDistributionInputsAndTasksForMedication language currentDate person setMedicationDistributionBoolInputMsg setMedicationDistributionAdministrationNoteMsg form medication =
    case medication of
        Mebendezole ->
            resolveMebendezoleDistributionInputsAndTasks language currentDate person setMedicationDistributionBoolInputMsg setMedicationDistributionAdministrationNoteMsg form

        Tenofovir ->
            resolveTenofovirDistributionInputsAndTasks language currentDate person setMedicationDistributionBoolInputMsg setMedicationDistributionAdministrationNoteMsg form

        Lamivudine ->
            resolveLamivudineDistributionInputsAndTasks language currentDate person setMedicationDistributionBoolInputMsg setMedicationDistributionAdministrationNoteMsg form

        Dolutegravir ->
            resolveDolutegravirDistributionInputsAndTasks language currentDate person setMedicationDistributionBoolInputMsg setMedicationDistributionAdministrationNoteMsg form

        TDF3TC ->
            resolveTDF3TCDistributionInputsAndTasks language currentDate person setMedicationDistributionBoolInputMsg setMedicationDistributionAdministrationNoteMsg form

        Iron ->
            resolveIronDistributionInputsAndTasks language currentDate person setMedicationDistributionBoolInputMsg setMedicationDistributionAdministrationNoteMsg form

        FolicAcid ->
            resolveFolicAcidDistributionInputsAndTasks language currentDate person setMedicationDistributionBoolInputMsg setMedicationDistributionAdministrationNoteMsg form

        Ceftriaxone ->
            resolveCeftriaxoneDistributionInputsAndTasks language currentDate person setMedicationDistributionBoolInputMsg setMedicationDistributionAdministrationNoteMsg form

        Azithromycin ->
            resolveAzithromycinDistributionInputsAndTasks language currentDate person setMedicationDistributionBoolInputMsg setMedicationDistributionAdministrationNoteMsg form

        Metronidazole ->
            resolveMetronidazoleDistributionInputsAndTasks language currentDate person setMedicationDistributionBoolInputMsg setMedicationDistributionAdministrationNoteMsg form

        VitaminA ->
            resolveVitaminADistributionInputsAndTasks language currentDate person setMedicationDistributionBoolInputMsg setMedicationDistributionAdministrationNoteMsg form

        Paracetamol ->
            resolveParacetamolDistributionInputsAndTasks language currentDate person setMedicationDistributionBoolInputMsg setMedicationDistributionAdministrationNoteMsg form

        -- Other medications are not prescribed at Prenatal encounter.
        _ ->
            ( [], 0, 0 )


resolveMebendezoleDistributionInputsAndTasks :
    Language
    -> NominalDate
    -> Person
    -> ((Bool -> MedicationDistributionForm -> MedicationDistributionForm) -> Bool -> msg)
    -> (Maybe AdministrationNote -> MedicationDistributionSign -> AdministrationNote -> msg)
    -> MedicationDistributionForm
    -> ( List (Html msg), Int, Int )
resolveMebendezoleDistributionInputsAndTasks language currentDate person setMedicationDistributionBoolInputMsg setMedicationDistributionAdministrationNoteMsg form =
    let
        instructions =
            resolveMebendezoleDosageAndIcon currentDate person
                |> Maybe.map
                    (\( dosage, icon ) ->
                        div [ class "instructions" ]
                            [ viewAdministeredMedicationCustomLabel language Translate.Administer (Translate.MedicationDistributionSign Mebendezole) ("(" ++ dosage ++ ")") icon "" Nothing
                            , div [ class "prescription" ] [ text <| translate language Translate.AdministerPrenatalMebendezoleHelper ++ "." ]
                            ]
                    )
                |> Maybe.withDefault emptyNode

        updateFunc value form_ =
            { form_ | mebendezole = Just value, nonAdministrationSigns = updateNonAdministrationSigns Mebendezole MedicationMebendezole value form_ }

        ( derivedInput, derivedTaskCompleted, derivedTaskActive ) =
            if form.mebendezole == Just False then
                ( viewMedicationDistributionDerivedQuestion language Mebendezole MedicationMebendezole setMedicationDistributionAdministrationNoteMsg form
                , taskCompleted <|
                    getCurrentReasonForMedicationNonAdministration MedicationMebendezole form
                , 1
                )

            else
                ( [], 0, 0 )
    in
    ( [ instructions
      , viewAdministeredMedicationQuestion language (Translate.MedicationDistributionSign Mebendezole)
      , viewBoolInput
            language
            form.mebendezole
            (setMedicationDistributionBoolInputMsg updateFunc)
            "mebendezole-medication"
            Nothing
      ]
        ++ derivedInput
    , taskCompleted form.mebendezole + derivedTaskCompleted
    , 1 + derivedTaskActive
    )


resolveTenofovirDistributionInputsAndTasks :
    Language
    -> NominalDate
    -> Person
    -> ((Bool -> MedicationDistributionForm -> MedicationDistributionForm) -> Bool -> msg)
    -> (Maybe AdministrationNote -> MedicationDistributionSign -> AdministrationNote -> msg)
    -> MedicationDistributionForm
    -> ( List (Html msg), Int, Int )
resolveTenofovirDistributionInputsAndTasks language currentDate person setMedicationDistributionBoolInputMsg setMedicationDistributionAdministrationNoteMsg form =
    let
        instructions =
            resolveTenofovirDosageAndIcon currentDate person
                |> Maybe.map
                    (\( dosage, icon ) ->
                        div [ class "instructions" ]
                            [ viewAdministeredMedicationCustomLabel language Translate.Administer (Translate.MedicationDistributionSign Tenofovir) ("(" ++ dosage ++ ")") icon "" Nothing
                            , div [ class "prescription" ] [ text <| translate language Translate.AdministerHIVARVHelper ++ "." ]
                            ]
                    )
                |> Maybe.withDefault emptyNode

        updateFunc value form_ =
            { form_ | tenofovir = Just value, nonAdministrationSigns = updateNonAdministrationSigns Tenofovir MedicationTenofovir value form_ }

        ( derivedInput, derivedTaskCompleted, derivedTaskActive ) =
            if form.tenofovir == Just False then
                ( viewMedicationDistributionDerivedQuestion language Tenofovir MedicationTenofovir setMedicationDistributionAdministrationNoteMsg form
                , taskCompleted <|
                    getCurrentReasonForMedicationNonAdministration MedicationTenofovir form
                , 1
                )

            else
                ( [], 0, 0 )
    in
    ( [ instructions
      , viewAdministeredMedicationQuestion language (Translate.MedicationDistributionSign Tenofovir)
      , viewBoolInput
            language
            form.tenofovir
            (setMedicationDistributionBoolInputMsg updateFunc)
            "tenofovir-medication"
            Nothing
      ]
        ++ derivedInput
    , taskCompleted form.tenofovir + derivedTaskCompleted
    , 1 + derivedTaskActive
    )


resolveLamivudineDistributionInputsAndTasks :
    Language
    -> NominalDate
    -> Person
    -> ((Bool -> MedicationDistributionForm -> MedicationDistributionForm) -> Bool -> msg)
    -> (Maybe AdministrationNote -> MedicationDistributionSign -> AdministrationNote -> msg)
    -> MedicationDistributionForm
    -> ( List (Html msg), Int, Int )
resolveLamivudineDistributionInputsAndTasks language currentDate person setMedicationDistributionBoolInputMsg setMedicationDistributionAdministrationNoteMsg form =
    let
        instructions =
            resolveLamivudineDosageAndIcon currentDate person
                |> Maybe.map
                    (\( dosage, icon ) ->
                        div [ class "instructions" ]
                            [ viewAdministeredMedicationCustomLabel language Translate.Administer (Translate.MedicationDistributionSign Lamivudine) ("(" ++ dosage ++ ")") icon "" Nothing
                            , div [ class "prescription" ] [ text <| translate language Translate.AdministerHIVARVHelper ++ "." ]
                            ]
                    )
                |> Maybe.withDefault emptyNode

        updateFunc value form_ =
            { form_ | lamivudine = Just value, nonAdministrationSigns = updateNonAdministrationSigns Lamivudine MedicationLamivudine value form_ }

        ( derivedInput, derivedTaskCompleted, derivedTaskActive ) =
            if form.lamivudine == Just False then
                ( viewMedicationDistributionDerivedQuestion language Lamivudine MedicationLamivudine setMedicationDistributionAdministrationNoteMsg form
                , taskCompleted <|
                    getCurrentReasonForMedicationNonAdministration MedicationLamivudine form
                , 1
                )

            else
                ( [], 0, 0 )
    in
    ( [ instructions
      , viewAdministeredMedicationQuestion language (Translate.MedicationDistributionSign Lamivudine)
      , viewBoolInput
            language
            form.lamivudine
            (setMedicationDistributionBoolInputMsg updateFunc)
            "lamivudine-medication"
            Nothing
      ]
        ++ derivedInput
    , taskCompleted form.lamivudine + derivedTaskCompleted
    , 1 + derivedTaskActive
    )


resolveDolutegravirDistributionInputsAndTasks :
    Language
    -> NominalDate
    -> Person
    -> ((Bool -> MedicationDistributionForm -> MedicationDistributionForm) -> Bool -> msg)
    -> (Maybe AdministrationNote -> MedicationDistributionSign -> AdministrationNote -> msg)
    -> MedicationDistributionForm
    -> ( List (Html msg), Int, Int )
resolveDolutegravirDistributionInputsAndTasks language currentDate person setMedicationDistributionBoolInputMsg setMedicationDistributionAdministrationNoteMsg form =
    let
        instructions =
            resolveDolutegravirDosageAndIcon currentDate person
                |> Maybe.map
                    (\( dosage, icon ) ->
                        div [ class "instructions" ]
                            [ viewAdministeredMedicationCustomLabel language Translate.Administer (Translate.MedicationDistributionSign Dolutegravir) ("(" ++ dosage ++ ")") icon "" Nothing
                            , div [ class "prescription" ] [ text <| translate language Translate.AdministerHIVARVHelper ++ "." ]
                            ]
                    )
                |> Maybe.withDefault emptyNode

        updateFunc value form_ =
            { form_ | dolutegravir = Just value, nonAdministrationSigns = updateNonAdministrationSigns Dolutegravir MedicationDolutegravir value form_ }

        ( derivedInput, derivedTaskCompleted, derivedTaskActive ) =
            if form.dolutegravir == Just False then
                ( viewMedicationDistributionDerivedQuestion language Dolutegravir MedicationDolutegravir setMedicationDistributionAdministrationNoteMsg form
                , taskCompleted <|
                    getCurrentReasonForMedicationNonAdministration MedicationDolutegravir form
                , 1
                )

            else
                ( [], 0, 0 )
    in
    ( [ instructions
      , viewAdministeredMedicationQuestion language (Translate.MedicationDistributionSign Dolutegravir)
      , viewBoolInput
            language
            form.dolutegravir
            (setMedicationDistributionBoolInputMsg updateFunc)
            "dolutegravir-medication"
            Nothing
      ]
        ++ derivedInput
    , taskCompleted form.dolutegravir + derivedTaskCompleted
    , 1 + derivedTaskActive
    )


resolveTDF3TCDistributionInputsAndTasks :
    Language
    -> NominalDate
    -> Person
    -> ((Bool -> MedicationDistributionForm -> MedicationDistributionForm) -> Bool -> msg)
    -> (Maybe AdministrationNote -> MedicationDistributionSign -> AdministrationNote -> msg)
    -> MedicationDistributionForm
    -> ( List (Html msg), Int, Int )
resolveTDF3TCDistributionInputsAndTasks language currentDate person setMedicationDistributionBoolInputMsg setMedicationDistributionAdministrationNoteMsg form =
    let
        instructions =
            div [ class "instructions" ]
                [ viewAdministeredMedicationCustomLabel language Translate.Administer (Translate.MedicationDistributionSign TDF3TC) "" "icon-pills" "" Nothing
                , div [ class "prescription" ] [ text <| translate language Translate.AdministerHIVARVHelper ++ "." ]
                ]

        updateFunc value form_ =
            { form_ | tdf3tc = Just value, nonAdministrationSigns = updateNonAdministrationSigns TDF3TC MedicationTDF3TC value form_ }

        ( derivedInput, derivedTaskCompleted, derivedTaskActive ) =
            if form.tdf3tc == Just False then
                ( viewMedicationDistributionDerivedQuestion language TDF3TC MedicationTDF3TC setMedicationDistributionAdministrationNoteMsg form
                , taskCompleted <|
                    getCurrentReasonForMedicationNonAdministration MedicationTDF3TC form
                , 1
                )

            else
                ( [], 0, 0 )
    in
    ( [ instructions
      , viewAdministeredMedicationQuestion language (Translate.MedicationDistributionSign TDF3TC)
      , viewBoolInput
            language
            form.tdf3tc
            (setMedicationDistributionBoolInputMsg updateFunc)
            "tdf3tc-medication"
            Nothing
      ]
        ++ derivedInput
    , taskCompleted form.tdf3tc + derivedTaskCompleted
    , 1 + derivedTaskActive
    )


resolveIronDistributionInputsAndTasks :
    Language
    -> NominalDate
    -> Person
    -> ((Bool -> MedicationDistributionForm -> MedicationDistributionForm) -> Bool -> msg)
    -> (Maybe AdministrationNote -> MedicationDistributionSign -> AdministrationNote -> msg)
    -> MedicationDistributionForm
    -> ( List (Html msg), Int, Int )
resolveIronDistributionInputsAndTasks language currentDate person setMedicationDistributionBoolInputMsg setMedicationDistributionAdministrationNoteMsg form =
    let
        instructions =
            resolveIronDosageAndIcon currentDate person
                |> Maybe.map
                    (\( dosage, icon ) ->
                        div [ class "instructions" ]
                            [ viewAdministeredMedicationCustomLabel language Translate.Administer (Translate.MedicationDistributionSign Iron) ("(" ++ dosage ++ ")") icon "" Nothing
                            , div [ class "prescription" ] [ text <| translate language Translate.AdministerIronHelper ++ "." ]
                            ]
                    )
                |> Maybe.withDefault emptyNode

        updateFunc value form_ =
            { form_ | iron = Just value, nonAdministrationSigns = updateNonAdministrationSigns Iron MedicationIron value form_ }

        ( derivedInput, derivedTaskCompleted, derivedTaskActive ) =
            if form.iron == Just False then
                ( viewMedicationDistributionDerivedQuestion language Iron MedicationIron setMedicationDistributionAdministrationNoteMsg form
                , taskCompleted <|
                    getCurrentReasonForMedicationNonAdministration MedicationIron form
                , 1
                )

            else
                ( [], 0, 0 )
    in
    ( [ instructions
      , viewAdministeredMedicationQuestion language (Translate.MedicationDistributionSign Iron)
      , viewBoolInput
            language
            form.iron
            (setMedicationDistributionBoolInputMsg updateFunc)
            "iron-medication"
            Nothing
      ]
        ++ derivedInput
    , taskCompleted form.iron + derivedTaskCompleted
    , 1 + derivedTaskActive
    )


resolveFolicAcidDistributionInputsAndTasks :
    Language
    -> NominalDate
    -> Person
    -> ((Bool -> MedicationDistributionForm -> MedicationDistributionForm) -> Bool -> msg)
    -> (Maybe AdministrationNote -> MedicationDistributionSign -> AdministrationNote -> msg)
    -> MedicationDistributionForm
    -> ( List (Html msg), Int, Int )
resolveFolicAcidDistributionInputsAndTasks language currentDate person setMedicationDistributionBoolInputMsg setMedicationDistributionAdministrationNoteMsg form =
    let
        instructions =
            resolveFolicAcidDosageAndIcon currentDate person
                |> Maybe.map
                    (\( dosage, icon ) ->
                        div [ class "instructions" ]
                            [ viewAdministeredMedicationCustomLabel language Translate.Administer (Translate.MedicationDistributionSign FolicAcid) ("(" ++ dosage ++ ")") icon "" Nothing
                            , div [ class "prescription" ] [ text <| translate language Translate.AdministerFolicAcidHelper ++ "." ]
                            ]
                    )
                |> Maybe.withDefault emptyNode

        updateFunc value form_ =
            { form_ | folicAcid = Just value, nonAdministrationSigns = updateNonAdministrationSigns FolicAcid MedicationFolicAcid value form_ }

        ( derivedInput, derivedTaskCompleted, derivedTaskActive ) =
            if form.folicAcid == Just False then
                ( viewMedicationDistributionDerivedQuestion language FolicAcid MedicationFolicAcid setMedicationDistributionAdministrationNoteMsg form
                , taskCompleted <|
                    getCurrentReasonForMedicationNonAdministration MedicationFolicAcid form
                , 1
                )

            else
                ( [], 0, 0 )
    in
    ( [ instructions
      , viewAdministeredMedicationQuestion language (Translate.MedicationDistributionSign FolicAcid)
      , viewBoolInput
            language
            form.folicAcid
            (setMedicationDistributionBoolInputMsg updateFunc)
            "folicAcid-medication"
            Nothing
      ]
        ++ derivedInput
    , taskCompleted form.folicAcid + derivedTaskCompleted
    , 1 + derivedTaskActive
    )


resolveCeftriaxoneDistributionInputsAndTasks :
    Language
    -> NominalDate
    -> Person
    -> ((Bool -> MedicationDistributionForm -> MedicationDistributionForm) -> Bool -> msg)
    -> (Maybe AdministrationNote -> MedicationDistributionSign -> AdministrationNote -> msg)
    -> MedicationDistributionForm
    -> ( List (Html msg), Int, Int )
resolveCeftriaxoneDistributionInputsAndTasks language currentDate person setMedicationDistributionBoolInputMsg setMedicationDistributionAdministrationNoteMsg form =
    let
        instructions =
            resolveCeftriaxoneDosageAndIcon currentDate person
                |> Maybe.map
                    (\( dosage, icon ) ->
                        div [ class "instructions" ]
                            [ viewAdministeredMedicationCustomLabel language Translate.Administer (Translate.MedicationDistributionSign Ceftriaxone) ("(" ++ dosage ++ ")") icon "" Nothing
                            , div [ class "prescription" ] [ text <| translate language Translate.AdministerCeftriaxoneHelper ++ "." ]
                            ]
                    )
                |> Maybe.withDefault emptyNode

        updateFunc value form_ =
            { form_ | ceftriaxone = Just value, nonAdministrationSigns = updateNonAdministrationSigns Ceftriaxone MedicationCeftriaxone value form_ }

        ( derivedInput, derivedTaskCompleted, derivedTaskActive ) =
            if form.ceftriaxone == Just False then
                ( viewMedicationDistributionDerivedQuestion language Ceftriaxone MedicationCeftriaxone setMedicationDistributionAdministrationNoteMsg form
                , taskCompleted <|
                    getCurrentReasonForMedicationNonAdministration MedicationCeftriaxone form
                , 1
                )

            else
                ( [], 0, 0 )
    in
    ( [ instructions
      , viewAdministeredMedicationQuestion language (Translate.MedicationDistributionSign Ceftriaxone)
      , viewBoolInput
            language
            form.ceftriaxone
            (setMedicationDistributionBoolInputMsg updateFunc)
            "ceftriaxone-medication"
            Nothing
      ]
        ++ derivedInput
    , taskCompleted form.ceftriaxone + derivedTaskCompleted
    , 1 + derivedTaskActive
    )


resolveAzithromycinDistributionInputsAndTasks :
    Language
    -> NominalDate
    -> Person
    -> ((Bool -> MedicationDistributionForm -> MedicationDistributionForm) -> Bool -> msg)
    -> (Maybe AdministrationNote -> MedicationDistributionSign -> AdministrationNote -> msg)
    -> MedicationDistributionForm
    -> ( List (Html msg), Int, Int )
resolveAzithromycinDistributionInputsAndTasks language currentDate person setMedicationDistributionBoolInputMsg setMedicationDistributionAdministrationNoteMsg form =
    let
        instructions =
            resolveAzithromycinDosageAndIcon currentDate person
                |> Maybe.map
                    (\( dosage, icon ) ->
                        div [ class "instructions" ]
                            [ viewAdministeredMedicationCustomLabel language Translate.Administer (Translate.MedicationDistributionSign Azithromycin) ("(" ++ dosage ++ ")") icon "" Nothing
                            , div [ class "prescription" ] [ text <| translate language Translate.AdministerAzithromycinHelper ++ "." ]
                            ]
                    )
                |> Maybe.withDefault emptyNode

        updateFunc value form_ =
            { form_ | azithromycin = Just value, nonAdministrationSigns = updateNonAdministrationSigns Azithromycin MedicationAzithromycin value form_ }

        ( derivedInput, derivedTaskCompleted, derivedTaskActive ) =
            if form.azithromycin == Just False then
                ( viewMedicationDistributionDerivedQuestion language Azithromycin MedicationAzithromycin setMedicationDistributionAdministrationNoteMsg form
                , taskCompleted <|
                    getCurrentReasonForMedicationNonAdministration MedicationAzithromycin form
                , 1
                )

            else
                ( [], 0, 0 )
    in
    ( [ instructions
      , viewAdministeredMedicationQuestion language (Translate.MedicationDistributionSign Azithromycin)
      , viewBoolInput
            language
            form.azithromycin
            (setMedicationDistributionBoolInputMsg updateFunc)
            "azithromycin-medication"
            Nothing
      ]
        ++ derivedInput
    , taskCompleted form.azithromycin + derivedTaskCompleted
    , 1 + derivedTaskActive
    )


resolveMetronidazoleDistributionInputsAndTasks :
    Language
    -> NominalDate
    -> Person
    -> ((Bool -> MedicationDistributionForm -> MedicationDistributionForm) -> Bool -> msg)
    -> (Maybe AdministrationNote -> MedicationDistributionSign -> AdministrationNote -> msg)
    -> MedicationDistributionForm
    -> ( List (Html msg), Int, Int )
resolveMetronidazoleDistributionInputsAndTasks language currentDate person setMedicationDistributionBoolInputMsg setMedicationDistributionAdministrationNoteMsg form =
    let
        instructions =
            resolveMetronidazoleDosageAndIcon currentDate person
                |> Maybe.map
                    (\( dosage, icon ) ->
                        div [ class "instructions" ]
                            [ viewAdministeredMedicationCustomLabel language Translate.Administer (Translate.MedicationDistributionSign Metronidazole) ("(" ++ dosage ++ ")") icon "" Nothing
                            , div [ class "prescription" ] [ text <| translate language Translate.AdministerMetronidazoleHelper ++ "." ]
                            ]
                    )
                |> Maybe.withDefault emptyNode

        updateFunc value form_ =
            { form_ | metronidazole = Just value, nonAdministrationSigns = updateNonAdministrationSigns Metronidazole MedicationMetronidazole value form_ }

        ( derivedInput, derivedTaskCompleted, derivedTaskActive ) =
            if form.metronidazole == Just False then
                ( viewMedicationDistributionDerivedQuestion language Metronidazole MedicationMetronidazole setMedicationDistributionAdministrationNoteMsg form
                , taskCompleted <|
                    getCurrentReasonForMedicationNonAdministration MedicationMetronidazole form
                , 1
                )

            else
                ( [], 0, 0 )
    in
    ( [ instructions
      , viewAdministeredMedicationQuestion language (Translate.MedicationDistributionSign Metronidazole)
      , viewBoolInput
            language
            form.metronidazole
            (setMedicationDistributionBoolInputMsg updateFunc)
            "metronidazole-medication"
            Nothing
      ]
        ++ derivedInput
    , taskCompleted form.metronidazole + derivedTaskCompleted
    , 1 + derivedTaskActive
    )


resolveVitaminADistributionInputsAndTasks :
    Language
    -> NominalDate
    -> Person
    -> ((Bool -> MedicationDistributionForm -> MedicationDistributionForm) -> Bool -> msg)
    -> (Maybe AdministrationNote -> MedicationDistributionSign -> AdministrationNote -> msg)
    -> MedicationDistributionForm
    -> ( List (Html msg), Int, Int )
resolveVitaminADistributionInputsAndTasks language currentDate person setMedicationDistributionBoolInputMsg setMedicationDistributionAdministrationNoteMsg form =
    let
        instructions =
            resolveVitaminADosageAndIcon currentDate person
                |> Maybe.map
                    (\( dosage, icon ) ->
                        div [ class "instructions" ]
                            [ viewAdministeredMedicationCustomLabel language Translate.Administer (Translate.MedicationDistributionSign VitaminA) (" (" ++ dosage ++ ")") icon "" Nothing
                            , div [ class "prescription" ] [ text <| translate language Translate.AdministerVitaminAHelperPrenatal ++ "." ]
                            ]
                    )
                |> Maybe.withDefault emptyNode

        updateFunc value form_ =
            { form_ | vitaminA = Just value, nonAdministrationSigns = updateNonAdministrationSigns VitaminA MedicationVitaminA value form_ }

        ( derivedInput, derivedTaskCompleted, derivedTaskActive ) =
            if form.vitaminA == Just False then
                ( viewMedicationDistributionDerivedQuestion language VitaminA MedicationVitaminA setMedicationDistributionAdministrationNoteMsg form
                , taskCompleted <|
                    getCurrentReasonForMedicationNonAdministration MedicationVitaminA form
                , 1
                )

            else
                ( [], 0, 0 )
    in
    ( [ instructions
      , viewAdministeredMedicationQuestion language (Translate.MedicationDistributionSign VitaminA)
      , viewBoolInput
            language
            form.vitaminA
            (setMedicationDistributionBoolInputMsg updateFunc)
            "vitamin-a-medication"
            Nothing
      ]
        ++ derivedInput
    , taskCompleted form.vitaminA + derivedTaskCompleted
    , 1 + derivedTaskActive
    )


resolveParacetamolDistributionInputsAndTasks :
    Language
    -> NominalDate
    -> Person
    -> ((Bool -> MedicationDistributionForm -> MedicationDistributionForm) -> Bool -> msg)
    -> (Maybe AdministrationNote -> MedicationDistributionSign -> AdministrationNote -> msg)
    -> MedicationDistributionForm
    -> ( List (Html msg), Int, Int )
resolveParacetamolDistributionInputsAndTasks language currentDate person setMedicationDistributionBoolInputMsg setMedicationDistributionAdministrationNoteMsg form =
    let
        instructions =
            resolveParacetamolDosageAndIcon currentDate person
                |> Maybe.map
                    (\( dosage, icon ) ->
                        div [ class "instructions" ]
                            [ viewAdministeredMedicationCustomLabel language Translate.Administer (Translate.MedicationDistributionSign Paracetamol) (" (" ++ dosage ++ ")") icon "" Nothing
                            , div [ class "prescription" ] [ text <| translate language Translate.AdministerParacetamolHelper ++ "." ]
                            ]
                    )
                |> Maybe.withDefault emptyNode

        updateFunc value form_ =
            { form_ | paracetamol = Just value, nonAdministrationSigns = updateNonAdministrationSigns Paracetamol MedicationParacetamol value form_ }

        ( derivedInput, derivedTaskCompleted, derivedTaskActive ) =
            if form.paracetamol == Just False then
                ( viewMedicationDistributionDerivedQuestion language Paracetamol MedicationParacetamol setMedicationDistributionAdministrationNoteMsg form
                , taskCompleted <|
                    getCurrentReasonForMedicationNonAdministration MedicationParacetamol form
                , 1
                )

            else
                ( [], 0, 0 )
    in
    ( [ instructions
      , viewAdministeredMedicationQuestion language (Translate.MedicationDistributionSign Paracetamol)
      , viewBoolInput
            language
            form.paracetamol
            (setMedicationDistributionBoolInputMsg updateFunc)
            "paracetamol-medication"
            Nothing
      ]
        ++ derivedInput
    , taskCompleted form.paracetamol + derivedTaskCompleted
    , 1 + derivedTaskActive
    )


viewMedicationDistributionDerivedQuestion :
    Language
    -> MedicationDistributionSign
    -> (AdministrationNote -> MedicationNonAdministrationSign)
    -> (Maybe AdministrationNote -> MedicationDistributionSign -> AdministrationNote -> msg)
    -> MedicationDistributionForm
    -> List (Html msg)
viewMedicationDistributionDerivedQuestion language medication reasonToSignFunc setMedicationDistributionAdministrationNoteMsg form =
    let
        currentValue =
            getCurrentReasonForMedicationNonAdministration reasonToSignFunc form
    in
    [ viewQuestionLabel language Translate.WhyNot
    , viewCheckBoxSelectInput language
        [ NonAdministrationLackOfStock, NonAdministrationKnownAllergy, NonAdministrationPatientUnableToAfford ]
        [ NonAdministrationPatientDeclined, NonAdministrationOther ]
        currentValue
        (setMedicationDistributionAdministrationNoteMsg currentValue medication)
        Translate.AdministrationNote
    ]


{-| When the answer for medication administration is Yes,
we clean the reason for not administering the medication.
-}
updateNonAdministrationSigns :
    MedicationDistributionSign
    -> (AdministrationNote -> MedicationNonAdministrationSign)
    -> Bool
    -> MedicationDistributionForm
    -> Maybe (EverySet MedicationNonAdministrationSign)
updateNonAdministrationSigns medication reasonToSignFunc value form =
    if value == True then
        form.nonAdministrationSigns
            |> Maybe.andThen
                (\nonAdministrationSigns ->
                    getCurrentReasonForMedicationNonAdministration reasonToSignFunc form
                        |> Maybe.map
                            (\reason ->
                                Just <| EverySet.remove (nonAdministrationReasonToSign medication reason) nonAdministrationSigns
                            )
                        |> Maybe.withDefault (Just nonAdministrationSigns)
                )

    else
        form.nonAdministrationSigns


resolveMebendezoleDosageAndIcon : NominalDate -> Person -> Maybe ( String, String )
resolveMebendezoleDosageAndIcon currentDate person =
    Just ( "500 mg", "icon-pills" )


resolveTenofovirDosageAndIcon : NominalDate -> Person -> Maybe ( String, String )
resolveTenofovirDosageAndIcon currentDate person =
    Just ( "300 mg", "icon-pills" )


resolveLamivudineDosageAndIcon : NominalDate -> Person -> Maybe ( String, String )
resolveLamivudineDosageAndIcon currentDate person =
    Just ( "300 mg", "icon-pills" )


resolveDolutegravirDosageAndIcon : NominalDate -> Person -> Maybe ( String, String )
resolveDolutegravirDosageAndIcon currentDate person =
    Just ( "50 mg", "icon-pills" )


resolveIronDosageAndIcon : NominalDate -> Person -> Maybe ( String, String )
resolveIronDosageAndIcon currentDate person =
    Just ( "120 mg", "icon-pills" )


resolveFolicAcidDosageAndIcon : NominalDate -> Person -> Maybe ( String, String )
resolveFolicAcidDosageAndIcon currentDate person =
    Just ( "400 IU", "icon-pills" )


resolveCeftriaxoneDosageAndIcon : NominalDate -> Person -> Maybe ( String, String )
resolveCeftriaxoneDosageAndIcon currentDate person =
    Just ( "250 mg", "icon-pills" )


resolveAzithromycinDosageAndIcon : NominalDate -> Person -> Maybe ( String, String )
resolveAzithromycinDosageAndIcon currentDate person =
    Just ( "1 g", "icon-pills" )


resolveMetronidazoleDosageAndIcon : NominalDate -> Person -> Maybe ( String, String )
resolveMetronidazoleDosageAndIcon currentDate person =
    Just ( "500 mg", "icon-pills" )


resolveVitaminADosageAndIcon : NominalDate -> Person -> Maybe ( String, String )
resolveVitaminADosageAndIcon currentDate person =
    Just ( "200,000 IU", "icon-pills" )


resolveParacetamolDosageAndIcon : NominalDate -> Person -> Maybe ( String, String )
resolveParacetamolDosageAndIcon currentDate person =
    Just ( "500 mg", "icon-pills" )


medicationsInitialPhase : List MedicationDistributionSign
medicationsInitialPhase =
    [ Mebendezole
    , TDF3TC
    , Dolutegravir
    , Ceftriaxone
    , Azithromycin
    , Metronidazole
    , VitaminA
    , Paracetamol
    ]


medicationsRecurrentPhase : List MedicationDistributionSign
medicationsRecurrentPhase =
    [ Iron
    , FolicAcid
    ]


hivProgramAtHC : PrenatalMeasurements -> Bool
hivProgramAtHC measurements =
    getMeasurementValueFunc measurements.hivTest
        |> Maybe.andThen .hivSigns
        |> Maybe.map (EverySet.member HIVProgramHC)
        |> Maybe.withDefault False


{-| Recommended Treatment activity appears on both initial and recurrent encounters.
Each one of them got unique set of signs that can be used, and at least one of
them must be set.
In order to know if activity was completed or not, we check if at least one
of those signs was set.
-}
recommendedTreatmentMeasurementTaken : List RecommendedTreatmentSign -> PrenatalMeasurements -> Bool
recommendedTreatmentMeasurementTaken allowedSigns measurements =
    getMeasurementValueFunc measurements.medicationDistribution
        |> Maybe.andThen .recommendedTreatmentSigns
        |> Maybe.map (Backend.Measurement.Utils.recommendedTreatmentMeasurementTaken allowedSigns)
        |> Maybe.withDefault False


resolveRecommendedTreatmentSectionState : Bool -> List RecommendedTreatmentSign -> Maybe (List RecommendedTreatmentSign) -> ( Int, Int )
resolveRecommendedTreatmentSectionState isDiagnosed allowedSigns currentSigns =
    if isDiagnosed then
        Maybe.map
            (\signs ->
                let
                    completed =
                        -- We know that section is completed when one of allowed
                        -- signs is set (as only single selection is allowed).
                        List.any (\sign -> List.member sign signs) allowedSigns
                in
                if completed then
                    ( 1, 1 )

                else
                    ( 0, 1 )
            )
            currentSigns
            |> Maybe.withDefault ( 0, 1 )

    else
        ( 0, 0 )


recommendedTreatmentSignsForHypertensionInitial : List RecommendedTreatmentSign
recommendedTreatmentSignsForHypertensionInitial =
    [ TreatmentMethyldopa2
    , TreatmentMethyldopa3
    , TreatmentMethyldopa4
    , NoTreatmentForHypertension
    ]


{-| Note: Even though name says Hypertension, it includes Moderate Preeclamsia as well.
-}
recommendedTreatmentSignsForHypertension : List RecommendedTreatmentSign
recommendedTreatmentSignsForHypertension =
    [ TreatmentMethyldopa2
    , TreatmentMethyldopa3
    , TreatmentMethyldopa4
    , TreatmentHypertensionAddCarvedilol
    , TreatmentHypertensionAddAmlodipine
    , NoTreatmentForHypertension
    ]


marginalBloodPressureCondition : Float -> Float -> Bool
marginalBloodPressureCondition dia sys =
    (dia >= 90 && dia < 110) || (sys >= 140 && sys < 160)


diagnosedHypertension : PrenatalEncounterPhase -> AssembledData -> Bool
diagnosedHypertension phase =
    case phase of
        PrenatalEncounterPhaseInitial ->
            diagnosedAnyOf
                [ DiagnosisChronicHypertensionImmediate
                , DiagnosisGestationalHypertensionImmediate
                ]

        PrenatalEncounterPhaseRecurrent ->
            diagnosedAnyOf
                [ DiagnosisChronicHypertensionAfterRecheck
                , DiagnosisGestationalHypertensionAfterRecheck
                ]


diagnosedHypertensionPrevoiusly : AssembledData -> Bool
diagnosedHypertensionPrevoiusly assembled =
    diagnosedPreviouslyAnyOf hypertensionDiagnoses assembled


resolvePreviousHypertensionDiagnosis : List PreviousEncounterData -> Maybe PrenatalDiagnosis
resolvePreviousHypertensionDiagnosis nursePreviousEncountersData =
    List.filterMap
        (\data ->
            EverySet.toList data.diagnoses
                |> List.filter
                    (\diagnosis ->
                        List.member diagnosis hypertensionDiagnoses
                            || List.member diagnosis moderatePreeclampsiaDiagnoses
                    )
                |> Just
        )
        nursePreviousEncountersData
        |> List.concat
        |> List.map hierarchalHypertensionlikeDiagnosisToNumber
        |> Maybe.Extra.values
        |> List.maximum
        |> Maybe.andThen hierarchalHypertensionlikeDiagnosisFromNumber


hypertensionDiagnoses : List PrenatalDiagnosis
hypertensionDiagnoses =
    [ DiagnosisChronicHypertensionImmediate
    , DiagnosisGestationalHypertensionImmediate
    , DiagnosisChronicHypertensionAfterRecheck
    , DiagnosisGestationalHypertensionAfterRecheck
    ]


diagnosedModeratePreeclampsiaPrevoiusly : AssembledData -> Bool
diagnosedModeratePreeclampsiaPrevoiusly assembled =
    diagnosedPreviouslyAnyOf moderatePreeclampsiaDiagnoses assembled


moderatePreeclampsiaDiagnoses : List PrenatalDiagnosis
moderatePreeclampsiaDiagnoses =
    [ DiagnosisModeratePreeclampsiaInitialPhase
    , DiagnosisModeratePreeclampsiaRecurrentPhase
    , DiagnosisModeratePreeclampsiaInitialPhaseEGA37Plus
    , DiagnosisModeratePreeclampsiaRecurrentPhaseEGA37Plus
    ]


resolveARVReferralDiagnosis : List PreviousEncounterData -> Maybe PrenatalDiagnosis
resolveARVReferralDiagnosis nursePreviousEncountersData =
    List.filterMap
        (\data ->
            if EverySet.member DiagnosisHIV data.diagnoses || knownAsHIVPositive data.measurements then
                Just DiagnosisHIV

            else if EverySet.member DiagnosisDiscordantPartnership data.diagnoses then
                Just DiagnosisDiscordantPartnership

            else
                Nothing
        )
        nursePreviousEncountersData
        |> List.head


knownAsHIVPositive : PrenatalMeasurements -> Bool
knownAsHIVPositive measurements =
    getMeasurementValueFunc measurements.hivTest
        |> Maybe.map (.executionNote >> (==) TestNoteKnownAsPositive)
        |> Maybe.withDefault False


resolveNCDReferralDiagnoses : List PreviousEncounterData -> List PrenatalDiagnosis
resolveNCDReferralDiagnoses nursePreviousEncountersData =
    Maybe.Extra.values
        [ resolvePreviousHypertensionlikeDiagnosis nursePreviousEncountersData
        , resolvePreviousDiabetesDiagnosis nursePreviousEncountersData
        ]


resolvePreviousHypertensionlikeDiagnosis : List PreviousEncounterData -> Maybe PrenatalDiagnosis
resolvePreviousHypertensionlikeDiagnosis nursePreviousEncountersData =
    List.filterMap
        (\data ->
            EverySet.toList data.diagnoses
                |> List.filter
                    (\diagnosis ->
                        List.member diagnosis hypertensionlikeDiagnoses
                    )
                |> Just
        )
        nursePreviousEncountersData
        |> List.concat
        |> List.map hierarchalHypertensionlikeDiagnosisToNumber
        |> Maybe.Extra.values
        |> List.maximum
        |> Maybe.andThen hierarchalHypertensionlikeDiagnosisFromNumber


hypertensionlikeDiagnoses : List PrenatalDiagnosis
hypertensionlikeDiagnoses =
    hypertensionDiagnoses
        ++ moderatePreeclampsiaDiagnoses
        ++ [ DiagnosisSeverePreeclampsiaInitialPhase
           , DiagnosisSeverePreeclampsiaInitialPhaseEGA37Plus
           , DiagnosisSeverePreeclampsiaRecurrentPhase
           , DiagnosisSeverePreeclampsiaRecurrentPhaseEGA37Plus
           , DiagnosisEclampsia
           ]


resolvePreviousDiabetesDiagnosis : List PreviousEncounterData -> Maybe PrenatalDiagnosis
resolvePreviousDiabetesDiagnosis nursePreviousEncountersData =
    List.filterMap
        (\data ->
            EverySet.toList data.diagnoses
                |> List.filter
                    (\diagnosis ->
                        List.member diagnosis diabetesDiagnoses
                    )
                |> Just
        )
        nursePreviousEncountersData
        |> List.concat
        |> List.head


diagnosedMalaria : AssembledData -> Bool
diagnosedMalaria =
    diagnosedAnyOf
        [ DiagnosisMalaria
        , DiagnosisMalariaWithAnemia
        , DiagnosisMalariaWithSevereAnemia
        ]


diagnosedSyphilis : AssembledData -> Bool
diagnosedSyphilis =
    diagnosedAnyOf
        [ DiagnosisSyphilis
        , DiagnosisSyphilisWithComplications
        ]


syphilisDiagnosesIncludingNeurosyphilis : List PrenatalDiagnosis
syphilisDiagnosesIncludingNeurosyphilis =
    DiagnosisNeurosyphilis :: syphilisDiagnoses


syphilisDiagnoses : List PrenatalDiagnosis
syphilisDiagnoses =
    [ DiagnosisSyphilis, DiagnosisSyphilisWithComplications ]


diabetesDiagnoses : List PrenatalDiagnosis
diabetesDiagnoses =
    [ DiagnosisDiabetes, DiagnosisGestationalDiabetes ]


outsideCareDiagnoses : List PrenatalDiagnosis
outsideCareDiagnoses =
    DiagnosisOther :: outsideCareDiagnosesLeftColumn ++ outsideCareDiagnosesRightColumn


outsideCareDiagnosesLeftColumn : List PrenatalDiagnosis
outsideCareDiagnosesLeftColumn =
    [ DiagnosisHIV
    , DiagnosisSyphilis
    , DiagnosisNeurosyphilis
    , DiagnosisMalaria
    , DiagnosisHepatitisB
    , DiagnosisModerateAnemia
    , DiagnosisSevereAnemia
    , DiagnosisPelvicPainIntense
    , Backend.PrenatalEncounter.Types.DiagnosisTuberculosis
    ]


outsideCareDiagnosesRightColumn : List PrenatalDiagnosis
outsideCareDiagnosesRightColumn =
    [ DiagnosisChronicHypertensionImmediate
    , DiagnosisGestationalHypertensionImmediate
    , DiagnosisModeratePreeclampsiaInitialPhase
    , DiagnosisDeepVeinThrombosis
    , DiagnosisPyelonephritis
    , DiagnosisHeartburnPersistent
    , DiagnosisPlacentaPrevia
    , DiagnosisHyperemesisGravidum
    ]


outsideCareDiagnosesWithPossibleMedication : List PrenatalDiagnosis
outsideCareDiagnosesWithPossibleMedication =
    [ DiagnosisHIV
    , DiagnosisSyphilis
    , DiagnosisMalaria
    , DiagnosisModerateAnemia
    , DiagnosisGestationalHypertensionImmediate
    , DiagnosisChronicHypertensionImmediate
    , DiagnosisModeratePreeclampsiaInitialPhase
    ]


generateVaccinationProgress : List PrenatalMeasurements -> VaccinationProgressDict
generateVaccinationProgress measurements =
    let
        tetanusImmunisations =
            List.filterMap (.tetanusImmunisation >> getMeasurementValueFunc)
                measurements
    in
    [ ( VaccineTetanus, generateVaccinationProgressForVaccine tetanusImmunisations )
    ]
        |> Dict.fromList


referralFormWithDefault : ReferralForm -> Maybe PrenatalReferralValue -> ReferralForm
referralFormWithDefault form saved =
    saved
        |> unwrap
            form
            (\value ->
                let
                    resolveFromHCSignsValue sign =
                        Maybe.map (EverySet.member sign) value.sendToHCSigns

                    resolveFromFacilitySignsValue sign =
                        Maybe.map (EverySet.member sign) value.referToFacilitySigns
                in
                { handReferralForm = or form.handReferralForm (resolveFromHCSignsValue HandReferrerForm)
                , referToHealthCenter = or form.referToHealthCenter (resolveFromHCSignsValue ReferToHealthCenter)
                , accompanyToHealthCenter = or form.accompanyToHealthCenter (resolveFromHCSignsValue PrenatalAccompanyToHC)
                , reasonForNotSendingToHC = or form.reasonForNotSendingToHC value.reasonForNotSendingToHC
                , referToHospital = or form.referToHospital (resolveFromFacilitySignsValue ReferToHospital)
                , referralFormHospital = or form.referralFormHospital (resolveFromFacilitySignsValue ReferralFormHospital)
                , referToMentalHealthSpecialist = or form.referToMentalHealthSpecialist (resolveFromFacilitySignsValue ReferToMentalHealthSpecialist)
                , referralFormMentalHealthSpecialist = or form.referralFormMentalHealthSpecialist (resolveFromFacilitySignsValue ReferralFormMentalHealthSpecialist)
                , accompanyToMentalHealthSpecialist = or form.accompanyToMentalHealthSpecialist (resolveFromFacilitySignsValue AccompanyToMentalHealthSpecialist)
                , referToARVProgram = or form.referToARVProgram (resolveFromFacilitySignsValue ReferToARVProgram)
                , referralFormARVProgram = or form.referralFormARVProgram (resolveFromFacilitySignsValue ReferralFormARVProgram)
                , accompanyToARVProgram = or form.accompanyToARVProgram (resolveFromFacilitySignsValue AccompanyToARVProgram)
                , referToNCDProgram = or form.referToNCDProgram (resolveFromFacilitySignsValue ReferToNCDProgram)
                , referralFormNCDProgram = or form.referralFormNCDProgram (resolveFromFacilitySignsValue ReferralFormNCDProgram)
                , accompanyToNCDProgram = or form.accompanyToNCDProgram (resolveFromFacilitySignsValue AccompanyToNCDProgram)
                , referToUltrasound = or form.referToUltrasound (resolveFromFacilitySignsValue ReferToUltrasound)
                , referralFormUltrasound = or form.referralFormUltrasound (resolveFromFacilitySignsValue ReferralFormUltrasound)
                , facilityNonReferralReasons = or form.facilityNonReferralReasons value.facilityNonReferralReasons
                }
            )


toPrenatalReferralValueWithDefault : Maybe PrenatalReferralValue -> ReferralForm -> Maybe PrenatalReferralValue
toPrenatalReferralValueWithDefault saved form =
    referralFormWithDefault form saved
        |> toPrenatalReferralValue


toPrenatalReferralValue : ReferralForm -> Maybe PrenatalReferralValue
toPrenatalReferralValue form =
    let
        sendToHCSignsByForm =
            [ ifNullableTrue HandReferrerForm form.handReferralForm
            , ifNullableTrue ReferToHealthCenter form.referToHealthCenter
            , ifNullableTrue PrenatalAccompanyToHC form.accompanyToHealthCenter
            ]
                |> Maybe.Extra.combine
                |> Maybe.map (List.foldl EverySet.union EverySet.empty)

        -- We need to handle situation when patient is not referred to HC for a reason -
        -- no signs are set, but reason is set => update signs to 'none' value.
        sendToHCSigns =
            if sendToHCSignsByForm == Just EverySet.empty && isJust form.reasonForNotSendingToHC then
                Just (EverySet.singleton NoSendToHCSigns)

            else
                sendToHCSignsByForm

        -- We need to handle situation when patient is not referred to any
        -- facility (there are multiple facilities), and we have reason set for
        -- at least one facility.
        -- In this case, we update signs to 'none' value.
        referToFacilitySignsByForm =
            [ ifNullableTrue ReferToHospital form.referToHospital
            , ifNullableTrue ReferralFormHospital form.referralFormHospital
            , ifNullableTrue ReferToMentalHealthSpecialist form.referToMentalHealthSpecialist
            , ifNullableTrue ReferralFormMentalHealthSpecialist form.referralFormMentalHealthSpecialist
            , ifNullableTrue AccompanyToMentalHealthSpecialist form.accompanyToMentalHealthSpecialist
            , ifNullableTrue ReferToARVProgram form.referToARVProgram
            , ifNullableTrue ReferralFormARVProgram form.referralFormARVProgram
            , ifNullableTrue AccompanyToARVProgram form.accompanyToARVProgram
            , ifNullableTrue ReferToNCDProgram form.referToNCDProgram
            , ifNullableTrue ReferralFormNCDProgram form.referralFormNCDProgram
            , ifNullableTrue AccompanyToNCDProgram form.accompanyToNCDProgram
            , ifNullableTrue ReferToUltrasound form.referToUltrasound
            , ifNullableTrue ReferralFormUltrasound form.referralFormUltrasound
            ]
                |> Maybe.Extra.combine
                |> Maybe.map (List.foldl EverySet.union EverySet.empty)

        referToFacilitySigns =
            if referToFacilitySignsByForm == Just EverySet.empty && isSet form.facilityNonReferralReasons then
                Just (EverySet.singleton NoReferToFacilitySigns)

            else
                referToFacilitySignsByForm

        isSet maybeSet =
            Maybe.map (\set -> EverySet.size set > 0) maybeSet
                |> Maybe.withDefault False
    in
    if isSet sendToHCSigns || isSet referToFacilitySigns then
        Just
            { sendToHCSigns = sendToHCSigns
            , reasonForNotSendingToHC = form.reasonForNotSendingToHC
            , referToFacilitySigns = referToFacilitySigns
            , facilityNonReferralReasons = form.facilityNonReferralReasons
            }

    else
        Nothing


resolveReferralToFacilityInputsAndTasks :
    Language
    -> NominalDate
    -> PrenatalEncounterPhase
    -> AssembledData
    -> ((Bool -> ReferralForm -> ReferralForm) -> Bool -> msg)
    -> (Maybe ReasonForNonReferral -> ReferralFacility -> ReasonForNonReferral -> msg)
    -> ReferralForm
    -> ReferralFacility
    -> ( List (Html msg), List (Maybe Bool) )
resolveReferralToFacilityInputsAndTasks language currentDate phase assembled setReferralBoolInputMsg setNonReferralReasonMsg form facility =
    let
        maybeConfig =
            case facility of
                FacilityHospital ->
                    let
                        referralReasons =
                            diagnosesCausingHospitalReferralByPhase phase assembled
                                |> EverySet.toList

                        referralContext =
                            if not <| List.isEmpty referralReasons then
                                let
                                    diagnosisTransId diagnosis =
                                        if diagnosis == DiagnosisChronicHypertensionImmediate then
                                            Translate.Hypertension

                                        else
                                            Translate.PrenatalDiagnosis diagnosis

                                    reasons =
                                        List.map (diagnosisTransId >> translate language) referralReasons
                                            |> String.join ", "
                                in
                                div [ class "label" ] [ text <| translate language Translate.PatientDiagnosedWithLabel ++ ": " ++ reasons ++ "." ]

                            else
                                emptyNode
                    in
                    Just
                        { header =
                            [ referralContext
                            , viewCustomLabel language Translate.HighRiskCaseHelper "." "instructions"
                            ]
                        , referralField = form.referToHospital
                        , referralUpdateFunc =
                            \value form_ ->
                                { form_
                                    | referToHospital = Just value
                                    , referralFormHospital = Nothing
                                }
                        , formField = form.referralFormHospital
                        , formUpdateFunc = \value form_ -> { form_ | referralFormHospital = Just value }
                        , accompanyConfig = Nothing
                        , reasonToSignFunc = NonReferralReasonHospital
                        }

                FacilityMentalHealthSpecialist ->
                    Just
                        { header = [ viewCustomLabel language Translate.PrenatalMentalHealthSpecialistHelper "." "instructions" ]
                        , referralField = form.referToMentalHealthSpecialist
                        , referralUpdateFunc =
                            \value form_ ->
                                { form_
                                    | referToMentalHealthSpecialist = Just value
                                    , referralFormMentalHealthSpecialist = Nothing
                                    , accompanyToMentalHealthSpecialist = Nothing
                                }
                        , formField = form.referralFormMentalHealthSpecialist
                        , formUpdateFunc = \value form_ -> { form_ | referralFormMentalHealthSpecialist = Just value }
                        , accompanyConfig =
                            Just
                                ( form.accompanyToMentalHealthSpecialist
                                , \value form_ -> { form_ | accompanyToMentalHealthSpecialist = Just value }
                                )
                        , reasonToSignFunc = NonReferralReasonMentalHealthSpecialist
                        }

                FacilityARVProgram ->
                    Just
                        { header =
                            let
                                forPostpartum =
                                    assembled.encounter.encounterType == NursePostpartumEncounter
                            in
                            if forPostpartum then
                                let
                                    header =
                                        resolveARVReferralDiagnosis assembled.nursePreviousEncountersData
                                            |> Maybe.map
                                                (\diagnosis ->
                                                    div [ class "label header" ]
                                                        [ text <| translate language Translate.PrenatalNCDProgramHeaderPrefix
                                                        , text " "
                                                        , text <| translate language <| Translate.PrenatalDiagnosis diagnosis
                                                        , text " "
                                                        , text <| translate language Translate.PrenatalNCDProgramHeaderSuffix
                                                        , text "."
                                                        ]
                                                )
                                            |> Maybe.withDefault emptyNode
                                in
                                [ header
                                , viewCustomLabel language (Translate.PrenatalARVProgramInstructions forPostpartum) "." "instructions"
                                ]

                            else
                                [ viewCustomLabel language (Translate.PrenatalARVProgramInstructions forPostpartum) "." "instructions"
                                ]
                        , referralField = form.referToARVProgram
                        , referralUpdateFunc =
                            \value form_ ->
                                { form_
                                    | referToARVProgram = Just value
                                    , referralFormARVProgram = Nothing
                                    , accompanyToARVProgram = Nothing
                                }
                        , formField = form.referralFormARVProgram
                        , formUpdateFunc = \value form_ -> { form_ | referralFormARVProgram = Just value }
                        , accompanyConfig =
                            Just
                                ( form.accompanyToARVProgram
                                , \value form_ -> { form_ | accompanyToARVProgram = Just value }
                                )
                        , reasonToSignFunc = NonReferralReasonARVProgram
                        }

                FacilityNCDProgram ->
                    Just
                        { header =
                            let
                                headerText =
                                    translate language Translate.PrenatalNCDProgramHeaderPrefix
                                        ++ " "
                                        ++ diagnosesForView
                                        ++ " "
                                        ++ translate language Translate.PrenatalNCDProgramHeaderSuffix
                                        ++ "."

                                diagnosesForView =
                                    resolveNCDReferralDiagnoses assembled.nursePreviousEncountersData
                                        |> List.map (Translate.PrenatalDiagnosis >> translate language)
                                        |> String.join ", "
                            in
                            [ div [ class "label" ] [ text headerText ]
                            , viewCustomLabel language Translate.PrenatalNCDProgramInstructions "." "instructions"
                            ]
                        , referralField = form.referToNCDProgram
                        , referralUpdateFunc =
                            \value form_ ->
                                { form_
                                    | referToNCDProgram = Just value
                                    , referralFormNCDProgram = Nothing
                                    , accompanyToNCDProgram = Nothing
                                }
                        , formField = form.referralFormNCDProgram
                        , formUpdateFunc = \value form_ -> { form_ | referralFormNCDProgram = Just value }
                        , accompanyConfig =
                            Just
                                ( form.accompanyToNCDProgram
                                , \value form_ -> { form_ | accompanyToNCDProgram = Just value }
                                )
                        , reasonToSignFunc = NonReferralReasonNCDProgram
                        }

                FacilityANCServices ->
                    -- Not in use at Prenatal
                    Nothing

                FacilityUltrasound ->
                    Just
                        { header =
                            [ div [ class "label" ] [ text <| translate language Translate.PrenatalUltrasoundHeader ++ "." ]
                            , viewCustomLabel language Translate.PrenatalUltrasoundInstructions "." "instructions"
                            ]
                        , referralField = form.referToUltrasound
                        , referralUpdateFunc =
                            \value form_ ->
                                { form_
                                    | referToUltrasound = Just value
                                    , referralFormUltrasound = Nothing
                                }
                        , formField = form.referralFormUltrasound
                        , formUpdateFunc = \value form_ -> { form_ | referralFormUltrasound = Just value }
                        , accompanyConfig = Nothing
                        , reasonToSignFunc = NonReferralReasonUltrasound
                        }

                FacilityHealthCenter ->
                    -- Referral to HC is not supported here as it
                    -- got special treatment.
                    Nothing
    in
    Maybe.map
        (\config ->
            let
                instructions =
                    case facility of
                        FacilityMentalHealthSpecialist ->
                            [ viewActionTakenLabel language (Translate.CompleteFacilityReferralForm facility) "icon-forms" Nothing ]

                        _ ->
                            [ viewActionTakenLabel language (Translate.CompleteFacilityReferralForm facility) "icon-forms" Nothing
                            , viewActionTakenLabel language (Translate.SendPatientToFacility facility) "icon-shuttle" Nothing
                            ]

                ( derivedSection, derivedTasks ) =
                    Maybe.map
                        (\referred ->
                            if referred then
                                let
                                    ( accompanySection, accompanyTasks ) =
                                        Maybe.map
                                            (\( field, updateFunc ) ->
                                                ( [ viewQuestionLabel language <| Translate.AccompanyToFacilityQuestion facility
                                                  , viewBoolInput
                                                        language
                                                        field
                                                        (setReferralBoolInputMsg updateFunc)
                                                        "accompany-to-hc"
                                                        Nothing
                                                  ]
                                                , [ field ]
                                                )
                                            )
                                            config.accompanyConfig
                                            |> Maybe.withDefault ( [], [] )
                                in
                                ( [ viewQuestionLabel language Translate.HandedReferralFormQuestion
                                  , viewBoolInput
                                        language
                                        config.formField
                                        (setReferralBoolInputMsg config.formUpdateFunc)
                                        "hand-referral-form"
                                        Nothing
                                  ]
                                    ++ accompanySection
                                , [ config.formField ] ++ accompanyTasks
                                )

                            else
                                ( nonReferralReasonSection language facility config.reasonToSignFunc setNonReferralReasonMsg form
                                , [ maybeToBoolTask <| getCurrentReasonForNonReferralByForm config.reasonToSignFunc form ]
                                )
                        )
                        config.referralField
                        |> Maybe.withDefault ( [], [] )
            in
            ( config.header
                ++ [ h2 [] [ text <| translate language Translate.ActionsToTake ++ ":" ]
                   , div [ class "instructions" ]
                        instructions
                   , viewQuestionLabel language <| Translate.ReferredPatientToFacilityQuestion facility
                   , viewBoolInput
                        language
                        config.referralField
                        (setReferralBoolInputMsg config.referralUpdateFunc)
                        "referral"
                        Nothing
                   ]
                ++ derivedSection
                ++ [ div [ class "separator" ] [] ]
            , [ config.referralField ] ++ derivedTasks
            )
        )
        maybeConfig
        |> Maybe.withDefault ( [], [] )


nonReferralReasonSection :
    Language
    -> ReferralFacility
    -> (ReasonForNonReferral -> NonReferralSign)
    -> (Maybe ReasonForNonReferral -> ReferralFacility -> ReasonForNonReferral -> msg)
    -> ReferralForm
    -> List (Html msg)
nonReferralReasonSection language facility reasonToSignFunc setNonReferralReasonMsg form =
    let
        currentValue =
            getCurrentReasonForNonReferralByForm reasonToSignFunc form

        options =
            if facility == FacilityHospital then
                [ ClientRefused
                , NoAmbulance
                , ClientUnableToAffordFees
                , ReasonForNonReferralNotIndicated
                , ReasonForNonReferralOther
                ]

            else
                [ ClientRefused
                , ClientAlreadyInCare
                , ReasonForNonReferralNotIndicated
                , ReasonForNonReferralOther
                ]
    in
    [ viewQuestionLabel language Translate.WhyNot
    , viewCheckBoxSelectInput language
        options
        []
        currentValue
        (setNonReferralReasonMsg currentValue facility)
        Translate.ReasonForNonReferral
    ]


getCurrentReasonForNonReferralByForm :
    (ReasonForNonReferral -> NonReferralSign)
    -> ReferralForm
    -> Maybe ReasonForNonReferral
getCurrentReasonForNonReferralByForm reasonToSignFunc form =
    getCurrentReasonForNonReferral reasonToSignFunc form.facilityNonReferralReasons


{-| Referal to facility is completed when we mark that facility was referred to,
or, reason was set for not referring to that facility.
-}
referralToFacilityCompleted : AssembledData -> ReferralFacility -> Bool
referralToFacilityCompleted assembled facility =
    getMeasurementValueFunc assembled.measurements.sendToHC
        |> Maybe.andThen
            (\value ->
                Maybe.map
                    (\referralSigns ->
                        Backend.Measurement.Utils.referralToFacilityCompleted referralSigns value.facilityNonReferralReasons facility
                    )
                    value.referToFacilitySigns
            )
        |> Maybe.withDefault False


undeterminedPostpartumDiagnoses : List PrenatalDiagnosis
undeterminedPostpartumDiagnoses =
    [ DiagnosisPostpartumAbdominalPain
    , DiagnosisPostpartumHeadache
    , DiagnosisPostpartumFatigue
    , DiagnosisPostpartumPerinealPainOrDischarge

    -- Fever is considered undertermined only when Mastitis is not
    -- diagnosed. If it is, Fever diagnosis will be eliminated
    -- by applyGeneralDiagnosesHierarchy.
    , DiagnosisPostpartumFever
    ]


applyDiagnosesHierarchy : EverySet PrenatalDiagnosis -> EverySet PrenatalDiagnosis
applyDiagnosesHierarchy =
    applyHypertensionlikeDiagnosesHierarchy
        >> applyMastitisDiagnosesHierarchy
        >> applyGeneralDiagnosesHierarchy


applyHypertensionlikeDiagnosesHierarchy : EverySet PrenatalDiagnosis -> EverySet PrenatalDiagnosis
applyHypertensionlikeDiagnosesHierarchy diagnoses =
    let
        ( bloodPressureDiagnoses, others ) =
            EverySet.toList diagnoses
                |> List.partition (\diagnosis -> List.member diagnosis hierarchalBloodPressureDiagnoses)

        topBloodPressureDiagnosis =
            List.map hierarchalHypertensionlikeDiagnosisToNumber bloodPressureDiagnoses
                |> Maybe.Extra.values
                |> List.maximum
                |> Maybe.andThen hierarchalHypertensionlikeDiagnosisFromNumber
                |> Maybe.map List.singleton
                |> Maybe.withDefault []
    in
    topBloodPressureDiagnosis
        ++ others
        |> EverySet.fromList


applyMastitisDiagnosesHierarchy : EverySet PrenatalDiagnosis -> EverySet PrenatalDiagnosis
applyMastitisDiagnosesHierarchy diagnoses =
    let
        ( mastitisDiagnoses, others ) =
            EverySet.toList diagnoses
                |> List.partition (\diagnosis -> List.member diagnosis hierarchalMastitisDiagnoses)

        topMastitisDiagnosis =
            List.map hierarchalMastitisDiagnosisToNumber mastitisDiagnoses
                |> Maybe.Extra.values
                |> List.maximum
                |> Maybe.andThen hierarchalMastitisDiagnosisFromNumber
                |> Maybe.map List.singleton
                |> Maybe.withDefault []
    in
    topMastitisDiagnosis
        ++ others
        |> EverySet.fromList


applyGeneralDiagnosesHierarchy : EverySet PrenatalDiagnosis -> EverySet PrenatalDiagnosis
applyGeneralDiagnosesHierarchy diagnoses =
    -- When Mastitis is diagnosed, we eliminate Fever diagnosis, because
    -- fever is one of the symptoms for Mastitis.
    if EverySet.member DiagnosisPostpartumMastitis diagnoses then
        EverySet.remove DiagnosisPostpartumFever diagnoses

    else
        diagnoses


expectMalariaPreventionActivity : PhaseRecorded -> AssembledData -> Bool
expectMalariaPreventionActivity phaseRecorded assembled =
    -- Measurement should be taken at current encounter.
    expectMalariaPreventionActivityByPastEncounters assembled
        && (-- Measurement was taken at current phase.
            -- We need to expect the activity to allow editing.
            getMeasurementValueFunc assembled.measurements.malariaPrevention
                |> Maybe.map (.phaseRecorded >> (==) phaseRecorded)
                |> -- No measurement taken on initial
                   -- phase of current encounter.
                   -- Need to take it current phase.
                   Maybe.withDefault True
           )


expectMalariaPreventionActivityByPastEncounters : AssembledData -> Bool
expectMalariaPreventionActivityByPastEncounters =
    .nursePreviousEncountersData
        >> List.filter
            (.measurements
                >> .malariaPrevention
                >> Maybe.map (Tuple.second >> .value >> .resources >> EverySet.member MosquitoNet)
                >> Maybe.withDefault False
            )
        >> List.isEmpty


malariaPreventionFormWithDefault : MalariaPreventionForm -> Maybe MalariaPreventionValue -> MalariaPreventionForm
malariaPreventionFormWithDefault form saved =
    saved
        |> unwrap
            form
            (\value ->
                { receivedMosquitoNet = or form.receivedMosquitoNet (EverySet.member MosquitoNet value.resources |> Just)
                }
            )


toMalariaPreventionValueWithDefault : PhaseRecorded -> Maybe MalariaPreventionValue -> MalariaPreventionForm -> Maybe MalariaPreventionValue
toMalariaPreventionValueWithDefault phaseRecorded saved form =
    malariaPreventionFormWithDefault form saved
        |> toMalariaPreventionValue phaseRecorded


toMalariaPreventionValue : PhaseRecorded -> MalariaPreventionForm -> Maybe MalariaPreventionValue
toMalariaPreventionValue phaseRecorded form =
    Maybe.map
        (\receivedMosquitoNet ->
            { resources = toEverySet MosquitoNet NoMalariaPreventionSigns receivedMosquitoNet
            , phaseRecorded = phaseRecorded
            }
        )
        form.receivedMosquitoNet