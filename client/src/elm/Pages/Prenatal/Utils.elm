module Pages.Prenatal.Utils exposing (..)

import AssocList as Dict exposing (Dict)
import Backend.Measurement.Model exposing (..)
import Backend.Measurement.Utils exposing (getMeasurementValueFunc)
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
import Measurement.Utils exposing (sendToHCFormWithDefault, vitalsFormWithDefault)
import Pages.AcuteIllness.Activity.Utils exposing (getCurrentReasonForMedicationNonAdministration, nonAdministrationReasonToSign)
import Pages.AcuteIllness.Activity.View exposing (viewAdministeredMedicationCustomLabel, viewAdministeredMedicationLabel, viewAdministeredMedicationQuestion)
import Pages.Prenatal.Model exposing (..)
import Pages.Utils
    exposing
        ( ifEverySetEmpty
        , ifNullableTrue
        , ifTrue
        , maybeValueConsideringIsDirtyField
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
    List.isEmpty assembled.nursePreviousMeasurementsWithDates


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


diagnosedPreviously : PrenatalDiagnosis -> AssembledData -> Bool
diagnosedPreviously diagnosis assembled =
    diagnosedPreviouslyAnyOf [ diagnosis ] assembled


diagnosedPreviouslyAnyOf : List PrenatalDiagnosis -> AssembledData -> Bool
diagnosedPreviouslyAnyOf diagnoses assembled =
    assembled.nursePreviousMeasurementsWithDates
        |> List.filter
            (\( _, encounterDiagnoses, _ ) ->
                List.any (\diagnosis -> EverySet.member diagnosis encounterDiagnoses) diagnoses
            )
        |> List.isEmpty
        |> not


listNonUrgentDiagnoses : List PrenatalDiagnosis -> List PrenatalDiagnosis
listNonUrgentDiagnoses diagnoses =
    let
        exclusions =
            NoPrenatalDiagnosis
                :: emergencyReferralDiagnosesInitial
                ++ emergencyReferralDiagnosesRecurrent
    in
    List.filter (\diagnosis -> not <| List.member diagnosis exclusions) diagnoses


emergencyReferralDiagnosesInitial : List PrenatalDiagnosis
emergencyReferralDiagnosesInitial =
    [ -- Diagnosed from Danger Signs.
      DiagnosisSeverePreeclampsiaImmediate
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

    -- Infection diagnosis will be available at latter phase.
    -- , DiagnosisInfection
    ]


emergencyReferralDiagnosesRecurrent : List PrenatalDiagnosis
emergencyReferralDiagnosesRecurrent =
    [ DiagnosisSeverePreeclampsiaAfterRecheck
    , DiagnosisSevereAnemiaWithComplications
    ]


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

                -- Following 8 do not participate at recurrent phase, therefore,
                -- resolved directly from value.
                , mebendezole = EverySet.member Mebendezole value.distributionSigns |> Just
                , tenofovir = EverySet.member Tenofovir value.distributionSigns |> Just
                , lamivudine = EverySet.member Lamivudine value.distributionSigns |> Just
                , dolutegravir = EverySet.member Dolutegravir value.distributionSigns |> Just
                , tdf3tc = EverySet.member TDF3TC value.distributionSigns |> Just
                , ceftriaxone = EverySet.member Ceftriaxone value.distributionSigns |> Just
                , azithromycin = EverySet.member Azithromycin value.distributionSigns |> Just
                , metronidazole = EverySet.member Metronidazole value.distributionSigns |> Just
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
            Pages.Utils.resolveMedicationsNonAdministrationReasons value
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


toMedicationDistributionValueWithDefaultInitialPhase : Maybe PrenatalMedicationDistributionValue -> MedicationDistributionForm -> Maybe PrenatalMedicationDistributionValue
toMedicationDistributionValueWithDefaultInitialPhase =
    toMedicationDistributionValueWithDefault NoMedicationDistributionSignsInitialPhase


toMedicationDistributionValueWithDefaultRecurrentPhase : Maybe PrenatalMedicationDistributionValue -> MedicationDistributionForm -> Maybe PrenatalMedicationDistributionValue
toMedicationDistributionValueWithDefaultRecurrentPhase =
    toMedicationDistributionValueWithDefault NoMedicationDistributionSignsRecurrentPhase


toMedicationDistributionValueWithDefault : MedicationDistributionSign -> Maybe PrenatalMedicationDistributionValue -> MedicationDistributionForm -> Maybe PrenatalMedicationDistributionValue
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

                    else if diagnosedHypertensionPrevoiusly assembled then
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
                                    recommendedTreatmentSignsForMalaria
                                    assembled
                                    form

                            else
                                ( [], 0, 0 )

                        ( heartburnInputs, heartburnCompleted, heartburnActive ) =
                            if diagnosed DiagnosisHeartburn assembled then
                                resolveRecommendedTreatmentForHeartburnInputsAndTasks language
                                    currentDate
                                    setRecommendedTreatmentSignMsg
                                    recommendedTreatmentSignsForHeartburn
                                    assembled
                                    form

                            else
                                ( [], 0, 0 )

                        ( urinaryTractInfectionInputs, urinaryTractInfectionCompleted, urinaryTractInfectionActive ) =
                            if diagnosed DiagnosisUrinaryTractInfection assembled then
                                resolveRecommendedTreatmentForUrinaryTractInfectionInputsAndTasks language
                                    currentDate
                                    setRecommendedTreatmentSignMsg
                                    recommendedTreatmentSignsForUrinaryTractInfection
                                    assembled
                                    form

                            else
                                ( [], 0, 0 )

                        ( candidiasisInputs, candidiasisCompleted, candidiasisActive ) =
                            if diagnosed DiagnosisCandidiasis assembled then
                                resolveRecommendedTreatmentForCandidiasisInputsAndTasks language
                                    currentDate
                                    setRecommendedTreatmentSignMsg
                                    recommendedTreatmentSignsForCandidiasis
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
                    , malariaCompleted
                        + heartburnCompleted
                        + hypertensionCompleted
                        + urinaryTractInfectionCompleted
                        + candidiasisCompleted
                    , malariaActive
                        + heartburnActive
                        + hypertensionActive
                        + urinaryTractInfectionActive
                        + candidiasisActive
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
    in
    ( viewCustomLabel language Translate.HypertensionRecommendedTreatmentHeader "." "instructions"
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
    Maybe.map2
        (\recommendationDosageUpdate recommendedMedication ->
            let
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

                currentTreatmentLabel =
                    getLatestTreatmentByTreatmentOptions recommendedTreatmentSignsForHypertension assembled
                        |> Maybe.map
                            (\currentTreatment ->
                                div [ class "label overview" ]
                                    [ text <| translate language Translate.HypertensionRecommendedTreatmentUpdateCurrentTreatment
                                    , text " "
                                    , text <| translate language <| Translate.RecommendedTreatmentSignLabel currentTreatment
                                    , text " "
                                    , text <| translate language <| Translate.RecommendedTreatmentSignDosage currentTreatment
                                    , text "."
                                    ]
                            )
                        |> Maybe.withDefault emptyNode

                newTreatmentLabel =
                    div [ class "label overview" ]
                        [ text <| translate language <| Translate.HypertensionRecommendedTreatmentUpdateNewTreatment recommendationDosageUpdate
                        , span [ class "highlight" ]
                            [ text <| translate language <| Translate.RecommendedTreatmentSignLabel recommendedMedication
                            , text " "
                            , text <| translate language <| Translate.RecommendedTreatmentSignDosage recommendedMedication
                            , text "."
                            ]
                        ]

                ( input, completed, active ) =
                    recommendedTreatmentForHypertensionInputAndTask language
                        currentDate
                        recommendedTreatmentSignsForHypertension
                        setRecommendedTreatmentSignMsg
                        assembled
                        form

                ( derrivedInput, derrivedCompleted, derrivedActive ) =
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
            ( [ viewCustomLabel language Translate.HypertensionRecommendedTreatmentUpdateHeader "." "label"
              , currentBPLabel
              , currentTreatmentLabel
              , newTreatmentLabel
              ]
                ++ input
                ++ derrivedInput
                ++ [ div [ class "separator" ] [] ]
            , completed + derrivedCompleted
            , active + derrivedActive
            )
        )
        (hypertensionTreatementUpdateRecommendationByBP assembled)
        (resolveHypertensionTreatementUpdateMedication assembled)
        |> Maybe.withDefault ( [], 0, 0 )


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
        -- to be able to determine current value.
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
            (viewTreatmentOptioForHypertension language)
      ]
    , taskCompleted currentValue
    , 1
    )


viewTreatmentOptioForHypertension : Language -> RecommendedTreatmentSign -> Html any
viewTreatmentOptioForHypertension language sign =
    let
        multipleTreatmentWithDosage =
            List.map (viewtreatmentWithDosage language)
                >> List.intersperse [ b [] [ text <| " " ++ (String.toUpper <| translate language Translate.And) ++ " " ] ]
                >> List.concat
                >> label []
    in
    case sign of
        TreatmentHypertensionAddCarvedilol ->
            multipleTreatmentWithDosage
                [ TreatmentMethyldopa4
                , TreatmentHypertensionAddCarvedilol
                ]

        TreatmentHypertensionAddAmlodipine ->
            multipleTreatmentWithDosage
                [ TreatmentMethyldopa4
                , TreatmentHypertensionAddCarvedilol
                , TreatmentHypertensionAddAmlodipine
                ]

        _ ->
            viewTreatmentOptionWithDosage language sign


updateHypertensionTreatmentWithMedication : AssembledData -> Bool
updateHypertensionTreatmentWithMedication assembled =
    resolveHypertensionTreatementUpdateMedication assembled
        |> isJust


updateHypertensionTreatmentWithHospitalization : AssembledData -> Bool
updateHypertensionTreatmentWithHospitalization assembled =
    resolveHypertensionTreatementUpdateRecommendation assembled
        |> Maybe.map ((==) hypertensionTreatementHospitalizationOption)
        |> Maybe.withDefault False


hypertensionTreatementHospitalizationOption : ( RecommendedTreatmentSign, Bool )
hypertensionTreatementHospitalizationOption =
    ( NoTreatmentForHypertension, True )


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


hypertensionTreatementUpdateRecommendationByBP : AssembledData -> Maybe HypertensionTreatementUpdateOption
hypertensionTreatementUpdateRecommendationByBP assembled =
    if diagnosedHypertensionPrevoiusly assembled then
        getMeasurementValueFunc assembled.measurements.vitals
            |> Maybe.andThen
                (\value ->
                    Maybe.map2
                        (\sys dia ->
                            let
                                bySys =
                                    hypertensionTreatementUpdateRecommendationBySys sys

                                byDia =
                                    hypertensionTreatementUpdateRecommendationBySys dia
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


hypertensionTreatementUpdateRecommendationBySys : Float -> HypertensionTreatementUpdateOption
hypertensionTreatementUpdateRecommendationBySys value =
    if value < 140 then
        TreatementUpdateMaintainCurrentDoasage

    else if value < 160 then
        TreatementUpdateIncreaseOneDose

    else if value < 160 then
        TreatementUpdateIncreaseTwoDoses

    else
        TreatementUpdateHospitalize


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
    List.reverse assembled.nursePreviousMeasurementsWithDates
        |> List.filterMap
            (\( _, _, measurements ) ->
                getMeasurementValueFunc measurements.medicationDistribution
                    |> Maybe.andThen
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
    -> List RecommendedTreatmentSign
    -> AssembledData
    -> MedicationDistributionForm
    -> ( List (Html msg), Int, Int )
resolveRecommendedTreatmentForMalariaInputsAndTasks language currentDate setRecommendedTreatmentSignMsg allowedSigns assembled form =
    let
        egaInWeeks =
            Maybe.map
                (calculateEGAWeeks currentDate)
                assembled.globalLmpDate

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
        -- to be able to determine current value.
        currentValue =
            Maybe.andThen
                (List.filter (\sign -> List.member sign recommendedTreatmentSignsForMalaria)
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
        -- to be able to determine current value.
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


viewTreatmentOptionWithDosage : Language -> RecommendedTreatmentSign -> Html any
viewTreatmentOptionWithDosage language sign =
    if
        List.member sign
            [ NoTreatmentForHypertension
            , NoTreatmentForMalaria
            , NoTreatmentForSyphilis
            ]
    then
        label [] [ text <| translate language <| Translate.RecommendedTreatmentSignLabel sign ]

    else
        viewtreatmentWithDosage language sign
            |> label []


viewtreatmentWithDosage : Language -> RecommendedTreatmentSign -> List (Html any)
viewtreatmentWithDosage language sign =
    [ span [ class "treatment" ] [ text <| (translate language <| Translate.RecommendedTreatmentSignLabel sign) ++ ":" ]
    , span [ class "dosage" ] [ text <| translate language <| Translate.RecommendedTreatmentSignDosage sign ]
    ]


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
    -> List RecommendedTreatmentSign
    -> AssembledData
    -> MedicationDistributionForm
    -> ( List (Html msg), Int, Int )
resolveRecommendedTreatmentForHeartburnInputsAndTasks language currentDate setRecommendedTreatmentSignMsg allowedSigns assembled form =
    let
        -- Since we may have values set for another diagnosis,
        -- we need to filter them out, to be able to determine current value.
        currentValue =
            Maybe.andThen
                (List.filter (\sign -> List.member sign recommendedTreatmentSignsForHeartburn)
                    >> List.head
                )
                form.recommendedTreatmentSigns
    in
    ( [ viewCustomLabel language Translate.HeartburnRecommendedTreatmentHeader "." "instructions"
      , h2 [] [ text <| translate language Translate.ActionsToTake ++ ":" ]
      , div [ class "instructions" ]
            [ viewInstructionsLabel "icon-pills" (text <| translate language Translate.HeartburnRecommendedTreatmentHelper ++ ".") ]
      , viewCheckBoxSelectCustomInput language
            recommendedTreatmentSignsForHeartburn
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
    -> List RecommendedTreatmentSign
    -> AssembledData
    -> MedicationDistributionForm
    -> ( List (Html msg), Int, Int )
resolveRecommendedTreatmentForUrinaryTractInfectionInputsAndTasks language currentDate setRecommendedTreatmentSignMsg allowedSigns assembled form =
    let
        -- Since we may have values set for another diagnosis,
        -- we need to filter them out, to be able to determine current value.
        currentValue =
            Maybe.andThen
                (List.filter (\sign -> List.member sign recommendedTreatmentSignsForUrinaryTractInfection)
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
            recommendedTreatmentSignsForUrinaryTractInfection
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
    [ TreatmentNitrfurantion
    , TreatmentAmoxicilin
    ]


resolveRecommendedTreatmentForCandidiasisInputsAndTasks :
    Language
    -> NominalDate
    -> (List RecommendedTreatmentSign -> RecommendedTreatmentSign -> msg)
    -> List RecommendedTreatmentSign
    -> AssembledData
    -> MedicationDistributionForm
    -> ( List (Html msg), Int, Int )
resolveRecommendedTreatmentForCandidiasisInputsAndTasks language currentDate setRecommendedTreatmentSignMsg allowedSigns assembled form =
    let
        -- Since we may have values set for another diagnosis,
        -- we need to filter them out, to be able to determine current value.
        currentValue =
            Maybe.andThen
                (List.filter (\sign -> List.member sign recommendedTreatmentSignsForCandidiasis)
                    >> List.head
                )
                form.recommendedTreatmentSigns
    in
    ( [ viewCustomLabel language Translate.CandidiasisRecommendedTreatmentHeader "." "instructions"
      , h2 [] [ text <| translate language Translate.ActionsToTake ++ ":" ]
      , div [ class "instructions" ]
            [ viewInstructionsLabel "icon-pills" (text <| translate language Translate.CandidiasisRecommendedTreatmentHelper ++ ".")
            , p [ class "instructions-warning" ] [ text <| translate language Translate.CandidiasisRecommendedTreatmentInstructions ++ "." ]
            ]
      , viewCheckBoxSelectCustomInput language
            recommendedTreatmentSignsForCandidiasis
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


{-| Medication Distribution activity appears on both initial and recurrent encounters.
Each one of them got unique set of signs that can be used.
In order to know if activity was completed or not, we check if at least one
of those signs was set.
-}
medicationDistributionMeasurementTaken : List MedicationDistributionSign -> PrenatalMeasurements -> Bool
medicationDistributionMeasurementTaken allowedSigns measurements =
    getMeasurementValueFunc measurements.medicationDistribution
        |> Maybe.map
            (.distributionSigns
                >> (\signs ->
                        List.any (\sign -> EverySet.member sign signs)
                            allowedSigns
                   )
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
        PrenatalEncounterPhaseInitial ->
            let
                mebendazoleSet =
                    let
                        prescribeMebendazole =
                            showMebendazoleQuestion currentDate assembled
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
            in
            Maybe.Extra.values [ mebendazoleSet, hivPositiveSet, discordantPartnershipSet, gonorheaSet, trichomonasOrBVSet ]

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


referToHospitalDueToAdverseEvent : AssembledData -> Bool
referToHospitalDueToAdverseEvent =
    referToHospitalDueToAdverseEventForDiagnosesTreatment
        [ DiagnosisHIV, DiagnosisChronicHypertensionImmediate, DiagnosisMalaria, DiagnosisModerateAnemia, DiagnosisSyphilis ]


referToHospitalDueToAdverseEventForHypertensionTreatment : AssembledData -> Bool
referToHospitalDueToAdverseEventForHypertensionTreatment =
    referToHospitalDueToAdverseEventForDiagnosesTreatment
        [ DiagnosisChronicHypertensionImmediate ]


referToHospitalDueToAdverseEventForAnemiaTreatment : AssembledData -> Bool
referToHospitalDueToAdverseEventForAnemiaTreatment =
    referToHospitalDueToAdverseEventForDiagnosesTreatment
        [ DiagnosisModerateAnemia ]


referToHospitalDueToAdverseEventForMalariaTreatment : AssembledData -> Bool
referToHospitalDueToAdverseEventForMalariaTreatment =
    referToHospitalDueToAdverseEventForDiagnosesTreatment
        [ DiagnosisMalaria ]


referToHospitalDueToAdverseEventForDiagnosesTreatment : List PrenatalDiagnosis -> AssembledData -> Bool
referToHospitalDueToAdverseEventForDiagnosesTreatment diagnoses assembled =
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
                            -- at Ttreatment Review activity.
                            _ ->
                                False

                    referByTreatment =
                        Maybe.map (EverySet.member MedicationTreatmentAdverseEventsHospitalization)
                            >> Maybe.withDefault False
                in
                List.map conditionByDiagnosis diagnoses
                    |> List.any ((==) True)
            )
        |> Maybe.withDefault False


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
                            (\( _, _, measurements ) ->
                                getMeasurementValueFunc measurements.medication
                                    |> Maybe.andThen .signs
                                    |> Maybe.map (EverySet.member DewormingPill)
                                    |> Maybe.withDefault False
                            )
                            assembled.nursePreviousMeasurementsWithDates
                            |> List.isEmpty

                    mebenadazoleNotPrescribed =
                        List.filter
                            (\( _, _, measurements ) ->
                                measurements.medicationDistribution
                                    |> Maybe.map (Tuple.second >> .value >> .distributionSigns >> EverySet.member Mebendezole)
                                    |> Maybe.withDefault False
                            )
                            assembled.nursePreviousMeasurementsWithDates
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

        ( derivedInput, derrivedTaskCompleted, derrivedTaskActive ) =
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
    , taskCompleted form.mebendezole + derrivedTaskCompleted
    , 1 + derrivedTaskActive
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

        ( derivedInput, derrivedTaskCompleted, derrivedTaskActive ) =
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
    , taskCompleted form.tenofovir + derrivedTaskCompleted
    , 1 + derrivedTaskActive
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

        ( derivedInput, derrivedTaskCompleted, derrivedTaskActive ) =
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
    , taskCompleted form.lamivudine + derrivedTaskCompleted
    , 1 + derrivedTaskActive
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

        ( derivedInput, derrivedTaskCompleted, derrivedTaskActive ) =
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
    , taskCompleted form.dolutegravir + derrivedTaskCompleted
    , 1 + derrivedTaskActive
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

        ( derivedInput, derrivedTaskCompleted, derrivedTaskActive ) =
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
    , taskCompleted form.tdf3tc + derrivedTaskCompleted
    , 1 + derrivedTaskActive
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

        ( derivedInput, derrivedTaskCompleted, derrivedTaskActive ) =
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
    , taskCompleted form.iron + derrivedTaskCompleted
    , 1 + derrivedTaskActive
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

        ( derivedInput, derrivedTaskCompleted, derrivedTaskActive ) =
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
    , taskCompleted form.folicAcid + derrivedTaskCompleted
    , 1 + derrivedTaskActive
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

        ( derivedInput, derrivedTaskCompleted, derrivedTaskActive ) =
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
    , taskCompleted form.ceftriaxone + derrivedTaskCompleted
    , 1 + derrivedTaskActive
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

        ( derivedInput, derrivedTaskCompleted, derrivedTaskActive ) =
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
    , taskCompleted form.azithromycin + derrivedTaskCompleted
    , 1 + derrivedTaskActive
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

        ( derivedInput, derrivedTaskCompleted, derrivedTaskActive ) =
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
    , taskCompleted form.metronidazole + derrivedTaskCompleted
    , 1 + derrivedTaskActive
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


medicationsInitialPhase : List MedicationDistributionSign
medicationsInitialPhase =
    [ Mebendezole
    , TDF3TC
    , Dolutegravir
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
        |> Maybe.map
            (\signs ->
                List.any (\sign -> EverySet.member sign signs)
                    allowedSigns
            )
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
    diagnosedPreviouslyAnyOf
        [ DiagnosisChronicHypertensionImmediate
        , DiagnosisGestationalHypertensionImmediate
        , DiagnosisChronicHypertensionAfterRecheck
        , DiagnosisGestationalHypertensionAfterRecheck
        ]
        assembled


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
