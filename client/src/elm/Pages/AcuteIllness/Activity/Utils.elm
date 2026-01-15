module Pages.AcuteIllness.Activity.Utils exposing (..)

import AssocList as Dict exposing (Dict)
import Backend.AcuteIllnessActivity.Model exposing (AcuteIllnessActivity(..))
import Backend.AcuteIllnessEncounter.Types exposing (AcuteIllnessDiagnosis(..))
import Backend.Entities exposing (PersonId)
import Backend.Measurement.Encoder exposing (malariaRapidTestResultAsString)
import Backend.Measurement.Model exposing (..)
import Backend.Measurement.Utils exposing (getMeasurementValueFunc, muacIndication)
import Backend.Person.Model exposing (Person)
import Backend.Person.Utils exposing (ageInMonths, ageInYears, isChildUnderAgeOf5, isPersonAFertileWoman)
import Backend.Utils exposing (tuberculosisManagementEnabled)
import EverySet exposing (EverySet)
import Gizra.Html exposing (emptyNode)
import Gizra.NominalDate exposing (NominalDate)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Maybe.Extra exposing (andMap, isJust, isNothing, or, unwrap)
import Measurement.Model exposing (HealthEducationForm, InvokationModule(..), VitalsFormConfig, VitalsFormMode(..))
import Measurement.Utils
    exposing
        ( fromListWithDefaultValue
        , healthEducationFormWithDefault
        , muacFormWithDefault
        , ongoingTreatmentReviewFormWithDefault
        , renderDatePart
        , sendToHCFormWithDefault
        , treatmentReviewInputsAndTasks
        , viewAdministeredMedicationCustomLabel
        , viewAdministeredMedicationQuestion
        , vitalsFormWithDefault
        )
import Measurement.View exposing (sendToFacilityInputsAndTasks, vitalsFormInputsAndTasks)
import Pages.AcuteIllness.Activity.Model exposing (..)
import Pages.AcuteIllness.Activity.Types exposing (..)
import Pages.AcuteIllness.Encounter.Model exposing (AssembledData)
import Pages.Utils
    exposing
        ( concatInputsAndTasksSections
        , getCurrentReasonForMedicationNonAdministration
        , ifEverySetEmpty
        , ifNullableTrue
        , ifTrue
        , maybeToBoolTask
        , maybeValueConsideringIsDirtyField
        , nonAdministrationReasonToSign
        , resolveTasksCompletedFromTotal
        , taskCompleted
        , valueConsideringIsDirtyField
        , viewBoolInput
        , viewCheckBoxMultipleSelectInput
        , viewCheckBoxSelectCustomInput
        , viewCheckBoxSelectInput
        , viewCustomLabel
        , viewCustomSelectListInput
        , viewInstructionsLabel
        , viewLabel
        , viewQuestionLabel
        , viewRedAlertForSelect
        )
import SyncManager.Model exposing (Site(..), SiteFeature)
import Translate exposing (TranslationId, translate)
import Translate.Model exposing (Language(..))


expectActivity : NominalDate -> Bool -> AssembledData -> AcuteIllnessActivity -> Bool
expectActivity currentDate isChw assembled activity =
    case activity of
        AcuteIllnessSymptoms ->
            assembled.initialEncounter

        AcuteIllnessPriorTreatment ->
            assembled.initialEncounter

        AcuteIllnessDangerSigns ->
            not assembled.initialEncounter

        AcuteIllnessPhysicalExam ->
            List.filter (expectPhysicalExamTask currentDate assembled.person isChw assembled.initialEncounter) physicalExamTasks
                |> List.isEmpty
                |> not

        AcuteIllnessLaboratory ->
            List.filter (expectLaboratoryTask currentDate isChw assembled) laboratoryTasks
                |> List.isEmpty
                |> not

        AcuteIllnessOngoingTreatment ->
            if assembled.initialEncounter then
                False

            else
                let
                    initialWithSubsequent =
                        if List.isEmpty assembled.secondInitialWithSubsequent then
                            assembled.firstInitialWithSubsequent

                        else
                            assembled.secondInitialWithSubsequent
                in
                -- Show activity, if medication was perscribed at any of previous encounters.
                List.filterMap
                    (.measurements
                        >> .medicationDistribution
                        >> Maybe.andThen
                            (Tuple.second
                                >> .value
                                >> .distributionSigns
                                >> (\medications ->
                                        if
                                            (medications /= EverySet.singleton NoMedicationDistributionSigns)
                                                -- Lemon juice does not count as a medication.
                                                && (medications /= EverySet.singleton LemonJuiceOrHoney)
                                        then
                                            Just True

                                        else
                                            Nothing
                                   )
                            )
                    )
                    initialWithSubsequent
                    |> List.isEmpty
                    |> not

        AcuteIllnessNextSteps ->
            resolveNextStepsTasks currentDate isChw assembled
                |> List.isEmpty
                |> not


activityCompleted : NominalDate -> Bool -> AssembledData -> AcuteIllnessActivity -> Bool
activityCompleted currentDate isChw assembled activity =
    let
        person =
            assembled.person

        measurements =
            assembled.measurements

        activityExpected =
            expectActivity currentDate isChw assembled
    in
    case activity of
        AcuteIllnessSymptoms ->
            mandatoryActivityCompletedFirstEncounter currentDate person isChw measurements AcuteIllnessSymptoms

        AcuteIllnessPhysicalExam ->
            if assembled.initialEncounter then
                mandatoryActivityCompletedFirstEncounter currentDate person isChw measurements AcuteIllnessPhysicalExam

            else
                mandatoryActivityCompletedSubsequentVisit currentDate isChw assembled AcuteIllnessPhysicalExam

        AcuteIllnessPriorTreatment ->
            isJust measurements.treatmentReview

        AcuteIllnessLaboratory ->
            (not <| activityExpected AcuteIllnessLaboratory)
                || List.all (laboratoryTaskCompleted currentDate isChw assembled) laboratoryTasks

        AcuteIllnessNextSteps ->
            (not <| activityExpected AcuteIllnessNextSteps)
                || (resolveNextStepsTasks currentDate isChw assembled
                        |> List.all (nextStepsTaskCompleted currentDate isChw assembled)
                   )

        AcuteIllnessDangerSigns ->
            mandatoryActivityCompletedSubsequentVisit currentDate isChw assembled AcuteIllnessDangerSigns

        AcuteIllnessOngoingTreatment ->
            mandatoryActivityCompletedSubsequentVisit currentDate isChw assembled AcuteIllnessOngoingTreatment


physicalExamTasks : List PhysicalExamTask
physicalExamTasks =
    [ PhysicalExamVitals
    , PhysicalExamCoreExam
    , PhysicalExamMuac
    , PhysicalExamNutrition
    , PhysicalExamAcuteFindings
    ]


symptomsGeneralDangerSigns : List SymptomsGeneralSign
symptomsGeneralDangerSigns =
    [ Lethargy
    , PoorSuck
    , UnableToDrink
    , UnableToEat
    , IncreasedThirst
    , DryMouth
    , SevereWeakness
    , YellowEyes
    , CokeColoredUrine
    , SymptomsGeneralConvulsions
    , SpontaneousBleeding
    ]


allSymptomsGeneralSigns : ( List SymptomsGeneralSign, SymptomsGeneralSign )
allSymptomsGeneralSigns =
    ( [ SymptomGeneralFever
      , Chills
      , NightSweats
      , BodyAches
      , Headache
      ]
        ++ symptomsGeneralDangerSigns
    , NoSymptomsGeneral
    )


allSymptomsRespiratorySigns : ( List SymptomsRespiratorySign, SymptomsRespiratorySign )
allSymptomsRespiratorySigns =
    ( [ Cough
      , ShortnessOfBreath
      , NasalCongestion
      , BloodInSputum
      , SoreThroat
      , LossOfSmell
      , StabbingChestPain
      ]
    , NoSymptomsRespiratory
    )


allSymptomsGISigns : ( List SymptomsGISign, SymptomsGISign )
allSymptomsGISigns =
    ( [ BloodyDiarrhea
      , NonBloodyDiarrhea
      , Nausea
      , Vomiting
      , SymptomGIAbdominalPain
      ]
    , NoSymptomsGI
    )


toggleSymptomsSign : SymptomsTask -> a -> a -> Dict a Int -> Dict a Int
toggleSymptomsSign task sign noneSign signs =
    if sign == noneSign then
        Dict.singleton sign 1

    else
        let
            signs_ =
                Dict.remove noneSign signs
        in
        if Dict.member sign signs_ then
            Dict.remove sign signs_

        else
            Dict.insert sign 1 signs_


symptomsTasksCompletedFromTotal : AcuteIllnessMeasurements -> SymptomsData -> SymptomsTask -> ( Int, Int )
symptomsTasksCompletedFromTotal measurements data task =
    case task of
        SymptomsGeneral ->
            let
                form =
                    getMeasurementValueFunc measurements.symptomsGeneral
                        |> symptomsGeneralFormWithDefault data.symptomsGeneralForm
            in
            ( taskNotCompleted (Dict.isEmpty form.signs)
            , 1
            )

        SymptomsRespiratory ->
            let
                ( coughCompleted, coughTotal ) =
                    Dict.get Cough form.signs
                        |> Maybe.map
                            (\value ->
                                case value of
                                    -- Value is set to 1 when Cough symptom is checked, but
                                    -- period (more / less than 2 weeks ) was not selected.
                                    1 ->
                                        ( 0, 1 )

                                    -- Possible values are symptomMaxDuration  (14),
                                    -- or coughLessThan2WeeksConstant (7). In both cases,
                                    -- Cough symptom is checked and period is selected.
                                    _ ->
                                        ( 1, 1 )
                            )
                        |> Maybe.withDefault ( 0, 0 )

                form =
                    getMeasurementValueFunc measurements.symptomsRespiratory
                        |> symptomsRespiratoryFormWithDefault data.symptomsRespiratoryForm
            in
            ( taskNotCompleted (Dict.isEmpty form.signs) + coughCompleted
            , 1 + coughTotal
            )

        SymptomsGI ->
            let
                form =
                    getMeasurementValueFunc measurements.symptomsGI
                        |> symptomsGIFormWithDefault data.symptomsGIForm

                totalDerived =
                    if Dict.member Vomiting form.signs then
                        1

                    else
                        0

                completedDerived =
                    if totalDerived > 0 then
                        taskNotCompleted (isNothing form.intractableVomiting)

                    else
                        0
            in
            ( taskNotCompleted (Dict.isEmpty form.signs) + completedDerived
            , 1 + totalDerived
            )


physicalExamTasksCompletedFromTotal :
    NominalDate
    -> Bool
    -> Person
    -> AssembledData
    -> PhysicalExamData
    -> PhysicalExamTask
    -> ( Int, Int )
physicalExamTasksCompletedFromTotal currentDate isChw person assembled data task =
    let
        measurements =
            assembled.measurements

        ( _, tasks ) =
            case task of
                PhysicalExamVitals ->
                    let
                        formConfig =
                            generateVitalsFormConfig isChw assembled
                    in
                    getMeasurementValueFunc measurements.vitals
                        |> vitalsFormWithDefault data.vitalsForm
                        |> vitalsFormInputsAndTasks English currentDate formConfig

                PhysicalExamCoreExam ->
                    getMeasurementValueFunc measurements.coreExam
                        |> coreExamFormWithDefault data.coreExamForm
                        |> coreExamFormInutsAndTasks English currentDate

                PhysicalExamMuac ->
                    getMeasurementValueFunc measurements.muac
                        |> muacFormWithDefault data.muacForm
                        |> Measurement.View.muacFormInputsAndTasks English currentDate SiteRwanda assembled.person Nothing SetMuac

                PhysicalExamAcuteFindings ->
                    getMeasurementValueFunc measurements.acuteFindings
                        |> acuteFindingsFormWithDefault data.acuteFindingsForm
                        |> acuteFindingsFormInutsAndTasks English currentDate

                PhysicalExamNutrition ->
                    getMeasurementValueFunc measurements.nutrition
                        |> nutritionFormWithDefault data.nutritionForm
                        |> nutritionFormInutsAndTasks English currentDate
    in
    resolveTasksCompletedFromTotal tasks


generateVitalsFormConfig : Bool -> AssembledData -> VitalsFormConfig Msg
generateVitalsFormConfig isChw assembled =
    { setIntInputMsg = SetVitalsIntInput
    , setFloatInputMsg = SetVitalsFloatInput
    , sysBloodPressurePreviousValue = resolvePreviousMaybeValue assembled .vitals .sys
    , diaBloodPressurePreviousValue = resolvePreviousMaybeValue assembled .vitals .dia
    , heartRatePreviousValue =
        resolvePreviousMaybeValue assembled .vitals .heartRate
            |> Maybe.map toFloat
    , respiratoryRatePreviousValue =
        resolvePreviousValue assembled .vitals .respiratoryRate
            |> Maybe.map toFloat
    , bodyTemperaturePreviousValue = resolvePreviousValue assembled .vitals .bodyTemperature
    , birthDate = assembled.person.birthDate
    , formClass = "vitals"
    , mode =
        if isChw then
            VitalsFormBasic

        else
            VitalsFormFull
    , invokationModule = InvokationModuleAcuteIllness
    }


acuteFindingsFormInutsAndTasks : Language -> NominalDate -> AcuteFindingsForm -> ( List (Html Msg), List (Maybe Bool) )
acuteFindingsFormInutsAndTasks language currentDate form =
    ( [ viewQuestionLabel language Translate.PatientExhibitAnyFindings
      , viewCustomLabel language Translate.CheckAllThatApply "." "helper"
      , viewCheckBoxMultipleSelectInput language
            [ LethargicOrUnconscious, AcuteFindingsPoorSuck, SunkenEyes, PoorSkinTurgor, Jaundice, NoAcuteFindingsGeneralSigns ]
            []
            (form.signsGeneral |> Maybe.withDefault [])
            Nothing
            SetAcuteFindingsGeneralSign
            Translate.AcuteFindingsGeneralSign
      , viewQuestionLabel language Translate.PatientExhibitAnyRespiratoryFindings
      , viewCustomLabel language Translate.CheckAllThatApply "." "helper"
      , viewCheckBoxMultipleSelectInput language
            [ Stridor, NasalFlaring, SevereWheezing, SubCostalRetractions, NoAcuteFindingsRespiratorySigns ]
            []
            (form.signsRespiratory |> Maybe.withDefault [])
            Nothing
            SetAcuteFindingsRespiratorySign
            Translate.AcuteFindingsRespiratorySign
      ]
    , [ maybeToBoolTask form.signsGeneral, maybeToBoolTask form.signsRespiratory ]
    )


coreExamFormInutsAndTasks : Language -> NominalDate -> AcuteIllnessCoreExamForm -> ( List (Html Msg), List (Maybe Bool) )
coreExamFormInutsAndTasks language currentDate form =
    ( [ div [ class "ui grid" ]
            [ div [ class "twelve wide column" ]
                [ viewLabel language Translate.Heart ]
            , div [ class "four wide column" ]
                [ viewRedAlertForSelect
                    (form.heart |> Maybe.map List.singleton |> Maybe.withDefault [])
                    [ NormalRateAndRhythm ]
                ]
            ]
      , viewCheckBoxSelectInput language
            [ IrregularRhythm, SinusTachycardia, NormalRateAndRhythm ]
            []
            form.heart
            SetCoreExamHeart
            Translate.HeartCPESign
      , div [ class "separator" ] []
      , div [ class "ui grid" ]
            [ div [ class "twelve wide column" ]
                [ viewLabel language Translate.Lungs ]
            , div [ class "four wide column" ]
                [ viewRedAlertForSelect
                    (form.lungs |> Maybe.withDefault [])
                    [ NormalLungs ]
                ]
            ]
      , viewCheckBoxMultipleSelectInput language
            [ Wheezes, Crackles, NormalLungs ]
            []
            (form.lungs |> Maybe.withDefault [])
            Nothing
            SetCoreExamLungs
            Translate.LungsCPESign
      ]
    , [ maybeToBoolTask form.heart, maybeToBoolTask form.lungs ]
    )


nutritionFormInutsAndTasks : Language -> NominalDate -> AcuteIllnessNutritionForm -> ( List (Html Msg), List (Maybe Bool) )
nutritionFormInutsAndTasks language currentDate form =
    ( [ p [] [ text <| translate language Translate.NutritionHelper ]
      , viewLabel language Translate.SelectAllSigns
      , viewCheckBoxMultipleSelectInput language
            [ Edema, AbdominalDistension, DrySkin ]
            [ Apathy, PoorAppetite, BrittleHair ]
            (form.signs |> Maybe.withDefault [])
            (Just NormalChildNutrition)
            SetNutritionSign
            Translate.ChildNutritionSignLabel
      ]
    , [ maybeToBoolTask form.signs ]
    )


laboratoryTasksCompletedFromTotal : NominalDate -> Person -> AcuteIllnessMeasurements -> LaboratoryData -> AILaboratoryTask -> ( Int, Int )
laboratoryTasksCompletedFromTotal currentDate person measurements data task =
    let
        ( _, tasks ) =
            case task of
                LaboratoryMalariaTesting ->
                    getMeasurementValueFunc measurements.malariaTesting
                        |> malariaTestingFormWithDefault data.malariaTestingForm
                        |> malariaTestingFormInputsAndTasks English currentDate person

                LaboratoryCovidTesting ->
                    getMeasurementValueFunc measurements.covidTesting
                        |> covidTestingFormWithDefault data.covidTestingForm
                        |> covidTestingFormInputsAndTasks English currentDate person
    in
    resolveTasksCompletedFromTotal tasks


malariaTestingFormInputsAndTasks : Language -> NominalDate -> Person -> MalariaTestingForm -> ( List (Html Msg), List (Maybe Bool) )
malariaTestingFormInputsAndTasks language currentDate person form =
    let
        testResultPositive =
            form.rapidTestResult == Just RapidTestPositive || form.rapidTestResult == Just RapidTestPositiveAndPregnant

        isPregnantSection =
            if testResultPositive && isPersonAFertileWoman currentDate person then
                isPregnantInputsAndTasks language SetIsPregnant form.isPregnant

            else
                ( [], [] )
    in
    concatInputsAndTasksSections
        [ ( [ viewLabel language Translate.MalariaRapidDiagnosticTest
            , viewCustomSelectListInput form.rapidTestResult
                [ RapidTestNegative, RapidTestPositive, RapidTestIndeterminate, RapidTestUnableToRun ]
                malariaRapidTestResultAsString
                SetRapidTestResult
                (Translate.RapidTestResult >> translate language)
                "form-input rapid-test-result"
                (isNothing form.rapidTestResult)
            ]
          , [ maybeToBoolTask form.rapidTestResult ]
          )
        , isPregnantSection
        ]


isPregnantInputsAndTasks : Language -> (Bool -> Msg) -> Maybe Bool -> ( List (Html Msg), List (Maybe Bool) )
isPregnantInputsAndTasks language setMsg currentValue =
    ( [ viewQuestionLabel language Translate.CurrentlyPregnantQuestion
      , viewBoolInput
            language
            currentValue
            setMsg
            "is-pregnant"
            Nothing
      ]
    , [ currentValue ]
    )


covidTestingFormInputsAndTasks : Language -> NominalDate -> Person -> CovidTestingForm -> ( List (Html Msg), List (Maybe Bool) )
covidTestingFormInputsAndTasks language currentDate person form =
    let
        derivedSection =
            Maybe.map
                (\testPerformed ->
                    let
                        isPregnantSection =
                            if isPersonAFertileWoman currentDate person then
                                isPregnantInputsAndTasks language
                                    (SetCovidTestingBoolInput (\value form_ -> { form_ | isPregnant = Just value }))
                                    form.isPregnant

                            else
                                ( [], [] )
                    in
                    if testPerformed then
                        let
                            isPregnantSectionForView =
                                if form.testPositive == Just True then
                                    isPregnantSection

                                else
                                    ( [], [] )
                        in
                        concatInputsAndTasksSections
                            [ ( [ viewQuestionLabel language Translate.TestResultsQuestion
                                , viewBoolInput
                                    language
                                    form.testPositive
                                    (SetCovidTestingBoolInput (\value form_ -> { form_ | testPositive = Just value, isPregnant = Nothing }))
                                    "test-result"
                                    (Just ( Translate.RapidTestResult RapidTestPositive, Translate.RapidTestResult RapidTestNegative ))
                                ]
                              , [ maybeToBoolTask form.testPositive ]
                              )
                            , isPregnantSectionForView
                            ]

                    else
                        concatInputsAndTasksSections
                            [ ( [ div [ class "why-not" ]
                                    [ viewQuestionLabel language Translate.WhyNot
                                    , viewCheckBoxSelectInput language
                                        [ AdministeredPreviously
                                        , NonAdministrationLackOfStock
                                        , NonAdministrationPatientDeclined
                                        , NonAdministrationPatientUnableToAfford
                                        , NonAdministrationOther
                                        ]
                                        []
                                        form.administrationNote
                                        SetCovidTestingAdministrationNote
                                        Translate.AdministrationNote
                                    ]
                                ]
                              , [ maybeToBoolTask form.administrationNote ]
                              )
                            , isPregnantSection
                            ]
                )
                form.testPerformed
                |> Maybe.withDefault ( [], [] )
    in
    concatInputsAndTasksSections
        [ ( [ viewCustomLabel language Translate.CovidTestingInstructions "." "instructions"
            , viewQuestionLabel language Translate.TestPerformedQuestion
            , viewBoolInput
                language
                form.testPerformed
                (SetCovidTestingBoolInput
                    (\value form_ ->
                        { form_
                            | testPerformed = Just value
                            , testPositive = Nothing
                            , isPregnant = Nothing
                            , administrationNote = Nothing
                        }
                    )
                )
                "test-performed"
                Nothing
            ]
          , [ form.testPerformed ]
          )
        , derivedSection
        ]


treatmentTasksCompletedFromTotal : NominalDate -> AcuteIllnessMeasurements -> PriorTreatmentData -> PriorTreatmentTask -> ( Int, Int )
treatmentTasksCompletedFromTotal currentDate measurements data task =
    case task of
        TreatmentReview ->
            let
                ( _, tasks ) =
                    getMeasurementValueFunc measurements.treatmentReview
                        |> treatmentReviewFormWithDefault data.treatmentReviewForm
                        |> treatmentReviewFormInutsAndTasks English currentDate
            in
            resolveTasksCompletedFromTotal tasks


treatmentReviewFormInutsAndTasks : Language -> NominalDate -> TreatmentReviewForm -> ( List (Html Msg), List (Maybe Bool) )
treatmentReviewFormInutsAndTasks language currentDate form =
    let
        feverPast6HoursUpdateFunc value form_ =
            if value then
                { form_ | feverPast6Hours = Just True }

            else
                { form_ | feverPast6Hours = Just False, feverPast6HoursHelped = Nothing }

        feverPast6HoursHelpedUpdateFunc value form_ =
            { form_ | feverPast6HoursHelped = Just value }

        malariaTodayUpdateFunc value form_ =
            if value then
                { form_ | malariaToday = Just True }

            else
                { form_ | malariaToday = Just False, malariaTodayHelped = Nothing }

        malariaTodayHelpedUpdateFunc value form_ =
            { form_ | malariaTodayHelped = Just value }

        malariaWithinPastMonthUpdateFunc value form_ =
            if value then
                { form_ | malariaWithinPastMonth = Just True }

            else
                { form_ | malariaWithinPastMonth = Just False, malariaWithinPastMonthHelped = Nothing }

        malariaWithinPastMonthHelpedUpdateFunc value form_ =
            { form_ | malariaWithinPastMonthHelped = Just value }

        medicationHelpedQuestion =
            div [ class "ui grid" ]
                [ div [ class "one wide column" ] []
                , div [ class "fifteen wide column" ]
                    [ viewQuestionLabel language Translate.MedicationHelpedQuestion ]
                ]

        feverPast6HoursSection =
            let
                feverPast6HoursPositive =
                    Maybe.withDefault False form.feverPast6Hours

                feverPast6HoursHelpedSection =
                    if feverPast6HoursPositive then
                        ( [ medicationHelpedQuestion
                          , viewBoolInput
                                language
                                form.feverPast6HoursHelped
                                (SetTreatmentReviewBoolInput feverPast6HoursHelpedUpdateFunc)
                                "fever-past-6-hours-helped derived"
                                Nothing
                          ]
                        , [ form.feverPast6HoursHelped ]
                        )

                    else
                        ( [], [] )
            in
            concatInputsAndTasksSections
                [ ( [ viewQuestionLabel language Translate.MedicationForFeverPast6HoursQuestion
                    , viewBoolInput
                        language
                        form.feverPast6Hours
                        (SetTreatmentReviewBoolInput feverPast6HoursUpdateFunc)
                        "fever-past-6-hours"
                        Nothing
                    ]
                  , [ form.feverPast6Hours ]
                  )
                , feverPast6HoursHelpedSection
                ]

        malariaTodaySection =
            let
                malariaTodayPositive =
                    Maybe.withDefault False form.malariaToday

                malariaTodayHelpedSection =
                    if malariaTodayPositive then
                        ( [ medicationHelpedQuestion
                          , viewBoolInput
                                language
                                form.malariaTodayHelped
                                (SetTreatmentReviewBoolInput malariaTodayHelpedUpdateFunc)
                                "malaria-today-helped derived"
                                Nothing
                          ]
                        , [ form.malariaTodayHelped ]
                        )

                    else
                        ( [], [] )
            in
            concatInputsAndTasksSections
                [ ( [ viewQuestionLabel language Translate.MedicationForMalariaTodayQuestion
                    , viewBoolInput
                        language
                        form.malariaToday
                        (SetTreatmentReviewBoolInput malariaTodayUpdateFunc)
                        "malaria-today"
                        Nothing
                    ]
                  , [ form.malariaToday ]
                  )
                , malariaTodayHelpedSection
                ]

        malariaWithinPastMonthSection =
            let
                malariaWithinPastMonthPositive =
                    Maybe.withDefault False form.malariaWithinPastMonth

                malariaWithinPastMonthHelpedSection =
                    if malariaWithinPastMonthPositive then
                        ( [ medicationHelpedQuestion
                          , viewBoolInput
                                language
                                form.malariaWithinPastMonthHelped
                                (SetTreatmentReviewBoolInput malariaWithinPastMonthHelpedUpdateFunc)
                                "malaria-within-past-month-helped derived"
                                Nothing
                          ]
                        , [ form.malariaWithinPastMonthHelped ]
                        )

                    else
                        ( [], [] )
            in
            concatInputsAndTasksSections
                [ ( [ viewQuestionLabel language Translate.MedicationForMalariaWithinPastMonthQuestion
                    , viewBoolInput
                        language
                        form.malariaWithinPastMonth
                        (SetTreatmentReviewBoolInput malariaWithinPastMonthUpdateFunc)
                        "malaria-within-past-month"
                        Nothing
                    ]
                  , [ form.malariaWithinPastMonth ]
                  )
                , malariaWithinPastMonthHelpedSection
                ]
    in
    concatInputsAndTasksSections
        [ feverPast6HoursSection
        , malariaTodaySection
        , malariaWithinPastMonthSection
        ]


nextStepsTasksCompletedFromTotal :
    NominalDate
    -> Bool
    -> Bool
    -> Person
    -> Maybe AcuteIllnessDiagnosis
    -> AcuteIllnessMeasurements
    -> NextStepsData
    -> NextStepsTask
    -> ( Int, Int )
nextStepsTasksCompletedFromTotal currentDate isChw initialEncounter person diagnosis measurements data task =
    let
        ( _, tasks ) =
            case task of
                NextStepsMedicationDistribution ->
                    getMeasurementValueFunc measurements.medicationDistribution
                        |> medicationDistributionFormWithDefault data.medicationDistributionForm
                        |> medicationDistributionFormInutsAndTasks English currentDate person diagnosis

                NextStepsSendToHC ->
                    let
                        facility =
                            if isChw then
                                FacilityHealthCenter

                            else
                                FacilityHospital
                    in
                    getMeasurementValueFunc measurements.sendToHC
                        |> sendToHCFormWithDefault data.sendToHCForm
                        |> sendToFacilityInputsAndTasks English
                            currentDate
                            facility
                            SetReferToHealthCenter
                            SetReasonForNonReferral
                            SetHandReferralForm
                            Nothing

                NextStepsHealthEducation ->
                    getMeasurementValueFunc measurements.healthEducation
                        |> healthEducationFormWithDefault data.healthEducationForm
                        |> healthEducationFormInutsAndTasks English currentDate diagnosis

                NextStepsSymptomsReliefGuidance ->
                    getMeasurementValueFunc measurements.healthEducation
                        |> healthEducationFormWithDefault data.healthEducationForm
                        |> symptomsReliefFormInutsAndTasks English currentDate

                NextStepsFollowUp ->
                    getMeasurementValueFunc measurements.followUp
                        |> followUpFormWithDefault data.followUpForm
                        |> followUpFormInutsAndTasks English currentDate isChw

                NextStepsContactTracing ->
                    if data.contactsTracingForm.finished then
                        ( [], [ Just True ] )

                    else
                        ( [], [ Nothing ] )
    in
    resolveTasksCompletedFromTotal tasks


viewHCRecommendation : Language -> HCRecommendation -> Html any
viewHCRecommendation language recommendation =
    let
        riskLevel =
            case recommendation of
                SendAmbulance ->
                    Translate.HighRiskCase

                HomeIsolation ->
                    Translate.HighRiskCase

                ComeToHealthCenter ->
                    Translate.LowRiskCase

                ChwMonitoring ->
                    Translate.LowRiskCase

                HCRecommendationNotApplicable ->
                    Translate.LowRiskCase
    in
    label []
        [ text <| translate language Translate.HealthCenterDetermined
        , span [ class "strong" ] [ text <| translate language riskLevel ]
        , text <| translate language Translate.AndSentence
        , span [ class "strong" ] [ text <| translate language <| Translate.HCRecommendation recommendation ]
        ]


medicationDistributionFormInutsAndTasks :
    Language
    -> NominalDate
    -> Person
    -> Maybe AcuteIllnessDiagnosis
    -> MedicationDistributionForm
    -> ( List (Html Msg), List (Maybe Bool) )
medicationDistributionFormInutsAndTasks language currentDate person diagnosis form =
    let
        ( instructions, ( inputs, tasks ) ) =
            let
                derivedQuestionInputsAndTasks medication reasonToSignFunc =
                    let
                        currentValue =
                            getCurrentReasonForMedicationNonAdministration reasonToSignFunc form
                    in
                    ( [ viewQuestionLabel language Translate.WhyNot
                      , viewCheckBoxSelectInput language
                            [ NonAdministrationLackOfStock, NonAdministrationKnownAllergy, NonAdministrationPatientUnableToAfford ]
                            [ NonAdministrationPatientDeclined, NonAdministrationOther ]
                            currentValue
                            (SetMedicationDistributionAdministrationNote currentValue medication)
                            Translate.AdministrationNote
                      ]
                    , [ maybeToBoolTask currentValue ]
                    )

                -- When answer for medication administartion is Yes, we clean the reason for not adminsetering the medication.
                updateNonAdministrationSigns medication reasonToSignFunc value form_ =
                    if value then
                        form_.nonAdministrationSigns
                            |> Maybe.andThen
                                (\nonAdministrationSigns ->
                                    getCurrentReasonForMedicationNonAdministration reasonToSignFunc form_
                                        |> Maybe.map
                                            (\reason ->
                                                Just <| EverySet.remove (nonAdministrationReasonToSign medication reason) nonAdministrationSigns
                                            )
                                        |> Maybe.withDefault (Just nonAdministrationSigns)
                                )

                    else
                        form_.nonAdministrationSigns

                amoxicillinAdministration =
                    resolveAmoxicillinDosage currentDate person
                        |> Maybe.map
                            (\( numberOfPills, pillMass, duration ) ->
                                let
                                    amoxicillinUpdateFunc value form_ =
                                        { form_ | amoxicillin = Just value, nonAdministrationSigns = updateNonAdministrationSigns Amoxicillin MedicationAmoxicillin value form_ }

                                    derivedQuestionSection =
                                        case form.amoxicillin of
                                            Just False ->
                                                derivedQuestionInputsAndTasks Amoxicillin MedicationAmoxicillin

                                            _ ->
                                                ( [], [] )

                                    administeredMedicationQuestion =
                                        if pillMass == "500" then
                                            viewQuestionLabel language Translate.AdministeredOneOfAboveMedicinesQuestion

                                        else
                                            viewAdministeredMedicationQuestion language (Translate.MedicationDistributionSign Amoxicillin)
                                in
                                ( div [ class "instructions respiratory-infection-uncomplicated" ] <|
                                    viewAmoxicillinAdministrationInstructions language numberOfPills pillMass duration Nothing
                                , concatInputsAndTasksSections
                                    [ ( [ administeredMedicationQuestion
                                        , viewBoolInput
                                            language
                                            form.amoxicillin
                                            (SetMedicationDistributionBoolInput amoxicillinUpdateFunc)
                                            "amoxicillin-medication"
                                            Nothing
                                        ]
                                      , [ form.amoxicillin ]
                                      )
                                    , derivedQuestionSection
                                    ]
                                )
                            )
                        |> Maybe.withDefault ( emptyNode, ( [], [] ) )
            in
            case diagnosis of
                Just DiagnosisMalariaUncomplicated ->
                    let
                        coartemUpdateFunc value form_ =
                            { form_ | coartem = Just value, nonAdministrationSigns = updateNonAdministrationSigns Coartem MedicationCoartem value form_ }

                        derivedQuestionSection =
                            case form.coartem of
                                Just False ->
                                    derivedQuestionInputsAndTasks Coartem MedicationCoartem

                                _ ->
                                    ( [], [] )
                    in
                    ( resolveCoartemDosage currentDate person
                        |> Maybe.map
                            (\dosage ->
                                div [ class "instructions malaria-uncomplicated" ]
                                    [ viewAdministeredMedicationLabel language Translate.Administer (Translate.MedicationDistributionSign Coartem) "icon-pills" Nothing
                                    , viewTabletsPrescription language dosage (Translate.ByMouthTwiceADayForXDays 3)
                                    ]
                            )
                        |> Maybe.withDefault emptyNode
                    , concatInputsAndTasksSections
                        [ ( [ viewAdministeredMedicationQuestion language (Translate.MedicationDistributionSign Coartem)
                            , viewBoolInput
                                language
                                form.coartem
                                (SetMedicationDistributionBoolInput coartemUpdateFunc)
                                "coartem-medication"
                                Nothing
                            ]
                          , [ form.coartem ]
                          )
                        , derivedQuestionSection
                        ]
                    )

                Just DiagnosisGastrointestinalInfectionUncomplicated ->
                    let
                        orsUpdateFunc value form_ =
                            { form_ | ors = Just value, nonAdministrationSigns = updateNonAdministrationSigns ORS MedicationORS value form_ }

                        zincUpdateFunc value form_ =
                            { form_ | zinc = Just value, nonAdministrationSigns = updateNonAdministrationSigns Zinc MedicationZinc value form_ }

                        orsDerivedQuestionSection =
                            case form.ors of
                                Just False ->
                                    derivedQuestionInputsAndTasks ORS MedicationORS

                                _ ->
                                    ( [], [] )

                        zincDerivedQuestionSection =
                            case form.zinc of
                                Just False ->
                                    derivedQuestionInputsAndTasks Zinc MedicationZinc

                                _ ->
                                    ( [], [] )
                    in
                    ( Maybe.map2
                        (\orsDosage zincDosage ->
                            div [ class "instructions gastrointestinal-uncomplicated" ]
                                [ viewAdministeredMedicationLabel language Translate.Administer (Translate.MedicationDistributionSign ORS) "icon-oral-solution" Nothing
                                , viewOralSolutionPrescription language orsDosage
                                , viewAdministeredMedicationLabel language Translate.Administer (Translate.MedicationDistributionSign Zinc) "icon-pills" Nothing
                                , viewTabletsPrescription language zincDosage (Translate.ByMouthDailyForXDays 10)
                                ]
                        )
                        (resolveORSDosage currentDate person)
                        (resolveZincDosage currentDate person)
                        |> Maybe.withDefault emptyNode
                    , concatInputsAndTasksSections
                        [ ( [ viewAdministeredMedicationQuestion language (Translate.MedicationDistributionSign ORS)
                            , viewBoolInput
                                language
                                form.ors
                                (SetMedicationDistributionBoolInput orsUpdateFunc)
                                "ors-medication"
                                Nothing
                            ]
                          , [ form.ors ]
                          )
                        , orsDerivedQuestionSection
                        , ( [ viewAdministeredMedicationQuestion language (Translate.MedicationDistributionSign Zinc)
                            , viewBoolInput
                                language
                                form.zinc
                                (SetMedicationDistributionBoolInput zincUpdateFunc)
                                "zinc-medication"
                                Nothing
                            ]
                          , [ form.zinc ]
                          )
                        , zincDerivedQuestionSection
                        ]
                    )

                Just DiagnosisSimpleColdAndCough ->
                    let
                        lemonJuiceOrHoneyUpdateFunc value form_ =
                            { form_ | lemonJuiceOrHoney = Just value }
                    in
                    ( div [ class "instructions simple-cough-and-cold" ]
                        [ viewAdministeredMedicationLabel language Translate.Administer (Translate.MedicationDistributionSign LemonJuiceOrHoney) "icon-pills" Nothing ]
                    , ( [ viewAdministeredMedicationQuestion language (Translate.MedicationDistributionSign LemonJuiceOrHoney)
                        , viewBoolInput
                            language
                            form.lemonJuiceOrHoney
                            (SetMedicationDistributionBoolInput lemonJuiceOrHoneyUpdateFunc)
                            "lemon-juice-or-honey-medication"
                            Nothing
                        ]
                      , [ form.lemonJuiceOrHoney ]
                      )
                    )

                Just DiagnosisRespiratoryInfectionUncomplicated ->
                    amoxicillinAdministration

                Just DiagnosisPneuminialCovid19 ->
                    amoxicillinAdministration

                _ ->
                    ( emptyNode, ( [], [] ) )
    in
    ( [ h2 [] [ text <| translate language Translate.ActionsToTake ++ ":" ]
      , instructions
      ]
        ++ inputs
    , tasks
    )


viewAmoxicillinAdministrationInstructions : Language -> String -> String -> TranslationId -> Maybe NominalDate -> List (Html any)
viewAmoxicillinAdministrationInstructions language numberOfPills pillMassInMg duration maybeDate =
    let
        ( medicationLabelSuffix, prescription ) =
            if numberOfPills == "0.5" then
                ( " (" ++ (translate language <| Translate.HalfOfDosage pillMassInMg) ++ ")"
                , div [ class "prescription" ]
                    [ text <| translate language Translate.SeeDosageScheduleByWeight ]
                )

            else
                ( " (" ++ pillMassInMg ++ ")"
                , viewTabletsPrescription language numberOfPills duration
                )

        alternateMedicineSection =
            if pillMassInMg == "500" then
                [ p [ class "or" ] [ text <| translate language Translate.Or ]
                , viewAdministeredMedicationCustomLabel
                    language
                    Translate.Administer
                    Translate.MedicationDoxycycline
                    ""
                    "icon-pills"
                    ":"
                    maybeDate
                , viewTabletsPrescription language "1" (Translate.ByMouthTwiceADayForXDays 5)
                ]

            else
                []
    in
    [ viewAdministeredMedicationCustomLabel
        language
        Translate.Administer
        (Translate.MedicationDistributionSign Amoxicillin)
        medicationLabelSuffix
        "icon-pills"
        ":"
        maybeDate
    , prescription
    ]
        ++ alternateMedicineSection


viewParacetamolAdministrationInstructions : Language -> Maybe NominalDate -> Bool -> List (Html any)
viewParacetamolAdministrationInstructions language maybeDate isAdult =
    let
        ( medicationLabelSuffix, prescription ) =
            if isAdult then
                ( " (1g)", Translate.ParacetamolPrescriptionForAdult )

            else
                ( " (15mg per kg)", Translate.SeeDosageScheduleByWeight )
    in
    [ viewAdministeredMedicationCustomLabel
        language
        Translate.Administer
        (Translate.MedicationDistributionSign Paracetamol)
        medicationLabelSuffix
        "icon-pills"
        ":"
        maybeDate
    , div [ class "prescription" ]
        [ text <| translate language prescription ]
    ]


viewAdministeredMedicationLabel : Language -> TranslationId -> TranslationId -> String -> Maybe NominalDate -> Html any
viewAdministeredMedicationLabel language administerTranslationId medicineTranslationId iconClass maybeDate =
    viewAdministeredMedicationCustomLabel language administerTranslationId medicineTranslationId "" iconClass "." maybeDate


viewTabletsPrescription : Language -> String -> TranslationId -> Html any
viewTabletsPrescription language dosage duration =
    div [ class "prescription" ]
        [ span [] [ text <| translate language (Translate.TabletSinglePlural dosage) ]
        , text " "
        , text <| translate language duration
        , text "."
        ]


viewOralSolutionPrescription : Language -> String -> Html any
viewOralSolutionPrescription language dosage =
    div [ class "prescription" ]
        [ span [] [ text <| translate language (Translate.Glass dosage) ]
        , text " "
        , text <| translate language Translate.AfterEachLiquidStool
        , text "."
        ]


healthEducationFormInutsAndTasks :
    Language
    -> NominalDate
    -> Maybe AcuteIllnessDiagnosis
    -> HealthEducationForm
    -> ( List (Html Msg), List (Maybe Bool) )
healthEducationFormInutsAndTasks language currentDate maybeDiagnosis form =
    Maybe.map
        (\diagnosis ->
            let
                healthEducationSection =
                    let
                        providedHealthEducation =
                            Maybe.withDefault True form.educationForDiagnosis

                        reasonForNotProvidingHealthEducationSection =
                            if not providedHealthEducation then
                                let
                                    reasonForNotProvidingHealthEducationOptions =
                                        [ PatientNeedsEmergencyReferral
                                        , ReceivedEmergencyCase
                                        , LackOfAppropriateEducationUserGuide
                                        , PatientRefused
                                        ]
                                in
                                ( [ viewQuestionLabel language Translate.WhyNot
                                  , viewCheckBoxSelectInput language
                                        reasonForNotProvidingHealthEducationOptions
                                        []
                                        form.reasonForNotProvidingHealthEducation
                                        SetReasonForNotProvidingHealthEducation
                                        Translate.ReasonForNotProvidingHealthEducation
                                  ]
                                , [ maybeToBoolTask form.reasonForNotProvidingHealthEducation ]
                                )

                            else
                                ( [], [] )
                    in
                    concatInputsAndTasksSections
                        [ ( [ div [ class "label" ]
                                [ text <| translate language Translate.ProvidedPreventionEducationQuestion
                                , text " "
                                , text <| translate language <| Translate.AcuteIllnessDiagnosis diagnosis
                                , text "?"
                                , viewBoolInput
                                    language
                                    form.educationForDiagnosis
                                    SetProvidedEducationForDiagnosis
                                    "education-for-diagnosis"
                                    Nothing
                                ]
                            ]
                          , [ form.educationForDiagnosis ]
                          )
                        , reasonForNotProvidingHealthEducationSection
                        ]
            in
            concatInputsAndTasksSections
                [ ( [ h2 [] [ text <| translate language Translate.ActionsToTake ++ ":" ]
                    , div [ class "instructions" ]
                        [ viewHealthEducationLabel language
                            Translate.ProvideHealthEducation
                            (Translate.AcuteIllnessDiagnosis diagnosis)
                            "icon-open-book"
                            Nothing
                        ]
                    ]
                  , []
                  )
                , healthEducationSection
                ]
        )
        maybeDiagnosis
        |> Maybe.withDefault ( [], [] )


viewHealthEducationLabel : Language -> TranslationId -> TranslationId -> String -> Maybe NominalDate -> Html any
viewHealthEducationLabel language actionTranslationId diagnosisTranslationId iconClass maybeDate =
    let
        message =
            div [] <|
                [ text <| translate language actionTranslationId
                , text " "
                , span [] [ text <| translate language diagnosisTranslationId ]
                ]
                    ++ renderDatePart language maybeDate
                    ++ [ text "." ]
    in
    viewInstructionsLabel iconClass message


symptomsReliefFormInutsAndTasks :
    Language
    -> NominalDate
    -> HealthEducationForm
    -> ( List (Html Msg), List (Maybe Bool) )
symptomsReliefFormInutsAndTasks language currentDate form =
    let
        viewSymptomRelief symptomsRelief =
            li [] [ text <| translate language <| Translate.SymptomRelief symptomsRelief ]

        symptomsReliefList =
            [ SymptomReliefParacetamol
            , SymptomReliefVitaminC
            , SymptomReliefPaidoterineSyrup
            , SymptomReliefCoughMixture
            ]
    in
    ( [ viewCustomLabel language Translate.AcuteIllnessLowRiskCaseHelper "." "instructions"
      , viewLabel language Translate.RecommendedSymptomRelief
      , ul [] <|
            List.map viewSymptomRelief symptomsReliefList
      , viewQuestionLabel language Translate.ProvidedSymtomReliefGuidanceQuestion
      , viewBoolInput
            language
            form.educationForDiagnosis
            SetProvidedEducationForDiagnosis
            "education-for-diagnosis"
            Nothing
      ]
    , [ form.educationForDiagnosis ]
    )


followUpFormInutsAndTasks : Language -> NominalDate -> Bool -> FollowUpForm -> ( List (Html Msg), List (Maybe Bool) )
followUpFormInutsAndTasks language currentDate isChw form =
    let
        ( headerHelper, label ) =
            if isChw then
                ( [], Translate.FollowUpByChwLabel )

            else
                ( [ h2 [] [ text <| translate language Translate.ActionsToTake ++ ":" ]
                  , div [ class "instructions" ]
                        [ viewFollowUpLabel language Translate.AlertChwToFollowUp "icon-house"
                        ]
                  ]
                , Translate.FollowUpLabel
                )
    in
    ( headerHelper
        ++ [ viewLabel language label
           , viewCheckBoxSelectInput language
                [ OneDay, ThreeDays, OneWeek, TwoWeeks, FollowUpNotNeeded ]
                []
                form.option
                SetFollowUpOption
                Translate.FollowUpOption
           ]
    , [ maybeToBoolTask form.option ]
    )


viewFollowUpLabel : Language -> TranslationId -> String -> Html any
viewFollowUpLabel language actionTranslationId iconClass =
    let
        message =
            div [] [ text <| translate language actionTranslationId ++ "." ]
    in
    viewInstructionsLabel iconClass message


ongoingTreatmentTasksCompletedFromTotal : NominalDate -> AcuteIllnessMeasurements -> OngoingTreatmentData -> OngoingTreatmentTask -> ( Int, Int )
ongoingTreatmentTasksCompletedFromTotal currentDate measurements data task =
    case task of
        OngoingTreatmentReview ->
            let
                ( _, tasks ) =
                    getMeasurementValueFunc measurements.treatmentOngoing
                        |> ongoingTreatmentReviewFormWithDefault data.treatmentReviewForm
                        |> treatmentReviewInputsAndTasks English
                            currentDate
                            SetOngoingTreatmentReviewBoolInput
                            SetReasonForNotTaking
                            SetTotalMissedDoses
                            SetAdverseEvent
            in
            resolveTasksCompletedFromTotal tasks


dangerSignsTasksCompletedFromTotal : NominalDate -> AcuteIllnessMeasurements -> DangerSignsData -> DangerSignsTask -> ( Int, Int )
dangerSignsTasksCompletedFromTotal currentDate measurements data task =
    case task of
        ReviewDangerSigns ->
            let
                ( _, tasks ) =
                    getMeasurementValueFunc measurements.dangerSigns
                        |> reviewDangerSignsFormWithDefault data.reviewDangerSignsForm
                        |> reviewDangerSignsFormInutsAndTasks English currentDate
            in
            resolveTasksCompletedFromTotal tasks


reviewDangerSignsFormInutsAndTasks : Language -> NominalDate -> ReviewDangerSignsForm -> ( List (Html Msg), List (Maybe Bool) )
reviewDangerSignsFormInutsAndTasks language currentDate form =
    ( [ viewQuestionLabel language Translate.ConditionImprovingQuestion
      , viewBoolInput
            language
            form.conditionImproving
            SetConditionImproving
            "conditionImproving"
            Nothing
      , viewQuestionLabel language Translate.HaveAnyOfTheFollowingQuestion
      , viewCustomLabel language Translate.CheckAllThatApply "." "helper"
      , viewCheckBoxMultipleSelectInput language
            [ DangerSignUnableDrinkSuck
            , DangerSignVomiting
            , DangerSignConvulsions
            , DangerSignLethargyUnconsciousness
            , DangerSignRespiratoryDistress
            , DangerSignSpontaneousBleeding
            , DangerSignBloodyDiarrhea
            , DangerSignNewSkinRash
            , NoAcuteIllnessDangerSign
            ]
            []
            (form.symptoms |> Maybe.withDefault [])
            Nothing
            SetDangerSign
            Translate.AcuteIllnessDangerSign
      ]
    , [ form.conditionImproving, maybeToBoolTask form.symptoms ]
    )


taskNotCompleted : Bool -> Int
taskNotCompleted notCompleted =
    if notCompleted then
        0

    else
        1


naTaskCompleted : a -> Maybe a -> Int
naTaskCompleted na maybe =
    Maybe.map List.singleton maybe
        |> naListTaskCompleted na


naListTaskCompleted : a -> Maybe (List a) -> Int
naListTaskCompleted na maybeList =
    case maybeList of
        Just [ value ] ->
            if value == na then
                0

            else
                1

        _ ->
            taskCompleted maybeList


symptomsGeneralFormWithDefault : SymptomsGeneralForm -> Maybe (Dict SymptomsGeneralSign Int) -> SymptomsGeneralForm
symptomsGeneralFormWithDefault form saved =
    if form.signsDirty then
        form

    else
        saved
            |> unwrap
                form
                (\value ->
                    if Dict.isEmpty form.signs then
                        SymptomsGeneralForm value False

                    else
                        form
                )


toSymptomsGeneralValueWithDefault : Maybe (Dict SymptomsGeneralSign Int) -> SymptomsGeneralForm -> Dict SymptomsGeneralSign Int
toSymptomsGeneralValueWithDefault saved form =
    symptomsGeneralFormWithDefault form saved
        |> .signs


symptomsRespiratoryFormWithDefault : SymptomsRespiratoryForm -> Maybe (Dict SymptomsRespiratorySign Int) -> SymptomsRespiratoryForm
symptomsRespiratoryFormWithDefault form saved =
    if form.signsDirty then
        form

    else
        saved
            |> unwrap
                form
                (\value ->
                    if Dict.isEmpty form.signs then
                        SymptomsRespiratoryForm value False

                    else
                        form
                )


toSymptomsRespiratoryValueWithDefault : Maybe (Dict SymptomsRespiratorySign Int) -> SymptomsRespiratoryForm -> Dict SymptomsRespiratorySign Int
toSymptomsRespiratoryValueWithDefault saved form =
    symptomsRespiratoryFormWithDefault form saved
        |> .signs


symptomsGIFormWithDefault : SymptomsGIForm -> Maybe SymptomsGIValue -> SymptomsGIForm
symptomsGIFormWithDefault form saved =
    saved
        |> unwrap
            form
            (\value ->
                let
                    signs =
                        if form.signsDirty then
                            form.signs

                        else if Dict.isEmpty form.signs then
                            value.signs

                        else
                            form.signs

                    intractableVomiting =
                        if form.intractableVomitingDirty then
                            form.intractableVomiting

                        else if isJust form.intractableVomiting then
                            form.intractableVomiting

                        else if EverySet.member IntractableVomiting value.derivedSigns then
                            Just True

                        else
                            Just False
                in
                { signs = signs
                , signsDirty = form.signsDirty
                , intractableVomiting = intractableVomiting
                , intractableVomitingDirty = form.intractableVomitingDirty
                }
            )


toSymptomsGIValueWithDefault : Maybe SymptomsGIValue -> SymptomsGIForm -> SymptomsGIValue
toSymptomsGIValueWithDefault saved form =
    let
        formWithDefault =
            symptomsGIFormWithDefault form saved

        derivedSigns =
            if Dict.member Vomiting formWithDefault.signs && formWithDefault.intractableVomiting == Just True then
                EverySet.singleton IntractableVomiting

            else
                EverySet.singleton NoSymptomsGIDerived
    in
    { signs = formWithDefault.signs
    , derivedSigns = derivedSigns
    }


fromAcuteFindingsValue : Maybe AcuteFindingsValue -> AcuteFindingsForm
fromAcuteFindingsValue saved =
    { signsGeneral = Maybe.map (.signsGeneral >> EverySet.toList) saved
    , signsRespiratory = Maybe.map (.signsRespiratory >> EverySet.toList) saved
    }


acuteFindingsFormWithDefault : AcuteFindingsForm -> Maybe AcuteFindingsValue -> AcuteFindingsForm
acuteFindingsFormWithDefault form saved =
    saved
        |> unwrap
            form
            (\value ->
                { signsGeneral = or form.signsGeneral (EverySet.toList value.signsGeneral |> Just)
                , signsRespiratory = or form.signsRespiratory (EverySet.toList value.signsRespiratory |> Just)
                }
            )


toAcuteFindingsValueWithDefault : Maybe AcuteFindingsValue -> AcuteFindingsForm -> Maybe AcuteFindingsValue
toAcuteFindingsValueWithDefault saved form =
    acuteFindingsFormWithDefault form saved
        |> toAcuteFindingsValue


toAcuteFindingsValue : AcuteFindingsForm -> Maybe AcuteFindingsValue
toAcuteFindingsValue form =
    let
        signsGeneralSet =
            form.signsGeneral
                |> Maybe.map (EverySet.fromList >> ifEverySetEmpty NoAcuteFindingsGeneralSigns)

        signsRespiratorySet =
            form.signsRespiratory
                |> Maybe.map (EverySet.fromList >> ifEverySetEmpty NoAcuteFindingsRespiratorySigns)
    in
    Maybe.map AcuteFindingsValue signsGeneralSet
        |> andMap signsRespiratorySet


fromMalariaTestingValue : Maybe RapidTestResult -> MalariaTestingForm
fromMalariaTestingValue saved =
    if saved == Just RapidTestPositiveAndPregnant then
        { rapidTestResult = Just RapidTestPositive
        , isPregnant = Just True
        }

    else
        { rapidTestResult = saved
        , isPregnant = Just False
        }


malariaTestingFormWithDefault : MalariaTestingForm -> Maybe RapidTestResult -> MalariaTestingForm
malariaTestingFormWithDefault form saved =
    saved
        |> unwrap
            form
            (\_ ->
                let
                    formWithDefault =
                        fromMalariaTestingValue saved
                in
                { rapidTestResult = or form.rapidTestResult formWithDefault.rapidTestResult
                , isPregnant = or form.isPregnant formWithDefault.isPregnant
                }
            )


toMalariaTestingValueWithDefault : Maybe RapidTestResult -> MalariaTestingForm -> Maybe RapidTestResult
toMalariaTestingValueWithDefault saved form =
    malariaTestingFormWithDefault form saved
        |> (\form_ ->
                if form_.rapidTestResult == Just RapidTestPositive && form_.isPregnant == Just True then
                    { form_ | rapidTestResult = Just RapidTestPositiveAndPregnant }

                else
                    form_
           )
        |> toMalariaTestingValue


toMalariaTestingValue : MalariaTestingForm -> Maybe RapidTestResult
toMalariaTestingValue form =
    form.rapidTestResult


fromTravelHistoryValue : Maybe (EverySet TravelHistorySign) -> TravelHistoryForm
fromTravelHistoryValue saved =
    { covid19Country = Maybe.map (EverySet.member COVID19Country) saved
    }


travelHistoryFormWithDefault : TravelHistoryForm -> Maybe (EverySet TravelHistorySign) -> TravelHistoryForm
travelHistoryFormWithDefault form saved =
    saved
        |> unwrap
            form
            (\value ->
                { covid19Country = or form.covid19Country (EverySet.member COVID19Country value |> Just)
                }
            )


toTravelHistoryValueWithDefault : Maybe (EverySet TravelHistorySign) -> TravelHistoryForm -> Maybe (EverySet TravelHistorySign)
toTravelHistoryValueWithDefault saved form =
    travelHistoryFormWithDefault form saved
        |> toTravelHistoryValue


toTravelHistoryValue : TravelHistoryForm -> Maybe (EverySet TravelHistorySign)
toTravelHistoryValue form =
    [ Maybe.map (ifTrue COVID19Country) form.covid19Country ]
        |> Maybe.Extra.combine
        |> Maybe.map (List.foldl EverySet.union EverySet.empty >> ifEverySetEmpty NoTravelHistorySigns)


fromExposureValue : Maybe (EverySet ExposureSign) -> ExposureForm
fromExposureValue saved =
    { covid19Symptoms = Maybe.map (EverySet.member COVID19Symptoms) saved
    }


exposureFormWithDefault : ExposureForm -> Maybe (EverySet ExposureSign) -> ExposureForm
exposureFormWithDefault form saved =
    saved
        |> unwrap
            form
            (\value ->
                { covid19Symptoms = or form.covid19Symptoms (EverySet.member COVID19Symptoms value |> Just)
                }
            )


toExposureValueWithDefault : Maybe (EverySet ExposureSign) -> ExposureForm -> Maybe (EverySet ExposureSign)
toExposureValueWithDefault saved form =
    exposureFormWithDefault form saved
        |> toExposureValue


toExposureValue : ExposureForm -> Maybe (EverySet ExposureSign)
toExposureValue form =
    [ Maybe.map (ifTrue COVID19Symptoms) form.covid19Symptoms ]
        |> Maybe.Extra.combine
        |> Maybe.map (List.foldl EverySet.union EverySet.empty >> ifEverySetEmpty NoExposureSigns)


fromIsolationValue : Maybe IsolationValue -> IsolationForm
fromIsolationValue saved =
    { patientIsolated = Maybe.map (.signs >> EverySet.member PatientIsolated) saved
    , signOnDoor = Maybe.map (.signs >> EverySet.member SignOnDoor) saved
    , healthEducation = Maybe.map (.signs >> EverySet.member HealthEducation) saved
    , reasonsForNotIsolating = Maybe.map (.reasonsForNotIsolating >> EverySet.toList) saved
    }


isolationFormWithDefault : IsolationForm -> Maybe IsolationValue -> IsolationForm
isolationFormWithDefault form saved =
    saved
        |> unwrap
            form
            (\value ->
                { patientIsolated = or form.patientIsolated (EverySet.member PatientIsolated value.signs |> Just)
                , signOnDoor = or form.signOnDoor (EverySet.member SignOnDoor value.signs |> Just)
                , healthEducation = or form.healthEducation (EverySet.member HealthEducation value.signs |> Just)
                , reasonsForNotIsolating = or form.reasonsForNotIsolating (value.reasonsForNotIsolating |> EverySet.toList |> Just)
                }
            )


toIsolationValueWithDefault : Maybe IsolationValue -> IsolationForm -> Maybe IsolationValue
toIsolationValueWithDefault saved form =
    isolationFormWithDefault form saved
        |> toIsolationValue
        |> isolationValuePostProcess


toIsolationValue : IsolationForm -> Maybe IsolationValue
toIsolationValue form =
    let
        signs =
            [ Maybe.map (ifTrue PatientIsolated) form.patientIsolated
            , ifNullableTrue SignOnDoor form.signOnDoor
            , Maybe.map (ifTrue HealthEducation) form.healthEducation
            ]
                |> Maybe.Extra.combine
                |> Maybe.map (List.foldl EverySet.union EverySet.empty >> ifEverySetEmpty NoIsolationSigns)

        reasonsForNotIsolating =
            form.reasonsForNotIsolating
                |> fromListWithDefaultValue IsolationReasonNotApplicable
                |> Just
    in
    Maybe.map IsolationValue signs
        |> andMap reasonsForNotIsolating


isolationValuePostProcess : Maybe IsolationValue -> Maybe IsolationValue
isolationValuePostProcess saved =
    saved
        |> Maybe.map
            (\value ->
                if EverySet.member PatientIsolated value.signs then
                    { value | reasonsForNotIsolating = EverySet.singleton IsolationReasonNotApplicable }

                else
                    { value | signs = EverySet.remove SignOnDoor value.signs }
            )


fromHCContactValue : Maybe HCContactValue -> HCContactForm
fromHCContactValue saved =
    { contactedHC = Maybe.map (.signs >> EverySet.member ContactedHealthCenter) saved
    , recommendations = Maybe.andThen (.recommendations >> EverySet.toList >> List.head) saved
    , responsePeriod = Maybe.andThen (.responsePeriod >> EverySet.toList >> List.head) saved
    , ambulanceArrivalPeriod = Maybe.andThen (.ambulanceArrivalPeriod >> EverySet.toList >> List.head) saved
    }


hcContactFormWithDefault : HCContactForm -> Maybe HCContactValue -> HCContactForm
hcContactFormWithDefault form saved =
    saved
        |> unwrap
            form
            (\value ->
                { contactedHC = or form.contactedHC (EverySet.member ContactedHealthCenter value.signs |> Just)
                , recommendations = or form.recommendations (value.recommendations |> EverySet.toList |> List.head)
                , responsePeriod = or form.responsePeriod (value.responsePeriod |> EverySet.toList |> List.head)
                , ambulanceArrivalPeriod = or form.ambulanceArrivalPeriod (value.ambulanceArrivalPeriod |> EverySet.toList |> List.head)
                }
            )


toHCContactValueWithDefault : Maybe HCContactValue -> HCContactForm -> Maybe HCContactValue
toHCContactValueWithDefault saved form =
    hcContactFormWithDefault form saved
        |> toHCContactValue
        |> hcContactValuePostProcess


toHCContactValue : HCContactForm -> Maybe HCContactValue
toHCContactValue form =
    let
        signs =
            [ Maybe.map (ifTrue ContactedHealthCenter) form.contactedHC ]
                |> Maybe.Extra.combine
                |> Maybe.map (List.foldl EverySet.union EverySet.empty >> ifEverySetEmpty NoHCContactSigns)
    in
    Maybe.map HCContactValue signs
        |> andMap (form.recommendations |> withDefaultValue HCRecommendationNotApplicable |> Just)
        |> andMap (form.responsePeriod |> withDefaultValue ResponsePeriodNotApplicable |> Just)
        |> andMap (form.ambulanceArrivalPeriod |> withDefaultValue ResponsePeriodNotApplicable |> Just)


hcContactValuePostProcess : Maybe HCContactValue -> Maybe HCContactValue
hcContactValuePostProcess saved =
    saved
        |> Maybe.map
            (\value ->
                if EverySet.member ContactedHealthCenter value.signs then
                    if EverySet.member SendAmbulance value.recommendations then
                        value

                    else
                        { value | ambulanceArrivalPeriod = EverySet.singleton ResponsePeriodNotApplicable }

                else
                    { value
                        | recommendations = EverySet.singleton HCRecommendationNotApplicable
                        , responsePeriod = EverySet.singleton ResponsePeriodNotApplicable
                        , ambulanceArrivalPeriod = EverySet.singleton ResponsePeriodNotApplicable
                    }
            )


fromCall114Value : Maybe Call114Value -> Call114Form
fromCall114Value saved =
    { called114 = Maybe.map (.signs >> EverySet.member Call114) saved
    , recommendation114 = Maybe.andThen (.recommendations114 >> EverySet.toList >> List.head) saved
    , recommendation114Dirty = False
    , contactedSite = Maybe.map (.signs >> EverySet.member ContactSite) saved
    , contactedSiteDirty = False
    , recommendationSite = Maybe.andThen (.recommendationsSite >> EverySet.toList >> List.head) saved
    , recommendationSiteDirty = False
    }


call114FormWithDefault : Call114Form -> Maybe Call114Value -> Call114Form
call114FormWithDefault form saved =
    saved
        |> unwrap
            form
            (\value ->
                { called114 = or form.called114 (EverySet.member Call114 value.signs |> Just)
                , recommendation114 =
                    maybeValueConsideringIsDirtyField form.recommendation114Dirty form.recommendation114 (value.recommendations114 |> EverySet.toList |> List.head)
                , recommendation114Dirty = form.recommendation114Dirty
                , contactedSite =
                    valueConsideringIsDirtyField form.contactedSiteDirty form.contactedSite (EverySet.member ContactSite value.signs)
                , contactedSiteDirty = form.contactedSiteDirty
                , recommendationSite =
                    maybeValueConsideringIsDirtyField form.recommendationSiteDirty form.recommendationSite (value.recommendationsSite |> EverySet.toList |> List.head)
                , recommendationSiteDirty = form.recommendationSiteDirty
                }
            )


toCall114ValueWithDefault : Maybe Call114Value -> Call114Form -> Maybe Call114Value
toCall114ValueWithDefault saved form =
    call114FormWithDefault form saved
        |> toCall114Value
        |> call114ValuePostProcess


toCall114Value : Call114Form -> Maybe Call114Value
toCall114Value form =
    let
        signs =
            [ Maybe.map (ifTrue Call114) form.called114
            , ifNullableTrue ContactSite form.contactedSite
            ]
                |> Maybe.Extra.combine
                |> Maybe.map (List.foldl EverySet.union EverySet.empty >> ifEverySetEmpty NoCall114Signs)
    in
    Maybe.map Call114Value signs
        |> andMap (form.recommendation114 |> withDefaultValue NoneOtherRecommendation114 |> Just)
        |> andMap (form.recommendationSite |> withDefaultValue RecommendationSiteNotApplicable |> Just)


call114ValuePostProcess : Maybe Call114Value -> Maybe Call114Value
call114ValuePostProcess saved =
    saved
        |> Maybe.map
            (\value ->
                let
                    recommendationSiteNotApplicable =
                        { value | recommendationsSite = EverySet.singleton RecommendationSiteNotApplicable }
                in
                if EverySet.member Call114 value.signs then
                    --  114 did not recomment to contact a site.
                    if EverySet.member OtherRecommendation114 value.recommendations114 then
                        recommendationSiteNotApplicable

                    else
                        value

                else
                    -- There was no attempt to contact 114.
                    recommendationSiteNotApplicable
            )


fromTreatmentReviewValue : Maybe (EverySet TreatmentReviewSign) -> TreatmentReviewForm
fromTreatmentReviewValue saved =
    { feverPast6Hours = Maybe.map (EverySet.member FeverPast6Hours) saved
    , feverPast6HoursHelped = Maybe.map (EverySet.member FeverPast6HoursHelped) saved
    , malariaToday = Maybe.map (EverySet.member MalariaToday) saved
    , malariaTodayHelped = Maybe.map (EverySet.member MalariaTodayHelped) saved
    , malariaWithinPastMonth = Maybe.map (EverySet.member MalariaWithinPastMonth) saved
    , malariaWithinPastMonthHelped = Maybe.map (EverySet.member MalariaWithinPastMonthHelped) saved
    }


treatmentReviewFormWithDefault : TreatmentReviewForm -> Maybe (EverySet TreatmentReviewSign) -> TreatmentReviewForm
treatmentReviewFormWithDefault form saved =
    saved
        |> unwrap
            form
            (\value ->
                { feverPast6Hours = or form.feverPast6Hours (EverySet.member FeverPast6Hours value |> Just)
                , feverPast6HoursHelped = or form.feverPast6HoursHelped (EverySet.member FeverPast6HoursHelped value |> Just)
                , malariaToday = or form.malariaToday (EverySet.member MalariaToday value |> Just)
                , malariaTodayHelped = or form.malariaTodayHelped (EverySet.member MalariaTodayHelped value |> Just)
                , malariaWithinPastMonth = or form.malariaWithinPastMonth (EverySet.member MalariaWithinPastMonth value |> Just)
                , malariaWithinPastMonthHelped = or form.malariaWithinPastMonthHelped (EverySet.member MalariaWithinPastMonthHelped value |> Just)
                }
            )


toTreatmentReviewValueWithDefault : Maybe (EverySet TreatmentReviewSign) -> TreatmentReviewForm -> Maybe (EverySet TreatmentReviewSign)
toTreatmentReviewValueWithDefault saved form =
    treatmentReviewFormWithDefault form saved
        |> toTreatmentReviewValue


toTreatmentReviewValue : TreatmentReviewForm -> Maybe (EverySet TreatmentReviewSign)
toTreatmentReviewValue form =
    [ Maybe.map (ifTrue FeverPast6Hours) form.feverPast6Hours
    , ifNullableTrue FeverPast6HoursHelped form.feverPast6HoursHelped
    , Maybe.map (ifTrue MalariaToday) form.malariaToday
    , ifNullableTrue MalariaTodayHelped form.malariaTodayHelped
    , Maybe.map (ifTrue MalariaWithinPastMonth) form.malariaWithinPastMonth
    , ifNullableTrue MalariaWithinPastMonthHelped form.malariaWithinPastMonthHelped
    ]
        |> Maybe.Extra.combine
        |> Maybe.map (List.foldl EverySet.union EverySet.empty >> ifEverySetEmpty NoTreatmentReviewSigns)


medicationDistributionFormWithDefault : MedicationDistributionForm -> Maybe MedicationDistributionValue -> MedicationDistributionForm
medicationDistributionFormWithDefault form saved =
    saved
        |> unwrap
            form
            (\value ->
                { amoxicillin = or form.amoxicillin (EverySet.member Amoxicillin value.distributionSigns |> Just)
                , coartem = or form.coartem (EverySet.member Coartem value.distributionSigns |> Just)
                , ors = or form.ors (EverySet.member ORS value.distributionSigns |> Just)
                , zinc = or form.zinc (EverySet.member Zinc value.distributionSigns |> Just)
                , lemonJuiceOrHoney = or form.lemonJuiceOrHoney (EverySet.member LemonJuiceOrHoney value.distributionSigns |> Just)
                , nonAdministrationSigns = or form.nonAdministrationSigns (Just value.nonAdministrationSigns)
                }
            )


toMedicationDistributionValueWithDefault : Maybe MedicationDistributionValue -> MedicationDistributionForm -> Maybe MedicationDistributionValue
toMedicationDistributionValueWithDefault saved form =
    medicationDistributionFormWithDefault form saved
        |> toMedicationDistributionValue


toMedicationDistributionValue : MedicationDistributionForm -> Maybe MedicationDistributionValue
toMedicationDistributionValue form =
    let
        distributionSigns =
            [ ifNullableTrue Amoxicillin form.amoxicillin
            , ifNullableTrue Coartem form.coartem
            , ifNullableTrue ORS form.ors
            , ifNullableTrue Zinc form.zinc
            , ifNullableTrue LemonJuiceOrHoney form.lemonJuiceOrHoney
            ]
                |> Maybe.Extra.combine
                |> Maybe.map (List.foldl EverySet.union EverySet.empty >> ifEverySetEmpty NoMedicationDistributionSigns)

        nonAdministrationSigns =
            form.nonAdministrationSigns
                |> Maybe.withDefault EverySet.empty
                |> ifEverySetEmpty NoMedicationNonAdministrationSigns
                |> Just
    in
    Maybe.map MedicationDistributionValue distributionSigns
        |> andMap nonAdministrationSigns


resolveCoartemDosage : NominalDate -> Person -> Maybe String
resolveCoartemDosage currentDate person =
    ageInYears currentDate person
        |> Maybe.map
            (\years ->
                if years < 3 then
                    "1"

                else if years < 8 then
                    "2"

                else if years < 14 then
                    "3"

                else
                    "4"
            )


resolveORSDosage : NominalDate -> Person -> Maybe String
resolveORSDosage currentDate person =
    ageInYears currentDate person
        |> Maybe.map
            (\years ->
                if years < 2 then
                    "½"

                else
                    "1"
            )


resolveZincDosage : NominalDate -> Person -> Maybe String
resolveZincDosage currentDate person =
    ageInMonths currentDate person
        |> Maybe.map
            (\months ->
                if months < 6 then
                    "1"

                else
                    "2"
            )


resolveAmoxicillinDosage : NominalDate -> Person -> Maybe ( String, String, TranslationId )
resolveAmoxicillinDosage currentDate person =
    ageInMonths currentDate person
        |> Maybe.map
            (\months ->
                if months < 2 then
                    ( "0.5", "125", Translate.SeeDosageScheduleByWeight )

                else if months < 5 then
                    ( "1", "125", Translate.ByMouthTwiceADayForXDays 5 )

                else if months < 12 then
                    ( "2", "125", Translate.ByMouthTwiceADayForXDays 5 )

                else if months < round (2.5 * 12) then
                    ( "3", "125", Translate.ByMouthTwiceADayForXDays 5 )

                else if months < (15 * 12) then
                    ( "4", "125", Translate.ByMouthTwiceADayForXDays 5 )

                else
                    ( "1", "500", Translate.ByMouthThreeTimesADayForXDays 5 )
            )


fromReviewDangerSignsValue : Maybe (EverySet AcuteIllnessDangerSign) -> ReviewDangerSignsForm
fromReviewDangerSignsValue saved =
    { conditionImproving = Maybe.map (EverySet.member DangerSignConditionNotImproving >> not) saved
    , symptoms =
        Maybe.map
            (EverySet.remove DangerSignConditionNotImproving
                >> ifEverySetEmpty NoAcuteIllnessDangerSign
                >> EverySet.toList
            )
            saved
    }


reviewDangerSignsFormWithDefault : ReviewDangerSignsForm -> Maybe (EverySet AcuteIllnessDangerSign) -> ReviewDangerSignsForm
reviewDangerSignsFormWithDefault form saved =
    saved
        |> unwrap
            form
            (\value ->
                { conditionImproving = or form.conditionImproving (EverySet.member DangerSignConditionNotImproving value |> not |> Just)
                , symptoms =
                    or form.symptoms
                        (EverySet.remove DangerSignConditionNotImproving value
                            |> ifEverySetEmpty NoAcuteIllnessDangerSign
                            |> EverySet.toList
                            |> Just
                        )
                }
            )


toReviewDangerSignsValueWithDefault : Maybe (EverySet AcuteIllnessDangerSign) -> ReviewDangerSignsForm -> Maybe (EverySet AcuteIllnessDangerSign)
toReviewDangerSignsValueWithDefault saved form =
    reviewDangerSignsFormWithDefault form saved
        |> toReviewDangerSignsValue


toReviewDangerSignsValue : ReviewDangerSignsForm -> Maybe (EverySet AcuteIllnessDangerSign)
toReviewDangerSignsValue form =
    Maybe.map2
        (\conditionImproving symptoms ->
            let
                conditionNotImprovingSet =
                    if conditionImproving then
                        EverySet.empty

                    else
                        EverySet.singleton DangerSignConditionNotImproving

                symptomsSet =
                    if List.member NoAcuteIllnessDangerSign symptoms && (not <| EverySet.isEmpty conditionNotImprovingSet) then
                        EverySet.empty

                    else
                        EverySet.fromList symptoms
            in
            EverySet.union conditionNotImprovingSet symptomsSet
        )
        form.conditionImproving
        form.symptoms


expectPhysicalExamTask : NominalDate -> Person -> Bool -> Bool -> PhysicalExamTask -> Bool
expectPhysicalExamTask currentDate person isChw isFirstEncounter task =
    case task of
        PhysicalExamVitals ->
            True

        PhysicalExamCoreExam ->
            not isChw

        -- We show Muac for children of 6 months to 5 years old.
        PhysicalExamMuac ->
            ageInMonths currentDate person
                |> Maybe.map (\ageMonths -> ageMonths > 5 && ageMonths < 60)
                |> Maybe.withDefault False

        -- We show Nutrition for children under age of 5.
        PhysicalExamNutrition ->
            isChildUnderAgeOf5 currentDate person

        -- We show Acute Finding only on first encounter
        PhysicalExamAcuteFindings ->
            isFirstEncounter


fromNutritionValue : Maybe (EverySet ChildNutritionSign) -> AcuteIllnessNutritionForm
fromNutritionValue saved =
    { signs = Maybe.map EverySet.toList saved }


nutritionFormWithDefault : AcuteIllnessNutritionForm -> Maybe (EverySet ChildNutritionSign) -> AcuteIllnessNutritionForm
nutritionFormWithDefault form saved =
    saved
        |> unwrap
            form
            (\value ->
                { signs = or form.signs (EverySet.toList value |> Just) }
            )


toNutritionValueWithDefault : Maybe (EverySet ChildNutritionSign) -> AcuteIllnessNutritionForm -> Maybe (EverySet ChildNutritionSign)
toNutritionValueWithDefault saved form =
    nutritionFormWithDefault form saved
        |> toNutritionValue


toNutritionValue : AcuteIllnessNutritionForm -> Maybe (EverySet ChildNutritionSign)
toNutritionValue form =
    Maybe.map (EverySet.fromList >> ifEverySetEmpty NormalChildNutrition) form.signs


fromCoreExamValue : Maybe AcuteIllnessCoreExamValue -> AcuteIllnessCoreExamForm
fromCoreExamValue saved =
    { heart = Maybe.andThen (.heart >> EverySet.toList >> List.head) saved
    , lungs = Maybe.map (.lungs >> EverySet.toList) saved
    }


coreExamFormWithDefault : AcuteIllnessCoreExamForm -> Maybe AcuteIllnessCoreExamValue -> AcuteIllnessCoreExamForm
coreExamFormWithDefault form saved =
    saved
        |> unwrap
            form
            (\value ->
                { heart = or form.heart (value.heart |> EverySet.toList |> List.head)
                , lungs = or form.lungs (value.lungs |> EverySet.toList |> Just)
                }
            )


toCoreExamValueWithDefault : Maybe AcuteIllnessCoreExamValue -> AcuteIllnessCoreExamForm -> Maybe AcuteIllnessCoreExamValue
toCoreExamValueWithDefault saved form =
    coreExamFormWithDefault form saved
        |> toCoreExamValue


toCoreExamValue : AcuteIllnessCoreExamForm -> Maybe AcuteIllnessCoreExamValue
toCoreExamValue form =
    Maybe.map AcuteIllnessCoreExamValue (Maybe.map EverySet.singleton form.heart)
        |> andMap (Maybe.map EverySet.fromList form.lungs)


fromCovidTestingValue : Maybe CovidTestingValue -> CovidTestingForm
fromCovidTestingValue saved =
    Maybe.map
        (\value ->
            let
                testPerformed =
                    value.result /= RapidTestUnableToRun |> Just

                testPositive =
                    case value.result of
                        RapidTestPositive ->
                            Just True

                        RapidTestPositiveAndPregnant ->
                            Just True

                        RapidTestNegative ->
                            Just False

                        _ ->
                            Nothing

                isPregnant =
                    Just <|
                        rapidTestPositive value.result
            in
            { testPerformed = testPerformed
            , testPositive = testPositive
            , isPregnant = isPregnant
            , administrationNote = value.administrationNote
            }
        )
        saved
        |> Maybe.withDefault emptyCovidTestingForm


covidTestingFormWithDefault : CovidTestingForm -> Maybe CovidTestingValue -> CovidTestingForm
covidTestingFormWithDefault form saved =
    saved
        |> unwrap
            form
            (\_ ->
                let
                    formWithDefault =
                        fromCovidTestingValue saved
                in
                { testPerformed = or form.testPerformed formWithDefault.testPerformed
                , testPositive = or form.testPositive formWithDefault.testPositive
                , isPregnant = or form.isPregnant formWithDefault.isPregnant
                , administrationNote = or form.administrationNote formWithDefault.administrationNote
                }
            )


toCovidTestingValueWithDefault : Maybe CovidTestingValue -> CovidTestingForm -> Maybe CovidTestingValue
toCovidTestingValueWithDefault saved form =
    covidTestingFormWithDefault form saved
        |> toCovidTestingValue


toCovidTestingValue : CovidTestingForm -> Maybe CovidTestingValue
toCovidTestingValue form =
    let
        maybeResult =
            Maybe.andThen
                (\testPerformed ->
                    if testPerformed then
                        case ( form.testPositive, form.isPregnant ) of
                            ( Just True, Just True ) ->
                                Just RapidTestPositiveAndPregnant

                            ( Just True, _ ) ->
                                Just RapidTestPositive

                            ( Just False, _ ) ->
                                Just RapidTestNegative

                            _ ->
                                Nothing

                    else if form.isPregnant == Just True then
                        Just RapidTestUnableToRunAndPregnant

                    else
                        Just RapidTestUnableToRun
                )
                form.testPerformed
    in
    Maybe.map
        (\result -> CovidTestingValue result form.administrationNote)
        maybeResult


expectLaboratoryTask : NominalDate -> Bool -> AssembledData -> AILaboratoryTask -> Bool
expectLaboratoryTask currentDate isChw assembled task =
    case task of
        LaboratoryMalariaTesting ->
            let
                covidNotDiagnosed =
                    Maybe.map (Tuple.second >> covid19Diagnosed >> not)
                        assembled.diagnosis
                        |> -- No diagnosis, so we need to display the task.
                           Maybe.withDefault True
            in
            if assembled.initialEncounter then
                -- Fever is mandatory condition to show Malaria RDT task.
                feverRecorded assembled.measurements
                    && ((-- Show Malaria RDT if mandatory data collected, unless
                         -- we have a confirmed Covid case.
                         mandatoryActivitiesCompletedFirstEncounter currentDate assembled.person isChw assembled.measurements
                            && covidNotDiagnosed
                        )
                            || (-- Show Malaria RDT if Covid test was suggested,
                                -- but not performed.
                                covidRapidTestResult assembled.measurements
                                    |> Maybe.map (\result -> List.member result [ RapidTestUnableToRun, RapidTestUnableToRunAndPregnant ])
                                    |> Maybe.withDefault False
                               )
                       )

            else
                let
                    initialWithSubsequent =
                        if List.isEmpty assembled.secondInitialWithSubsequent then
                            assembled.firstInitialWithSubsequent

                        else
                            assembled.secondInitialWithSubsequent
                in
                -- If patient was not diagnosed with Covid, and fever is recorded
                -- on current encounter, and patient did not test positive
                -- to Malaria during one of previous encounters,
                -- we want patient to take Malaria test.
                covidNotDiagnosed
                    && feverRecorded assembled.measurements
                    && (List.filter
                            (.measurements
                                >> .malariaTesting
                                >> getMeasurementValueFunc
                                >> Maybe.map rapidTestPositive
                                >> Maybe.withDefault False
                            )
                            initialWithSubsequent
                            |> List.isEmpty
                       )

        LaboratoryCovidTesting ->
            not isChw
                && covid19SuspectDiagnosed assembled.measurements


covid19Diagnosed : AcuteIllnessDiagnosis -> Bool
covid19Diagnosed diagnosis =
    List.member diagnosis [ DiagnosisSevereCovid19, DiagnosisPneuminialCovid19, DiagnosisLowRiskCovid19 ]


laboratoryTaskCompleted : NominalDate -> Bool -> AssembledData -> AILaboratoryTask -> Bool
laboratoryTaskCompleted currentDate isChw assembled task =
    let
        measurements =
            assembled.measurements

        taskExpected =
            expectLaboratoryTask currentDate isChw assembled
    in
    case task of
        LaboratoryMalariaTesting ->
            (not <| taskExpected LaboratoryMalariaTesting) || isJust measurements.malariaTesting

        LaboratoryCovidTesting ->
            (not <| taskExpected LaboratoryCovidTesting) || isJust measurements.covidTesting


laboratoryTasks : List AILaboratoryTask
laboratoryTasks =
    [ LaboratoryCovidTesting, LaboratoryMalariaTesting ]


resolveNextStepFirstEncounter : NominalDate -> Bool -> AssembledData -> Maybe NextStepsTask
resolveNextStepFirstEncounter currentDate isChw assembled =
    resolveNextStepsTasks currentDate isChw assembled
        |> List.head


resolveNextStepSubsequentEncounter : NominalDate -> Bool -> AssembledData -> Maybe NextStepsTask
resolveNextStepSubsequentEncounter currentDate isChw assembled =
    resolveNextStepsTasks currentDate isChw assembled
        |> List.head


resolveNextStepsTasks : NominalDate -> Bool -> AssembledData -> List NextStepsTask
resolveNextStepsTasks currentDate isChw assembled =
    if assembled.initialEncounter then
        -- The order is important. Do not change.
        [ NextStepsContactTracing, NextStepsMedicationDistribution, NextStepsSendToHC, NextStepsSymptomsReliefGuidance, NextStepsFollowUp ]
            |> List.filter (expectNextStepsTask currentDate isChw assembled)

    else if mandatoryActivitiesCompletedSubsequentVisit currentDate isChw assembled then
        -- The order is important. Do not change.
        [ NextStepsMedicationDistribution, NextStepsSendToHC, NextStepsHealthEducation, NextStepsFollowUp ]
            |> List.filter (expectNextStepsTask currentDate isChw assembled)

    else
        []


expectNextStepsTask : NominalDate -> Bool -> AssembledData -> NextStepsTask -> Bool
expectNextStepsTask currentDate isChw assembled task =
    let
        diagnosis =
            Maybe.map Tuple.second assembled.diagnosis
    in
    if assembled.initialEncounter then
        expectNextStepsTaskFirstEncounter currentDate isChw assembled.person diagnosis assembled.measurements task

    else
        expectNextStepsTaskSubsequentEncounter currentDate assembled.person diagnosis assembled.measurements task


expectNextStepsTaskFirstEncounter : NominalDate -> Bool -> Person -> Maybe AcuteIllnessDiagnosis -> AcuteIllnessMeasurements -> NextStepsTask -> Bool
expectNextStepsTaskFirstEncounter currentDate isChw person diagnosis measurements task =
    let
        ( ageMonths0To2, ageMonths0To6, ageMonths2To60 ) =
            ageInMonths currentDate person
                |> Maybe.map (\ageMonths -> ( ageMonths < 2, ageMonths < 6, ageMonths >= 2 && ageMonths < 60 ))
                |> Maybe.withDefault ( False, False, False )

        medicationPrescribed =
            (diagnosis == Just DiagnosisMalariaUncomplicated && not ageMonths0To6)
                || (diagnosis == Just DiagnosisGastrointestinalInfectionUncomplicated)
                || (diagnosis == Just DiagnosisSimpleColdAndCough && ageMonths2To60)
                || (diagnosis == Just DiagnosisRespiratoryInfectionUncomplicated && ageMonths2To60)
                || (diagnosis == Just DiagnosisPneuminialCovid19)
    in
    case task of
        NextStepsMedicationDistribution ->
            medicationPrescribed

        NextStepsSendToHC ->
            sendToHCByMalariaTesting ageMonths0To6 diagnosis
                || (diagnosis == Just DiagnosisGastrointestinalInfectionComplicated)
                || (diagnosis == Just DiagnosisSimpleColdAndCough && ageMonths0To2)
                || (diagnosis == Just DiagnosisRespiratoryInfectionUncomplicated && ageMonths0To2)
                || (diagnosis == Just DiagnosisRespiratoryInfectionComplicated)
                || (diagnosis == Just DiagnosisFeverOfUnknownOrigin)
                || (diagnosis == Just DiagnosisUndeterminedMoreEvaluationNeeded)
                || (diagnosis == Just DiagnosisSevereCovid19)
                -- Medication was perscribed, but it's out of stock, or patient is alergic,
                -- excluding case when medication is given for Covid19 (only when there's
                -- Covid with Pneumonia).
                || (medicationPrescribed
                        && sendToHCDueToMedicationNonAdministration measurements
                        && (diagnosis /= Just DiagnosisPneuminialCovid19)
                   )
                || (isChw && diagnosis == Just DiagnosisTuberculosisSuspect)

        NextStepsHealthEducation ->
            False

        NextStepsContactTracing ->
            (diagnosis == Just DiagnosisSevereCovid19)
                || (diagnosis == Just DiagnosisPneuminialCovid19)
                || (diagnosis == Just DiagnosisLowRiskCovid19)

        NextStepsFollowUp ->
            if diagnosis == Just DiagnosisTuberculosisSuspect then
                -- This is only at initial encounter, as Tuberculosis Suspect
                -- diagnosis is not tracked (does not have and option for
                -- subsequent encounters).
                True

            else if List.member diagnosis [ Just DiagnosisSevereCovid19, Just DiagnosisFeverOfUnknownOrigin ] then
                -- In these cases patient is sent to hospital, and
                -- there's no need for CHW to follow up.
                False

            else
                -- Whenever any other next step exists.
                expectNextStepsTaskFirstEncounter currentDate isChw person diagnosis measurements NextStepsMedicationDistribution
                    || expectNextStepsTaskFirstEncounter currentDate isChw person diagnosis measurements NextStepsSendToHC

        NextStepsSymptomsReliefGuidance ->
            diagnosis == Just DiagnosisLowRiskCovid19


expectNextStepsTaskSubsequentEncounter : NominalDate -> Person -> Maybe AcuteIllnessDiagnosis -> AcuteIllnessMeasurements -> NextStepsTask -> Bool
expectNextStepsTaskSubsequentEncounter currentDate person diagnosis measurements task =
    let
        malariaDiagnosedAtCurrentEncounter =
            malariaRapidTestResult measurements == Just RapidTestPositive

        ageMonths0To6 =
            ageInMonths currentDate person
                |> Maybe.map (\ageMonths -> ageMonths < 6)
                |> Maybe.withDefault False
    in
    case task of
        NextStepsMedicationDistribution ->
            malariaDiagnosedAtCurrentEncounter
                && (not <| sendToHCByMalariaTesting ageMonths0To6 diagnosis)

        NextStepsSendToHC ->
            if malariaDiagnosedAtCurrentEncounter then
                sendToHCByMalariaTesting ageMonths0To6 diagnosis
                    || -- Medication was perscribed, but it's out of stock, or patient is alergic.
                       sendToHCDueToMedicationNonAdministration measurements

            else
                noImprovementOnSubsequentVisit currentDate person measurements

        NextStepsHealthEducation ->
            not malariaDiagnosedAtCurrentEncounter

        NextStepsFollowUp ->
            -- Whenever we have a next step task that is other than NextStepsHealthEducation.
            -- When there's only NextStepsHealthEducation, illness will be resolved, therefore,
            -- there's no need for a follow up.
            expectNextStepsTaskSubsequentEncounter currentDate person diagnosis measurements NextStepsMedicationDistribution
                || expectNextStepsTaskSubsequentEncounter currentDate person diagnosis measurements NextStepsSendToHC

        _ ->
            False


nextStepsTaskCompleted : NominalDate -> Bool -> AssembledData -> NextStepsTask -> Bool
nextStepsTaskCompleted currentDate isChw assembled task =
    let
        measurements =
            assembled.measurements

        taskExpected =
            expectNextStepsTask currentDate isChw assembled
    in
    case task of
        NextStepsMedicationDistribution ->
            (not <| taskExpected NextStepsMedicationDistribution) || isJust measurements.medicationDistribution

        NextStepsSendToHC ->
            (not <| taskExpected NextStepsSendToHC) || isJust measurements.sendToHC

        NextStepsHealthEducation ->
            (not <| taskExpected NextStepsHealthEducation) || isJust measurements.healthEducation

        NextStepsFollowUp ->
            (not <| taskExpected NextStepsFollowUp) || isJust measurements.followUp

        NextStepsContactTracing ->
            (not <| taskExpected NextStepsContactTracing) || isJust measurements.contactsTracing

        NextStepsSymptomsReliefGuidance ->
            (not <| taskExpected NextStepsSymptomsReliefGuidance) || isJust measurements.healthEducation


{-| Send patient to health center if patient is alergic to any of prescribed medications,
or, if any of prescribed medications is out of stock.
-}
sendToHCDueToMedicationNonAdministration : AcuteIllnessMeasurements -> Bool
sendToHCDueToMedicationNonAdministration measurements =
    resolveMedicationsNonAdministrationReasons measurements
        |> Dict.toList
        |> List.filter
            (\( _, reason ) ->
                reason == NonAdministrationLackOfStock || reason == NonAdministrationKnownAllergy
            )
        |> List.isEmpty
        |> not


resolveMedicationsNonAdministrationReasons : AcuteIllnessMeasurements -> Dict MedicationDistributionSign AdministrationNote
resolveMedicationsNonAdministrationReasons measurements =
    getMeasurementValueFunc measurements.medicationDistribution
        |> Maybe.map Measurement.Utils.resolveMedicationsNonAdministrationReasons
        |> Maybe.withDefault Dict.empty


talkedTo114 : AcuteIllnessMeasurements -> Bool
talkedTo114 measurements =
    getMeasurementValueFunc measurements.call114
        |> Maybe.map (.signs >> EverySet.member Call114)
        |> Maybe.withDefault False


healthCenterRecommendedToCome : AcuteIllnessMeasurements -> Bool
healthCenterRecommendedToCome measurements =
    getMeasurementValueFunc measurements.hcContact
        |> Maybe.map (.recommendations >> EverySet.member ComeToHealthCenter)
        |> Maybe.withDefault False


sendToHCByMalariaTesting : Bool -> Maybe AcuteIllnessDiagnosis -> Bool
sendToHCByMalariaTesting ageMonths0To6 diagnosis =
    (diagnosis == Just DiagnosisMalariaUncomplicated && ageMonths0To6)
        || (diagnosis == Just DiagnosisMalariaComplicated)
        || (diagnosis == Just DiagnosisMalariaUncomplicatedAndPregnant)


noImprovementOnSubsequentVisit : NominalDate -> Person -> AcuteIllnessMeasurements -> Bool
noImprovementOnSubsequentVisit currentDate person measurements =
    noImprovementOnSubsequentVisitWithoutDangerSigns currentDate person measurements
        || dangerSignPresentOnSubsequentVisit measurements


noImprovementOnSubsequentVisitWithoutDangerSigns : NominalDate -> Person -> AcuteIllnessMeasurements -> Bool
noImprovementOnSubsequentVisitWithoutDangerSigns currentDate person measurements =
    (not <| dangerSignPresentOnSubsequentVisit measurements)
        && (conditionNotImprovingOnSubsequentVisit measurements
                || sendToHCOnSubsequentVisitByVitals currentDate person measurements
                || sendToHCOnSubsequentVisitByMuac measurements
                || sendToHCOnSubsequentVisitByNutrition measurements
           )


conditionNotImprovingOnSubsequentVisit : AcuteIllnessMeasurements -> Bool
conditionNotImprovingOnSubsequentVisit measurements =
    measurements.dangerSigns
        |> Maybe.map
            (Tuple.second
                >> .value
                >> EverySet.member DangerSignConditionNotImproving
            )
        |> Maybe.withDefault False


dangerSignPresentOnSubsequentVisit : AcuteIllnessMeasurements -> Bool
dangerSignPresentOnSubsequentVisit measurements =
    measurements.dangerSigns
        |> Maybe.map
            (Tuple.second
                >> .value
                >> (EverySet.toList
                        >> (\signs ->
                                List.any (\sign -> List.member sign signs)
                                    [ DangerSignUnableDrinkSuck
                                    , DangerSignVomiting
                                    , DangerSignConvulsions
                                    , DangerSignLethargyUnconsciousness
                                    , DangerSignRespiratoryDistress
                                    , DangerSignSpontaneousBleeding
                                    , DangerSignBloodyDiarrhea
                                    , DangerSignNewSkinRash
                                    ]
                           )
                   )
            )
        |> Maybe.withDefault False


sendToHCOnSubsequentVisitByVitals : NominalDate -> Person -> AcuteIllnessMeasurements -> Bool
sendToHCOnSubsequentVisitByVitals currentDate person measurements =
    feverRecorded measurements
        || respiratoryRateElevated currentDate person measurements


sendToHCOnSubsequentVisitByMuac : AcuteIllnessMeasurements -> Bool
sendToHCOnSubsequentVisitByMuac measurements =
    muacRedOnSubsequentVisit measurements


sendToHCOnSubsequentVisitByNutrition : AcuteIllnessMeasurements -> Bool
sendToHCOnSubsequentVisitByNutrition measurements =
    measurements.nutrition
        |> Maybe.map
            (Tuple.second
                >> .value
                >> (EverySet.toList
                        >> (\signs ->
                                -- Any of 4 signs requires sending patient to HC.
                                List.any (\sign -> List.member sign signs) [ AbdominalDistension, Apathy, Edema, PoorAppetite ]
                                    -- Existance of both signs togetehr requires sending patient to HC.
                                    || List.all (\sign -> List.member sign signs) [ BrittleHair, DrySkin ]
                           )
                   )
            )
        |> Maybe.withDefault False


muacRedOnSubsequentVisit : AcuteIllnessMeasurements -> Bool
muacRedOnSubsequentVisit measurements =
    measurements.muac
        |> Maybe.map
            (Tuple.second
                >> .value
                >> muacIndication
                >> (==) ColorAlertRed
            )
        |> Maybe.withDefault False


{-| These are the activities that are mandatory for us to come up with diagnosis during first encounter.
Covid19 diagnosis is special, therefore, we assume here that Covid19 is negative.
-}
mandatoryActivitiesCompletedFirstEncounter : NominalDate -> Person -> Bool -> AcuteIllnessMeasurements -> Bool
mandatoryActivitiesCompletedFirstEncounter currentDate person isChw measurements =
    [ AcuteIllnessSymptoms, AcuteIllnessPhysicalExam ]
        |> List.all (mandatoryActivityCompletedFirstEncounter currentDate person isChw measurements)


mandatoryActivityCompletedFirstEncounter : NominalDate -> Person -> Bool -> AcuteIllnessMeasurements -> AcuteIllnessActivity -> Bool
mandatoryActivityCompletedFirstEncounter currentDate person isChw measurements activity =
    case activity of
        AcuteIllnessSymptoms ->
            isJust measurements.symptomsGeneral
                && isJust measurements.symptomsRespiratory
                && isJust measurements.symptomsGI

        AcuteIllnessPhysicalExam ->
            isJust measurements.vitals
                && isJust measurements.acuteFindings
                && ((not <| expectPhysicalExamTask currentDate person isChw True PhysicalExamMuac) || isJust measurements.muac)
                && ((not <| expectPhysicalExamTask currentDate person isChw True PhysicalExamNutrition) || isJust measurements.nutrition)
                && ((not <| expectPhysicalExamTask currentDate person isChw True PhysicalExamCoreExam) || isJust measurements.coreExam)

        _ ->
            False


{-| These are the activities that are mandatory for us to come up with next steps during subsequent encounter.
-}
mandatoryActivitiesCompletedSubsequentVisit : NominalDate -> Bool -> AssembledData -> Bool
mandatoryActivitiesCompletedSubsequentVisit currentDate isChw data =
    [ AcuteIllnessDangerSigns, AcuteIllnessPhysicalExam, AcuteIllnessOngoingTreatment, AcuteIllnessLaboratory ]
        |> List.all (mandatoryActivityCompletedSubsequentVisit currentDate isChw data)


mandatoryActivityCompletedSubsequentVisit : NominalDate -> Bool -> AssembledData -> AcuteIllnessActivity -> Bool
mandatoryActivityCompletedSubsequentVisit currentDate isChw data activity =
    let
        measurements =
            data.measurements
    in
    case activity of
        AcuteIllnessDangerSigns ->
            isJust measurements.dangerSigns

        AcuteIllnessPhysicalExam ->
            let
                person =
                    data.person
            in
            isJust measurements.vitals
                && ((not <| expectPhysicalExamTask currentDate person isChw False PhysicalExamMuac) || isJust measurements.muac)
                && ((not <| expectPhysicalExamTask currentDate person isChw False PhysicalExamNutrition) || isJust measurements.nutrition)
                && ((not <| expectPhysicalExamTask currentDate person isChw False PhysicalExamAcuteFindings) || isJust measurements.acuteFindings)
                && ((not <| expectPhysicalExamTask currentDate person isChw True PhysicalExamCoreExam) || isJust measurements.coreExam)

        AcuteIllnessOngoingTreatment ->
            (not <| expectActivity currentDate isChw data AcuteIllnessOngoingTreatment)
                || isJust measurements.treatmentOngoing

        AcuteIllnessLaboratory ->
            (not <| expectActivity currentDate isChw data AcuteIllnessLaboratory)
                || isJust measurements.malariaTesting

        _ ->
            False


ageDependentUncomplicatedMalariaNextStep : NominalDate -> Person -> Maybe NextStepsTask
ageDependentUncomplicatedMalariaNextStep currentDate person =
    ageInMonths currentDate person
        |> Maybe.map
            (\ageMonths ->
                if ageMonths < 6 then
                    NextStepsSendToHC

                else
                    NextStepsMedicationDistribution
            )


ageDependentARINextStep : NominalDate -> Person -> Maybe NextStepsTask
ageDependentARINextStep currentDate person =
    ageInMonths currentDate person
        |> Maybe.andThen
            (\ageMonths ->
                if ageMonths < 2 then
                    Just NextStepsSendToHC

                else if ageMonths < 60 then
                    Just NextStepsMedicationDistribution

                else
                    Nothing
            )


resolveAcuteIllnessDiagnosis : NominalDate -> EverySet SiteFeature -> Bool -> AssembledData -> Maybe AcuteIllnessDiagnosis
resolveAcuteIllnessDiagnosis currentDate features isChw assembled =
    if assembled.initialEncounter then
        -- First we check for Covid19.
        let
            covid19AcuteIllnessDiagnosis =
                covid19DiagnosisPath currentDate assembled.person isChw assembled.measurements
        in
        if isJust covid19AcuteIllnessDiagnosis then
            covid19AcuteIllnessDiagnosis

        else
            nonCovid19DiagnosisPath currentDate features assembled.person isChw assembled.measurements

    else
        malariaRapidTestResult assembled.measurements
            |> Maybe.andThen
                (\testResult ->
                    case testResult of
                        RapidTestPositive ->
                            if dangerSignPresentOnSubsequentVisit assembled.measurements then
                                Just DiagnosisMalariaComplicated

                            else
                                Just DiagnosisMalariaUncomplicated

                        RapidTestPositiveAndPregnant ->
                            if dangerSignPresentOnSubsequentVisit assembled.measurements then
                                Just DiagnosisMalariaComplicated

                            else
                                Just DiagnosisMalariaUncomplicatedAndPregnant

                        _ ->
                            Nothing
                )


covid19SuspectDiagnosed : AcuteIllnessMeasurements -> Bool
covid19SuspectDiagnosed measurements =
    let
        generalSymptomsCount =
            let
                excludesGeneral =
                    [ SymptomGeneralFever ] ++ symptomsGeneralDangerSigns
            in
            countGeneralSymptoms measurements excludesGeneral

        respiratorySymptomsCount =
            countRespiratorySymptoms measurements []

        feverOnRecord =
            feverRecorded measurements

        malariaRDTResult =
            malariaRapidTestResult measurements

        feverAndRdtNotPositive =
            feverOnRecord
                && isJust malariaRDTResult
                && (malariaRDTResult /= Just RapidTestPositive)
                && (malariaRDTResult /= Just RapidTestPositiveAndPregnant)
    in
    feverAndRdtNotPositive && (respiratorySymptomsCount > 0 || generalSymptomsCount > 1)


{-| This may result in Covid diagnosis, or Malaria diagnosis,
if Covid RDT could not be perfrmed.
-}
covid19DiagnosisPath : NominalDate -> Person -> Bool -> AcuteIllnessMeasurements -> Maybe AcuteIllnessDiagnosis
covid19DiagnosisPath currentDate person isChw measurements =
    if
        -- CHW may not diagnose COVID anymore.
        isChw
            || (not <| covid19SuspectDiagnosed measurements)
            || -- In case we have cough symptom for more than 2 weeks,
               -- we must diagnose Tuberculosis suspect.
               -- Therefore, we need to exit COVID19 path.
               coughForMoreThan2Weeks measurements
    then
        Nothing

    else
        covidRapidTestResult measurements
            |> Maybe.map
                (\rdtResult ->
                    let
                        positiveCovidDecisionMatrix =
                            -- Any other result is treated as if RDT was positive.
                            if mandatoryActivitiesCompletedFirstEncounter currentDate person isChw measurements then
                                if
                                    bloodPressureIndicatesSevereCovid19 measurements
                                        || respiratoryRateElevatedForCovid19 currentDate person measurements
                                        || lethargyAtSymptoms measurements
                                        || lethargicOrUnconsciousAtAcuteFindings measurements
                                        || acuteFindinsgRespiratoryDangerSignPresent measurements
                                then
                                    Just DiagnosisSevereCovid19

                                else if
                                    bloodPressureIndicatesCovid19WithPneumonia measurements
                                        || (countRespiratorySymptoms measurements [] > 0)
                                        || (countGeneralSymptoms measurements [] > 0)
                                        && cracklesAtCoreExam measurements
                                then
                                    Just DiagnosisPneuminialCovid19

                                else
                                    Just DiagnosisLowRiskCovid19

                            else
                                -- We don't have enough data to make a decision on COVID severity
                                -- diagnosis, so we report of suspected Covid case.
                                Just DiagnosisCovid19Suspect
                    in
                    case rdtResult of
                        RapidTestNegative ->
                            -- On negative result, we can state that this is not a Covid case.
                            Nothing

                        RapidTestIndeterminate ->
                            -- This is not an option at Covid RDT, so we can
                            -- rule it out.
                            Nothing

                        RapidTestPositiveAndPregnant ->
                            -- Covid with pregnancy always concidered as severe case.
                            Just DiagnosisSevereCovid19

                        RapidTestUnableToRun ->
                            -- Per requirements, if we have a suspected Covid case, but
                            -- can not perform Covid RDT, we treat the case as a positive for Covid.
                            -- However, when test can not be performed and fever is recorded,
                            -- we run Malaria RDT first, to rule out Malaria.
                            if feverRecorded measurements then
                                malariaRapidTestResult measurements
                                    |> Maybe.map
                                        (\result ->
                                            if rapidTestPositive result then
                                                malariaTypeForPositiveRDT measurements result

                                            else
                                                positiveCovidDecisionMatrix
                                        )
                                    |> Maybe.withDefault (Just DiagnosisCovid19Suspect)

                            else
                                positiveCovidDecisionMatrix

                        RapidTestUnableToRunAndPregnant ->
                            -- Per requirements, if we have a suspected Covid case, but
                            -- can not perform Covid RDT, we treat the case as a positive for Covid.
                            -- Confirmed Covid with pregnancy always concidered as severe case.
                            -- However, when test can not be performed and fever is recorded,
                            -- we run Malaria RDT first, to rule out Malaria.
                            if feverRecorded measurements then
                                malariaRapidTestResult measurements
                                    |> Maybe.map
                                        (\result ->
                                            if rapidTestPositive result then
                                                malariaTypeForPositiveRDT measurements result

                                            else
                                                Just DiagnosisSevereCovid19
                                        )
                                    |> Maybe.withDefault (Just DiagnosisCovid19Suspect)

                            else
                                Just DiagnosisSevereCovid19

                        RapidTestPositive ->
                            positiveCovidDecisionMatrix
                )
            -- RDT was not taken yet, so we report of suspected Covid case.
            |> Maybe.withDefault (Just DiagnosisCovid19Suspect)


rapidTestPositive : RapidTestResult -> Bool
rapidTestPositive result =
    List.member result [ RapidTestPositiveAndPregnant, RapidTestPositive ]


nonCovid19DiagnosisPath : NominalDate -> EverySet SiteFeature -> Person -> Bool -> AcuteIllnessMeasurements -> Maybe AcuteIllnessDiagnosis
nonCovid19DiagnosisPath currentDate features person isChw measurements =
    -- Verify that we have enough data to make a decision on diagnosis.
    if mandatoryActivitiesCompletedFirstEncounter currentDate person isChw measurements then
        if tuberculosisManagementEnabled features && coughForMoreThan2Weeks measurements then
            Just DiagnosisTuberculosisSuspect

        else if feverRecorded measurements then
            resolveAcuteIllnessDiagnosisByMalariaRDT measurements

        else if respiratoryInfectionDangerSignsPresent measurements then
            Just DiagnosisRespiratoryInfectionComplicated

        else if gastrointestinalInfectionDangerSignsPresent False measurements then
            Just DiagnosisGastrointestinalInfectionComplicated

        else if respiratoryInfectionSymptomsPresent measurements then
            if respiratoryRateElevated currentDate person measurements then
                Just DiagnosisRespiratoryInfectionUncomplicated

            else
                Just DiagnosisSimpleColdAndCough

        else if nonBloodyDiarrheaAtSymptoms measurements then
            -- Non Bloody Diarrhea is the only GI symptom that is diagnosed as Uncomplicated, when fever is  not recorded.
            Just DiagnosisGastrointestinalInfectionUncomplicated

        else if mildGastrointestinalInfectionSymptomsPresent measurements then
            Just DiagnosisUndeterminedMoreEvaluationNeeded

        else
            Nothing

    else
        Nothing


resolveAcuteIllnessDiagnosisByMalariaRDT : AcuteIllnessMeasurements -> Maybe AcuteIllnessDiagnosis
resolveAcuteIllnessDiagnosisByMalariaRDT measurements =
    malariaRapidTestResult measurements
        |> Maybe.map
            (\result ->
                case malariaTypeForPositiveRDT measurements result of
                    Just dignosis ->
                        dignosis

                    Nothing ->
                        if respiratoryInfectionDangerSignsPresent measurements then
                            DiagnosisRespiratoryInfectionComplicated

                        else if gastrointestinalInfectionDangerSignsPresent True measurements then
                            -- Fever with Diarrhea is considered to be a complicated case.
                            DiagnosisGastrointestinalInfectionComplicated

                        else
                            DiagnosisFeverOfUnknownOrigin
            )


malariaTypeForPositiveRDT : AcuteIllnessMeasurements -> RapidTestResult -> Maybe AcuteIllnessDiagnosis
malariaTypeForPositiveRDT measurements result =
    case result of
        RapidTestPositive ->
            if malariaDangerSignsPresent measurements then
                Just DiagnosisMalariaComplicated

            else
                Just DiagnosisMalariaUncomplicated

        RapidTestPositiveAndPregnant ->
            if malariaDangerSignsPresent measurements then
                Just DiagnosisMalariaComplicated

            else
                Just DiagnosisMalariaUncomplicatedAndPregnant

        _ ->
            Nothing


countSymptoms : Maybe ( id, m ) -> (m -> Dict k v) -> List k -> Int
countSymptoms measurement getSymptomsListFunc exclusions =
    Maybe.map
        (Tuple.second
            >> getSymptomsListFunc
            >> Dict.keys
            >> List.filter (\sign -> List.member sign exclusions |> not)
            >> List.length
        )
        measurement
        |> Maybe.withDefault 0


countGeneralSymptoms : AcuteIllnessMeasurements -> List SymptomsGeneralSign -> Int
countGeneralSymptoms measurements exclusions =
    countSymptoms measurements.symptomsGeneral .value (NoSymptomsGeneral :: exclusions)


countRespiratorySymptoms : AcuteIllnessMeasurements -> List SymptomsRespiratorySign -> Int
countRespiratorySymptoms measurements exclusions =
    countSymptoms measurements.symptomsRespiratory .value (NoSymptomsRespiratory :: exclusions)


countGISymptoms : AcuteIllnessMeasurements -> List SymptomsGISign -> Int
countGISymptoms measurements exclusions =
    countSymptoms measurements.symptomsGI (.value >> .signs) (NoSymptomsGI :: exclusions)


feverRecorded : AcuteIllnessMeasurements -> Bool
feverRecorded measurements =
    feverAtSymptoms measurements || isJust (feverAtPhysicalExam measurements)


feverAtSymptoms : AcuteIllnessMeasurements -> Bool
feverAtSymptoms measurements =
    getMeasurementValueFunc measurements.symptomsGeneral
        |> Maybe.map (symptomAppearsAtSymptomsDict SymptomGeneralFever)
        |> Maybe.withDefault False


feverAtPhysicalExam : AcuteIllnessMeasurements -> Maybe Float
feverAtPhysicalExam measurements =
    measurements.vitals
        |> Maybe.andThen
            (\measurement ->
                let
                    bodyTemperature =
                        Tuple.second measurement
                            |> .value
                            |> .bodyTemperature
                in
                if bodyTemperature >= 37.5 then
                    Just bodyTemperature

                else
                    Nothing
            )


respiratoryRateElevated : NominalDate -> Person -> AcuteIllnessMeasurements -> Bool
respiratoryRateElevated currentDate person measurements =
    Maybe.map
        (\measurement ->
            let
                maybeAgeMonths =
                    ageInMonths currentDate person

                respiratoryRate =
                    Tuple.second measurement
                        |> .value
                        |> .respiratoryRate
            in
            respiratoryRateElevatedByAge maybeAgeMonths respiratoryRate
        )
        measurements.vitals
        |> Maybe.withDefault False


respiratoryRateElevatedForCovid19 : NominalDate -> Person -> AcuteIllnessMeasurements -> Bool
respiratoryRateElevatedForCovid19 currentDate person measurements =
    Maybe.map
        (\measurement ->
            let
                maybeAgeMonths =
                    ageInMonths currentDate person

                respiratoryRate =
                    Tuple.second measurement
                        |> .value
                        |> .respiratoryRate
            in
            respiratoryRateElevatedByAgeForCovid19 maybeAgeMonths respiratoryRate
        )
        measurements.vitals
        |> Maybe.withDefault False


respiratoryRateAbnormalForAge : Maybe Int -> Int -> Bool
respiratoryRateAbnormalForAge maybeAgeMonths rate =
    respiratoryRateElevatedByAge maybeAgeMonths rate
        || respiratoryRateRecessedByAge maybeAgeMonths rate


respiratoryRateElevatedByAge : Maybe Int -> Int -> Bool
respiratoryRateElevatedByAge maybeAgeMonths rate =
    maybeAgeMonths
        |> Maybe.map
            (\ageMonths ->
                if ageMonths < 12 then
                    rate >= 50

                else if ageMonths < 5 * 12 then
                    rate >= 40

                else
                    rate > 30
            )
        |> Maybe.withDefault False


respiratoryRateElevatedByAgeForCovid19 : Maybe Int -> Int -> Bool
respiratoryRateElevatedByAgeForCovid19 maybeAgeMonths rate =
    maybeAgeMonths
        |> Maybe.map
            (\ageMonths ->
                if ageMonths < 2 then
                    rate >= 60

                else if ageMonths < 12 then
                    rate >= 50

                else if ageMonths < 5 * 12 then
                    rate >= 40

                else
                    rate > 30
            )
        |> Maybe.withDefault False


respiratoryRateRecessedByAge : Maybe Int -> Int -> Bool
respiratoryRateRecessedByAge maybeAgeMonths rate =
    maybeAgeMonths
        |> Maybe.map
            (\ageMonths ->
                if ageMonths < 12 then
                    rate < 30

                else if ageMonths < (5 * 12) then
                    rate < 24

                else
                    rate < 18
            )
        |> Maybe.withDefault False


{-| We consider suspected case to be confirmed, if Covid RDT was performed and
turned out positive, or was not taken for whatever reason.
Only option to rule out Covid is for RDT result to be negative.
-}
covidCaseConfirmed : AcuteIllnessMeasurements -> Bool
covidCaseConfirmed measurements =
    isJust measurements.covidTesting
        && (covidRapidTestResult measurements /= Just RapidTestNegative)


covidRapidTestResult : AcuteIllnessMeasurements -> Maybe RapidTestResult
covidRapidTestResult measurements =
    getMeasurementValueFunc measurements.covidTesting
        |> Maybe.map .result


malariaRapidTestResult : AcuteIllnessMeasurements -> Maybe RapidTestResult
malariaRapidTestResult measurements =
    getMeasurementValueFunc measurements.malariaTesting


malariaDangerSignsPresent : AcuteIllnessMeasurements -> Bool
malariaDangerSignsPresent measurements =
    Maybe.map3
        (\symptomsGeneral symptomsGI acuteFindings ->
            let
                symptomsGeneralDict =
                    Tuple.second symptomsGeneral |> .value

                symptomsGIDict =
                    Tuple.second symptomsGI |> .value |> .signs

                symptomsGISet =
                    Tuple.second symptomsGI |> .value |> .derivedSigns

                acuteFindingsValue =
                    Tuple.second acuteFindings |> .value

                lethargy =
                    symptomAppearsAtSymptomsDict Lethargy symptomsGeneralDict
                        || EverySet.member LethargicOrUnconscious acuteFindingsValue.signsGeneral

                poorSuck =
                    symptomAppearsAtSymptomsDict PoorSuck symptomsGeneralDict
                        || EverySet.member AcuteFindingsPoorSuck acuteFindingsValue.signsGeneral

                unableToDrink =
                    symptomAppearsAtSymptomsDict UnableToDrink symptomsGeneralDict

                unableToEat =
                    symptomAppearsAtSymptomsDict UnableToEat symptomsGeneralDict

                severeWeakness =
                    symptomAppearsAtSymptomsDict SevereWeakness symptomsGeneralDict

                cokeColoredUrine =
                    symptomAppearsAtSymptomsDict CokeColoredUrine symptomsGeneralDict

                convulsions =
                    symptomAppearsAtSymptomsDict SymptomsGeneralConvulsions symptomsGeneralDict

                spontaneousBleeding =
                    symptomAppearsAtSymptomsDict SpontaneousBleeding symptomsGeneralDict

                unconsciousness =
                    EverySet.member LethargicOrUnconscious acuteFindingsValue.signsGeneral

                jaundice =
                    EverySet.member Jaundice acuteFindingsValue.signsGeneral

                stridor =
                    EverySet.member Stridor acuteFindingsValue.signsRespiratory

                nasalFlaring =
                    EverySet.member NasalFlaring acuteFindingsValue.signsRespiratory

                severeWheezing =
                    EverySet.member SevereWheezing acuteFindingsValue.signsRespiratory

                subCostalRetractions =
                    EverySet.member SubCostalRetractions acuteFindingsValue.signsRespiratory

                vomiting =
                    symptomAppearsAtSymptomsDict Vomiting symptomsGIDict

                intractableVomiting =
                    vomiting && EverySet.member IntractableVomiting symptomsGISet

                increasedThirst =
                    symptomAppearsAtSymptomsDict IncreasedThirst symptomsGeneralDict

                dryMouth =
                    symptomAppearsAtSymptomsDict DryMouth symptomsGeneralDict

                severeDehydration =
                    intractableVomiting && increasedThirst && dryMouth
            in
            lethargy
                || poorSuck
                || unableToDrink
                || unableToEat
                || severeWeakness
                || cokeColoredUrine
                || convulsions
                || spontaneousBleeding
                || unconsciousness
                || jaundice
                || stridor
                || nasalFlaring
                || severeWheezing
                || subCostalRetractions
                || vomiting
                || severeDehydration
        )
        measurements.symptomsGeneral
        measurements.symptomsGI
        measurements.acuteFindings
        |> Maybe.withDefault False


respiratoryInfectionSymptomsPresent : AcuteIllnessMeasurements -> Bool
respiratoryInfectionSymptomsPresent measurements =
    countRespiratorySymptoms measurements [ LossOfSmell, ShortnessOfBreath, StabbingChestPain ] > 0


respiratoryInfectionDangerSignsPresent : AcuteIllnessMeasurements -> Bool
respiratoryInfectionDangerSignsPresent measurements =
    Maybe.map2
        (\symptomsRespiratory acuteFindings ->
            let
                symptomsRespiratoryDict =
                    Tuple.second symptomsRespiratory |> .value

                acuteFindingsValue =
                    Tuple.second acuteFindings |> .value

                stridor =
                    EverySet.member Stridor acuteFindingsValue.signsRespiratory

                nasalFlaring =
                    EverySet.member NasalFlaring acuteFindingsValue.signsRespiratory

                severeWheezing =
                    EverySet.member SevereWheezing acuteFindingsValue.signsRespiratory

                subCostalRetractions =
                    EverySet.member SubCostalRetractions acuteFindingsValue.signsRespiratory

                stabbingChestPain =
                    symptomAppearsAtSymptomsDict StabbingChestPain symptomsRespiratoryDict

                shortnessOfBreath =
                    symptomAppearsAtSymptomsDict ShortnessOfBreath symptomsRespiratoryDict
            in
            stridor
                || nasalFlaring
                || severeWheezing
                || subCostalRetractions
                || stabbingChestPain
                || shortnessOfBreath
        )
        measurements.symptomsRespiratory
        measurements.acuteFindings
        |> Maybe.withDefault False


coughForMoreThan2Weeks : AcuteIllnessMeasurements -> Bool
coughForMoreThan2Weeks =
    .symptomsRespiratory
        >> getMeasurementValueFunc
        >> Maybe.andThen
            (Dict.get Cough
                >> Maybe.map
                    -- For more than 2 weeks, value is set to
                    -- max duration of a symptom.
                    ((==) symptomMaxDuration)
            )
        >> Maybe.withDefault False


gastrointestinalInfectionDangerSignsPresent : Bool -> AcuteIllnessMeasurements -> Bool
gastrointestinalInfectionDangerSignsPresent fever measurements =
    Maybe.map
        (\symptomsGI ->
            let
                symptomsGIDict =
                    Tuple.second symptomsGI |> .value |> .signs

                bloodyDiarrhea =
                    symptomAppearsAtSymptomsDict BloodyDiarrhea symptomsGIDict

                nonBloodyDiarrhea =
                    symptomAppearsAtSymptomsDict NonBloodyDiarrhea symptomsGIDict
            in
            if fever then
                bloodyDiarrhea || nonBloodyDiarrhea

            else
                let
                    symptomsGISet =
                        Tuple.second symptomsGI |> .value |> .derivedSigns

                    intractableVomiting =
                        symptomAppearsAtSymptomsDict Vomiting symptomsGIDict
                            && EverySet.member IntractableVomiting symptomsGISet
                in
                bloodyDiarrhea || (nonBloodyDiarrhea && intractableVomiting)
        )
        measurements.symptomsGI
        |> Maybe.withDefault False


mildGastrointestinalInfectionSymptomsPresent : AcuteIllnessMeasurements -> Bool
mildGastrointestinalInfectionSymptomsPresent measurements =
    Maybe.map
        (\symptomsGI ->
            let
                symptomsGIDict =
                    Tuple.second symptomsGI |> .value |> .signs
            in
            symptomAppearsAtSymptomsDict SymptomGIAbdominalPain symptomsGIDict
                || symptomAppearsAtSymptomsDict Nausea symptomsGIDict
                || symptomAppearsAtSymptomsDict Vomiting symptomsGIDict
        )
        measurements.symptomsGI
        |> Maybe.withDefault False


nonBloodyDiarrheaAtSymptoms : AcuteIllnessMeasurements -> Bool
nonBloodyDiarrheaAtSymptoms measurements =
    getMeasurementValueFunc measurements.symptomsGI
        |> Maybe.map (.signs >> symptomAppearsAtSymptomsDict NonBloodyDiarrhea)
        |> Maybe.withDefault False


vomitingAtSymptoms : AcuteIllnessMeasurements -> Bool
vomitingAtSymptoms measurements =
    getMeasurementValueFunc measurements.symptomsGI
        |> Maybe.map (.signs >> symptomAppearsAtSymptomsDict Vomiting)
        |> Maybe.withDefault False


bloodPressureIndicatesSevereCovid19 : AcuteIllnessMeasurements -> Bool
bloodPressureIndicatesSevereCovid19 measurements =
    getMeasurementValueFunc measurements.vitals
        |> Maybe.andThen
            (\value ->
                Maybe.map2
                    (\sys dia ->
                        sys < 90 || dia < 60
                    )
                    value.sys
                    value.dia
            )
        |> Maybe.withDefault False


bloodPressureIndicatesCovid19WithPneumonia : AcuteIllnessMeasurements -> Bool
bloodPressureIndicatesCovid19WithPneumonia measurements =
    getMeasurementValueFunc measurements.vitals
        |> Maybe.andThen
            (\value ->
                Maybe.map (\sys -> sys <= 100) value.sys
            )
        |> Maybe.withDefault False


lethargyAtSymptoms : AcuteIllnessMeasurements -> Bool
lethargyAtSymptoms measurements =
    getMeasurementValueFunc measurements.symptomsGeneral
        |> Maybe.map (symptomAppearsAtSymptomsDict Lethargy)
        |> Maybe.withDefault False


lethargicOrUnconsciousAtAcuteFindings : AcuteIllnessMeasurements -> Bool
lethargicOrUnconsciousAtAcuteFindings measurements =
    getMeasurementValueFunc measurements.acuteFindings
        |> Maybe.map (.signsGeneral >> EverySet.member LethargicOrUnconscious)
        |> Maybe.withDefault False


cracklesAtCoreExam : AcuteIllnessMeasurements -> Bool
cracklesAtCoreExam measurements =
    getMeasurementValueFunc measurements.coreExam
        |> Maybe.map (.lungs >> EverySet.member Crackles)
        |> Maybe.withDefault False


acuteFindinsgRespiratoryDangerSignPresent : AcuteIllnessMeasurements -> Bool
acuteFindinsgRespiratoryDangerSignPresent measurements =
    getMeasurementValueFunc measurements.acuteFindings
        |> Maybe.map
            (.signsRespiratory
                >> EverySet.toList
                >> List.filter ((/=) NoAcuteFindingsRespiratorySigns)
                >> List.isEmpty
                >> not
            )
        |> Maybe.withDefault False


fromContactsTracingValue : Maybe (List ContactTraceItem) -> ContactsTracingForm
fromContactsTracingValue saved =
    { state = ContactsTracingFormSummary
    , contacts = generateContactsFromTraceItems saved
    , finished = False
    }


contactsTracingFormWithDefault : ContactsTracingForm -> Maybe (List ContactTraceItem) -> ContactsTracingForm
contactsTracingFormWithDefault form saved =
    saved
        |> unwrap
            form
            (\_ ->
                { state = form.state
                , contacts = or form.contacts (generateContactsFromTraceItems saved)
                , finished = form.finished
                }
            )


generateContactsFromTraceItems : Maybe (List ContactTraceItem) -> Maybe (Dict PersonId ContactTraceItem)
generateContactsFromTraceItems items =
    Maybe.map
        (List.map (\item -> ( item.personId, item ))
            >> Dict.fromList
        )
        items


toContactsTracingValueWithDefault : Maybe (List ContactTraceItem) -> ContactsTracingForm -> Maybe (List ContactTraceItem)
toContactsTracingValueWithDefault saved form =
    contactsTracingFormWithDefault form saved
        |> toContactsTracingValue


toContactsTracingValue : ContactsTracingForm -> Maybe (List ContactTraceItem)
toContactsTracingValue form =
    Maybe.map Dict.values form.contacts


followUpFormWithDefault : FollowUpForm -> Maybe AcuteIllnessFollowUpValue -> FollowUpForm
followUpFormWithDefault form saved =
    saved
        |> unwrap
            form
            (\value ->
                { option = or form.option (EverySet.toList value.options |> List.head)
                , diagnosis = or form.diagnosis value.diagnosis
                , resolutionDate = or form.resolutionDate value.resolutionDate
                }
            )


toFollowUpValueWithDefault : Maybe AcuteIllnessFollowUpValue -> FollowUpForm -> Maybe AcuteIllnessFollowUpValue
toFollowUpValueWithDefault saved form =
    followUpFormWithDefault form saved
        |> toFollowUpValue


toFollowUpValue : FollowUpForm -> Maybe AcuteIllnessFollowUpValue
toFollowUpValue form =
    Maybe.map
        (\options ->
            AcuteIllnessFollowUpValue options form.resolutionDate form.diagnosis
        )
        (Maybe.map (List.singleton >> EverySet.fromList) form.option)



-- HELPER FUNCTIONS


symptomAppearsAtSymptomsDict : a -> Dict a Int -> Bool
symptomAppearsAtSymptomsDict symptom dict =
    Dict.get symptom dict
        |> Maybe.map ((<) 0)
        |> Maybe.withDefault False


resolvePreviousValue : AssembledData -> (AcuteIllnessMeasurements -> Maybe ( id, AcuteIllnessMeasurement a )) -> (a -> b) -> Maybe b
resolvePreviousValue assembled measurementFunc valueFunc =
    assembled.previousEncountersData
        |> List.filterMap
            (.measurements
                >> measurementFunc
                >> Maybe.map (Tuple.second >> .value >> valueFunc)
            )
        |> List.reverse
        |> List.head


resolvePreviousMaybeValue : AssembledData -> (AcuteIllnessMeasurements -> Maybe ( id, AcuteIllnessMeasurement a )) -> (a -> Maybe b) -> Maybe b
resolvePreviousMaybeValue assembled measurementFunc valueFunc =
    assembled.previousEncountersData
        |> List.filterMap
            (.measurements
                >> measurementFunc
                >> Maybe.andThen (Tuple.second >> .value >> valueFunc)
            )
        |> List.reverse
        |> List.head


withDefaultValue : a -> Maybe a -> EverySet a
withDefaultValue default maybe =
    Maybe.map List.singleton maybe
        |> fromListWithDefaultValue default


symptomMaxDuration : Int
symptomMaxDuration =
    14


coughLessThan2WeeksConstant : Int
coughLessThan2WeeksConstant =
    7
