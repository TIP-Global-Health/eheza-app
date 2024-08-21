module Pages.AcuteIllness.Activity.Utils exposing (..)

import AssocList as Dict exposing (Dict)
import Backend.AcuteIllnessActivity.Model exposing (AcuteIllnessActivity(..))
import Backend.AcuteIllnessEncounter.Types exposing (AcuteIllnessDiagnosis(..))
import Backend.Entities exposing (PersonId)
import Backend.Measurement.Model exposing (..)
import Backend.Measurement.Utils exposing (getMeasurementValueFunc, muacIndication)
import Backend.Person.Model exposing (Person)
import Backend.Person.Utils exposing (ageInMonths, ageInYears, isChildUnderAgeOf5, isPersonAFertileWoman)
import Backend.Utils exposing (tuberculosisManagementEnabled)
import EverySet exposing (EverySet)
import Gizra.NominalDate exposing (NominalDate)
import Maybe.Extra exposing (andMap, isJust, isNothing, or, unwrap)
import Measurement.Utils
    exposing
        ( followUpFormWithDefault
        , fromListWithDefaultValue
        , healthEducationFormWithDefault
        , muacFormWithDefault
        , ongoingTreatmentReviewFormWithDefault
        , sendToHCFormWithDefault
        , treatmentReviewInputsAndTasks
        , vitalsFormWithDefault
        )
import Pages.AcuteIllness.Activity.Model exposing (..)
import Pages.AcuteIllness.Activity.Types exposing (..)
import Pages.AcuteIllness.Encounter.Model exposing (AssembledData)
import Pages.Utils
    exposing
        ( getCurrentReasonForMedicationNonAdministration
        , ifEverySetEmpty
        , ifNullableTrue
        , ifTrue
        , maybeValueConsideringIsDirtyField
        , taskCompleted
        , valueConsideringIsDirtyField
        )
import SyncManager.Model exposing (SiteFeature)
import Translate exposing (Language, TranslationId)


expectActivity : NominalDate -> Bool -> AssembledData -> AcuteIllnessActivity -> Bool
expectActivity currentDate isChw assembled activity =
    case activity of
        AcuteIllnessSymptoms ->
            assembled.initialEncounter

        AcuteIllnessExposure ->
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

        AcuteIllnessExposure ->
            mandatoryActivityCompletedFirstEncounter currentDate person isChw measurements AcuteIllnessExposure

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
                    measurements.symptomsGI
                        |> getMeasurementValueFunc
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


physicalExamTasksCompletedFromTotal : NominalDate -> Bool -> Person -> AcuteIllnessMeasurements -> PhysicalExamData -> PhysicalExamTask -> ( Int, Int )
physicalExamTasksCompletedFromTotal currentDate isChw person measurements data task =
    case task of
        PhysicalExamVitals ->
            let
                form =
                    measurements.vitals
                        |> getMeasurementValueFunc
                        |> vitalsFormWithDefault data.vitalsForm
            in
            if isChw then
                ( taskCompleted form.respiratoryRate + taskCompleted form.bodyTemperature
                , 2
                )

            else
                Maybe.map
                    (\birthDate ->
                        let
                            ageYears =
                                Gizra.NominalDate.diffYears birthDate currentDate

                            ( ageDependentInputsCompleted, ageDependentInputsActive ) =
                                if ageYears < 12 then
                                    ( 0, 0 )

                                else
                                    ( taskCompleted form.sysBloodPressure
                                        + taskCompleted form.diaBloodPressure
                                    , 2
                                    )
                        in
                        ( taskCompleted form.heartRate
                            + taskCompleted form.respiratoryRate
                            + taskCompleted form.bodyTemperature
                            + ageDependentInputsCompleted
                        , 3 + ageDependentInputsActive
                        )
                    )
                    person.birthDate
                    |> Maybe.withDefault ( 0, 0 )

        PhysicalExamCoreExam ->
            let
                form =
                    measurements.coreExam
                        |> getMeasurementValueFunc
                        |> coreExamFormWithDefault data.coreExamForm
            in
            ( taskCompleted form.heart + taskCompleted form.lungs
            , 2
            )

        PhysicalExamMuac ->
            let
                form =
                    measurements.muac
                        |> getMeasurementValueFunc
                        |> muacFormWithDefault data.muacForm
            in
            ( taskCompleted form.muac
            , 1
            )

        PhysicalExamAcuteFindings ->
            let
                form =
                    measurements.acuteFindings
                        |> getMeasurementValueFunc
                        |> acuteFindingsFormWithDefault data.acuteFindingsForm
            in
            ( taskCompleted form.signsGeneral + taskCompleted form.signsRespiratory
            , 2
            )

        PhysicalExamNutrition ->
            let
                form =
                    measurements.nutrition
                        |> getMeasurementValueFunc
                        |> nutritionFormWithDefault data.nutritionForm
            in
            ( taskCompleted form.signs
            , 1
            )


laboratoryTasksCompletedFromTotal : NominalDate -> Person -> AcuteIllnessMeasurements -> LaboratoryData -> AILaboratoryTask -> ( Int, Int )
laboratoryTasksCompletedFromTotal currentDate person measurements data task =
    case task of
        LaboratoryMalariaTesting ->
            let
                form =
                    measurements.malariaTesting
                        |> getMeasurementValueFunc
                        |> malariaTestingFormWithDefault data.malariaTestingForm

                testResultPositive =
                    Maybe.map rapidTestPositive form.rapidTestResult
                        |> Maybe.withDefault False

                ( isPregnantActive, isPregnantCompleted ) =
                    if testResultPositive && isPersonAFertileWoman currentDate person then
                        if isJust form.isPregnant then
                            ( 1, 1 )

                        else
                            ( 1, 0 )

                    else
                        ( 0, 0 )
            in
            ( taskCompleted form.rapidTestResult + isPregnantCompleted
            , 1 + isPregnantActive
            )

        LaboratoryCovidTesting ->
            let
                form =
                    measurements.covidTesting
                        |> getMeasurementValueFunc
                        |> covidTestingFormWithDefault data.covidTestingForm

                ( derivedCompleted, derivedActive ) =
                    Maybe.map
                        (\testPerformed ->
                            let
                                ( isPregnantActive, isPregnantCompleted ) =
                                    if isPersonAFertileWoman currentDate person then
                                        if isJust form.isPregnant then
                                            ( 1, 1 )

                                        else
                                            ( 1, 0 )

                                    else
                                        ( 0, 0 )
                            in
                            if testPerformed then
                                Maybe.map
                                    (\testPositive ->
                                        if testPositive then
                                            ( 1 + isPregnantCompleted, 1 + isPregnantActive )

                                        else
                                            ( 1, 1 )
                                    )
                                    form.testPositive
                                    |> Maybe.withDefault ( 0, 1 )

                            else
                                ( taskCompleted form.administrationNote + isPregnantCompleted
                                , 1 + isPregnantActive
                                )
                        )
                        form.testPerformed
                        |> Maybe.withDefault ( 0, 0 )
            in
            ( taskCompleted form.testPerformed + derivedCompleted
            , 1 + derivedActive
            )


exposureTasksCompletedFromTotal : AcuteIllnessMeasurements -> ExposureData -> ExposureTask -> ( Int, Int )
exposureTasksCompletedFromTotal measurements data task =
    case task of
        ExposureTravel ->
            let
                form =
                    measurements.travelHistory
                        |> getMeasurementValueFunc
                        |> travelHistoryFormWithDefault data.travelHistoryForm
            in
            ( taskCompleted form.covid19Country
            , 1
            )

        ExposureExposure ->
            let
                form =
                    measurements.exposure
                        |> getMeasurementValueFunc
                        |> exposureFormWithDefault data.exposureForm
            in
            ( taskCompleted form.covid19Symptoms
            , 1
            )


treatmentTasksCompletedFromTotal : AcuteIllnessMeasurements -> PriorTreatmentData -> PriorTreatmentTask -> ( Int, Int )
treatmentTasksCompletedFromTotal measurements data task =
    case task of
        TreatmentReview ->
            let
                form =
                    measurements.treatmentReview
                        |> getMeasurementValueFunc
                        |> treatmentReviewFormWithDefault data.treatmentReviewForm

                ( feverActive, feverCompleted ) =
                    form.feverPast6Hours
                        |> Maybe.map
                            (\receivedTreatment ->
                                if receivedTreatment then
                                    if isJust form.feverPast6HoursHelped then
                                        ( 2, 2 )

                                    else
                                        ( 1, 2 )

                                else
                                    ( 1, 1 )
                            )
                        |> Maybe.withDefault ( 0, 1 )

                ( malariaTodayActive, malariaTodayCompleted ) =
                    form.malariaToday
                        |> Maybe.map
                            (\receivedTreatment ->
                                if receivedTreatment then
                                    if isJust form.malariaTodayHelped then
                                        ( 2, 2 )

                                    else
                                        ( 1, 2 )

                                else
                                    ( 1, 1 )
                            )
                        |> Maybe.withDefault ( 0, 1 )

                ( malariaWithinPastMonth, malariaWithinPastMonthCompleted ) =
                    form.malariaWithinPastMonth
                        |> Maybe.map
                            (\receivedTreatment ->
                                if receivedTreatment then
                                    if isJust form.malariaWithinPastMonthHelped then
                                        ( 2, 2 )

                                    else
                                        ( 1, 2 )

                                else
                                    ( 1, 1 )
                            )
                        |> Maybe.withDefault ( 0, 1 )
            in
            ( feverActive + malariaTodayActive + malariaWithinPastMonth
            , feverCompleted + malariaTodayCompleted + malariaWithinPastMonthCompleted
            )


nextStepsTasksCompletedFromTotal :
    Bool
    -> Bool
    -> Maybe AcuteIllnessDiagnosis
    -> AcuteIllnessMeasurements
    -> NextStepsData
    -> NextStepsTask
    -> ( Int, Int )
nextStepsTasksCompletedFromTotal isChw initialEncounter diagnosis measurements data task =
    case task of
        NextStepsIsolation ->
            let
                form =
                    measurements.isolation
                        |> getMeasurementValueFunc
                        |> isolationFormWithDefault data.isolationForm

                ( derivedActive, derivedCompleted ) =
                    case form.patientIsolated of
                        Just True ->
                            if isChw then
                                ( 2, taskCompleted form.healthEducation + taskCompleted form.signOnDoor )

                            else
                                ( 1, taskCompleted form.healthEducation )

                        Just False ->
                            ( 2, taskCompleted form.healthEducation + naListTaskCompleted IsolationReasonNotApplicable form.reasonsForNotIsolating )

                        Nothing ->
                            ( 0, 0 )
            in
            ( taskCompleted form.patientIsolated + derivedCompleted
            , 1 + derivedActive
            )

        NextStepsContactHC ->
            let
                form =
                    measurements.hcContact
                        |> getMeasurementValueFunc
                        |> hcContactFormWithDefault data.hcContactForm
            in
            form.contactedHC
                |> Maybe.map
                    (\contactedHC ->
                        if contactedHC then
                            let
                                recommendationsCompleted =
                                    naTaskCompleted HCRecommendationNotApplicable form.recommendations

                                ( ambulanceActive, ambulanceCompleted ) =
                                    form.recommendations
                                        |> Maybe.map
                                            (\recommendations ->
                                                if recommendations == SendAmbulance then
                                                    ( naTaskCompleted ResponsePeriodNotApplicable form.ambulanceArrivalPeriod
                                                    , naTaskCompleted ResponsePeriodNotApplicable form.ambulanceArrivalPeriod
                                                    )

                                                else
                                                    ( 0, 0 )
                                            )
                                        |> Maybe.withDefault ( 0, 0 )
                            in
                            ( 1 + recommendationsCompleted + naTaskCompleted ResponsePeriodNotApplicable form.responsePeriod + ambulanceCompleted
                            , 2 + naTaskCompleted ResponsePeriodNotApplicable form.responsePeriod + ambulanceActive
                            )

                        else
                            ( 1, 1 )
                    )
                |> Maybe.withDefault ( 0, 1 )

        NextStepsCall114 ->
            let
                form =
                    measurements.call114
                        |> getMeasurementValueFunc
                        |> call114FormWithDefault data.call114Form
            in
            form.called114
                |> Maybe.map
                    (\called114 ->
                        if called114 then
                            form.recommendation114
                                |> Maybe.map
                                    (\recommendation114 ->
                                        -- We do not show qustions about contacting site, if
                                        -- 114 did not recommend to contact a site.
                                        if List.member recommendation114 [ OtherRecommendation114, NoneNoAnswer, NoneBusySignal, NoneOtherRecommendation114 ] then
                                            ( 2, 2 )

                                        else
                                            form.contactedSite
                                                |> Maybe.map
                                                    (\_ ->
                                                        if isJust form.recommendationSite then
                                                            ( 4, 4 )

                                                        else
                                                            ( 3, 4 )
                                                    )
                                                |> Maybe.withDefault ( 2, 3 )
                                    )
                                |> Maybe.withDefault ( 1, 2 )

                        else if isJust form.recommendation114 then
                            ( 2, 2 )

                        else
                            ( 1, 2 )
                    )
                |> Maybe.withDefault ( 0, 1 )

        NextStepsMedicationDistribution ->
            let
                form =
                    measurements.medicationDistribution
                        |> getMeasurementValueFunc
                        |> medicationDistributionFormWithDefault data.medicationDistributionForm

                derivedQuestionExists formValue =
                    if formValue == Just False then
                        1

                    else
                        0

                derivedQuestionCompleted medication reasonToSignFunc formValue =
                    if formValue /= Just False then
                        0

                    else
                        let
                            valueSet =
                                getCurrentReasonForMedicationNonAdministration reasonToSignFunc form
                                    |> isJust
                        in
                        if valueSet then
                            1

                        else
                            0
            in
            case diagnosis of
                Just DiagnosisMalariaUncomplicated ->
                    ( taskCompleted form.coartem + derivedQuestionCompleted Coartem MedicationCoartem form.coartem
                    , 1 + derivedQuestionExists form.coartem
                    )

                Just DiagnosisGastrointestinalInfectionUncomplicated ->
                    ( taskCompleted form.ors
                        + taskCompleted form.zinc
                        + derivedQuestionCompleted ORS MedicationORS form.ors
                        + derivedQuestionCompleted Zinc MedicationZinc form.zinc
                    , 2
                        + derivedQuestionExists form.ors
                        + derivedQuestionExists form.zinc
                    )

                Just DiagnosisSimpleColdAndCough ->
                    ( taskCompleted form.lemonJuiceOrHoney
                    , 1
                    )

                -- This is for child form 2 month old, to 5 years old.
                Just DiagnosisRespiratoryInfectionUncomplicated ->
                    ( taskCompleted form.amoxicillin + derivedQuestionCompleted Amoxicillin MedicationAmoxicillin form.amoxicillin
                    , 1 + derivedQuestionExists form.amoxicillin
                    )

                Just DiagnosisPneuminialCovid19 ->
                    ( taskCompleted form.amoxicillin + derivedQuestionCompleted Amoxicillin MedicationAmoxicillin form.amoxicillin
                    , 1 + derivedQuestionExists form.amoxicillin
                    )

                _ ->
                    ( 0, 1 )

        NextStepsSendToHC ->
            let
                form =
                    measurements.sendToHC
                        |> getMeasurementValueFunc
                        |> sendToHCFormWithDefault data.sendToHCForm

                ( reasonForNotSentActive, reasonForNotSentCompleted ) =
                    form.referToHealthCenter
                        |> Maybe.map
                            (\sentToHC ->
                                if not sentToHC then
                                    if isJust form.reasonForNotSendingToHC then
                                        ( 2, 2 )

                                    else
                                        ( 1, 2 )

                                else
                                    ( 1, 1 )
                            )
                        |> Maybe.withDefault ( 0, 1 )
            in
            ( reasonForNotSentActive + taskCompleted form.handReferralForm
            , reasonForNotSentCompleted + 1
            )

        NextStepsHealthEducation ->
            let
                form =
                    measurements.healthEducation
                        |> getMeasurementValueFunc
                        |> healthEducationFormWithDefault data.healthEducationForm

                ( reasonForProvidingEducationActive, reasonForProvidingEducationCompleted ) =
                    form.educationForDiagnosis
                        |> Maybe.map
                            (\providedHealthEducation ->
                                if not providedHealthEducation then
                                    if isJust form.reasonForNotProvidingHealthEducation then
                                        ( 1, 1 )

                                    else
                                        ( 0, 1 )

                                else
                                    ( 0, 0 )
                            )
                        |> Maybe.withDefault ( 0, 0 )
            in
            ( reasonForProvidingEducationActive + taskCompleted form.educationForDiagnosis
            , reasonForProvidingEducationCompleted + 1
            )

        NextStepsFollowUp ->
            let
                form =
                    measurements.followUp
                        |> getMeasurementValueFunc
                        |> followUpFormWithDefault data.followUpForm
            in
            ( taskCompleted form.option
            , 1
            )

        NextStepsContactTracing ->
            if data.contactsTracingForm.finished then
                ( 1, 1 )

            else
                ( 0, 1 )

        NextStepsSymptomsReliefGuidance ->
            let
                form =
                    measurements.healthEducation
                        |> getMeasurementValueFunc
                        |> healthEducationFormWithDefault data.healthEducationForm
            in
            ( taskCompleted form.educationForDiagnosis
            , 1
            )


ongoingTreatmentTasksCompletedFromTotal : Language -> NominalDate -> AcuteIllnessMeasurements -> OngoingTreatmentData -> OngoingTreatmentTask -> ( Int, Int )
ongoingTreatmentTasksCompletedFromTotal language currentDate measurements data task =
    case task of
        OngoingTreatmentReview ->
            let
                form =
                    getMeasurementValueFunc measurements.treatmentOngoing
                        |> ongoingTreatmentReviewFormWithDefault data.treatmentReviewForm

                ( _, tasks ) =
                    treatmentReviewInputsAndTasks language
                        currentDate
                        SetOngoingTreatmentReviewBoolInput
                        SetReasonForNotTaking
                        SetTotalMissedDoses
                        SetAdverseEvent
                        form
            in
            ( Maybe.Extra.values tasks
                |> List.length
            , List.length tasks
            )


dangerSignsTasksCompletedFromTotal : AcuteIllnessMeasurements -> DangerSignsData -> DangerSignsTask -> ( Int, Int )
dangerSignsTasksCompletedFromTotal measurements data task =
    case task of
        ReviewDangerSigns ->
            let
                form =
                    measurements.dangerSigns
                        |> getMeasurementValueFunc
                        |> reviewDangerSignsFormWithDefault data.reviewDangerSignsForm
            in
            ( taskCompleted form.conditionImproving + taskCompleted form.symptoms
            , 2
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
            not isChw && covid19SuspectDiagnosed assembled.measurements


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
        [ NextStepsContactTracing, NextStepsIsolation, NextStepsCall114, NextStepsContactHC, NextStepsMedicationDistribution, NextStepsSendToHC, NextStepsSymptomsReliefGuidance, NextStepsFollowUp ]
            |> List.filter (expectNextStepsTask currentDate isChw assembled)

    else if mandatoryActivitiesCompletedSubsequentVisit currentDate isChw assembled then
        -- The order is important. Do not change.
        [ NextStepsContactHC, NextStepsMedicationDistribution, NextStepsSendToHC, NextStepsHealthEducation, NextStepsFollowUp ]
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
        NextStepsIsolation ->
            (isChw && (diagnosis == Just DiagnosisCovid19Suspect))
                || (diagnosis == Just DiagnosisPneuminialCovid19)
                || (diagnosis == Just DiagnosisLowRiskCovid19)

        NextStepsCall114 ->
            isChw && (diagnosis == Just DiagnosisCovid19Suspect)

        NextStepsContactHC ->
            isChw && (diagnosis == Just DiagnosisCovid19Suspect) && isJust measurements.call114 && (not <| talkedTo114 measurements)

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
                expectNextStepsTaskFirstEncounter currentDate isChw person diagnosis measurements NextStepsIsolation
                    || expectNextStepsTaskFirstEncounter currentDate isChw person diagnosis measurements NextStepsCall114
                    || expectNextStepsTaskFirstEncounter currentDate isChw person diagnosis measurements NextStepsContactHC
                    || expectNextStepsTaskFirstEncounter currentDate isChw person diagnosis measurements NextStepsMedicationDistribution
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
                -- No improvement, without danger signs.
                noImprovementOnSubsequentVisitWithoutDangerSigns currentDate person measurements
                    || -- No improvement, with danger signs, and diagnosis is not Covid19 suspect.
                       (noImprovementOnSubsequentVisitWithDangerSigns currentDate person measurements && diagnosis /= Just DiagnosisCovid19Suspect)
                    || -- No improvement, with danger signs, diagnosis is Covid19, and HC recomended to send patient over.
                       (noImprovementOnSubsequentVisitWithDangerSigns currentDate person measurements
                            && (diagnosis == Just DiagnosisCovid19Suspect)
                            && healthCenterRecommendedToCome measurements
                       )

        NextStepsContactHC ->
            not malariaDiagnosedAtCurrentEncounter
                && -- No improvement, with danger signs, and diagnosis is Covid19.
                   (noImprovementOnSubsequentVisitWithDangerSigns currentDate person measurements && diagnosis == Just DiagnosisCovid19Suspect)

        NextStepsHealthEducation ->
            not malariaDiagnosedAtCurrentEncounter

        NextStepsFollowUp ->
            -- Whenever we have a next step task that is other than NextStepsHealthEducation.
            -- When there's only NextStepsHealthEducation, illness will be resolved, therefore,
            -- there's no need for a follow up.
            expectNextStepsTaskSubsequentEncounter currentDate person diagnosis measurements NextStepsMedicationDistribution
                || expectNextStepsTaskSubsequentEncounter currentDate person diagnosis measurements NextStepsSendToHC
                || expectNextStepsTaskSubsequentEncounter currentDate person diagnosis measurements NextStepsContactHC

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
        NextStepsIsolation ->
            (not <| taskExpected NextStepsIsolation) || isJust measurements.isolation

        NextStepsContactHC ->
            (not <| taskExpected NextStepsContactHC) || isJust measurements.hcContact

        NextStepsCall114 ->
            (not <| taskExpected NextStepsCall114) || isJust measurements.call114

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
        || noImprovementOnSubsequentVisitWithDangerSigns currentDate person measurements


noImprovementOnSubsequentVisitWithoutDangerSigns : NominalDate -> Person -> AcuteIllnessMeasurements -> Bool
noImprovementOnSubsequentVisitWithoutDangerSigns currentDate person measurements =
    (not <| dangerSignPresentOnSubsequentVisit measurements)
        && (conditionNotImprovingOnSubsequentVisit measurements
                || sendToHCOnSubsequentVisitByVitals currentDate person measurements
                || sendToHCOnSubsequentVisitByMuac measurements
                || sendToHCOnSubsequentVisitByNutrition measurements
           )


noImprovementOnSubsequentVisitWithDangerSigns : NominalDate -> Person -> AcuteIllnessMeasurements -> Bool
noImprovementOnSubsequentVisitWithDangerSigns currentDate person measurements =
    dangerSignPresentOnSubsequentVisit measurements


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
    [ AcuteIllnessSymptoms, AcuteIllnessExposure, AcuteIllnessPhysicalExam ]
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

        AcuteIllnessExposure ->
            isJust measurements.travelHistory
                && isJust measurements.exposure

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
        countSigns measurement_ exclusion =
            measurement_
                |> Maybe.map
                    (\measurement ->
                        let
                            set =
                                Tuple.second measurement |> .value

                            setSize =
                                EverySet.size set
                        in
                        case setSize of
                            1 ->
                                if EverySet.member exclusion set then
                                    0

                                else
                                    1

                            _ ->
                                setSize
                    )
                |> Maybe.withDefault 0

        generalSymptomsCount =
            let
                excludesGeneral =
                    [ SymptomGeneralFever ] ++ symptomsGeneralDangerSigns
            in
            countGeneralSymptoms measurements excludesGeneral

        respiratorySymptomsCount =
            countRespiratorySymptoms measurements []

        giSymptomsCount =
            countGISymptoms measurements []

        symptomsIndicateCovid =
            if giSymptomsCount > 0 then
                respiratorySymptomsCount > 0

            else
                let
                    totalSymptoms =
                        generalSymptomsCount + respiratorySymptomsCount + giSymptomsCount
                in
                totalSymptoms > 1

        totalSigns =
            countSigns measurements.travelHistory NoTravelHistorySigns
                + countSigns measurements.exposure NoExposureSigns

        signsIndicateCovid =
            totalSigns > 0

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
    (signsIndicateCovid && symptomsIndicateCovid)
        || (signsIndicateCovid && feverOnRecord)
        || (not signsIndicateCovid && feverAndRdtNotPositive && respiratorySymptomsCount > 0)
        || (not signsIndicateCovid && feverAndRdtNotPositive && generalSymptomsCount > 1)


{-| This may result in Covid diagnosis, or Malaria diagnosis,
if Covid RDT could not be perfrmed.
-}
covid19DiagnosisPath : NominalDate -> Person -> Bool -> AcuteIllnessMeasurements -> Maybe AcuteIllnessDiagnosis
covid19DiagnosisPath currentDate person isChw measurements =
    if
        (not <| covid19SuspectDiagnosed measurements)
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
    measurements.covidTesting
        |> getMeasurementValueFunc
        |> Maybe.map .result


malariaRapidTestResult : AcuteIllnessMeasurements -> Maybe RapidTestResult
malariaRapidTestResult measurements =
    measurements.malariaTesting
        |> getMeasurementValueFunc


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
