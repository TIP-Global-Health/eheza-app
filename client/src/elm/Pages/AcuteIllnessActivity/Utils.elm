module Pages.AcuteIllnessActivity.Utils exposing (..)

import AssocList as Dict exposing (Dict)
import Backend.AcuteIllnessEncounter.Model exposing (AcuteIllnessDiagnosis(..))
import Backend.Measurement.Model
    exposing
        ( AcuteFindingsGeneralSign(..)
        , AcuteFindingsRespiratorySign(..)
        , AcuteFindingsValue
        , AcuteIllnessDangerSign(..)
        , AcuteIllnessMeasurement
        , AcuteIllnessMeasurements
        , AcuteIllnessVitalsValue
        , AdverseEvent(..)
        , Call114Sign(..)
        , Call114Value
        , ChildNutritionSign(..)
        , ExposureSign(..)
        , HCContactSign(..)
        , HCContactValue
        , HCRecommendation(..)
        , IsolationSign(..)
        , IsolationValue
        , MalariaRapidTestResult(..)
        , MedicationDistributionSign(..)
        , MedicationDistributionValue
        , MedicationNonAdministrationReason(..)
        , MedicationNonAdministrationSign(..)
        , MuacInCm(..)
        , ReasonForNotIsolating(..)
        , ReasonForNotTaking(..)
        , Recommendation114(..)
        , RecommendationSite(..)
        , ResponsePeriod(..)
        , SendToHCSign(..)
        , SymptomsGIDerivedSign(..)
        , SymptomsGISign(..)
        , SymptomsGIValue
        , SymptomsGeneralSign(..)
        , SymptomsRespiratorySign(..)
        , TravelHistorySign(..)
        , TreatmentOngoingSign(..)
        , TreatmentOngoingValue
        , TreatmentReviewSign(..)
        )
import Backend.Person.Model exposing (Person)
import Backend.Person.Utils exposing (ageInMonths, ageInYears, isChildUnderAgeOf5)
import EverySet exposing (EverySet)
import Gizra.NominalDate exposing (NominalDate)
import Maybe.Extra exposing (andMap, isJust, isNothing, or, unwrap)
import Pages.AcuteIllnessActivity.Model exposing (..)
import Pages.AcuteIllnessEncounter.Model exposing (AssembledData)
import Pages.PrenatalActivity.Utils exposing (ifFalse, ifNullableTrue, ifTrue)
import Pages.Utils exposing (ifEverySetEmpty, maybeValueConsideringIsDirtyField, taskCompleted, valueConsideringIsDirtyField)


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
                    measurements.symptomsGeneral
                        |> Maybe.map (Tuple.second >> .value)
                        |> symptomsGeneralFormWithDefault data.symptomsGeneralForm
            in
            ( taskNotCompleted (Dict.isEmpty form.signs)
            , 1
            )

        SymptomsRespiratory ->
            let
                form =
                    measurements.symptomsRespiratory
                        |> Maybe.map (Tuple.second >> .value)
                        |> symptomsRespiratoryFormWithDefault data.symptomsRespiratoryForm
            in
            ( taskNotCompleted (Dict.isEmpty form.signs)
            , 1
            )

        SymptomsGI ->
            let
                form =
                    measurements.symptomsGI
                        |> Maybe.map (Tuple.second >> .value)
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


physicalExamTasksCompletedFromTotal : AcuteIllnessMeasurements -> PhysicalExamData -> PhysicalExamTask -> ( Int, Int )
physicalExamTasksCompletedFromTotal measurements data task =
    case task of
        PhysicalExamVitals ->
            let
                form =
                    measurements.vitals
                        |> Maybe.map (Tuple.second >> .value)
                        |> vitalsFormWithDefault data.vitalsForm
            in
            ( taskCompleted form.respiratoryRate + taskCompleted form.bodyTemperature
            , 2
            )

        PhysicalExamMuac ->
            let
                form =
                    measurements.muac
                        |> Maybe.map (Tuple.second >> .value)
                        |> muacFormWithDefault data.muacForm
            in
            ( taskCompleted form.muac
            , 1
            )

        PhysicalExamAcuteFindings ->
            let
                form =
                    measurements.acuteFindings
                        |> Maybe.map (Tuple.second >> .value)
                        |> acuteFindingsFormWithDefault data.acuteFindingsForm
            in
            ( taskCompleted form.signsGeneral + taskCompleted form.signsRespiratory
            , 2
            )

        PhysicalExamNutrition ->
            let
                form =
                    measurements.nutrition
                        |> Maybe.map (Tuple.second >> .value)
                        |> nutritionFormWithDefault data.nutritionForm
            in
            ( taskCompleted form.signs
            , 1
            )


laboratoryTasksCompletedFromTotal : AcuteIllnessMeasurements -> LaboratoryData -> LaboratoryTask -> ( Int, Int )
laboratoryTasksCompletedFromTotal measurements data task =
    case task of
        LaboratoryMalariaTesting ->
            let
                form =
                    measurements.malariaTesting
                        |> Maybe.map (Tuple.second >> .value)
                        |> malariaTestingFormWithDefault data.malariaTestingForm
            in
            ( taskCompleted form.rapidTestResult
            , 1
            )


exposureTasksCompletedFromTotal : AcuteIllnessMeasurements -> ExposureData -> ExposureTask -> ( Int, Int )
exposureTasksCompletedFromTotal measurements data task =
    case task of
        ExposureTravel ->
            let
                form =
                    measurements.travelHistory
                        |> Maybe.map (Tuple.second >> .value)
                        |> travelHistoryFormWithDefault data.travelHistoryForm
            in
            ( taskCompleted form.covid19Country
            , 1
            )

        ExposureExposure ->
            let
                form =
                    measurements.exposure
                        |> Maybe.map (Tuple.second >> .value)
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
                        |> Maybe.map (Tuple.second >> .value)
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


nextStepsTasksCompletedFromTotal : Maybe AcuteIllnessDiagnosis -> AcuteIllnessMeasurements -> NextStepsData -> NextStepsTask -> ( Int, Int )
nextStepsTasksCompletedFromTotal diagnosis measurements data task =
    case task of
        NextStepsIsolation ->
            let
                form =
                    measurements.isolation
                        |> Maybe.map (Tuple.second >> .value)
                        |> isolationFormWithDefault data.isolationForm

                ( derivedActive, derivedCompleted ) =
                    case form.patientIsolated of
                        Just True ->
                            ( 2, taskCompleted form.healthEducation + taskCompleted form.signOnDoor )

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
                        |> Maybe.map (Tuple.second >> .value)
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
                        |> Maybe.map (Tuple.second >> .value)
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
                                                    (\contactedSite ->
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
                        |> Maybe.map (Tuple.second >> .value)
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
                            nonAdministrationSigns =
                                form.nonAdministrationSigns |> Maybe.withDefault EverySet.empty

                            valueSet =
                                getCurrentReasonForMedicaitonNonAdministration reasonToSignFunc form
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

                _ ->
                    ( 0, 1 )

        NextStepsSendToHC ->
            let
                form =
                    measurements.sendToHC
                        |> Maybe.map (Tuple.second >> .value)
                        |> sendToHCFormWithDefault data.sendToHCForm
            in
            ( taskCompleted form.handReferralForm + taskCompleted form.handReferralForm
            , 2
            )


ongoingTreatmentTasksCompletedFromTotal : AcuteIllnessMeasurements -> OngoingTreatmentData -> OngoingTreatmentTask -> ( Int, Int )
ongoingTreatmentTasksCompletedFromTotal measurements data task =
    case task of
        OngoingTreatmentReview ->
            let
                form =
                    measurements.treatmentOngoing
                        |> Maybe.map (Tuple.second >> .value)
                        |> ongoingTreatmentReviewFormWithDefault data.treatmentReviewForm

                ( takenAsPrescribedActive, takenAsPrescribedComleted ) =
                    form.takenAsPrescribed
                        |> Maybe.map
                            (\takenAsPrescribed ->
                                if not takenAsPrescribed then
                                    if isJust form.reasonForNotTaking then
                                        ( 2, 2 )

                                    else
                                        ( 1, 2 )

                                else
                                    ( 1, 1 )
                            )
                        |> Maybe.withDefault ( 0, 1 )

                ( missedDosesActive, missedDosesCompleted ) =
                    form.missedDoses
                        |> Maybe.map
                            (\missedDoses ->
                                if missedDoses then
                                    if isJust form.totalMissedDoses then
                                        ( 2, 2 )

                                    else
                                        ( 1, 2 )

                                else
                                    ( 1, 1 )
                            )
                        |> Maybe.withDefault ( 0, 1 )

                ( adverseEventsActive, adverseEventsCompleted ) =
                    form.sideEffects
                        |> Maybe.map
                            (\sideEffects ->
                                if sideEffects then
                                    if isJust form.adverseEvents then
                                        ( 2, 2 )

                                    else
                                        ( 1, 2 )

                                else
                                    ( 1, 1 )
                            )
                        |> Maybe.withDefault ( 0, 1 )
            in
            ( takenAsPrescribedActive + missedDosesActive + adverseEventsActive + taskCompleted form.feelingBetter
            , takenAsPrescribedComleted + missedDosesCompleted + adverseEventsCompleted + 1
            )


dangerSignsTasksCompletedFromTotal : AcuteIllnessMeasurements -> DangerSignsData -> DangerSignsTask -> ( Int, Int )
dangerSignsTasksCompletedFromTotal measurements data task =
    case task of
        ReviewDangerSigns ->
            let
                form =
                    measurements.dangerSigns
                        |> Maybe.map (Tuple.second >> .value)
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


fromVitalsValue : Maybe AcuteIllnessVitalsValue -> VitalsForm
fromVitalsValue saved =
    { respiratoryRate = Maybe.map .respiratoryRate saved
    , respiratoryRateDirty = False
    , bodyTemperature = Maybe.map .bodyTemperature saved
    , bodyTemperatureDirty = False
    }


vitalsFormWithDefault : VitalsForm -> Maybe AcuteIllnessVitalsValue -> VitalsForm
vitalsFormWithDefault form saved =
    saved
        |> unwrap
            form
            (\value ->
                { respiratoryRate = valueConsideringIsDirtyField form.respiratoryRateDirty form.respiratoryRate value.respiratoryRate
                , respiratoryRateDirty = form.respiratoryRateDirty
                , bodyTemperature = valueConsideringIsDirtyField form.bodyTemperatureDirty form.bodyTemperature value.bodyTemperature
                , bodyTemperatureDirty = form.bodyTemperatureDirty
                }
            )


toVitalsValueWithDefault : Maybe AcuteIllnessVitalsValue -> VitalsForm -> Maybe AcuteIllnessVitalsValue
toVitalsValueWithDefault saved form =
    vitalsFormWithDefault form saved
        |> toVitalsValue


toVitalsValue : VitalsForm -> Maybe AcuteIllnessVitalsValue
toVitalsValue form =
    Maybe.map AcuteIllnessVitalsValue form.respiratoryRate
        |> andMap form.bodyTemperature


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


fromMalariaTestingValue : Maybe MalariaRapidTestResult -> MalariaTestingForm
fromMalariaTestingValue saved =
    if saved == Just RapidTestPositiveAndPregnant then
        { rapidTestResult = Just RapidTestPositive
        , isPregnant = Just True
        }

    else
        { rapidTestResult = saved
        , isPregnant = Just False
        }


malariaTestingFormWithDefault : MalariaTestingForm -> Maybe MalariaRapidTestResult -> MalariaTestingForm
malariaTestingFormWithDefault form saved =
    saved
        |> unwrap
            form
            (\value ->
                let
                    formWithDefault =
                        fromMalariaTestingValue saved
                in
                { rapidTestResult = or form.rapidTestResult formWithDefault.rapidTestResult
                , isPregnant = or form.isPregnant formWithDefault.isPregnant
                }
            )


toMalariaTestingValueWithDefault : Maybe MalariaRapidTestResult -> MalariaTestingForm -> Maybe MalariaRapidTestResult
toMalariaTestingValueWithDefault saved form =
    malariaTestingFormWithDefault form saved
        |> (\form_ ->
                if form_.rapidTestResult == Just RapidTestPositive && form_.isPregnant == Just True then
                    { form_ | rapidTestResult = Just RapidTestPositiveAndPregnant }

                else
                    form_
           )
        |> toMalariaTestingValue


toMalariaTestingValue : MalariaTestingForm -> Maybe MalariaRapidTestResult
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


fromSendToHCValue : Maybe (EverySet SendToHCSign) -> SendToHCForm
fromSendToHCValue saved =
    { handReferralForm = Maybe.map (EverySet.member HandReferrerForm) saved
    , referToHealthCenter = Maybe.map (EverySet.member ReferToHealthCenter) saved
    }


sendToHCFormWithDefault : SendToHCForm -> Maybe (EverySet SendToHCSign) -> SendToHCForm
sendToHCFormWithDefault form saved =
    saved
        |> unwrap
            form
            (\value ->
                { handReferralForm = or form.handReferralForm (EverySet.member HandReferrerForm value |> Just)
                , referToHealthCenter = or form.referToHealthCenter (EverySet.member ReferToHealthCenter value |> Just)
                }
            )


toSendToHCValueWithDefault : Maybe (EverySet SendToHCSign) -> SendToHCForm -> Maybe (EverySet SendToHCSign)
toSendToHCValueWithDefault saved form =
    sendToHCFormWithDefault form saved
        |> toSendToHCValue


toSendToHCValue : SendToHCForm -> Maybe (EverySet SendToHCSign)
toSendToHCValue form =
    [ Maybe.map (ifTrue HandReferrerForm) form.handReferralForm
    , Maybe.map (ifTrue ReferToHealthCenter) form.referToHealthCenter
    ]
        |> Maybe.Extra.combine
        |> Maybe.map (List.foldl EverySet.union EverySet.empty >> ifEverySetEmpty NoSendToHCSigns)


fromMedicationDistributionValue : Maybe MedicationDistributionValue -> MedicationDistributionForm
fromMedicationDistributionValue saved =
    { amoxicillin = Maybe.map (.distributionSigns >> EverySet.member Amoxicillin) saved
    , coartem = Maybe.map (.distributionSigns >> EverySet.member Coartem) saved
    , ors = Maybe.map (.distributionSigns >> EverySet.member ORS) saved
    , zinc = Maybe.map (.distributionSigns >> EverySet.member Zinc) saved
    , lemonJuiceOrHoney = Maybe.map (.distributionSigns >> EverySet.member LemonJuiceOrHoney) saved
    , nonAdministrationSigns = Maybe.map .nonAdministrationSigns saved
    }


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


resolveAmoxicillinDosage : NominalDate -> Person -> Maybe String
resolveAmoxicillinDosage currentDate person =
    ageInMonths currentDate person
        |> Maybe.andThen
            (\months ->
                if months < 2 then
                    Nothing

                else if months < 5 then
                    Just "1"

                else if months < 12 then
                    Just "2"

                else if months < 30 then
                    Just "3"

                else if months < 60 then
                    Just "4"

                else
                    Nothing
            )


getCurrentReasonForMedicaitonNonAdministration :
    (MedicationNonAdministrationReason -> MedicationNonAdministrationSign)
    -> MedicationDistributionForm
    -> Maybe MedicationNonAdministrationReason
getCurrentReasonForMedicaitonNonAdministration reasonToSignFunc form =
    let
        nonAdministrationSigns =
            form.nonAdministrationSigns |> Maybe.withDefault EverySet.empty
    in
    [ NonAdministrationLackOfStock, NonAdministrationKnownAllergy, NonAdministrationPatientDeclined, NonAdministrationOther ]
        |> List.filterMap
            (\reason ->
                if EverySet.member (reasonToSignFunc reason) nonAdministrationSigns then
                    Just reason

                else
                    Nothing
            )
        |> List.head


nonAdministrationReasonToSign : MedicationDistributionSign -> MedicationNonAdministrationReason -> MedicationNonAdministrationSign
nonAdministrationReasonToSign sign reason =
    case sign of
        Amoxicillin ->
            MedicationAmoxicillin reason

        Coartem ->
            MedicationCoartem reason

        ORS ->
            MedicationORS reason

        Zinc ->
            MedicationZinc reason

        _ ->
            NoMedicationNonAdministrationSigns


fromMuacValue : Maybe MuacInCm -> MuacForm
fromMuacValue saved =
    { muac = Maybe.map (\(MuacInCm cm) -> cm) saved
    , muacDirty = False
    }


muacFormWithDefault : MuacForm -> Maybe MuacInCm -> MuacForm
muacFormWithDefault form saved =
    saved
        |> unwrap
            form
            (\value ->
                { muac = valueConsideringIsDirtyField form.muacDirty form.muac (value |> (\(MuacInCm cm) -> cm))
                , muacDirty = form.muacDirty
                }
            )


toMuacValueWithDefault : Maybe MuacInCm -> MuacForm -> Maybe MuacInCm
toMuacValueWithDefault saved form =
    muacFormWithDefault form saved
        |> toMuacValue


toMuacValue : MuacForm -> Maybe MuacInCm
toMuacValue form =
    Maybe.map MuacInCm form.muac


fromOngoingTreatmentReviewValue : Maybe TreatmentOngoingValue -> OngoingTreatmentReviewForm
fromOngoingTreatmentReviewValue saved =
    { takenAsPrescribed = Maybe.map (.signs >> EverySet.member TakenAsPrescribed) saved
    , missedDoses = Maybe.map (.signs >> EverySet.member MissedDoses) saved
    , feelingBetter = Maybe.map (.signs >> EverySet.member FeelingBetter) saved
    , sideEffects = Maybe.map (.signs >> EverySet.member SideEffects) saved
    , reasonForNotTaking = Maybe.map .reasonForNotTaking saved
    , reasonForNotTakingDirty = False
    , totalMissedDoses = Maybe.map .missedDoses saved
    , totalMissedDosesDirty = False
    , adverseEvents = Maybe.map (.adverseEvents >> EverySet.toList) saved
    , adverseEventsDirty = False
    }


ongoingTreatmentReviewFormWithDefault : OngoingTreatmentReviewForm -> Maybe TreatmentOngoingValue -> OngoingTreatmentReviewForm
ongoingTreatmentReviewFormWithDefault form saved =
    saved
        |> unwrap
            form
            (\value ->
                { takenAsPrescribed = or form.takenAsPrescribed (EverySet.member TakenAsPrescribed value.signs |> Just)
                , missedDoses = or form.missedDoses (EverySet.member MissedDoses value.signs |> Just)
                , feelingBetter = or form.feelingBetter (EverySet.member FeelingBetter value.signs |> Just)
                , sideEffects = or form.sideEffects (EverySet.member SideEffects value.signs |> Just)
                , reasonForNotTaking = valueConsideringIsDirtyField form.reasonForNotTakingDirty form.reasonForNotTaking value.reasonForNotTaking
                , reasonForNotTakingDirty = form.reasonForNotTakingDirty
                , totalMissedDoses = valueConsideringIsDirtyField form.totalMissedDosesDirty form.totalMissedDoses value.missedDoses
                , totalMissedDosesDirty = form.totalMissedDosesDirty
                , adverseEvents = maybeValueConsideringIsDirtyField form.adverseEventsDirty form.adverseEvents (value.adverseEvents |> EverySet.toList |> Just)
                , adverseEventsDirty = form.adverseEventsDirty
                }
            )


toOngoingTreatmentReviewValueWithDefault : Maybe TreatmentOngoingValue -> OngoingTreatmentReviewForm -> Maybe TreatmentOngoingValue
toOngoingTreatmentReviewValueWithDefault saved form =
    ongoingTreatmentReviewFormWithDefault form saved
        |> toOngoingTreatmentReviewValue


toOngoingTreatmentReviewValue : OngoingTreatmentReviewForm -> Maybe TreatmentOngoingValue
toOngoingTreatmentReviewValue form =
    let
        signs =
            [ Maybe.map (ifTrue TakenAsPrescribed) form.takenAsPrescribed
            , Maybe.map (ifTrue MissedDoses) form.missedDoses
            , Maybe.map (ifTrue FeelingBetter) form.feelingBetter
            , Maybe.map (ifTrue SideEffects) form.sideEffects
            ]
                |> Maybe.Extra.combine
                |> Maybe.map (List.foldl EverySet.union EverySet.empty >> ifEverySetEmpty NoTreatmentOngoingSign)
    in
    Maybe.map TreatmentOngoingValue signs
        |> andMap (form.reasonForNotTaking |> Maybe.withDefault NoReasonForNotTakingSign |> Just)
        |> andMap (form.totalMissedDoses |> Maybe.withDefault 0 |> Just)
        |> andMap (form.adverseEvents |> fromListWithDefaultValue NoAdverseEvent |> Just)


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


fromNutritionValue : Maybe (EverySet ChildNutritionSign) -> NutritionForm
fromNutritionValue saved =
    { signs = Maybe.map EverySet.toList saved }


nutritionFormWithDefault : NutritionForm -> Maybe (EverySet ChildNutritionSign) -> NutritionForm
nutritionFormWithDefault form saved =
    saved
        |> unwrap
            form
            (\value -> { signs = or form.signs (EverySet.toList value |> Just) })


toNutritionValueWithDefault : Maybe (EverySet ChildNutritionSign) -> NutritionForm -> Maybe (EverySet ChildNutritionSign)
toNutritionValueWithDefault saved form =
    nutritionFormWithDefault form saved
        |> toNutritionValue


toNutritionValue : NutritionForm -> Maybe (EverySet ChildNutritionSign)
toNutritionValue form =
    Maybe.map (EverySet.fromList >> ifEverySetEmpty NormalChildNutrition) form.signs


expectPhysicalExamTask : NominalDate -> Person -> Bool -> PhysicalExamTask -> Bool
expectPhysicalExamTask currentDate person isFirstEncounter task =
    case task of
        PhysicalExamVitals ->
            True

        -- We show Muac for children under age of 5.
        PhysicalExamMuac ->
            isChildUnderAgeOf5 currentDate person

        -- We show Nutrition for children under age of 5.
        PhysicalExamNutrition ->
            isChildUnderAgeOf5 currentDate person

        -- We show Acute Finding only on first encounter
        PhysicalExamAcuteFindings ->
            isFirstEncounter



-- HELPER FUNCTIONS


resolvePreviousValue : AssembledData -> (AcuteIllnessMeasurements -> Maybe ( id, AcuteIllnessMeasurement a )) -> (a -> b) -> Maybe b
resolvePreviousValue assembled measurementFunc valueFunc =
    assembled.previousMeasurementsWithDates
        |> List.filterMap
            (\( _, measurements ) ->
                measurementFunc measurements
                    |> Maybe.map (Tuple.second >> .value >> valueFunc)
            )
        |> List.reverse
        |> List.head


withDefaultValue : a -> Maybe a -> EverySet a
withDefaultValue default maybe =
    Maybe.map List.singleton maybe
        |> fromListWithDefaultValue default


fromListWithDefaultValue : a -> Maybe (List a) -> EverySet a
fromListWithDefaultValue default maybeList =
    case maybeList of
        Just list ->
            EverySet.fromList list |> ifEverySetEmpty default

        Nothing ->
            EverySet.singleton default
