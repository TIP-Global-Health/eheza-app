module Pages.AcuteIllnessActivity.Utils exposing (allSymptomsGISigns, allSymptomsGeneralSigns, allSymptomsRespiratorySigns, exposureFormWithDefault, exposureTasksCompletedFromTotal, fromExposureValue, fromHCContactValue, fromIsolationValue, fromListWithDefaultValue, fromMalariaTestingValue, fromTravelHistoryValue, fromTreatmentReviewValue, fromVitalsValue, hcContactFormWithDefault, hcContactValuePostProcess, isolationFormWithDefault, isolationValuePostProcess, laboratoryTasksCompletedFromTotal, malariaTestingFormWithDefault, naListTaskCompleted, naTaskCompleted, physicalExamTasksCompletedFromTotal, symptomsGIFormWithDefault, symptomsGeneralFormWithDefault, symptomsRespiratoryFormWithDefault, symptomsTasksCompletedFromTotal, taskNotCompleted, toExposureValue, toExposureValueWithDefault, toHCContactValue, toHCContactValueWithDefault, toIsolationValue, toIsolationValueWithDefault, toMalariaTestingValue, toMalariaTestingValueWithDefault, toSymptomsGIValueWithDefault, toSymptomsGeneralValueWithDefault, toSymptomsRespiratoryValueWithDefault, toTravelHistoryValue, toTravelHistoryValueWithDefault, toTreatmentReviewValue, toTreatmentReviewValueWithDefault, toVitalsValue, toVitalsValueWithDefault, toggleSymptomsSign, travelHistoryFormWithDefault, treatmentHistoryFormWithDefault, treatmentTasksCompletedFromTotal, vitalsFormWithDefault, withDefaultValue)

import AssocList as Dict exposing (Dict)
import Backend.Measurement.Model
    exposing
        ( AcuteIllnessMeasurements
        , AcuteIllnessVitalsValue
        , ExposureSign(..)
        , HCContactSign(..)
        , HCContactValue
        , HCRecomendation(..)
        , IsolationSign(..)
        , IsolationValue
        , MalariaTestingSign(..)
        , ReasonForNotIsolating(..)
        , ResponsePeriod(..)
        , SymptomsGIDerivedSign(..)
        , SymptomsGISign(..)
        , SymptomsGIValue
        , SymptomsGeneralSign(..)
        , SymptomsRespiratorySign(..)
        , TravelHistorySign(..)
        , TreatmentReviewSign(..)
        )
import EverySet exposing (EverySet)
import Maybe.Extra exposing (andMap, isJust, isNothing, or, unwrap)
import Pages.AcuteIllnessActivity.Model exposing (..)
import Pages.PrenatalActivity.Utils exposing (ifEmpty, ifNullableTrue, ifTrue)
import Pages.Utils exposing (taskCompleted)


allSymptomsGeneralSigns : ( List SymptomsGeneralSign, SymptomsGeneralSign )
allSymptomsGeneralSigns =
    ( [ SymptomGeneralFever
      , Chills
      , NightSweats
      , BodyAches
      , Headache
      , Lethargy
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
    , NoSymptomsGeneral
    )


allSymptomsRespiratorySigns : ( List SymptomsRespiratorySign, SymptomsRespiratorySign )
allSymptomsRespiratorySigns =
    ( [ Cough
      , ShortnessOfBreath
      , NasalCongestion
      , BloodInSputum
      , SoreThroat
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
            ( taskCompleted form.rapidTestPositive
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
            ( taskCompleted form.covid19Symptoms + taskCompleted form.similarSymptoms
            , 2
            )

        ExposureIsolation ->
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

        ExposureContactHC ->
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
                                recomendationsCompleted =
                                    naTaskCompleted HCRecomendationNotApplicable form.recomendations

                                ( ambulanceActive, ambulanceCompleted ) =
                                    form.recomendations
                                        |> Maybe.map
                                            (\recomendations ->
                                                if recomendations == SendAmbulance then
                                                    ( naTaskCompleted ResponsePeriodNotApplicable form.ambulanceArrivalPeriod
                                                    , naTaskCompleted ResponsePeriodNotApplicable form.ambulanceArrivalPeriod
                                                    )

                                                else
                                                    ( 0, 0 )
                                            )
                                        |> Maybe.withDefault ( 0, 0 )
                            in
                            ( 1 + recomendationsCompleted + naTaskCompleted ResponsePeriodNotApplicable form.responsePeriod + ambulanceCompleted
                            , 2 + naTaskCompleted ResponsePeriodNotApplicable form.responsePeriod + ambulanceActive
                            )

                        else
                            ( 1, 1 )
                    )
                |> Maybe.withDefault ( 0, 1 )


treatmentTasksCompletedFromTotal : AcuteIllnessMeasurements -> PriorTreatmentData -> TreatmentTask -> ( Int, Int )
treatmentTasksCompletedFromTotal measurements data task =
    case task of
        TreatmentReview ->
            let
                form =
                    measurements.treatmentHistory
                        |> Maybe.map (Tuple.second >> .value)
                        |> treatmentHistoryFormWithDefault data.treatmentHistoryForm

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
    saved
        |> unwrap
            form
            (\value ->
                if Dict.isEmpty form.signs then
                    SymptomsGeneralForm value

                else
                    form
            )


toSymptomsGeneralValueWithDefault : Maybe (Dict SymptomsGeneralSign Int) -> SymptomsGeneralForm -> Dict SymptomsGeneralSign Int
toSymptomsGeneralValueWithDefault saved form =
    symptomsGeneralFormWithDefault form saved
        |> .signs



--
--
-- toSymptomsGeneralValue : SymptomsGeneralForm -> Maybe (Dict SymptomsGeneralSign Int)
-- toSymptomsGeneralValue form =
--     form.signs
--
--
-- fromSymptomsRespiratoryValue : Maybe (Dict SymptomsRespiratorySign Int) -> SymptomsRespiratoryForm
-- fromSymptomsRespiratoryValue saved =
--     { signs = saved }
--
--


symptomsRespiratoryFormWithDefault : SymptomsRespiratoryForm -> Maybe (Dict SymptomsRespiratorySign Int) -> SymptomsRespiratoryForm
symptomsRespiratoryFormWithDefault form saved =
    saved
        |> unwrap
            form
            (\value ->
                if Dict.isEmpty form.signs then
                    SymptomsRespiratoryForm value

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
                        if Dict.isEmpty form.signs then
                            value.signs

                        else
                            form.signs

                    intractableVomiting =
                        if isJust form.intractableVomiting then
                            form.intractableVomiting

                        else if EverySet.member IntractableVomiting value.derivedSigns then
                            Just True

                        else
                            Just False
                in
                { signs = signs
                , intractableVomiting = intractableVomiting
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
    , bodyTemperature = Maybe.map .bodyTemperature saved
    }


vitalsFormWithDefault : VitalsForm -> Maybe AcuteIllnessVitalsValue -> VitalsForm
vitalsFormWithDefault form saved =
    saved
        |> unwrap
            form
            (\value ->
                { respiratoryRate = or form.respiratoryRate (Just value.respiratoryRate)
                , bodyTemperature = or form.bodyTemperature (Just value.bodyTemperature)
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


fromMalariaTestingValue : Maybe (EverySet MalariaTestingSign) -> MalariaTestingForm
fromMalariaTestingValue saved =
    { rapidTestPositive = Maybe.map (EverySet.member RapidTestPositive) saved
    }


malariaTestingFormWithDefault : MalariaTestingForm -> Maybe (EverySet MalariaTestingSign) -> MalariaTestingForm
malariaTestingFormWithDefault form saved =
    saved
        |> unwrap
            form
            (\value ->
                { rapidTestPositive = or form.rapidTestPositive (EverySet.member RapidTestPositive value |> Just)
                }
            )


toMalariaTestingValueWithDefault : Maybe (EverySet MalariaTestingSign) -> MalariaTestingForm -> Maybe (EverySet MalariaTestingSign)
toMalariaTestingValueWithDefault saved form =
    malariaTestingFormWithDefault form saved
        |> toMalariaTestingValue


toMalariaTestingValue : MalariaTestingForm -> Maybe (EverySet MalariaTestingSign)
toMalariaTestingValue form =
    [ Maybe.map (ifTrue RapidTestPositive) form.rapidTestPositive
    ]
        |> Maybe.Extra.combine
        |> Maybe.map (List.foldl EverySet.union EverySet.empty >> ifEmpty NoMalariaTestingSigns)


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
    [ Maybe.map (ifTrue COVID19Country) form.covid19Country
    ]
        |> Maybe.Extra.combine
        |> Maybe.map (List.foldl EverySet.union EverySet.empty >> ifEmpty NoTravelHistorySigns)


fromExposureValue : Maybe (EverySet ExposureSign) -> ExposureForm
fromExposureValue saved =
    { covid19Symptoms = Maybe.map (EverySet.member COVID19Symptoms) saved
    , similarSymptoms = Maybe.map (EverySet.member SimilarSymptoms) saved
    }


exposureFormWithDefault : ExposureForm -> Maybe (EverySet ExposureSign) -> ExposureForm
exposureFormWithDefault form saved =
    saved
        |> unwrap
            form
            (\value ->
                { covid19Symptoms = or form.covid19Symptoms (EverySet.member COVID19Symptoms value |> Just)
                , similarSymptoms = or form.similarSymptoms (EverySet.member SimilarSymptoms value |> Just)
                }
            )


toExposureValueWithDefault : Maybe (EverySet ExposureSign) -> ExposureForm -> Maybe (EverySet ExposureSign)
toExposureValueWithDefault saved form =
    exposureFormWithDefault form saved
        |> toExposureValue


toExposureValue : ExposureForm -> Maybe (EverySet ExposureSign)
toExposureValue form =
    [ Maybe.map (ifTrue COVID19Symptoms) form.covid19Symptoms
    , Maybe.map (ifTrue SimilarSymptoms) form.similarSymptoms
    ]
        |> Maybe.Extra.combine
        |> Maybe.map (List.foldl EverySet.union EverySet.empty >> ifEmpty NoExposureSigns)


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
                |> Maybe.map (List.foldl EverySet.union EverySet.empty >> ifEmpty NoIsolationSigns)

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
    , recomendations = Maybe.andThen (.recomendations >> EverySet.toList >> List.head) saved
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
                , recomendations = or form.recomendations (value.recomendations |> EverySet.toList |> List.head)
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
                |> Maybe.map (List.foldl EverySet.union EverySet.empty >> ifEmpty NoHCContactSigns)
    in
    Maybe.map HCContactValue signs
        |> andMap (form.recomendations |> withDefaultValue HCRecomendationNotApplicable |> Just)
        |> andMap (form.responsePeriod |> withDefaultValue ResponsePeriodNotApplicable |> Just)
        |> andMap (form.ambulanceArrivalPeriod |> withDefaultValue ResponsePeriodNotApplicable |> Just)


hcContactValuePostProcess : Maybe HCContactValue -> Maybe HCContactValue
hcContactValuePostProcess saved =
    saved
        |> Maybe.map
            (\value ->
                if EverySet.member ContactedHealthCenter value.signs then
                    if EverySet.member SendAmbulance value.recomendations then
                        value

                    else
                        { value | ambulanceArrivalPeriod = EverySet.singleton ResponsePeriodNotApplicable }

                else
                    { value
                        | recomendations = EverySet.singleton HCRecomendationNotApplicable
                        , responsePeriod = EverySet.singleton ResponsePeriodNotApplicable
                        , ambulanceArrivalPeriod = EverySet.singleton ResponsePeriodNotApplicable
                    }
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


treatmentHistoryFormWithDefault : TreatmentReviewForm -> Maybe (EverySet TreatmentReviewSign) -> TreatmentReviewForm
treatmentHistoryFormWithDefault form saved =
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
    treatmentHistoryFormWithDefault form saved
        |> toTreatmentReviewValue


toTreatmentReviewValue : TreatmentReviewForm -> Maybe (EverySet TreatmentReviewSign)
toTreatmentReviewValue form =
    [ Maybe.map (ifTrue FeverPast6Hours) form.feverPast6Hours
    , Maybe.map (ifTrue FeverPast6HoursHelped) form.feverPast6HoursHelped
    , Maybe.map (ifTrue MalariaToday) form.malariaToday
    , Maybe.map (ifTrue MalariaTodayHelped) form.malariaTodayHelped
    , Maybe.map (ifTrue MalariaWithinPastMonth) form.malariaWithinPastMonth
    , Maybe.map (ifTrue MalariaWithinPastMonthHelped) form.malariaWithinPastMonthHelped
    ]
        |> Maybe.Extra.combine
        |> Maybe.map (List.foldl EverySet.union EverySet.empty >> ifEmpty NoTreatmentReviewSigns)



-- HELPER FUNCTIONS


withDefaultValue : a -> Maybe a -> EverySet a
withDefaultValue default maybe =
    Maybe.map List.singleton maybe
        |> fromListWithDefaultValue default


fromListWithDefaultValue : a -> Maybe (List a) -> EverySet a
fromListWithDefaultValue default maybeList =
    case maybeList of
        Just list ->
            EverySet.fromList list |> ifEmpty default

        Nothing ->
            EverySet.singleton default
