module Pages.AcuteIllnessActivity.Utils exposing (allSymptomsGISigns, allSymptomsGeneralSigns, allSymptomsRespiratorySigns, exposureFormWithDefault, exposureTasksCompletedFromTotal, fromExposureValue, fromHCContactValue, fromIsolationValue, fromMalariaTestingValue, fromTravelHistoryValue, fromVitalsValue, hcContactFormWithDefault, isolationFormWithDefault, laboratoryTasksCompletedFromTotal, malariaTestingFormWithDefault, physicalExamTasksCompletedFromTotal, symptomsGIFormWithDefault, symptomsGeneralFormWithDefault, symptomsRespiratoryFormWithDefault, symptomsTasksCompletedFromTotal, taskNotCompleted, toExposureValue, toExposureValueWithDefault, toHCContactValue, toHCContactValueWithDefault, toIsolationValue, toIsolationValueWithDefault, toMalariaTestingValue, toMalariaTestingValueWithDefault, toSymptomsGIValueWithDefault, toSymptomsGeneralValueWithDefault, toSymptomsRespiratoryValueWithDefault, toTravelHistoryValue, toTravelHistoryValueWithDefault, toVitalsValue, toVitalsValueWithDefault, toggleSymptomsSign, travelHistoryFormWithDefault, vitalsFormWithDefault)

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
        , SymptomsGISign(..)
        , SymptomsGeneralSign(..)
        , SymptomsRespiratorySign(..)
        , TravelHistorySign(..)
        )
import EverySet exposing (EverySet)
import Maybe.Extra exposing (andMap, isJust, or, unwrap)
import Pages.AcuteIllnessActivity.Model exposing (..)
import Pages.PrenatalActivity.Utils exposing (ifEmpty, ifTrue)
import Pages.Utils exposing (taskCompleted)


taskNotCompleted : Bool -> Int
taskNotCompleted notCompleted =
    if notCompleted then
        0

    else
        1


allSymptomsGeneralSigns : ( List SymptomsGeneralSign, SymptomsGeneralSign )
allSymptomsGeneralSigns =
    ( [ SymptomGeneralFever
      , Chills
      , NightSweats
      , BodyAches
      , Headache
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


toggleSymptomsSign : SymptomsTask -> a -> a -> { signs : Dict a Int } -> { signs : Dict a Int }
toggleSymptomsSign task sign noneSign form =
    let
        signs =
            form.signs

        updatedSigns =
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
    in
    { form | signs = updatedSigns }


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
            in
            ( taskNotCompleted (Dict.isEmpty form.signs)
            , 1
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

                ( derrivedActive, derrivedCompleted ) =
                    case form.patientIsolated of
                        Just True ->
                            ( 1, taskCompleted form.signOnDoor )

                        Just False ->
                            ( 1, taskCompleted form.reasonsForNotIsolating )

                        Nothing ->
                            ( 0, 0 )
            in
            ( taskCompleted form.patientIsolated + taskCompleted form.healthEducation + derrivedCompleted
            , 2 + derrivedActive
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
                                ( ambulanceActive, ambulanceCompleted ) =
                                    form.recomendations
                                        |> Maybe.map
                                            (\recomendations ->
                                                if List.member SendAmbulance recomendations then
                                                    ( taskCompleted form.ambulanceArrivalPeriod, taskCompleted form.ambulanceArrivalPeriod )

                                                else
                                                    ( 0, 0 )
                                            )
                                        |> Maybe.withDefault ( 0, 0 )
                            in
                            ( 1 + taskCompleted form.recomendations + taskCompleted form.responsePeriod + ambulanceCompleted
                            , 2 + taskCompleted form.responsePeriod + ambulanceActive
                            )

                        else
                            ( 1, 1 )
                    )
                |> Maybe.withDefault ( 0, 1 )


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



--
--
-- toSymptomsRespiratoryValue : SymptomsRespiratoryForm -> Maybe (Dict SymptomsRespiratorySign Int)
-- toSymptomsRespiratoryValue form =
--     form.signs
--
--
-- fromSymptomsGIValue : Maybe (Dict SymptomsGISign Int) -> SymptomsGIForm
-- fromSymptomsGIValue saved =
--     { signs = saved }
--
--


symptomsGIFormWithDefault : SymptomsGIForm -> Maybe (Dict SymptomsGISign Int) -> SymptomsGIForm
symptomsGIFormWithDefault form saved =
    saved
        |> unwrap
            form
            (\value ->
                if Dict.isEmpty form.signs then
                    SymptomsGIForm value

                else
                    form
            )


toSymptomsGIValueWithDefault : Maybe (Dict SymptomsGISign Int) -> SymptomsGIForm -> Dict SymptomsGISign Int
toSymptomsGIValueWithDefault saved form =
    symptomsGIFormWithDefault form saved
        |> .signs


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


toIsolationValue : IsolationForm -> Maybe IsolationValue
toIsolationValue form =
    let
        signs =
            [ Maybe.map (ifTrue PatientIsolated) form.patientIsolated
            , Maybe.map (ifTrue SignOnDoor) form.signOnDoor
            , Maybe.map (ifTrue HealthEducation) form.healthEducation
            ]
                |> Maybe.Extra.combine
                |> Maybe.map (List.foldl EverySet.union EverySet.empty >> ifEmpty NoIsolationSigns)

        reasonsForNotIsolating =
            form.reasonsForNotIsolating
                |> Maybe.map (EverySet.fromList >> ifEmpty IsolationReasonNotApplicable)
    in
    Maybe.map IsolationValue signs
        |> andMap reasonsForNotIsolating


fromHCContactValue : Maybe HCContactValue -> HCContactForm
fromHCContactValue saved =
    { contactedHC = Maybe.map (.signs >> EverySet.member ContactedHealthCenter) saved
    , recomendations = Maybe.map (.recomendations >> EverySet.toList) saved
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
                , recomendations = or form.recomendations (value.recomendations |> EverySet.toList |> Just)
                , responsePeriod = or form.responsePeriod (value.responsePeriod |> EverySet.toList |> List.head)
                , ambulanceArrivalPeriod = or form.ambulanceArrivalPeriod (value.ambulanceArrivalPeriod |> EverySet.toList |> List.head)
                }
            )


toHCContactValueWithDefault : Maybe HCContactValue -> HCContactForm -> Maybe HCContactValue
toHCContactValueWithDefault saved form =
    hcContactFormWithDefault form saved
        |> toHCContactValue


toHCContactValue : HCContactForm -> Maybe HCContactValue
toHCContactValue form =
    let
        signs =
            [ Maybe.map (ifTrue ContactedHealthCenter) form.contactedHC ]
                |> Maybe.Extra.combine
                |> Maybe.map (List.foldl EverySet.union EverySet.empty >> ifEmpty NoHCContactSigns)

        recomendations =
            form.recomendations
                |> Maybe.map (EverySet.fromList >> ifEmpty HCRecomendationNotApplicable)
    in
    Maybe.map HCContactValue signs
        |> andMap recomendations
        |> andMap (Maybe.map EverySet.singleton form.responsePeriod)
        |> andMap (Maybe.map EverySet.singleton form.ambulanceArrivalPeriod)
