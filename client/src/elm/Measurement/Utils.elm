module Measurement.Utils exposing (..)

import Activity.Utils exposing (expectCounselingActivity, expectParticipantConsent)
import AssocList as Dict exposing (Dict)
import Backend.Entities exposing (..)
import Backend.Measurement.Model exposing (..)
import Backend.Measurement.Utils exposing (..)
import Backend.NutritionEncounter.Utils
    exposing
        ( calculateZScoreWeightForAge
        , muacModerate
        , muacSevere
        , resolveIndividualNutritionValues
        , resolveIndividualWellChildValues
        , zScoreWeightForAgeModerate
        , zScoreWeightForAgeSevere
        )
import Backend.Session.Model exposing (EditableSession)
import Backend.Session.Utils exposing (getChildMeasurementData, getMotherMeasurementData)
import Date exposing (Unit(..))
import DateSelector.Model exposing (DateSelectorConfig)
import DateSelector.SelectorPopup exposing (viewCalendarPopup)
import EverySet exposing (EverySet)
import Gizra.Html exposing (emptyNode, showIf)
import Gizra.NominalDate exposing (NominalDate, formatDDMMYYYY)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import List.Extra
import LocalData
import Maybe.Extra exposing (andMap, isJust, isNothing, or, unwrap)
import Measurement.Model exposing (..)
import Pages.Session.Model
import Pages.Utils
    exposing
        ( emptySelectOption
        , ifEverySetEmpty
        , ifNullableTrue
        , ifTrue
        , maybeToBoolTask
        , maybeValueConsideringIsDirtyField
        , taskCompleted
        , taskCompletedWithException
        , valueConsideringIsDirtyField
        , viewBoolInput
        , viewCheckBoxMultipleSelectCustomInput
        , viewCheckBoxMultipleSelectInput
        , viewCheckBoxSelectInput
        , viewCustomLabel
        , viewLabel
        , viewQuestionLabel
        )
import Translate exposing (Language, translate)
import Translate.Model exposing (Language(..))
import Utils.Html exposing (viewModal)


{-| This is a convenience for cases where the form values ought to be redefined
to allow multiple values. So, it should go away eventually.
-}
toEverySet : a -> a -> Bool -> EverySet a
toEverySet presentValue absentValue present =
    if present then
        EverySet.singleton presentValue

    else
        EverySet.singleton absentValue


getInputConstraintsHeight : FloatInputConstraints
getInputConstraintsHeight =
    { minVal = 25
    , maxVal = 250
    }


getInputConstraintsMuac : FloatInputConstraints
getInputConstraintsMuac =
    { minVal = 5
    , maxVal = 99
    }


getInputConstraintsWeight : FloatInputConstraints
getInputConstraintsWeight =
    { minVal = 0.5
    , maxVal = 200
    }


{-| Initialize (or reset) a form with the given data.
-}
fromChildMeasurementData : MeasurementData ChildMeasurements -> ModelChild
fromChildMeasurementData data =
    let
        fromData measuremntFunc mappingFunc =
            mapMeasurementData measuremntFunc data
                |> currentValue
                |> Maybe.map mappingFunc
    in
    { height =
        fromData .height (.value >> getHeightValue >> String.fromFloat)
            |> Maybe.withDefault ""
    , muac =
        fromData .muac (.value >> muacValueFunc >> String.fromFloat)
            |> Maybe.withDefault ""
    , nutrition =
        fromData .nutrition .value
            |> Maybe.withDefault emptyNutritionValue
    , counseling =
        fromData .counselingSession .value
    , photo =
        fromData .photo .value
    , weight =
        fromData .weight (.value >> weightValueFunc >> String.fromFloat)
            |> Maybe.withDefault ""
    , fbfForm =
        fromData .fbf (.value >> fbfValueToForm)
            |> Maybe.withDefault emptyFbfForm
    , contributingFactorsForm =
        fromData .contributingFactors (.value >> Just >> fromContributingFactorsValue)
            |> Maybe.withDefault emptyContributingFactorsForm
    , followUpForm =
        fromData .followUp (.value >> Just >> fromFollowUpValue)
            |> Maybe.withDefault emptyFollowUpForm
    , healthEducationForm =
        fromData .healthEducation (.value >> Just >> fromHealthEducationValue)
            |> Maybe.withDefault emptyHealthEducationForm
    , sendToHCForm =
        fromData .sendToHC (.value >> Just >> fromSendToHCValue)
            |> Maybe.withDefault emptySendToHCForm
    }


{-| Initialize (or reset) a form with the given data.
-}
fromMotherMeasurementData : MeasurementData MotherMeasurements -> ModelMother
fromMotherMeasurementData data =
    let
        -- We show the UI as completed for all current consents
        progress =
            data
                |> mapMeasurementData .consent
                |> currentValues
                |> List.map (Tuple.second >> .value >> .formId)
                |> List.map (\formId -> ( formId, completedParticipantFormProgress ))
                |> Dict.fromList
    in
    { familyPlanningSigns =
        data
            |> mapMeasurementData .familyPlanning
            |> currentValue
            |> Maybe.map .value
            |> Maybe.withDefault EverySet.empty
    , participantConsent =
        { expected = Dict.empty
        , view = Nothing
        , progress = progress
        }
    , lactationForm =
        data
            |> mapMeasurementData .lactation
            |> currentValue
            |> Maybe.map (.value >> lactationSignsToForm)
            |> Maybe.withDefault (LactationForm Nothing)
    , fbfForm =
        data
            |> mapMeasurementData .fbf
            |> currentValue
            |> Maybe.map (.value >> fbfValueToForm)
            |> Maybe.withDefault (FbfForm Nothing Nothing)
    }


getMotherForm : PersonId -> Pages.Session.Model.Model -> EditableSession -> ModelMother
getMotherForm motherId pages session =
    -- Could use `Maybe.withDefault` here instead, but then
    -- `fromMotherMeasurementData` would get calculated every time
    case Dict.get motherId pages.motherForms of
        Just motherForm ->
            motherForm

        Nothing ->
            getMotherMeasurementData motherId session
                |> LocalData.unwrap
                    emptyModelMother
                    (fromMotherMeasurementData
                        >> (\form ->
                                { form
                                    | participantConsent =
                                        { expected = expectParticipantConsent session.offlineSession motherId
                                        , view = Nothing
                                        , progress = form.participantConsent.progress
                                        }
                                }
                           )
                    )


getChildForm : PersonId -> Pages.Session.Model.Model -> EditableSession -> ModelChild
getChildForm childId pages session =
    -- Could use `Maybe.withDefault` here instead, but then
    -- `fromChildMeasurementData` would get calculated every time
    case Dict.get childId pages.childForms of
        Just childForm ->
            childForm

        Nothing ->
            getChildMeasurementData childId session
                |> LocalData.unwrap
                    emptyModelChild
                    (fromChildMeasurementData
                        >> (\form ->
                                -- We need some special logic for the counseling
                                -- session, to fill in the correct kind of session.
                                -- This seems to be the best place to do that, though
                                -- that may need some more thinking at some point.
                                case form.counseling of
                                    Just _ ->
                                        form

                                    Nothing ->
                                        { form
                                            | counseling =
                                                expectCounselingActivity session childId
                                                    |> Maybe.map
                                                        (\timing ->
                                                            ( timing, EverySet.empty )
                                                        )
                                        }
                           )
                    )


withinConstraints : FloatInputConstraints -> Float -> Bool
withinConstraints constraints value =
    value >= constraints.minVal && value <= constraints.maxVal


lactationSignsToForm : EverySet LactationSign -> LactationForm
lactationSignsToForm signs =
    EverySet.member Breastfeeding signs
        |> Just
        |> LactationForm


lactationFormToSigns : LactationForm -> EverySet LactationSign
lactationFormToSigns form =
    form.breastfeeding
        |> Maybe.map
            (\breastfeeding ->
                if breastfeeding then
                    EverySet.singleton Breastfeeding

                else
                    EverySet.singleton NoLactationSigns
            )
        |> Maybe.withDefault (EverySet.singleton NoLactationSigns)


fbfValueToForm : FbfValue -> FbfForm
fbfValueToForm value =
    FbfForm (Just value.distributedAmount) (Just value.distributionNotice)


fbfFormToValue : FbfForm -> FbfValue
fbfFormToValue form =
    Maybe.map2
        (\distributedAmount distributionNotice ->
            FbfValue distributedAmount distributionNotice
        )
        form.distributedAmount
        form.distributionNotice
        -- We should never get here, as we always expect to have
        -- these fields filled.
        |> Maybe.withDefault (FbfValue 0 DistributedFully)


resolveIndividualNutritionValue :
    List ( NominalDate, ( NutritionEncounterId, NutritionMeasurements ) )
    -> (NutritionMeasurements -> Maybe ( id, NutritionMeasurement a ))
    -> (a -> b)
    -> Maybe ( NominalDate, b )
resolveIndividualNutritionValue measurementsWithDates measurementFunc valueFunc =
    resolveIndividualNutritionValues measurementsWithDates measurementFunc valueFunc
        |> List.head


resolveIndividualWellChildValue :
    List ( NominalDate, ( WellChildEncounterId, WellChildMeasurements ) )
    -> (WellChildMeasurements -> Maybe ( id, WellChildMeasurement a ))
    -> (a -> b)
    -> Maybe ( NominalDate, b )
resolveIndividualWellChildValue measurementsWithDates measurementFunc valueFunc =
    resolveIndividualWellChildValues measurementsWithDates measurementFunc valueFunc
        |> List.head


fromHeightValue : Maybe HeightInCm -> HeightForm
fromHeightValue saved =
    { height = Maybe.map getHeightValue saved
    , heightDirty = False
    }


heightFormWithDefault : HeightForm -> Maybe HeightInCm -> HeightForm
heightFormWithDefault form saved =
    saved
        |> unwrap
            form
            (\value ->
                { height = valueConsideringIsDirtyField form.heightDirty form.height (value |> getHeightValue)
                , heightDirty = form.heightDirty
                }
            )


toHeightValueWithDefault : Maybe HeightInCm -> HeightForm -> Maybe HeightInCm
toHeightValueWithDefault saved form =
    heightFormWithDefault form saved
        |> toHeightValue


toHeightValue : HeightForm -> Maybe HeightInCm
toHeightValue form =
    Maybe.map HeightInCm form.height


fromMuacValue : Maybe MuacInCm -> MuacForm
fromMuacValue saved =
    { muac = Maybe.map muacValueFunc saved
    , muacDirty = False
    }


muacFormWithDefault : MuacForm -> Maybe MuacInCm -> MuacForm
muacFormWithDefault form saved =
    saved
        |> unwrap
            form
            (\value ->
                { muac = valueConsideringIsDirtyField form.muacDirty form.muac (value |> muacValueFunc)
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


fromNutritionValue : Maybe NutritionValue -> NutritionForm
fromNutritionValue saved =
    { signs = Maybe.map (.signs >> EverySet.toList) saved
    , assesment = Maybe.map .assesment saved
    }


nutritionFormWithDefault : NutritionForm -> Maybe NutritionValue -> NutritionForm
nutritionFormWithDefault form saved =
    saved
        |> unwrap
            form
            (\value ->
                { signs = or form.signs (EverySet.toList value.signs |> Just)
                , assesment = or form.assesment (Just value.assesment)
                }
            )


toNutritionValueWithDefault : Maybe NutritionValue -> NutritionForm -> Maybe NutritionValue
toNutritionValueWithDefault saved form =
    nutritionFormWithDefault form saved
        |> toNutritionValue


toNutritionValue : NutritionForm -> Maybe NutritionValue
toNutritionValue form =
    let
        signs =
            Maybe.map (EverySet.fromList >> ifEverySetEmpty NormalChildNutrition) form.signs
    in
    Maybe.map NutritionValue signs
        |> andMap form.assesment


fromWeightValue : Maybe WeightInKg -> WeightForm
fromWeightValue saved =
    { weight = Maybe.map weightValueFunc saved
    , weightDirty = False
    }


weightFormWithDefault : WeightForm -> Maybe WeightInKg -> WeightForm
weightFormWithDefault form saved =
    saved
        |> unwrap
            form
            (\value ->
                { weight = valueConsideringIsDirtyField form.weightDirty form.weight (weightValueFunc value)
                , weightDirty = form.weightDirty
                }
            )


toWeightValueWithDefault : Maybe WeightInKg -> WeightForm -> Maybe WeightInKg
toWeightValueWithDefault saved form =
    weightFormWithDefault form saved
        |> toWeightValue


toWeightValue : WeightForm -> Maybe WeightInKg
toWeightValue form =
    Maybe.map WeightInKg form.weight


fromContributingFactorsValue : Maybe (EverySet ContributingFactorsSign) -> ContributingFactorsForm
fromContributingFactorsValue saved =
    { signs = Maybe.map EverySet.toList saved }


contributingFactorsFormWithDefault : ContributingFactorsForm -> Maybe (EverySet ContributingFactorsSign) -> ContributingFactorsForm
contributingFactorsFormWithDefault form saved =
    saved
        |> unwrap
            form
            (\value ->
                { signs = or form.signs (EverySet.toList value |> Just) }
            )


toContributingFactorsValueWithDefault : Maybe (EverySet ContributingFactorsSign) -> ContributingFactorsForm -> Maybe (EverySet ContributingFactorsSign)
toContributingFactorsValueWithDefault saved form =
    contributingFactorsFormWithDefault form saved
        |> toContributingFactorsValue


toContributingFactorsValue : ContributingFactorsForm -> Maybe (EverySet ContributingFactorsSign)
toContributingFactorsValue form =
    Maybe.map (EverySet.fromList >> ifEverySetEmpty NoContributingFactorsSign) form.signs


fromFollowUpValue : Maybe FollowUpValue -> FollowUpForm
fromFollowUpValue saved =
    { option = Maybe.andThen (.options >> EverySet.toList >> List.head) saved
    , assesment = Maybe.map .assesment saved
    , resolutionDate = Maybe.andThen .resolutionDate saved
    }


followUpFormWithDefault : FollowUpForm -> Maybe FollowUpValue -> FollowUpForm
followUpFormWithDefault form saved =
    saved
        |> unwrap
            form
            (\value ->
                { option = or form.option (EverySet.toList value.options |> List.head)
                , assesment = or form.assesment (Just value.assesment)
                , resolutionDate = or form.resolutionDate value.resolutionDate
                }
            )


toFollowUpValueWithDefault : Maybe FollowUpValue -> FollowUpForm -> Maybe FollowUpValue
toFollowUpValueWithDefault saved form =
    followUpFormWithDefault form saved
        |> toFollowUpValue


toFollowUpValue : FollowUpForm -> Maybe FollowUpValue
toFollowUpValue form =
    Maybe.map2
        (\options assesment ->
            FollowUpValue options assesment form.resolutionDate
        )
        (Maybe.map (List.singleton >> EverySet.fromList) form.option)
        form.assesment


fromHealthEducationValue : Maybe HealthEducationValue -> HealthEducationForm
fromHealthEducationValue saved =
    { educationForDiagnosis = Maybe.map (.signs >> EverySet.member MalariaPrevention) saved
    , reasonForNotProvidingHealthEducation = Maybe.map .reasonForNotProvidingHealthEducation saved
    }


healthEducationFormWithDefault : HealthEducationForm -> Maybe HealthEducationValue -> HealthEducationForm
healthEducationFormWithDefault form saved =
    saved
        |> unwrap
            form
            (\value ->
                { educationForDiagnosis = or form.educationForDiagnosis (EverySet.member MalariaPrevention value.signs |> Just)
                , reasonForNotProvidingHealthEducation = or form.reasonForNotProvidingHealthEducation (value.reasonForNotProvidingHealthEducation |> Just)
                }
            )


toHealthEducationValueWithDefault : Maybe HealthEducationValue -> HealthEducationForm -> Maybe HealthEducationValue
toHealthEducationValueWithDefault saved form =
    healthEducationFormWithDefault form saved
        |> toHealthEducationValue


toHealthEducationValue : HealthEducationForm -> Maybe HealthEducationValue
toHealthEducationValue form =
    let
        signs =
            [ Maybe.map (ifTrue MalariaPrevention) form.educationForDiagnosis ]
                |> Maybe.Extra.combine
                |> Maybe.map (List.foldl EverySet.union EverySet.empty >> ifEverySetEmpty NoHealthEducationSigns)

        reasonForNotProvidingHealthEducation =
            form.reasonForNotProvidingHealthEducation
                |> Maybe.withDefault NoReasonForNotProvidingHealthEducation
                |> Just
    in
    Maybe.map HealthEducationValue signs
        |> andMap reasonForNotProvidingHealthEducation


fromSendToHCValue : Maybe SendToHCValue -> SendToHCForm
fromSendToHCValue saved =
    { handReferralForm = Maybe.map (.signs >> EverySet.member HandReferrerForm) saved
    , referToHealthCenter = Maybe.map (.signs >> EverySet.member ReferToHealthCenter) saved
    , accompanyToHealthCenter = Maybe.map (.signs >> EverySet.member PrenatalAccompanyToHC) saved
    , enrollToNutritionProgram = Maybe.map (.signs >> EverySet.member EnrollToNutritionProgram) saved
    , referToNutritionProgram = Maybe.map (.signs >> EverySet.member ReferToNutritionProgram) saved
    , reasonForNotSendingToHC = Maybe.map .reasonForNotSendingToHC saved
    }


sendToHCFormWithDefault : SendToHCForm -> Maybe SendToHCValue -> SendToHCForm
sendToHCFormWithDefault form saved =
    saved
        |> unwrap
            form
            (\value ->
                { handReferralForm = or form.handReferralForm (EverySet.member HandReferrerForm value.signs |> Just)
                , referToHealthCenter = or form.referToHealthCenter (EverySet.member ReferToHealthCenter value.signs |> Just)
                , accompanyToHealthCenter = or form.accompanyToHealthCenter (EverySet.member PrenatalAccompanyToHC value.signs |> Just)
                , enrollToNutritionProgram = or form.enrollToNutritionProgram (EverySet.member EnrollToNutritionProgram value.signs |> Just)
                , referToNutritionProgram = or form.referToNutritionProgram (EverySet.member ReferToNutritionProgram value.signs |> Just)
                , reasonForNotSendingToHC = or form.reasonForNotSendingToHC (value.reasonForNotSendingToHC |> Just)
                }
            )


toSendToHCValueWithDefault : Maybe SendToHCValue -> SendToHCForm -> Maybe SendToHCValue
toSendToHCValueWithDefault saved form =
    sendToHCFormWithDefault form saved
        |> toSendToHCValue


toSendToHCValue : SendToHCForm -> Maybe SendToHCValue
toSendToHCValue form =
    let
        signs =
            [ ifNullableTrue HandReferrerForm form.handReferralForm
            , ifNullableTrue ReferToHealthCenter form.referToHealthCenter
            , ifNullableTrue PrenatalAccompanyToHC form.accompanyToHealthCenter
            , ifNullableTrue EnrollToNutritionProgram form.enrollToNutritionProgram
            , ifNullableTrue ReferToNutritionProgram form.referToNutritionProgram
            ]
                |> Maybe.Extra.combine
                |> Maybe.map (List.foldl EverySet.union EverySet.empty >> ifEverySetEmpty NoSendToHCSigns)

        reasonForNotSendingToHC =
            form.reasonForNotSendingToHC
                |> Maybe.withDefault NoReasonForNonReferral
                |> Just
    in
    Maybe.map SendToHCValue signs
        |> andMap reasonForNotSendingToHC


allNextStepsTasks : List NextStepsTask
allNextStepsTasks =
    [ NextStepContributingFactors, NextStepsHealthEducation, NextStepsSendToHC, NextStepFollowUp ]


vitalsFormWithDefault : VitalsForm -> Maybe VitalsValue -> VitalsForm
vitalsFormWithDefault form saved =
    saved
        |> unwrap
            form
            (\value ->
                { sysBloodPressure = maybeValueConsideringIsDirtyField form.sysBloodPressureDirty form.sysBloodPressure value.sys
                , sysBloodPressureDirty = form.sysBloodPressureDirty
                , diaBloodPressure = maybeValueConsideringIsDirtyField form.diaBloodPressureDirty form.diaBloodPressure value.dia
                , diaBloodPressureDirty = form.diaBloodPressureDirty
                , heartRate = maybeValueConsideringIsDirtyField form.heartRateDirty form.heartRate value.heartRate
                , heartRateDirty = form.heartRateDirty
                , respiratoryRate = valueConsideringIsDirtyField form.respiratoryRateDirty form.respiratoryRate value.respiratoryRate
                , respiratoryRateDirty = form.respiratoryRateDirty
                , bodyTemperature = valueConsideringIsDirtyField form.bodyTemperatureDirty form.bodyTemperature value.bodyTemperature
                , bodyTemperatureDirty = form.bodyTemperatureDirty
                , sysRepeated = maybeValueConsideringIsDirtyField form.sysRepeatedDirty form.sysRepeated value.sysRepeated
                , sysRepeatedDirty = form.sysRepeatedDirty
                , diaRepeated = maybeValueConsideringIsDirtyField form.diaRepeatedDirty form.diaRepeated value.diaRepeated
                , diaRepeatedDirty = form.diaRepeatedDirty
                }
            )


toVitalsValueWithDefault : Maybe VitalsValue -> VitalsForm -> Maybe VitalsValue
toVitalsValueWithDefault saved form =
    vitalsFormWithDefault form saved
        |> toVitalsValue


toVitalsValue : VitalsForm -> Maybe VitalsValue
toVitalsValue form =
    Maybe.map2
        (\respiratoryRate bodyTemperature ->
            VitalsValue form.sysBloodPressure
                form.diaBloodPressure
                form.heartRate
                respiratoryRate
                bodyTemperature
                form.sysRepeated
                form.diaRepeated
        )
        form.respiratoryRate
        form.bodyTemperature


resolveMedicationsNonAdministrationReasons :
    { v | nonAdministrationSigns : EverySet MedicationNonAdministrationSign }
    -> Dict MedicationDistributionSign AdministrationNote
resolveMedicationsNonAdministrationReasons value =
    EverySet.toList value.nonAdministrationSigns
        |> List.filterMap
            (\sign ->
                case sign of
                    MedicationAmoxicillin reason ->
                        Just ( Amoxicillin, reason )

                    MedicationCoartem reason ->
                        Just ( Coartem, reason )

                    MedicationORS reason ->
                        Just ( ORS, reason )

                    MedicationZinc reason ->
                        Just ( Zinc, reason )

                    MedicationParacetamol reason ->
                        Just ( Paracetamol, reason )

                    MedicationMebendezole reason ->
                        Just ( Mebendezole, reason )

                    MedicationTenofovir reason ->
                        Just ( Tenofovir, reason )

                    MedicationLamivudine reason ->
                        Just ( Lamivudine, reason )

                    MedicationDolutegravir reason ->
                        Just ( Dolutegravir, reason )

                    MedicationTDF3TC reason ->
                        Just ( TDF3TC, reason )

                    MedicationIron reason ->
                        Just ( Iron, reason )

                    MedicationFolicAcid reason ->
                        Just ( FolicAcid, reason )

                    MedicationCeftriaxone reason ->
                        Just ( Ceftriaxone, reason )

                    MedicationAzithromycin reason ->
                        Just ( Azithromycin, reason )

                    MedicationMetronidazole reason ->
                        Just ( Metronidazole, reason )

                    MedicationVitaminA reason ->
                        Just ( VitaminA, reason )

                    NoMedicationNonAdministrationSigns ->
                        Nothing
            )
        |> Dict.fromList


vaccinationFormWithDefault : VaccinationForm msg -> Maybe VaccinationValue -> VaccinationForm msg
vaccinationFormWithDefault form saved =
    unwrap
        form
        (\value ->
            let
                administrationNote =
                    valueConsideringIsDirtyField form.administrationNoteDirty form.administrationNote value.administrationNote
            in
            { administeredDoses = or form.administeredDoses (Just value.administeredDoses)
            , administeredDosesDirty = form.administeredDosesDirty
            , administrationDates = or form.administrationDates (Just value.administrationDates)
            , administrationNote = administrationNote
            , administrationNoteDirty = form.administrationNoteDirty
            , viewMode = form.viewMode
            , updatePreviousVaccines = or form.updatePreviousVaccines (Just False)
            , willReceiveVaccineToday = or form.willReceiveVaccineToday (administrationNote == Just AdministeredToday |> Just)
            , vaccinationUpdateDate = form.vaccinationUpdateDate
            , dateSelectorPopupState = form.dateSelectorPopupState
            }
        )
        saved


toVaccinationValueWithDefault : Maybe VaccinationValue -> VaccinationForm msg -> Maybe VaccinationValue
toVaccinationValueWithDefault saved form =
    vaccinationFormWithDefault form saved
        |> toVaccinationValue


toVaccinationValue : VaccinationForm msg -> Maybe VaccinationValue
toVaccinationValue form =
    let
        administeredDoses =
            Maybe.withDefault EverySet.empty form.administeredDoses

        administrationDates =
            Maybe.withDefault EverySet.empty form.administrationDates

        administrationNote =
            Maybe.withDefault AdministeredPreviously form.administrationNote
    in
    Just <| VaccinationValue administeredDoses administrationDates administrationNote


generateVaccinationProgressForVaccine : List VaccinationValue -> Dict VaccineDose NominalDate
generateVaccinationProgressForVaccine vaccinations =
    List.foldl
        (\vaccination accum ->
            let
                doses =
                    EverySet.toList vaccination.administeredDoses
                        |> List.sortBy vaccineDoseToComparable

                dates =
                    EverySet.toList vaccination.administrationDates
                        |> List.sortWith Date.compare
            in
            accum ++ List.Extra.zip doses dates
        )
        []
        vaccinations
        |> List.sortBy (Tuple.first >> vaccineDoseToComparable)
        |> Dict.fromList


getNextVaccineDose : VaccineDose -> Maybe VaccineDose
getNextVaccineDose dose =
    case dose of
        VaccineDoseFirst ->
            Just VaccineDoseSecond

        VaccineDoseSecond ->
            Just VaccineDoseThird

        VaccineDoseThird ->
            Just VaccineDoseFourth

        VaccineDoseFourth ->
            Just VaccineDoseFifth

        VaccineDoseFifth ->
            Nothing


vaccineDoseToComparable : VaccineDose -> Int
vaccineDoseToComparable dose =
    case dose of
        VaccineDoseFirst ->
            1

        VaccineDoseSecond ->
            2

        VaccineDoseThird ->
            3

        VaccineDoseFourth ->
            4

        VaccineDoseFifth ->
            5


vaccinationFormDynamicContentAndTasks :
    Language
    -> NominalDate
    -> VaccinationFormDynamicContentAndTasksConfig msg
    -> VaccineType
    -> VaccinationForm msg
    -> ( List (Html msg), Int, Int )
vaccinationFormDynamicContentAndTasks language currentDate config vaccineType form =
    let
        birthDate =
            config.birthDate

        expectedDoses =
            config.expectedDoses

        dosesFromPreviousEncountersData =
            config.dosesFromPreviousEncountersData

        dosesFromCurrentEncounterData =
            config.dosesFromCurrentEncounterData

        allDosesGivenData =
            dosesFromPreviousEncountersData
                ++ dosesFromCurrentEncounterData

        allDosesGiven =
            List.map Tuple.first allDosesGivenData

        dosesMissing =
            List.filter (\dose -> not <| List.member dose allDosesGiven)
                expectedDoses

        lastDoseData =
            List.filter (\( dose, date ) -> date /= currentDate)
                allDosesGivenData
                |> List.reverse
                |> List.head

        doseGivenToday =
            List.filter
                (\( dose, date ) ->
                    date == currentDate
                )
                dosesFromCurrentEncounterData
                |> List.head
                |> Maybe.map Tuple.first

        historySection =
            case form.viewMode of
                ViewModeInitial ->
                    let
                        updatePreviousVaccines =
                            (form.updatePreviousVaccines == Just True)
                                && (form.administrationNote /= Just AdministeredToday)

                        noDoseGivenToday =
                            List.filter
                                (\( _, date ) ->
                                    date == currentDate
                                )
                                dosesFromCurrentEncounterData
                                |> List.isEmpty

                        doseAllowedForDeletion =
                            List.filter
                                (\( dose, date ) ->
                                    date /= currentDate
                                )
                                dosesFromCurrentEncounterData
                                |> List.reverse
                                |> List.head
                                |> Maybe.map Tuple.first

                        dosesFromPreviousEncountersForView =
                            List.map (\( dose, date ) -> viewHistoryEntry dose (Just date) False False)
                                dosesFromPreviousEncountersData

                        dosesFromCurrentEncounterForView =
                            List.map
                                (\( dose, date ) ->
                                    let
                                        allowDelete =
                                            noDoseGivenToday
                                                && (doseAllowedForDeletion == Just dose)
                                    in
                                    viewHistoryEntry dose (Just date) False allowDelete
                                )
                                dosesFromCurrentEncounterData

                        administeredDosesForView =
                            dosesFromPreviousEncountersForView
                                ++ dosesFromCurrentEncounterForView

                        dosesForView =
                            if List.isEmpty administeredDosesForView then
                                [ viewCustomLabel language Translate.VaccinationNoDosesAdministered "." "label" ]

                            else
                                administeredDosesForView
                    in
                    [ div [ class "history" ]
                        dosesForView
                    ]

                ViewModeVaccinationUpdate dose ->
                    [ div [ class "history" ]
                        [ viewHistoryEntry dose Nothing False False ]
                    ]

        viewHistoryEntry dose date updateAllowed deleteAllowed =
            let
                dateForView =
                    Maybe.map formatDDMMYYYY date
                        |> Maybe.withDefault "--/--/----"

                deleteButton =
                    Maybe.map
                        (\date_ ->
                            div
                                [ class "delete"
                                , onClick <| config.deleteVaccinationUpdateDateMsg dose date_
                                ]
                                [ text <| translate language Translate.Delete ]
                        )
                        date
                        |> Maybe.withDefault emptyNode
            in
            div [ class "history-entry" ]
                [ div [ class "dose" ] [ text <| String.fromInt <| vaccineDoseToComparable dose ]
                , div [ class "date" ] [ text dateForView ]
                , showIf updateAllowed <|
                    div
                        [ class "update"
                        , onClick <| config.setVaccinationFormViewModeMsg (ViewModeVaccinationUpdate dose)
                        ]
                        [ text <| translate language Translate.Update ]
                , showIf deleteAllowed <| deleteButton
                ]

        ( inputs, tasksCompleted, tasksActive ) =
            case form.viewMode of
                ViewModeInitial ->
                    Maybe.Extra.or doseGivenToday (List.head dosesMissing)
                        |> Maybe.map
                            (\nextDose ->
                                let
                                    ( vaccineDoseAdministeredPreviouslyQuestion, vaccineDoseAdministeredTodayQuestion, administrationNoteForImmunisationTransId ) =
                                        case vaccineType of
                                            PrenatalVaccine type_ ->
                                                let
                                                    label =
                                                        Translate.PrenatalVaccineLabel type_
                                                            |> translate language
                                                in
                                                ( Translate.VaccineDoseAdministeredPreviouslyPrenatalQuestion label
                                                , Translate.VaccineDoseAdministeredTodayPrenatalQuestion label
                                                , Translate.AdministrationNoteForPrenatalImmunisation
                                                )

                                            WellChildVaccine type_ ->
                                                let
                                                    label =
                                                        Translate.WellChildVaccineLabel type_
                                                            |> translate language
                                                in
                                                ( Translate.VaccineDoseAdministeredPreviouslyWellChildQuestion label
                                                , Translate.VaccineDoseAdministeredTodayWellChildQuestion label
                                                , Translate.AdministrationNoteForWellChildImmunisation
                                                )

                                    -- This is the date starting from which we allow
                                    -- vaccine administration for todays dose.
                                    expectedOnDate =
                                        Maybe.andThen
                                            (\( lastDose, lastDoseDate ) ->
                                                config.nextVaccinationDataForVaccine lastDoseDate lastDose
                                                    |> Maybe.map Tuple.second
                                            )
                                            lastDoseData
                                            |> Maybe.withDefault config.firstDoseExpectedFrom
                                in
                                if Date.compare expectedOnDate currentDate == GT then
                                    -- We've not reached the date on which next dose
                                    -- administration is allowed, therefore, we do not
                                    -- show the input.
                                    ( [], 0, 0 )

                                else
                                    let
                                        ( previousDosesInput, previousDosesTaskCompleted, previousDosesTaskActive ) =
                                            if form.willReceiveVaccineToday == Just True then
                                                ( [], 0, 0 )

                                            else
                                                ( [ viewQuestionLabel language vaccineDoseAdministeredPreviouslyQuestion
                                                  , viewBoolInput
                                                        language
                                                        form.updatePreviousVaccines
                                                        (config.setUpdatePreviousVaccinesMsg nextDose)
                                                        ""
                                                        Nothing
                                                  ]
                                                , taskCompleted form.updatePreviousVaccines
                                                , 1
                                                )

                                        ( todaysDoseInputs, todaysDoseTasksCompleted, todaysDoseTasksActive ) =
                                            if form.updatePreviousVaccines == Just False then
                                                let
                                                    ( whyNotIpnut, whyNotTaskCompleted, whyNotTaskActive ) =
                                                        if form.willReceiveVaccineToday == Just False then
                                                            ( [ div [ class "why-not" ]
                                                                    [ viewQuestionLabel language Translate.WhyNot
                                                                    , viewCheckBoxSelectInput language
                                                                        [ NonAdministrationLackOfStock, NonAdministrationPatientDeclined, NonAdministrationKnownAllergy ]
                                                                        [ NonAdministrationPatientUnableToAfford, NonAdministrationTooIll, NonAdministrationOther ]
                                                                        form.administrationNote
                                                                        config.setAdministrationNoteMsg
                                                                        administrationNoteForImmunisationTransId
                                                                    ]
                                                              ]
                                                            , taskCompletedWithException form.administrationNote AdministeredToday
                                                            , 1
                                                            )

                                                        else
                                                            ( [], 0, 0 )
                                                in
                                                ( [ viewQuestionLabel language vaccineDoseAdministeredTodayQuestion
                                                  , viewBoolInput
                                                        language
                                                        form.willReceiveVaccineToday
                                                        (config.setWillReceiveVaccineTodayMsg nextDose)
                                                        ""
                                                        Nothing
                                                  ]
                                                    ++ whyNotIpnut
                                                , taskCompleted form.willReceiveVaccineToday + whyNotTaskCompleted
                                                , 1 + whyNotTaskActive
                                                )

                                            else
                                                ( [], 0, 0 )
                                    in
                                    ( previousDosesInput ++ todaysDoseInputs
                                    , previousDosesTaskCompleted + todaysDoseTasksCompleted
                                    , previousDosesTaskActive + todaysDoseTasksActive
                                    )
                            )
                        |> Maybe.withDefault ( [], 0, 0 )

                ViewModeVaccinationUpdate dose ->
                    let
                        vaccinationUpdateDateForView =
                            Maybe.map formatDDMMYYYY form.vaccinationUpdateDate
                                |> Maybe.withDefault ""

                        dateFrom =
                            Maybe.andThen
                                (\( lastDoseAdministered, lastDoseDate ) ->
                                    config.nextVaccinationDataForVaccine lastDoseDate lastDoseAdministered
                                )
                                lastDoseData
                                |> Maybe.map Tuple.second
                                -- No doses were given yet, so we will set start date to
                                -- expected due date of first dose.
                                |> Maybe.withDefault config.firstDoseExpectedFrom

                        dateSelectorConfig =
                            { select = config.setVaccinationUpdateDateMsg
                            , close = config.setVaccinationUpdateDateSelectorStateMsg Nothing
                            , dateFrom = dateFrom
                            , dateTo = Date.add Days -1 currentDate
                            , dateDefault = Just dateFrom
                            }
                    in
                    ( [ viewLabel language Translate.SelectDate
                      , div
                            [ class "form-input date"
                            , onClick <| config.setVaccinationUpdateDateSelectorStateMsg (Just dateSelectorConfig)
                            ]
                            [ text vaccinationUpdateDateForView ]
                      , viewModal <| viewCalendarPopup language form.dateSelectorPopupState form.vaccinationUpdateDate
                      , div [ class "update actions" ]
                            [ div
                                [ class "ui primary button"
                                , onClick <| config.setVaccinationFormViewModeMsg ViewModeInitial
                                ]
                                [ text <| translate language Translate.Cancel
                                ]
                            , div
                                [ classList
                                    [ ( "ui primary button", True )
                                    , ( "disabled", isNothing form.vaccinationUpdateDate )
                                    ]
                                , onClick <| config.saveVaccinationUpdateDateMsg dose
                                ]
                                [ text <| translate language Translate.Save ]
                            ]
                      ]
                    , taskCompleted form.vaccinationUpdateDate
                    , 1
                    )
    in
    ( historySection ++ inputs, tasksCompleted, tasksActive )


corePhysicalExamFormWithDefault : CorePhysicalExamForm -> Maybe CorePhysicalExamValue -> CorePhysicalExamForm
corePhysicalExamFormWithDefault form saved =
    saved
        |> unwrap
            form
            (\value ->
                { brittleHair = or form.brittleHair (value.hairHead |> EverySet.member BrittleHairCPE |> Just)
                , paleConjuctiva = or form.paleConjuctiva (value.eyes |> EverySet.member PaleConjuctiva |> Just)
                , neck = or form.neck (value.neck |> EverySet.toList |> Just)
                , heart = or form.heart (value.heart |> EverySet.toList |> List.head)
                , heartMurmur = or form.heartMurmur (Just value.heartMurmur)
                , lungs = or form.lungs (value.lungs |> EverySet.toList |> Just)
                , abdomen = or form.abdomen (value.abdomen |> EverySet.toList |> Just)
                , hands = or form.hands (value.hands |> EverySet.toList |> Just)
                , legs = or form.legs (value.legs |> EverySet.toList |> Just)
                }
            )


toCorePhysicalExamValueWithDefault : Maybe CorePhysicalExamValue -> CorePhysicalExamForm -> Maybe CorePhysicalExamValue
toCorePhysicalExamValueWithDefault saved form =
    corePhysicalExamFormWithDefault form saved
        |> toCorePhysicalExamValue


toCorePhysicalExamValue : CorePhysicalExamForm -> Maybe CorePhysicalExamValue
toCorePhysicalExamValue form =
    Maybe.map CorePhysicalExamValue (Maybe.map (toEverySet BrittleHairCPE NormalHairHead) form.brittleHair)
        |> andMap (Maybe.map (toEverySet PaleConjuctiva NormalEyes) form.paleConjuctiva)
        |> andMap (Maybe.map EverySet.singleton form.heart)
        |> andMap form.heartMurmur
        |> andMap (Maybe.map EverySet.fromList form.neck)
        |> andMap (Maybe.map EverySet.fromList form.lungs)
        |> andMap (Maybe.map EverySet.fromList form.abdomen)
        |> andMap (Maybe.map EverySet.fromList form.hands)
        |> andMap (Maybe.map EverySet.fromList form.legs)


familyPlanningFormWithDefault : FamilyPlanningForm -> Maybe (EverySet FamilyPlanningSign) -> FamilyPlanningForm
familyPlanningFormWithDefault form saved =
    saved
        |> unwrap
            form
            (\value ->
                { signs = or form.signs (EverySet.toList value |> Just) }
            )


toFamilyPlanningValueWithDefault : Maybe (EverySet FamilyPlanningSign) -> FamilyPlanningForm -> Maybe (EverySet FamilyPlanningSign)
toFamilyPlanningValueWithDefault saved form =
    familyPlanningFormWithDefault form saved
        |> toFamilyPlanningValue


toFamilyPlanningValue : FamilyPlanningForm -> Maybe (EverySet FamilyPlanningSign)
toFamilyPlanningValue form =
    Maybe.map (EverySet.fromList >> ifEverySetEmpty NoFamilyPlanning) form.signs


outsideCareFormWithDefault : OutsideCareForm diagnosis -> Maybe (OutsideCareValue diagnosis) -> OutsideCareForm diagnosis
outsideCareFormWithDefault form saved =
    saved
        |> unwrap
            form
            (\value ->
                let
                    malariaMedications =
                        filterIllnessOptions outsideCareMedicationOptionsMalaria

                    hypertensionMedications =
                        filterIllnessOptions outsideCareMedicationOptionsHypertension

                    syphilisMedications =
                        filterIllnessOptions outsideCareMedicationOptionsSyphilis

                    anemiaMedications =
                        filterIllnessOptions outsideCareMedicationOptionsAnemia

                    hivMedications =
                        filterIllnessOptions outsideCareMedicationOptionsHIV

                    filterIllnessOptions options =
                        Maybe.map
                            (EverySet.toList
                                >> List.filter
                                    (\medication ->
                                        List.member medication options
                                    )
                            )
                            value.medications
                in
                { seenAtAnotherFacility = or form.seenAtAnotherFacility (EverySet.member SeenAtAnotherFacility value.signs |> Just)
                , givenNewDiagnosis = or form.givenNewDiagnosis (EverySet.member GivenNewDiagnoses value.signs |> Just)
                , givenMedicine = or form.givenMedicine (EverySet.member GivenMedicine value.signs |> Just)
                , plannedFollowUp = or form.plannedFollowUp (EverySet.member PlannedFollowUpCareWithSpecialist value.signs |> Just)
                , diagnoses = maybeValueConsideringIsDirtyField form.diagnosesDirty form.diagnoses (value.diagnoses |> Maybe.map EverySet.toList)
                , diagnosesDirty = form.diagnosesDirty
                , malariaMedications = or form.malariaMedications malariaMedications
                , hypertensionMedications = or form.hypertensionMedications hypertensionMedications
                , syphilisMedications = or form.syphilisMedications syphilisMedications
                , hivMedications = or form.hivMedications hivMedications
                , anemiaMedications = or form.anemiaMedications anemiaMedications
                }
            )


toOutsideCareValueWithDefault : diagnosis -> Maybe (OutsideCareValue diagnosis) -> OutsideCareForm diagnosis -> Maybe (OutsideCareValue diagnosis)
toOutsideCareValueWithDefault noneValue saved form =
    outsideCareFormWithDefault form saved
        |> toOutsideCareValue noneValue


toOutsideCareValue : diagnosis -> OutsideCareForm diagnosis -> Maybe (OutsideCareValue diagnosis)
toOutsideCareValue noneValue form =
    let
        maybeSigns =
            [ Maybe.map (ifTrue SeenAtAnotherFacility) form.seenAtAnotherFacility
            , ifNullableTrue GivenNewDiagnoses form.givenNewDiagnosis
            , ifNullableTrue GivenMedicine form.givenMedicine
            , ifNullableTrue PlannedFollowUpCareWithSpecialist form.plannedFollowUp
            ]
                |> Maybe.Extra.combine
                |> Maybe.map (List.foldl EverySet.union EverySet.empty >> ifEverySetEmpty NoOutsideCareSigns)
    in
    Maybe.map
        (\signs ->
            let
                diagnoses =
                    Maybe.map (EverySet.fromList >> ifEverySetEmpty noneValue) form.diagnoses

                mapMedications illnessMedications =
                    [ Maybe.map (EverySet.fromList >> Just) illnessMedications
                        |> Maybe.withDefault (Just EverySet.empty)
                    ]

                medications =
                    mapMedications form.malariaMedications
                        ++ mapMedications form.hypertensionMedications
                        ++ mapMedications form.syphilisMedications
                        ++ mapMedications form.anemiaMedications
                        ++ mapMedications form.hivMedications
                        |> Maybe.Extra.combine
                        |> Maybe.map (List.foldl EverySet.union EverySet.empty >> ifEverySetEmpty NoOutsideCareMedications)
            in
            { signs = signs
            , diagnoses = diagnoses
            , medications = medications
            }
        )
        maybeSigns


type alias OutsideCareConfig diagnosis msg =
    { setBoolInputMsg : (Bool -> OutsideCareForm diagnosis -> OutsideCareForm diagnosis) -> Bool -> msg
    , setDiagnosisMsg : diagnosis -> msg
    , setMalariaMedicationMsg : OutsideCareMedication -> msg
    , setHypertensionMedicationMsg : OutsideCareMedication -> msg
    , setSyphilisMedicationMsg : OutsideCareMedication -> msg
    , setAnemiaMedicationMsg : OutsideCareMedication -> msg
    , setHIVMedicationMsg : OutsideCareMedication -> msg
    , malariaDiagnoses : List diagnosis
    , hypertensionDiagnoses : List diagnosis
    , syphilisDiagnoses : List diagnosis
    , anemiaDiagnoses : List diagnosis
    , hivDiagnoses : List diagnosis
    , malariaHeaderTransId : Translate.TranslationId
    , resolveHypertensionHeaderTransId : List diagnosis -> Translate.TranslationId
    , syphilisHeaderTransId : Translate.TranslationId
    , anemiaHeaderTransId : Translate.TranslationId
    , hivHeaderTransId : Translate.TranslationId
    , diagnosesLeftColumn : List diagnosis
    , diagnosesRightColumn : List diagnosis
    , otherDiagnosis : diagnosis
    , diagnosisTransId : diagnosis -> Translate.TranslationId
    }


outsideCareFormInputsAndTasks : Language -> OutsideCareConfig diagnosis msg -> OutsideCareStep -> OutsideCareForm diagnosis -> ( List (Html msg), List (Maybe Bool) )
outsideCareFormInputsAndTasks language config step form =
    case step of
        OutsideCareStepDiagnoses ->
            outsideCareFormInputsAndTasksDiagnoses language config form

        OutsideCareStepMedications ->
            outsideCareFormInputsAndTasksMedications language config form


outsideCareFormInputsAndTasksDiagnoses : Language -> OutsideCareConfig diagnosis msg -> OutsideCareForm diagnosis -> ( List (Html msg), List (Maybe Bool) )
outsideCareFormInputsAndTasksDiagnoses language config form =
    let
        ( givenNewDiagnosisSection, givenNewDiagnosisTasks ) =
            if form.seenAtAnotherFacility == Just True then
                let
                    ( newDiagnosisSection, newDiagnosisTasks ) =
                        if form.givenNewDiagnosis == Just True then
                            let
                                ( givenMedicineSection, givenMedicineTasks ) =
                                    Maybe.map
                                        (\diagnoses ->
                                            let
                                                diagnosesWithPossibleMedication =
                                                    config.malariaDiagnoses
                                                        ++ config.hypertensionDiagnoses
                                                        ++ config.syphilisDiagnoses
                                                        ++ config.anemiaDiagnoses
                                                        ++ config.hivDiagnoses
                                            in
                                            if
                                                List.any (\diagnosis -> List.member diagnosis diagnoses)
                                                    diagnosesWithPossibleMedication
                                            then
                                                ( [ viewQuestionLabel language <| Translate.OutsideCareSignQuestion GivenMedicine
                                                  , viewBoolInput
                                                        language
                                                        form.givenMedicine
                                                        (config.setBoolInputMsg
                                                            (\value form_ ->
                                                                { form_
                                                                    | givenMedicine = Just value
                                                                    , malariaMedications = Nothing
                                                                    , hypertensionMedications = Nothing
                                                                    , syphilisMedications = Nothing
                                                                    , hivMedications = Nothing
                                                                    , anemiaMedications = Nothing
                                                                }
                                                            )
                                                        )
                                                        "given-medicine"
                                                        Nothing
                                                  ]
                                                , [ form.givenMedicine ]
                                                )

                                            else
                                                ( [], [] )
                                        )
                                        form.diagnoses
                                        |> Maybe.withDefault ( [], [] )
                            in
                            ( [ viewLabel language Translate.SelectAllDiagnoses
                              , viewCheckBoxMultipleSelectInput language
                                    config.diagnosesLeftColumn
                                    config.diagnosesRightColumn
                                    (form.diagnoses |> Maybe.withDefault [])
                                    (Just config.otherDiagnosis)
                                    config.setDiagnosisMsg
                                    config.diagnosisTransId
                              , viewQuestionLabel language <| Translate.OutsideCareSignQuestion PlannedFollowUpCareWithSpecialist
                              , viewBoolInput
                                    language
                                    form.plannedFollowUp
                                    (config.setBoolInputMsg
                                        (\value form_ -> { form_ | plannedFollowUp = Just value })
                                    )
                                    "planned-follow-up"
                                    Nothing
                              ]
                                ++ givenMedicineSection
                            , [ maybeToBoolTask form.diagnoses
                              , form.plannedFollowUp
                              ]
                                ++ givenMedicineTasks
                            )

                        else
                            ( [], [] )
                in
                ( [ viewQuestionLabel language <| Translate.OutsideCareSignQuestion GivenNewDiagnoses
                  , viewBoolInput
                        language
                        form.givenNewDiagnosis
                        (config.setBoolInputMsg
                            (\value form_ ->
                                { form_
                                    | givenNewDiagnosis = Just value
                                    , givenMedicine = Nothing
                                    , diagnoses = Nothing
                                    , diagnosesDirty = True
                                }
                            )
                        )
                        "given-new-diagnosis"
                        Nothing
                  ]
                    ++ newDiagnosisSection
                , [ form.givenNewDiagnosis ] ++ newDiagnosisTasks
                )

            else
                ( [], [] )
    in
    ( [ viewQuestionLabel language <| Translate.OutsideCareSignQuestion SeenAtAnotherFacility
      , viewBoolInput
            language
            form.seenAtAnotherFacility
            (config.setBoolInputMsg
                (\value form_ ->
                    { form_
                        | seenAtAnotherFacility = Just value
                        , givenNewDiagnosis = Nothing
                        , givenMedicine = Nothing
                        , diagnoses = Nothing
                        , diagnosesDirty = True
                    }
                )
            )
            "seen-at-another-facility"
            Nothing
      ]
        ++ givenNewDiagnosisSection
    , [ form.seenAtAnotherFacility ] ++ givenNewDiagnosisTasks
    )


outsideCareFormInputsAndTasksMedications : Language -> OutsideCareConfig diagnosis msg -> OutsideCareForm diagnosis -> ( List (Html msg), List (Maybe Bool) )
outsideCareFormInputsAndTasksMedications language config form =
    if form.givenMedicine == Just True then
        Maybe.map
            (\diagnoses ->
                let
                    ( malariaInputs, malariaTasks ) =
                        if List.any (\diagnosis -> List.member diagnosis diagnoses) config.malariaDiagnoses then
                            ( [ viewHeader config.malariaHeaderTransId
                              , selectTreatmentOptionsInput outsideCareMedicationOptionsMalaria
                                    NoOutsideCareMedicationForMalaria
                                    form.malariaMedications
                                    config.setMalariaMedicationMsg
                              ]
                            , [ maybeToBoolTask form.malariaMedications ]
                            )

                        else
                            ( [], [] )

                    ( hypertensionInputs, hypertensionTasks ) =
                        if List.any (\diagnosis -> List.member diagnosis diagnoses) config.hypertensionDiagnoses then
                            ( [ viewHeader <| config.resolveHypertensionHeaderTransId diagnoses
                              , selectTreatmentOptionsInput outsideCareMedicationOptionsHypertension
                                    NoOutsideCareMedicationForHypertension
                                    form.hypertensionMedications
                                    config.setHypertensionMedicationMsg
                              ]
                            , [ maybeToBoolTask form.hypertensionMedications ]
                            )

                        else
                            ( [], [] )

                    ( syphilisInputs, syphilisTasks ) =
                        if List.any (\diagnosis -> List.member diagnosis diagnoses) config.syphilisDiagnoses then
                            ( [ viewHeader config.syphilisHeaderTransId
                              , selectTreatmentOptionsInput outsideCareMedicationOptionsSyphilis
                                    NoOutsideCareMedicationForSyphilis
                                    form.syphilisMedications
                                    config.setSyphilisMedicationMsg
                              ]
                            , [ maybeToBoolTask form.syphilisMedications ]
                            )

                        else
                            ( [], [] )

                    ( anemiaInputs, anemiaTasks ) =
                        if List.any (\diagnosis -> List.member diagnosis diagnoses) config.anemiaDiagnoses then
                            ( [ viewHeader config.anemiaHeaderTransId
                              , selectTreatmentOptionsInput outsideCareMedicationOptionsAnemia
                                    NoOutsideCareMedicationForAnemia
                                    form.anemiaMedications
                                    config.setAnemiaMedicationMsg
                              ]
                            , [ maybeToBoolTask form.anemiaMedications ]
                            )

                        else
                            ( [], [] )

                    ( hivInputs, hivTasks ) =
                        if List.any (\diagnosis -> List.member diagnosis diagnoses) config.hivDiagnoses then
                            ( [ viewHeader config.hivHeaderTransId
                              , selectTreatmentOptionsInput outsideCareMedicationOptionsHIV
                                    NoOutsideCareMedicationForHIV
                                    form.hivMedications
                                    config.setHIVMedicationMsg
                              ]
                            , [ maybeToBoolTask form.hivMedications ]
                            )

                        else
                            ( [], [] )

                    selectTreatmentOptionsInput allOptions noneOption currentValue setMsg =
                        let
                            options =
                                List.filter ((/=) noneOption) allOptions
                        in
                        viewCheckBoxMultipleSelectCustomInput language
                            options
                            []
                            (Maybe.withDefault [] currentValue)
                            (Just noneOption)
                            setMsg
                            (viewOutsideCareMedicationOption language)

                    viewHeader diagnosisTransId =
                        div [ class "label" ]
                            [ span [] [ text <| translate language Translate.DiagnosedAtAnotherFacilityPrefix ]
                            , text " "
                            , span [ class "diagnosis" ] [ text <| translate language diagnosisTransId ]
                            , text " "
                            , span [] [ text <| translate language Translate.DiagnosedAtAnotherFacilitySuffix ]
                            ]
                in
                ( malariaInputs ++ hypertensionInputs ++ syphilisInputs ++ anemiaInputs ++ hivInputs
                , malariaTasks ++ hypertensionTasks ++ syphilisTasks ++ anemiaTasks ++ hivTasks
                )
            )
            form.diagnoses
            |> Maybe.withDefault ( [], [] )

    else
        ( [], [] )


viewOutsideCareMedicationOption : Language -> OutsideCareMedication -> Html any
viewOutsideCareMedicationOption language medication =
    if List.member medication noOutsideCareMedicationOptions then
        label [] [ text <| translate language <| Translate.OutsideCareMedicationLabel medication ]

    else
        viewOutsideCareMedicationOptionWithDosage language medication


outsideCareMedicationOptionsMalaria : List OutsideCareMedication
outsideCareMedicationOptionsMalaria =
    [ OutsideCareMedicationQuinineSulphate
    , OutsideCareMedicationCoartem
    , NoOutsideCareMedicationForMalaria
    ]


outsideCareMedicationOptionsHypertension : List OutsideCareMedication
outsideCareMedicationOptionsHypertension =
    [ OutsideCareMedicationMethyldopa2
    , OutsideCareMedicationMethyldopa3
    , OutsideCareMedicationMethyldopa4
    , OutsideCareMedicationCarvedilol
    , OutsideCareMedicationAmlodipine
    , NoOutsideCareMedicationForHypertension
    ]


outsideCareMedicationOptionsSyphilis : List OutsideCareMedication
outsideCareMedicationOptionsSyphilis =
    [ OutsideCareMedicationPenecilin1
    , OutsideCareMedicationPenecilin3
    , OutsideCareMedicationErythromycin
    , OutsideCareMedicationAzithromycin
    , OutsideCareMedicationCeftriaxon
    , NoOutsideCareMedicationForSyphilis
    ]


outsideCareMedicationOptionsAnemia : List OutsideCareMedication
outsideCareMedicationOptionsAnemia =
    [ OutsideCareMedicationIron1
    , OutsideCareMedicationIron2
    , OutsideCareMedicationFolicAcid
    , NoOutsideCareMedicationForAnemia
    ]


outsideCareMedicationOptionsHIV : List OutsideCareMedication
outsideCareMedicationOptionsHIV =
    [ OutsideCareMedicationTDF3TC
    , OutsideCareMedicationDolutegravir
    , NoOutsideCareMedicationForHIV
    ]


noOutsideCareMedicationOptions : List OutsideCareMedication
noOutsideCareMedicationOptions =
    [ NoOutsideCareMedicationForMalaria
    , NoOutsideCareMedicationForHypertension
    , NoOutsideCareMedicationForSyphilis
    , NoOutsideCareMedicationForAnemia
    , NoOutsideCareMedicationForHIV
    ]


viewOutsideCareMedicationOptionWithDosage : Language -> OutsideCareMedication -> Html any
viewOutsideCareMedicationOptionWithDosage language medication =
    label []
        [ span [ class "treatment" ] [ text <| translate language <| Translate.OutsideCareMedicationLabel medication ]
        , text ": "
        , span [ class "dosage" ] [ text <| translate language <| Translate.OutsideCareMedicationDosage medication ]
        ]


hivTestFormWithDefault : HIVTestForm msg -> Maybe HIVTestValue -> HIVTestForm msg
hivTestFormWithDefault form saved =
    saved
        |> unwrap
            form
            (\value ->
                let
                    knownAsPositiveValue =
                        List.member value.executionNote [ TestNoteKnownAsPositive ]

                    testPerformedValue =
                        List.member value.executionNote [ TestNoteRunToday, TestNoteRunPreviously ]

                    testPerformedTodayFromValue =
                        value.executionNote == TestNoteRunToday

                    hivProgramHCValue =
                        Maybe.map (EverySet.member HIVProgramHC)
                            value.hivSigns
                            |> Maybe.withDefault False

                    partnerHIVPositiveValue =
                        Maybe.map (EverySet.member PartnerHIVPositive)
                            value.hivSigns
                            |> Maybe.withDefault False

                    partnerTakingARVValue =
                        Maybe.map (EverySet.member PartnerTakingARV)
                            value.hivSigns
                            |> Maybe.withDefault False

                    partnerSurpressedViralLoadValue =
                        Maybe.map (EverySet.member PartnerSurpressedViralLoad)
                            value.hivSigns
                            |> Maybe.withDefault False
                in
                { knownAsPositive = or form.knownAsPositive (Just knownAsPositiveValue)
                , testPerformed = valueConsideringIsDirtyField form.testPerformedDirty form.testPerformed testPerformedValue
                , testPerformedDirty = form.testPerformedDirty
                , testPerformedToday = valueConsideringIsDirtyField form.testPerformedTodayDirty form.testPerformedToday testPerformedTodayFromValue
                , testPerformedTodayDirty = form.testPerformedTodayDirty
                , executionNote = valueConsideringIsDirtyField form.executionNoteDirty form.executionNote value.executionNote
                , executionNoteDirty = form.executionNoteDirty
                , executionDate = maybeValueConsideringIsDirtyField form.executionDateDirty form.executionDate value.executionDate
                , executionDateDirty = form.executionDateDirty
                , testResult = or form.testResult value.testResult
                , hivProgramHC = valueConsideringIsDirtyField form.hivProgramHCDirty form.hivProgramHC hivProgramHCValue
                , hivProgramHCDirty = form.hivProgramHCDirty
                , partnerHIVPositive = valueConsideringIsDirtyField form.partnerHIVPositiveDirty form.partnerHIVPositive partnerHIVPositiveValue
                , partnerHIVPositiveDirty = form.partnerHIVPositiveDirty
                , partnerTakingARV = valueConsideringIsDirtyField form.partnerTakingARVDirty form.partnerTakingARV partnerTakingARVValue
                , partnerTakingARVDirty = form.partnerTakingARVDirty
                , partnerSurpressedViralLoad = valueConsideringIsDirtyField form.partnerSurpressedViralLoadDirty form.partnerSurpressedViralLoad partnerSurpressedViralLoadValue
                , partnerSurpressedViralLoadDirty = form.partnerSurpressedViralLoadDirty
                , dateSelectorPopupState = form.dateSelectorPopupState
                }
            )


toHIVTestValueWithDefault : Maybe HIVTestValue -> HIVTestForm msg -> Maybe HIVTestValue
toHIVTestValueWithDefault saved form =
    hivTestFormWithDefault form saved
        |> toHIVTestValue


toHIVTestValue : HIVTestForm msg -> Maybe HIVTestValue
toHIVTestValue form =
    Maybe.map
        (\executionNote ->
            let
                hivSigns =
                    [ ifNullableTrue HIVProgramHC form.hivProgramHC
                    , ifNullableTrue PartnerHIVPositive form.partnerHIVPositive
                    , ifNullableTrue PartnerTakingARV form.partnerTakingARV
                    , ifNullableTrue PartnerSurpressedViralLoad form.partnerSurpressedViralLoad
                    ]
                        |> Maybe.Extra.combine
                        |> Maybe.map (List.foldl EverySet.union EverySet.empty >> ifEverySetEmpty NoPrenatalHIVSign)
            in
            { executionNote = executionNote
            , executionDate = form.executionDate
            , testResult = form.testResult
            , hivSigns = hivSigns
            }
        )
        form.executionNote


malariaTestFormWithDefault : MalariaTestForm msg -> Maybe MalariaTestValue -> MalariaTestForm msg
malariaTestFormWithDefault form saved =
    saved
        |> unwrap
            form
            (\value ->
                let
                    testPerformedValue =
                        List.member value.executionNote [ TestNoteRunToday, TestNoteRunPreviously ]

                    testPerformedTodayFromValue =
                        value.executionNote == TestNoteRunToday
                in
                { testPerformed = valueConsideringIsDirtyField form.testPerformedDirty form.testPerformed testPerformedValue
                , testPerformedDirty = form.testPerformedDirty
                , testPerformedToday = valueConsideringIsDirtyField form.testPerformedTodayDirty form.testPerformedToday testPerformedTodayFromValue
                , testPerformedTodayDirty = form.testPerformedTodayDirty
                , executionNote = valueConsideringIsDirtyField form.executionNoteDirty form.executionNote value.executionNote
                , executionNoteDirty = form.executionNoteDirty
                , executionDate = maybeValueConsideringIsDirtyField form.executionDateDirty form.executionDate value.executionDate
                , executionDateDirty = form.executionDateDirty
                , testResult = or form.testResult value.testResult
                , dateSelectorPopupState = form.dateSelectorPopupState
                }
            )


toMalariaTestValueWithDefault : Maybe MalariaTestValue -> MalariaTestForm msg -> Maybe MalariaTestValue
toMalariaTestValueWithDefault saved form =
    malariaTestFormWithDefault form saved
        |> toMalariaTestValue


toMalariaTestValue : MalariaTestForm msg -> Maybe MalariaTestValue
toMalariaTestValue form =
    Maybe.map
        (\executionNote ->
            { executionNote = executionNote
            , executionDate = form.executionDate
            , testResult = form.testResult
            }
        )
        form.executionNote


urineDipstickFormWithDefault : UrineDipstickForm msg -> Maybe UrineDipstickTestValue -> UrineDipstickForm msg
urineDipstickFormWithDefault form saved =
    saved
        |> unwrap
            form
            (\value ->
                let
                    testPerformedValue =
                        List.member value.executionNote [ TestNoteRunToday, TestNoteRunPreviously ]

                    testPerformedTodayFromValue =
                        value.executionNote == TestNoteRunToday
                in
                { testPerformed = valueConsideringIsDirtyField form.testPerformedDirty form.testPerformed testPerformedValue
                , testPerformedDirty = form.testPerformedDirty
                , testPerformedToday = valueConsideringIsDirtyField form.testPerformedTodayDirty form.testPerformedToday testPerformedTodayFromValue
                , testPerformedTodayDirty = form.testPerformedTodayDirty
                , testVariant = or form.testVariant value.testVariant
                , executionNote = valueConsideringIsDirtyField form.executionNoteDirty form.executionNote value.executionNote
                , executionNoteDirty = form.executionNoteDirty
                , executionDate = maybeValueConsideringIsDirtyField form.executionDateDirty form.executionDate value.executionDate
                , executionDateDirty = form.executionDateDirty
                , dateSelectorPopupState = form.dateSelectorPopupState
                }
            )


toUrineDipstickTestValueWithDefault : Maybe UrineDipstickTestValue -> UrineDipstickForm msg -> Maybe UrineDipstickTestValue
toUrineDipstickTestValueWithDefault saved form =
    urineDipstickFormWithDefault form saved
        |> toUrineDipstickTestValue


toUrineDipstickTestValue : UrineDipstickForm msg -> Maybe UrineDipstickTestValue
toUrineDipstickTestValue form =
    Maybe.map
        (\executionNote ->
            { testVariant = form.testVariant
            , executionNote = executionNote
            , executionDate = form.executionDate
            , protein = Nothing
            , ph = Nothing
            , glucose = Nothing
            , leukocytes = Nothing
            , nitrite = Nothing
            , urobilinogen = Nothing
            , haemoglobin = Nothing
            , ketone = Nothing
            , bilirubin = Nothing
            }
        )
        form.executionNote


randomBloodSugarFormWithDefault : RandomBloodSugarForm msg -> Maybe RandomBloodSugarTestValue -> RandomBloodSugarForm msg
randomBloodSugarFormWithDefault form saved =
    saved
        |> unwrap
            form
            (\value ->
                let
                    testPerformedValue =
                        List.member value.executionNote [ TestNoteRunToday, TestNoteRunPreviously ]

                    testPerformedTodayFromValue =
                        value.executionNote == TestNoteRunToday

                    patientFastedValue =
                        Maybe.map (EverySet.member PrerequisiteFastFor12h)
                            value.testPrerequisites
                in
                { testPerformed = valueConsideringIsDirtyField form.testPerformedDirty form.testPerformed testPerformedValue
                , testPerformedDirty = form.testPerformedDirty
                , patientFasted = or form.patientFasted patientFastedValue
                , testPerformedToday = valueConsideringIsDirtyField form.testPerformedTodayDirty form.testPerformedToday testPerformedTodayFromValue
                , testPerformedTodayDirty = form.testPerformedTodayDirty
                , executionNote = valueConsideringIsDirtyField form.executionNoteDirty form.executionNote value.executionNote
                , executionNoteDirty = form.executionNoteDirty
                , executionDate = maybeValueConsideringIsDirtyField form.executionDateDirty form.executionDate value.executionDate
                , executionDateDirty = form.executionDateDirty
                , dateSelectorPopupState = form.dateSelectorPopupState
                }
            )


toRandomBloodSugarTestValueWithDefault : Maybe RandomBloodSugarTestValue -> RandomBloodSugarForm msg -> Maybe RandomBloodSugarTestValue
toRandomBloodSugarTestValueWithDefault saved form =
    randomBloodSugarFormWithDefault form saved
        |> toRandomBloodSugarTestValue


toRandomBloodSugarTestValue : RandomBloodSugarForm msg -> Maybe RandomBloodSugarTestValue
toRandomBloodSugarTestValue form =
    Maybe.map
        (\executionNote ->
            let
                testPrerequisites =
                    Maybe.map
                        (\patientFasted ->
                            if patientFasted then
                                EverySet.singleton PrerequisiteFastFor12h

                            else
                                EverySet.singleton NoTestPrerequisites
                        )
                        form.patientFasted
            in
            { executionNote = executionNote
            , executionDate = form.executionDate
            , testPrerequisites = testPrerequisites
            , sugarCount = Nothing
            , originatingEncounter = Nothing
            }
        )
        form.executionNote


nonRDTFormWithDefault :
    NonRDTForm msg
    -> Maybe { value | executionNote : TestExecutionNote, executionDate : Maybe NominalDate }
    -> NonRDTForm msg
nonRDTFormWithDefault form saved =
    saved
        |> unwrap
            form
            (\value ->
                let
                    knownAsPositiveValue =
                        List.member value.executionNote [ TestNoteKnownAsPositive ]

                    testPerformedValue =
                        List.member value.executionNote [ TestNoteRunToday, TestNoteRunPreviously ]

                    testPerformedTodayFromValue =
                        value.executionNote == TestNoteRunToday
                in
                { knownAsPositive = or form.knownAsPositive (Just knownAsPositiveValue)
                , testPerformed = valueConsideringIsDirtyField form.testPerformedDirty form.testPerformed testPerformedValue
                , testPerformedDirty = form.testPerformedDirty
                , testPerformedToday = valueConsideringIsDirtyField form.testPerformedTodayDirty form.testPerformedToday testPerformedTodayFromValue
                , testPerformedTodayDirty = form.testPerformedTodayDirty
                , executionNote = valueConsideringIsDirtyField form.executionNoteDirty form.executionNote value.executionNote
                , executionNoteDirty = form.executionNoteDirty
                , executionDate = maybeValueConsideringIsDirtyField form.executionDateDirty form.executionDate value.executionDate
                , executionDateDirty = form.executionDateDirty
                , dateSelectorPopupState = form.dateSelectorPopupState
                }
            )


toNonRDTValueWithDefault :
    Maybe { value | executionNote : TestExecutionNote, executionDate : Maybe NominalDate }
    -> (TestExecutionNote -> Maybe NominalDate -> { value | executionNote : TestExecutionNote, executionDate : Maybe NominalDate })
    -> NonRDTForm msg
    -> Maybe { value | executionNote : TestExecutionNote, executionDate : Maybe NominalDate }
toNonRDTValueWithDefault saved withEmptyResultsFunc form =
    let
        formWithDefault =
            nonRDTFormWithDefault form saved
    in
    Maybe.map (\executionNote -> withEmptyResultsFunc executionNote formWithDefault.executionDate)
        formWithDefault.executionNote


toHepatitisBTestValueWithEmptyResults : TestExecutionNote -> Maybe NominalDate -> HepatitisBTestValue
toHepatitisBTestValueWithEmptyResults note date =
    HepatitisBTestValue note date Nothing Nothing


toSyphilisTestValueWithEmptyResults : TestExecutionNote -> Maybe NominalDate -> SyphilisTestValue
toSyphilisTestValueWithEmptyResults note date =
    SyphilisTestValue note date Nothing Nothing Nothing


toHemoglobinTestValueWithEmptyResults : TestExecutionNote -> Maybe NominalDate -> HemoglobinTestValue
toHemoglobinTestValueWithEmptyResults note date =
    HemoglobinTestValue note date Nothing


toBloodGpRsTestValueWithEmptyResults : TestExecutionNote -> Maybe NominalDate -> BloodGpRsTestValue
toBloodGpRsTestValueWithEmptyResults note date =
    BloodGpRsTestValue note date Nothing Nothing Nothing


toHIVPCRTestValueWithEmptyResults : TestExecutionNote -> Maybe NominalDate -> HIVPCRTestValue
toHIVPCRTestValueWithEmptyResults note date =
    HIVPCRTestValue note date Nothing Nothing


viewHIVTestForm :
    Language
    -> NominalDate
    -> ContentAndTasksLaboratoryTestInitialConfig msg
    -> ContentAndTasksForPerformedLaboratoryTestConfig msg
    -> HIVTestForm msg
    -> ( Html msg, Int, Int )
viewHIVTestForm language currentDate configInitial configPerformed form =
    let
        ( initialSection, initialTasksCompleted, initialTasksTotal ) =
            contentAndTasksLaboratoryTestKnownAsPositive language currentDate configInitial TaskHIVTest form

        emptySection =
            ( [], 0, 0 )

        ( derivedSection, derivedTasksCompleted, derivedTasksTotal ) =
            if form.knownAsPositive /= Just False then
                emptySection

            else
                let
                    ( rdtSection, rdtTasksCompleted, rdtTasksTotal ) =
                        prenatalRDTFormInputsAndTasks language currentDate configInitial configPerformed TaskHIVTest form

                    ( hivSignsSection, hivSignsTasksCompleted, hivSignsTasksTotal ) =
                        Maybe.map
                            (\testResult ->
                                case testResult of
                                    PrenatalTestPositive ->
                                        let
                                            updateFunc =
                                                \value form_ ->
                                                    { form_ | hivProgramHC = Just value, hivProgramHCDirty = True }
                                        in
                                        ( [ viewQuestionLabel language <| Translate.PrenatalHIVSignQuestion HIVProgramHC
                                          , viewBoolInput
                                                language
                                                form.hivProgramHC
                                                (configInitial.setHIVTestFormBoolInputMsg updateFunc)
                                                "hiv-program"
                                                Nothing
                                          ]
                                        , taskCompleted form.hivProgramHC
                                        , 1
                                        )

                                    PrenatalTestNegative ->
                                        let
                                            partnerHIVPositiveUpdateFunc =
                                                \value form_ ->
                                                    { form_
                                                        | partnerHIVPositive = Just value
                                                        , partnerHIVPositiveDirty = True
                                                        , partnerTakingARV = Nothing
                                                        , partnerTakingARVDirty = True
                                                        , partnerSurpressedViralLoad = Nothing
                                                        , partnerSurpressedViralLoadDirty = True
                                                    }

                                            ( partnerHivStatusSection, partnerHivStatusTasksCompleted, partnerHivStatusTasksTotal ) =
                                                if form.partnerHIVPositive == Just True then
                                                    let
                                                        partnerTakingARVUpdateFunc =
                                                            \value form_ ->
                                                                { form_
                                                                    | partnerTakingARV = Just value
                                                                    , partnerTakingARVDirty = True
                                                                    , partnerSurpressedViralLoad = Nothing
                                                                    , partnerSurpressedViralLoadDirty = True
                                                                }

                                                        ( partnerARVSection, partnerARVTasksCompleted, partnerARVTasksTotal ) =
                                                            if form.partnerTakingARV == Just True then
                                                                let
                                                                    partnerSurpressedViralLoadUpdateFunc =
                                                                        \value form_ ->
                                                                            { form_ | partnerSurpressedViralLoad = Just value, partnerSurpressedViralLoadDirty = True }
                                                                in
                                                                ( [ viewQuestionLabel language <| Translate.PrenatalHIVSignQuestion PartnerSurpressedViralLoad
                                                                  , viewBoolInput
                                                                        language
                                                                        form.partnerSurpressedViralLoad
                                                                        (configInitial.setHIVTestFormBoolInputMsg partnerSurpressedViralLoadUpdateFunc)
                                                                        "partner-surpressed-viral-load"
                                                                        Nothing
                                                                  ]
                                                                , taskCompleted form.partnerSurpressedViralLoad
                                                                , 1
                                                                )

                                                            else
                                                                emptySection
                                                    in
                                                    ( [ viewQuestionLabel language <| Translate.PrenatalHIVSignQuestion PartnerTakingARV
                                                      , viewBoolInput
                                                            language
                                                            form.partnerTakingARV
                                                            (configInitial.setHIVTestFormBoolInputMsg partnerTakingARVUpdateFunc)
                                                            "partner-taking-arv"
                                                            Nothing
                                                      ]
                                                        ++ partnerARVSection
                                                    , taskCompleted form.partnerTakingARV + partnerARVTasksCompleted
                                                    , 1 + partnerARVTasksTotal
                                                    )

                                                else
                                                    emptySection
                                        in
                                        ( [ viewQuestionLabel language <| Translate.PrenatalHIVSignQuestion PartnerHIVPositive
                                          , viewBoolInput
                                                language
                                                form.partnerHIVPositive
                                                (configInitial.setHIVTestFormBoolInputMsg partnerHIVPositiveUpdateFunc)
                                                "partner-hiv-positive"
                                                Nothing
                                          ]
                                            ++ partnerHivStatusSection
                                        , taskCompleted form.partnerHIVPositive + partnerHivStatusTasksCompleted
                                        , 1 + partnerHivStatusTasksTotal
                                        )

                                    PrenatalTestIndeterminate ->
                                        emptySection
                            )
                            form.testResult
                            |> Maybe.withDefault emptySection
                in
                ( rdtSection ++ hivSignsSection
                , rdtTasksCompleted + hivSignsTasksCompleted
                , rdtTasksTotal + hivSignsTasksTotal
                )
    in
    ( div [ class "ui form laboratory hiv" ] <|
        [ viewCustomLabel language (Translate.PrenatalLaboratoryTaskLabel TaskHIVTest) "" "label header"
        ]
            ++ initialSection
            ++ derivedSection
    , initialTasksCompleted + derivedTasksCompleted
    , initialTasksTotal + derivedTasksTotal
    )


viewMalariaTestForm :
    Language
    -> NominalDate
    -> ContentAndTasksLaboratoryTestInitialConfig msg
    -> ContentAndTasksForPerformedLaboratoryTestConfig msg
    -> MalariaTestForm msg
    -> ( Html msg, Int, Int )
viewMalariaTestForm language currentDate configInitial configPerformed form =
    let
        ( inputs, tasksCompleted, tasksTotal ) =
            prenatalRDTFormInputsAndTasks language currentDate configInitial configPerformed TaskMalariaTest form
    in
    ( div [ class "ui form laboratory malaria" ] <|
        [ viewCustomLabel language (Translate.PrenatalLaboratoryTaskLabel TaskMalariaTest) "" "label header" ]
            ++ inputs
    , tasksCompleted
    , tasksTotal
    )


prenatalRDTFormInputsAndTasks :
    Language
    -> NominalDate
    -> ContentAndTasksLaboratoryTestInitialConfig msg
    -> ContentAndTasksForPerformedLaboratoryTestConfig msg
    -> LaboratoryTask
    ->
        { f
            | testPerformed : Maybe Bool
            , testPerformedToday : Maybe Bool
            , executionNote : Maybe TestExecutionNote
            , executionDate : Maybe NominalDate
            , testResult : Maybe PrenatalTestResult
            , dateSelectorPopupState : Maybe (DateSelectorConfig msg)
        }
    -> ( List (Html msg), Int, Int )
prenatalRDTFormInputsAndTasks language currentDate configInitial configPerformed task form =
    let
        ( initialSection, initialTasksCompleted, initialTasksTotal ) =
            contentAndTasksLaboratoryTestInitial language currentDate configInitial task form

        ( derivedSection, derivedTasksCompleted, derivedTasksTotal ) =
            if form.testPerformed == Just True then
                let
                    ( performedTestSection, performedTestTasksCompleted, performedTestTasksTotal ) =
                        contentAndTasksForPerformedLaboratoryTest language currentDate configPerformed task form

                    setTestResultMsg =
                        case task of
                            TaskHIVTest ->
                                Just configInitial.setHIVTestResultMsg

                            TaskMalariaTest ->
                                Just configInitial.setMalariaTestResultMsg

                            _ ->
                                Nothing

                    ( testResultSection, testResultTasksCompleted, testResultTasksTotal ) =
                        if isNothing form.executionDate then
                            ( [], 0, 0 )

                        else
                            Maybe.map
                                (\setResultMsg ->
                                    let
                                        emptyOption =
                                            if isNothing form.testResult then
                                                emptySelectOption True

                                            else
                                                emptyNode
                                    in
                                    ( [ viewLabel language <| Translate.PrenatalLaboratoryTaskResult task
                                      , emptyOption
                                            :: List.map
                                                (\result ->
                                                    option
                                                        [ value (prenatalTestResultToString result)
                                                        , selected (form.testResult == Just result)
                                                        ]
                                                        [ text <| translate language <| Translate.PrenatalTestResult result ]
                                                )
                                                [ PrenatalTestPositive, PrenatalTestNegative, PrenatalTestIndeterminate ]
                                            |> select
                                                [ onInput setResultMsg
                                                , class "form-input select"
                                                ]
                                      ]
                                    , taskCompleted form.testResult
                                    , 1
                                    )
                                )
                                setTestResultMsg
                                |> Maybe.withDefault ( [], 0, 0 )
                in
                ( performedTestSection ++ testResultSection
                , performedTestTasksCompleted + testResultTasksCompleted
                , performedTestTasksTotal + testResultTasksTotal
                )

            else
                ( [], 0, 0 )
    in
    ( initialSection ++ derivedSection
    , initialTasksCompleted + derivedTasksCompleted
    , initialTasksTotal + derivedTasksTotal
    )


viewUrineDipstickForm :
    Language
    -> NominalDate
    -> ContentAndTasksLaboratoryTestInitialConfig msg
    -> ContentAndTasksForPerformedLaboratoryTestConfig msg
    -> UrineDipstickForm msg
    -> ( Html msg, Int, Int )
viewUrineDipstickForm language currentDate configInitial configPerformed form =
    let
        ( initialSection, initialTasksCompleted, initialTasksTotal ) =
            contentAndTasksLaboratoryTestInitial language currentDate configInitial TaskUrineDipstickTest form

        ( derivedSection, derivedTasksCompleted, derivedTasksTotal ) =
            if form.testPerformed == Just True then
                let
                    ( performedTestSection, performedTestTasksCompleted, performedTestTasksTotal ) =
                        contentAndTasksForPerformedLaboratoryTest language currentDate configPerformed TaskUrineDipstickTest form

                    ( testVariantSection, testVariantTasksCompleted, testVariantTasksTotal ) =
                        ( [ viewQuestionLabel language Translate.TestVariantUrineDipstickQuestion
                          , viewCheckBoxSelectInput language
                                [ VariantShortTest ]
                                [ VariantLongTest ]
                                form.testVariant
                                configInitial.setUrineDipstickTestVariantMsg
                                Translate.PrenatalUrineDipstickTestVariant
                          ]
                        , taskCompleted form.testVariant
                        , 1
                        )

                    testResultSection =
                        if isNothing form.executionDate then
                            []

                        else
                            [ viewCustomLabel language Translate.PrenatalLaboratoryTaskResultsHelper "." "label" ]
                in
                ( testVariantSection ++ performedTestSection ++ testResultSection
                , performedTestTasksCompleted + testVariantTasksCompleted
                , performedTestTasksTotal + testVariantTasksTotal
                )

            else
                ( [], 0, 0 )
    in
    ( div [ class "ui form laboratory urine-dipstick" ] <|
        [ viewCustomLabel language (Translate.PrenatalLaboratoryTaskLabel TaskUrineDipstickTest) "" "label header"
        ]
            ++ initialSection
            ++ derivedSection
    , initialTasksCompleted + derivedTasksCompleted
    , initialTasksTotal + derivedTasksTotal
    )


viewRandomBloodSugarForm :
    Language
    -> NominalDate
    -> ContentAndTasksLaboratoryTestInitialConfig msg
    -> ContentAndTasksForPerformedLaboratoryTestConfig msg
    -> RandomBloodSugarForm msg
    -> ( Html msg, Int, Int )
viewRandomBloodSugarForm language currentDate configInitial configPerformed form =
    let
        ( initialSection, initialTasksCompleted, initialTasksTotal ) =
            contentAndTasksLaboratoryTestInitial language currentDate configInitial TaskRandomBloodSugarTest form

        ( derivedSection, derivedTasksCompleted, derivedTasksTotal ) =
            if form.testPerformed == Just True then
                let
                    ( performedTestSection, performedTestTasksCompleted, performedTestTasksTotal ) =
                        contentAndTasksForPerformedLaboratoryTest language currentDate configPerformed TaskRandomBloodSugarTest form

                    ( testPrerequisitesSection, testPrerequisitesTasksCompleted, testPrerequisitesTasksTotal ) =
                        ( [ viewQuestionLabel language <| Translate.TestPrerequisiteQuestion PrerequisiteFastFor12h
                          , viewBoolInput
                                language
                                form.patientFasted
                                (configInitial.setRandomBloodSugarTestFormBoolInputMsg (\value form_ -> { form_ | patientFasted = Just value }))
                                "patient-fasted"
                                Nothing
                          ]
                        , taskCompleted form.patientFasted
                        , 1
                        )

                    testResultSection =
                        if isNothing form.executionDate then
                            []

                        else
                            [ viewCustomLabel language Translate.PrenatalLaboratoryTaskResultsHelper "." "label" ]
                in
                ( testPrerequisitesSection ++ performedTestSection ++ testResultSection
                , performedTestTasksCompleted + testPrerequisitesTasksCompleted
                , performedTestTasksTotal + testPrerequisitesTasksTotal
                )

            else
                ( [], 0, 0 )
    in
    ( div [ class "ui form laboratory urine-dipstick" ] <|
        [ viewCustomLabel language (Translate.PrenatalLaboratoryTaskLabel TaskRandomBloodSugarTest) "" "label header"
        ]
            ++ initialSection
            ++ derivedSection
    , initialTasksCompleted + derivedTasksCompleted
    , initialTasksTotal + derivedTasksTotal
    )


viewNonRDTFormCheckKnownAsPositive :
    Language
    -> NominalDate
    -> ContentAndTasksLaboratoryTestInitialConfig msg
    -> ContentAndTasksForPerformedLaboratoryTestConfig msg
    -> LaboratoryTask
    -> NonRDTForm msg
    -> ( Html msg, Int, Int )
viewNonRDTFormCheckKnownAsPositive language currentDate configInitial configPerformed task form =
    let
        ( initialSection, initialTasksCompleted, initialTasksTotal ) =
            contentAndTasksLaboratoryTestKnownAsPositive language currentDate configInitial task form

        ( derivedSection, derivedTasksCompleted, derivedTasksTotal ) =
            if form.knownAsPositive /= Just False then
                ( [], 0, 0 )

            else
                nonRDTFormInputsAndTasks language currentDate configInitial configPerformed task form
    in
    ( div [ class "ui form laboratory non-rdt" ] <|
        [ viewCustomLabel language (Translate.PrenatalLaboratoryTaskLabel task) "" "label header"
        ]
            ++ initialSection
            ++ derivedSection
    , initialTasksCompleted + derivedTasksCompleted
    , initialTasksTotal + derivedTasksTotal
    )


viewNonRDTForm :
    Language
    -> NominalDate
    -> ContentAndTasksLaboratoryTestInitialConfig msg
    -> ContentAndTasksForPerformedLaboratoryTestConfig msg
    -> LaboratoryTask
    -> NonRDTForm msg
    -> ( Html msg, Int, Int )
viewNonRDTForm language currentDate configInitial configPerformed task form =
    let
        ( inputs, tasksCompleted, tasksTotal ) =
            nonRDTFormInputsAndTasks language currentDate configInitial configPerformed task form
    in
    ( div [ class "ui form laboratory non-rdt" ] <|
        [ viewCustomLabel language (Translate.PrenatalLaboratoryTaskLabel task) "" "label header"
        ]
            ++ inputs
    , tasksCompleted
    , tasksTotal
    )


nonRDTFormInputsAndTasks :
    Language
    -> NominalDate
    -> ContentAndTasksLaboratoryTestInitialConfig msg
    -> ContentAndTasksForPerformedLaboratoryTestConfig msg
    -> LaboratoryTask
    -> NonRDTForm msg
    -> ( List (Html msg), Int, Int )
nonRDTFormInputsAndTasks language currentDate configInitial configPerformed task form =
    let
        ( initialSection, initialTasksCompleted, initialTasksTotal ) =
            contentAndTasksLaboratoryTestInitial language currentDate configInitial task form

        ( derivedSection, derivedTasksCompleted, derivedTasksTotal ) =
            if form.testPerformed == Just True then
                let
                    ( performedTestSection, performedTestTasksCompleted, performedTestTasksTotal ) =
                        contentAndTasksForPerformedLaboratoryTest language currentDate configPerformed task form

                    testResultSection =
                        if isNothing form.executionDate then
                            []

                        else
                            [ viewCustomLabel language Translate.PrenatalLaboratoryTaskResultsHelper "." "label" ]
                in
                ( performedTestSection ++ testResultSection
                , performedTestTasksCompleted
                , performedTestTasksTotal
                )

            else
                ( [], 0, 0 )
    in
    ( initialSection ++ derivedSection
    , initialTasksCompleted + derivedTasksCompleted
    , initialTasksTotal + derivedTasksTotal
    )


type alias ContentAndTasksLaboratoryTestInitialConfig msg =
    { setHIVTestFormBoolInputMsg : (Bool -> HIVTestForm msg -> HIVTestForm msg) -> Bool -> msg
    , setHIVTestExecutionNoteMsg : TestExecutionNote -> msg
    , setHIVTestResultMsg : String -> msg
    , setSyphilisTestFormBoolInputMsg : (Bool -> NonRDTForm msg -> NonRDTForm msg) -> Bool -> msg
    , setSyphilisTestExecutionNoteMsg : TestExecutionNote -> msg
    , setHepatitisBTestFormBoolInputMsg : (Bool -> NonRDTForm msg -> NonRDTForm msg) -> Bool -> msg
    , setHepatitisBTestExecutionNoteMsg : TestExecutionNote -> msg
    , setMalariaTestFormBoolInputMsg : (Bool -> MalariaTestForm msg -> MalariaTestForm msg) -> Bool -> msg
    , setMalariaTestExecutionNoteMsg : TestExecutionNote -> msg
    , setMalariaTestResultMsg : String -> msg
    , setBloodGpRsTestFormBoolInputMsg : (Bool -> NonRDTForm msg -> NonRDTForm msg) -> Bool -> msg
    , setBloodGpRsTestExecutionNoteMsg : TestExecutionNote -> msg
    , setUrineDipstickTestFormBoolInputMsg : (Bool -> UrineDipstickForm msg -> UrineDipstickForm msg) -> Bool -> msg
    , setUrineDipstickTestExecutionNoteMsg : TestExecutionNote -> msg
    , setUrineDipstickTestVariantMsg : TestVariant -> msg
    , setHemoglobinTestFormBoolInputMsg : (Bool -> NonRDTForm msg -> NonRDTForm msg) -> Bool -> msg
    , setHemoglobinTestExecutionNoteMsg : TestExecutionNote -> msg
    , setRandomBloodSugarTestFormBoolInputMsg : (Bool -> RandomBloodSugarForm msg -> RandomBloodSugarForm msg) -> Bool -> msg
    , setRandomBloodSugarTestExecutionNoteMsg : TestExecutionNote -> msg
    , setHIVPCRTestFormBoolInputMsg : (Bool -> NonRDTForm msg -> NonRDTForm msg) -> Bool -> msg
    , setHIVPCRTestExecutionNoteMsg : TestExecutionNote -> msg
    , noOpMsg : msg
    }


contentAndTasksLaboratoryTestInitial :
    Language
    -> NominalDate
    -> ContentAndTasksLaboratoryTestInitialConfig msg
    -> LaboratoryTask
    ->
        { f
            | testPerformed : Maybe Bool
            , executionNote : Maybe TestExecutionNote
        }
    -> ( List (Html msg), Int, Int )
contentAndTasksLaboratoryTestInitial language currentDate config task form =
    let
        boolInputUpdateFunc =
            \value form_ ->
                { form_
                    | testPerformed = Just value
                    , testPerformedDirty = True
                    , testPerformedToday = Nothing
                    , testPerformedTodayDirty = True
                    , executionNote = Nothing
                    , executionNoteDirty = True
                    , executionDate = Nothing
                    , executionDateDirty = True
                }

        msgs =
            case task of
                TaskHIVTest ->
                    { setBoolInputMsg = config.setHIVTestFormBoolInputMsg boolInputUpdateFunc
                    , setExecutionNoteMsg = config.setHIVTestExecutionNoteMsg
                    }

                TaskSyphilisTest ->
                    { setBoolInputMsg = config.setSyphilisTestFormBoolInputMsg boolInputUpdateFunc
                    , setExecutionNoteMsg = config.setSyphilisTestExecutionNoteMsg
                    }

                TaskHepatitisBTest ->
                    { setBoolInputMsg = config.setHepatitisBTestFormBoolInputMsg boolInputUpdateFunc
                    , setExecutionNoteMsg = config.setHepatitisBTestExecutionNoteMsg
                    }

                TaskMalariaTest ->
                    { setBoolInputMsg = config.setMalariaTestFormBoolInputMsg boolInputUpdateFunc
                    , setExecutionNoteMsg = config.setMalariaTestExecutionNoteMsg
                    }

                TaskBloodGpRsTest ->
                    { setBoolInputMsg = config.setBloodGpRsTestFormBoolInputMsg boolInputUpdateFunc
                    , setExecutionNoteMsg = config.setBloodGpRsTestExecutionNoteMsg
                    }

                TaskUrineDipstickTest ->
                    { setBoolInputMsg = config.setUrineDipstickTestFormBoolInputMsg boolInputUpdateFunc
                    , setExecutionNoteMsg = config.setUrineDipstickTestExecutionNoteMsg
                    }

                TaskHemoglobinTest ->
                    { setBoolInputMsg = config.setHemoglobinTestFormBoolInputMsg boolInputUpdateFunc
                    , setExecutionNoteMsg = config.setHemoglobinTestExecutionNoteMsg
                    }

                TaskRandomBloodSugarTest ->
                    { setBoolInputMsg = config.setRandomBloodSugarTestFormBoolInputMsg boolInputUpdateFunc
                    , setExecutionNoteMsg = config.setRandomBloodSugarTestExecutionNoteMsg
                    }

                TaskHIVPCRTest ->
                    { setBoolInputMsg = config.setHIVPCRTestFormBoolInputMsg boolInputUpdateFunc
                    , setExecutionNoteMsg = config.setHIVPCRTestExecutionNoteMsg
                    }

                TaskCompletePreviousTests ->
                    -- Not in use, as this task got a proprietary form.
                    { setBoolInputMsg = always config.noOpMsg
                    , setExecutionNoteMsg = always config.noOpMsg
                    }

        ( derivedSection, derivedTasksCompleted, derivedTasksTotal ) =
            Maybe.map
                (\testPerformed ->
                    if testPerformed then
                        ( [], 0, 0 )

                    else
                        ( [ div [ class "why-not" ]
                                [ viewQuestionLabel language Translate.WhyNot
                                , viewCheckBoxSelectInput language
                                    [ TestNoteLackOfReagents
                                    , TestNoteLackOfOtherSupplies
                                    , TestNoteBrokenEquipment
                                    ]
                                    [ TestNoteNoEquipment
                                    , TestNoteNotIndicated
                                    ]
                                    form.executionNote
                                    msgs.setExecutionNoteMsg
                                    Translate.TestExecutionNote
                                ]
                          ]
                        , taskCompleted form.executionNote
                        , 1
                        )
                )
                form.testPerformed
                |> Maybe.withDefault ( [], 0, 0 )
    in
    ( [ viewQuestionLabel language Translate.TestPerformedQuestion
      , viewBoolInput
            language
            form.testPerformed
            msgs.setBoolInputMsg
            "test-performed"
            Nothing
      ]
        ++ derivedSection
    , taskCompleted form.testPerformed + derivedTasksCompleted
    , 1 + derivedTasksTotal
    )


type alias ContentAndTasksForPerformedLaboratoryTestConfig msg =
    { setHIVTestFormBoolInputMsg : (Bool -> HIVTestForm msg -> HIVTestForm msg) -> Bool -> msg
    , setHIVTestExecutionDateMsg : NominalDate -> msg
    , setHIVTestDateSelectorStateMsg : Maybe (DateSelectorConfig msg) -> msg
    , setSyphilisTestFormBoolInputMsg : (Bool -> NonRDTForm msg -> NonRDTForm msg) -> Bool -> msg
    , setSyphilisTestExecutionDateMsg : NominalDate -> msg
    , setSyphilisTestDateSelectorStateMsg : Maybe (DateSelectorConfig msg) -> msg
    , setHepatitisBTestFormBoolInputMsg : (Bool -> NonRDTForm msg -> NonRDTForm msg) -> Bool -> msg
    , setHepatitisBTestExecutionDateMsg : NominalDate -> msg
    , setHepatitisBTestDateSelectorStateMsg : Maybe (DateSelectorConfig msg) -> msg
    , setMalariaTestFormBoolInputMsg : (Bool -> MalariaTestForm msg -> MalariaTestForm msg) -> Bool -> msg
    , setMalariaTestExecutionDateMsg : NominalDate -> msg
    , setMalariaTestDateSelectorStateMsg : Maybe (DateSelectorConfig msg) -> msg
    , setBloodGpRsTestFormBoolInputMsg : (Bool -> NonRDTForm msg -> NonRDTForm msg) -> Bool -> msg
    , setBloodGpRsTestExecutionDateMsg : NominalDate -> msg
    , setBloodGpRsTestDateSelectorStateMsg : Maybe (DateSelectorConfig msg) -> msg
    , setUrineDipstickTestFormBoolInputMsg : (Bool -> UrineDipstickForm msg -> UrineDipstickForm msg) -> Bool -> msg
    , setUrineDipstickTestExecutionDateMsg : NominalDate -> msg
    , setUrineDipstickTestDateSelectorStateMsg : Maybe (DateSelectorConfig msg) -> msg
    , setHemoglobinTestFormBoolInputMsg : (Bool -> NonRDTForm msg -> NonRDTForm msg) -> Bool -> msg
    , setHemoglobinTestExecutionDateMsg : NominalDate -> msg
    , setHemoglobinTestDateSelectorStateMsg : Maybe (DateSelectorConfig msg) -> msg
    , setRandomBloodSugarTestFormBoolInputMsg : (Bool -> RandomBloodSugarForm msg -> RandomBloodSugarForm msg) -> Bool -> msg
    , setRandomBloodSugarTestExecutionDateMsg : NominalDate -> msg
    , setRandomBloodSugarTestDateSelectorStateMsg : Maybe (DateSelectorConfig msg) -> msg
    , setHIVPCRTestFormBoolInputMsg : (Bool -> NonRDTForm msg -> NonRDTForm msg) -> Bool -> msg
    , setHIVPCRTestExecutionDateMsg : NominalDate -> msg
    , setHIVPCRTestDateSelectorStateMsg : Maybe (DateSelectorConfig msg) -> msg
    , noOpMsg : msg
    }


contentAndTasksForPerformedLaboratoryTest :
    Language
    -> NominalDate
    -> ContentAndTasksForPerformedLaboratoryTestConfig msg
    -> LaboratoryTask
    ->
        { f
            | testPerformed : Maybe Bool
            , testPerformedToday : Maybe Bool
            , executionNote : Maybe TestExecutionNote
            , executionDate : Maybe NominalDate
            , dateSelectorPopupState : Maybe (DateSelectorConfig msg)
        }
    -> ( List (Html msg), Int, Int )
contentAndTasksForPerformedLaboratoryTest language currentDate config task form =
    if form.testPerformed /= Just True then
        ( [], 0, 0 )

    else
        let
            boolInputUpdateFunc =
                \value form_ ->
                    let
                        ( executionNote, executionDate ) =
                            if value == True then
                                ( Just TestNoteRunToday, Just currentDate )

                            else
                                ( Just TestNoteRunPreviously, Nothing )
                    in
                    { form_
                        | testPerformedToday = Just value
                        , testPerformedTodayDirty = True
                        , executionNote = executionNote
                        , executionNoteDirty = True
                        , executionDate = executionDate
                        , executionDateDirty = True
                    }

            msgs =
                case task of
                    TaskHIVTest ->
                        { setBoolInputMsg = config.setHIVTestFormBoolInputMsg boolInputUpdateFunc
                        , setExecutionDateMsg = config.setHIVTestExecutionDateMsg
                        , setDateSelectorStateMsg = config.setHIVTestDateSelectorStateMsg
                        }

                    TaskSyphilisTest ->
                        { setBoolInputMsg = config.setSyphilisTestFormBoolInputMsg boolInputUpdateFunc
                        , setExecutionDateMsg = config.setSyphilisTestExecutionDateMsg
                        , setDateSelectorStateMsg = config.setSyphilisTestDateSelectorStateMsg
                        }

                    TaskHepatitisBTest ->
                        { setBoolInputMsg = config.setHepatitisBTestFormBoolInputMsg boolInputUpdateFunc
                        , setExecutionDateMsg = config.setHepatitisBTestExecutionDateMsg
                        , setDateSelectorStateMsg = config.setHepatitisBTestDateSelectorStateMsg
                        }

                    TaskMalariaTest ->
                        { setBoolInputMsg = config.setMalariaTestFormBoolInputMsg boolInputUpdateFunc
                        , setExecutionDateMsg = config.setMalariaTestExecutionDateMsg
                        , setDateSelectorStateMsg = config.setMalariaTestDateSelectorStateMsg
                        }

                    TaskBloodGpRsTest ->
                        { setBoolInputMsg = config.setBloodGpRsTestFormBoolInputMsg boolInputUpdateFunc
                        , setExecutionDateMsg = config.setBloodGpRsTestExecutionDateMsg
                        , setDateSelectorStateMsg = config.setBloodGpRsTestDateSelectorStateMsg
                        }

                    TaskUrineDipstickTest ->
                        { setBoolInputMsg = config.setUrineDipstickTestFormBoolInputMsg boolInputUpdateFunc
                        , setExecutionDateMsg = config.setUrineDipstickTestExecutionDateMsg
                        , setDateSelectorStateMsg = config.setUrineDipstickTestDateSelectorStateMsg
                        }

                    TaskHemoglobinTest ->
                        { setBoolInputMsg = config.setHemoglobinTestFormBoolInputMsg boolInputUpdateFunc
                        , setExecutionDateMsg = config.setHemoglobinTestExecutionDateMsg
                        , setDateSelectorStateMsg = config.setHemoglobinTestDateSelectorStateMsg
                        }

                    TaskRandomBloodSugarTest ->
                        { setBoolInputMsg = config.setRandomBloodSugarTestFormBoolInputMsg boolInputUpdateFunc
                        , setExecutionDateMsg = config.setRandomBloodSugarTestExecutionDateMsg
                        , setDateSelectorStateMsg = config.setRandomBloodSugarTestDateSelectorStateMsg
                        }

                    TaskHIVPCRTest ->
                        { setBoolInputMsg = config.setHIVPCRTestFormBoolInputMsg boolInputUpdateFunc
                        , setExecutionDateMsg = config.setHIVPCRTestExecutionDateMsg
                        , setDateSelectorStateMsg = config.setHIVPCRTestDateSelectorStateMsg
                        }

                    TaskCompletePreviousTests ->
                        -- Not in use, as this task got a proprietary form.
                        { setBoolInputMsg = always config.noOpMsg
                        , setExecutionDateMsg = always config.noOpMsg
                        , setDateSelectorStateMsg = always config.noOpMsg
                        }

            ( derivedSection, derivedTasksCompleted, derivedTasksTotal ) =
                Maybe.map
                    (\testPerformedToday ->
                        let
                            ( executionDateContent, executionDateTasksCompleted, executionDateTasksTotal ) =
                                if testPerformedToday then
                                    ( [ p [ class "test-date" ] [ text <| formatDDMMYYYY currentDate ] ], 0, 0 )

                                else
                                    let
                                        executionDateForView =
                                            Maybe.map formatDDMMYYYY form.executionDate
                                                |> Maybe.withDefault ""

                                        dateSelectorConfig =
                                            let
                                                dateTo =
                                                    Date.add Days -1 currentDate
                                            in
                                            { select = msgs.setExecutionDateMsg
                                            , close = msgs.setDateSelectorStateMsg Nothing
                                            , dateFrom = Date.add Days -35 currentDate
                                            , dateTo = dateTo
                                            , dateDefault = Just dateTo
                                            }
                                    in
                                    ( [ div
                                            [ class "form-input date"
                                            , onClick <| msgs.setDateSelectorStateMsg (Just dateSelectorConfig)
                                            ]
                                            [ text executionDateForView
                                            ]
                                      , viewModal <| viewCalendarPopup language form.dateSelectorPopupState form.executionDate
                                      ]
                                    , taskCompleted form.executionDate
                                    , 1
                                    )
                        in
                        ( (viewLabel language <| Translate.PrenatalLaboratoryTaskDate task)
                            :: executionDateContent
                        , executionDateTasksCompleted
                        , executionDateTasksTotal
                        )
                    )
                    form.testPerformedToday
                    |> Maybe.withDefault ( [], 0, 0 )
        in
        ( [ viewQuestionLabel language Translate.TestPerformedTodayQuestion
          , viewBoolInput
                language
                form.testPerformedToday
                msgs.setBoolInputMsg
                "test-performed-today"
                Nothing
          ]
            ++ derivedSection
        , taskCompleted form.testPerformedToday + derivedTasksCompleted
        , 1 + derivedTasksTotal
        )


contentAndTasksLaboratoryTestKnownAsPositive :
    Language
    -> NominalDate
    -> ContentAndTasksLaboratoryTestInitialConfig msg
    -> LaboratoryTask
    -> { f | knownAsPositive : Maybe Bool }
    -> ( List (Html msg), Int, Int )
contentAndTasksLaboratoryTestKnownAsPositive language currentDate config task form =
    let
        updateFunc =
            \knownAsPositive form_ ->
                let
                    executionNote =
                        if knownAsPositive then
                            Just TestNoteKnownAsPositive

                        else
                            Nothing
                in
                { form_
                    | knownAsPositive = Just knownAsPositive
                    , testPerformed = Nothing
                    , testPerformedDirty = True
                    , testPerformedToday = Nothing
                    , testPerformedTodayDirty = True
                    , executionNote = executionNote
                    , executionNoteDirty = True
                    , executionDate = Nothing
                    , executionDateDirty = True
                }

        setMsg =
            case task of
                TaskHIVTest ->
                    config.setHIVTestFormBoolInputMsg updateFunc

                TaskSyphilisTest ->
                    config.setSyphilisTestFormBoolInputMsg updateFunc

                TaskHepatitisBTest ->
                    config.setHepatitisBTestFormBoolInputMsg updateFunc

                -- Known as positive is not applicable for other tests.
                _ ->
                    always config.noOpMsg
    in
    ( [ viewQuestionLabel language <| Translate.KnownAsPositiveQuestion task
      , viewBoolInput
            language
            form.knownAsPositive
            setMsg
            "known-as-positive"
            Nothing
      ]
    , taskCompleted form.knownAsPositive
    , 1
    )
