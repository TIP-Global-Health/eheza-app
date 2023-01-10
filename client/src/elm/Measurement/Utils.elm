module Measurement.Utils exposing (..)

import AssocList as Dict exposing (Dict)
import Backend.Counseling.Model exposing (CounselingTiming(..))
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
import Backend.ParticipantConsent.Model exposing (ParticipantForm)
import Backend.Session.Model exposing (EditableSession, OfflineSession)
import Backend.Session.Utils exposing (getChild, getChildHistoricalMeasurements, getChildMeasurementData, getChildMeasurementData2, getChildren, getMother, getMotherHistoricalMeasurements, getMotherMeasurementData, getMotherMeasurementData2, getMyMother)
import Date exposing (Unit(..))
import DateSelector.Model exposing (DateSelectorConfig)
import DateSelector.SelectorPopup exposing (viewCalendarPopup)
import EverySet exposing (EverySet)
import Gizra.Html exposing (emptyNode, showIf)
import Gizra.NominalDate exposing (NominalDate, diffDays, diffMonths, formatDDMMYYYY)
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
        , viewMeasurementInput
        , viewQuestionLabel
        )
import Round
import Translate exposing (Language, TranslationId, translate)
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
    , ncdaData =
        fromData .ncda (.value >> Just >> fromNCDAValue >> (\form -> NCDAData form Nothing))
            |> Maybe.withDefault emptyNCDAData
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
                , step = form.step
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
                        testPerformedByExecutionNote value.executionNote

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
                        testPerformedByExecutionNote value.executionNote

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
                        testPerformedByExecutionNote value.executionNote

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


randomBloodSugarFormWithDefault : RandomBloodSugarForm msg -> Maybe (RandomBloodSugarTestValue encounterId) -> RandomBloodSugarForm msg
randomBloodSugarFormWithDefault form saved =
    saved
        |> unwrap
            form
            (\value ->
                let
                    testPerformedValue =
                        testPerformedByExecutionNote value.executionNote

                    testPerformedTodayFromValue =
                        value.executionNote == TestNoteRunToday

                    patientFastedValue =
                        Maybe.map (EverySet.member PrerequisiteFastFor12h)
                            value.testPrerequisites

                    immediateResultValue =
                        Maybe.map (EverySet.member PrerequisiteImmediateResult)
                            value.testPrerequisites
                in
                { testPerformed = valueConsideringIsDirtyField form.testPerformedDirty form.testPerformed testPerformedValue
                , testPerformedDirty = form.testPerformedDirty
                , patientFasted = or form.patientFasted patientFastedValue
                , immediateResult = or form.immediateResult immediateResultValue
                , testPerformedToday = valueConsideringIsDirtyField form.testPerformedTodayDirty form.testPerformedToday testPerformedTodayFromValue
                , testPerformedTodayDirty = form.testPerformedTodayDirty
                , executionNote = valueConsideringIsDirtyField form.executionNoteDirty form.executionNote value.executionNote
                , executionNoteDirty = form.executionNoteDirty
                , executionDate = maybeValueConsideringIsDirtyField form.executionDateDirty form.executionDate value.executionDate
                , executionDateDirty = form.executionDateDirty
                , dateSelectorPopupState = form.dateSelectorPopupState
                , sugarCount = maybeValueConsideringIsDirtyField form.sugarCountDirty form.sugarCount value.sugarCount
                , sugarCountDirty = form.sugarCountDirty
                }
            )


toRandomBloodSugarTestValueWithDefault : Maybe (RandomBloodSugarTestValue encounterId) -> RandomBloodSugarForm msg -> Maybe (RandomBloodSugarTestValue encounterId)
toRandomBloodSugarTestValueWithDefault saved form =
    randomBloodSugarFormWithDefault form saved
        |> toRandomBloodSugarTestValue


toRandomBloodSugarTestValue : RandomBloodSugarForm msg -> Maybe (RandomBloodSugarTestValue encounterId)
toRandomBloodSugarTestValue form =
    Maybe.map
        (\executionNote ->
            let
                testPrerequisites =
                    Maybe.map2
                        (\patientFasted immediateResult ->
                            case ( patientFasted, immediateResult ) of
                                ( True, True ) ->
                                    EverySet.fromList [ PrerequisiteFastFor12h, PrerequisiteImmediateResult ]

                                ( True, False ) ->
                                    EverySet.singleton PrerequisiteFastFor12h

                                ( False, True ) ->
                                    EverySet.singleton PrerequisiteImmediateResult

                                ( False, False ) ->
                                    EverySet.singleton NoTestPrerequisites
                        )
                        form.patientFasted
                        form.immediateResult
            in
            { executionNote = executionNote
            , executionDate = form.executionDate
            , testPrerequisites = testPrerequisites
            , sugarCount = form.sugarCount
            , originatingEncounter = Nothing
            }
        )
        form.executionNote


pregnancyTestFormWithDefault : PregnancyTestForm msg -> Maybe PregnancyTestValue -> PregnancyTestForm msg
pregnancyTestFormWithDefault form saved =
    saved
        |> unwrap
            form
            (\value ->
                let
                    knownAsPositiveValue =
                        List.member value.executionNote [ TestNoteKnownAsPositive ]

                    testPerformedValue =
                        testPerformedByExecutionNote value.executionNote

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
                , testResult = or form.testResult value.testResult
                , dateSelectorPopupState = form.dateSelectorPopupState
                }
            )


toPregnancyTestValueWithDefault : Maybe PregnancyTestValue -> PregnancyTestForm msg -> Maybe PregnancyTestValue
toPregnancyTestValueWithDefault saved form =
    pregnancyTestFormWithDefault form saved
        |> toPregnancyTestValue


toPregnancyTestValue : PregnancyTestForm msg -> Maybe PregnancyTestValue
toPregnancyTestValue form =
    Maybe.map
        (\executionNote ->
            { executionNote = executionNote
            , executionDate = form.executionDate
            , testResult = form.testResult
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
                        testPerformedByExecutionNote value.executionNote

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


toHepatitisBTestValueWithEmptyResults : TestExecutionNote -> Maybe NominalDate -> HepatitisBTestValue encounterId
toHepatitisBTestValueWithEmptyResults note date =
    HepatitisBTestValue note date Nothing Nothing


toSyphilisTestValueWithEmptyResults : TestExecutionNote -> Maybe NominalDate -> SyphilisTestValue encounterId
toSyphilisTestValueWithEmptyResults note date =
    SyphilisTestValue note date Nothing Nothing Nothing


toHemoglobinTestValueWithEmptyResults : TestExecutionNote -> Maybe NominalDate -> HemoglobinTestValue
toHemoglobinTestValueWithEmptyResults note date =
    HemoglobinTestValue note date Nothing


toBloodGpRsTestValueWithEmptyResults : TestExecutionNote -> Maybe NominalDate -> BloodGpRsTestValue encounterId
toBloodGpRsTestValueWithEmptyResults note date =
    BloodGpRsTestValue note date Nothing Nothing Nothing


toHIVPCRTestValueWithEmptyResults : TestExecutionNote -> Maybe NominalDate -> HIVPCRTestValue
toHIVPCRTestValueWithEmptyResults note date =
    HIVPCRTestValue note date Nothing Nothing


toCreatinineTestValueWithEmptyResults : TestExecutionNote -> Maybe NominalDate -> CreatinineTestValue
toCreatinineTestValueWithEmptyResults note date =
    CreatinineTestValue note date Nothing Nothing


toLiverFunctionTestValueWithEmptyResults : TestExecutionNote -> Maybe NominalDate -> LiverFunctionTestValue
toLiverFunctionTestValueWithEmptyResults note date =
    LiverFunctionTestValue note date Nothing Nothing


toLipidPanelTestValueWithEmptyResults : TestExecutionNote -> Maybe NominalDate -> LipidPanelTestValue
toLipidPanelTestValueWithEmptyResults note date =
    LipidPanelTestValue note date Nothing Nothing Nothing Nothing Nothing


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
                                    TestPositive ->
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

                                    TestNegative ->
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

                                    TestIndeterminate ->
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
        [ viewCustomLabel language (Translate.LaboratoryTaskLabel TaskHIVTest) "" "label header"
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
        [ viewCustomLabel language (Translate.LaboratoryTaskLabel TaskMalariaTest) "" "label header" ]
            ++ inputs
    , tasksCompleted
    , tasksTotal
    )


viewPregnancyTestForm :
    Language
    -> NominalDate
    -> ContentAndTasksLaboratoryTestInitialConfig msg
    -> ContentAndTasksForPerformedLaboratoryTestConfig msg
    -> PregnancyTestForm msg
    -> ( Html msg, Int, Int )
viewPregnancyTestForm language currentDate configInitial configPerformed form =
    let
        ( initialSection, initialTasksCompleted, initialTasksTotal ) =
            contentAndTasksLaboratoryTestKnownAsPositive language currentDate configInitial TaskPregnancyTest form

        ( derivedSection, derivedTasksCompleted, derivedTasksTotal ) =
            if form.knownAsPositive /= Just False then
                ( [], 0, 0 )

            else
                prenatalRDTFormInputsAndTasks language currentDate configInitial configPerformed TaskPregnancyTest form
    in
    ( div [ class "ui form laboratory malaria" ] <|
        [ viewCustomLabel language (Translate.LaboratoryTaskLabel TaskPregnancyTest) "" "label header" ]
            ++ initialSection
            ++ derivedSection
    , initialTasksCompleted + derivedTasksCompleted
    , initialTasksTotal + derivedTasksTotal
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
            , testResult : Maybe TestResult
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

                            TaskPregnancyTest ->
                                Just configInitial.setPregnancyTestResultMsg

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
                                    ( [ viewLabel language <| Translate.LaboratoryTaskResult task
                                      , emptyOption
                                            :: List.map
                                                (\result ->
                                                    option
                                                        [ value (testResultToString result)
                                                        , selected (form.testResult == Just result)
                                                        ]
                                                        [ text <| translate language <| Translate.TestResult result ]
                                                )
                                                [ TestPositive, TestNegative, TestIndeterminate ]
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
                                Translate.UrineDipstickTestVariant
                          ]
                        , taskCompleted form.testVariant
                        , 1
                        )

                    testResultSection =
                        if isNothing form.executionDate then
                            []

                        else
                            [ viewCustomLabel language Translate.LaboratoryTaskResultsHelper "." "label" ]
                in
                ( testVariantSection ++ performedTestSection ++ testResultSection
                , performedTestTasksCompleted + testVariantTasksCompleted
                , performedTestTasksTotal + testVariantTasksTotal
                )

            else
                ( [], 0, 0 )
    in
    ( div [ class "ui form laboratory urine-dipstick" ] <|
        [ viewCustomLabel language (Translate.LaboratoryTaskLabel TaskUrineDipstickTest) "" "label header"
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
                        ( [ viewQuestionLabel language <| Translate.TestPrerequisiteQuestion PrerequisiteImmediateResult
                          , viewBoolInput
                                language
                                form.immediateResult
                                (configInitial.setRandomBloodSugarTestFormBoolInputMsg
                                    (\value form_ ->
                                        { form_
                                            | immediateResult = Just value
                                            , sugarCount = Nothing
                                            , sugarCountDirty = True
                                        }
                                    )
                                )
                                "immediate-result"
                                (Just ( Translate.PointOfCare, Translate.Lab ))
                          , viewQuestionLabel language <| Translate.TestPrerequisiteQuestion PrerequisiteFastFor12h
                          , viewBoolInput
                                language
                                form.patientFasted
                                (configInitial.setRandomBloodSugarTestFormBoolInputMsg
                                    (\value form_ -> { form_ | patientFasted = Just value })
                                )
                                "patient-fasted"
                                Nothing
                          ]
                        , taskCompleted form.patientFasted + taskCompleted form.immediateResult
                        , 2
                        )

                    ( testResultSection, testResultTasksCompleted, testResultTasksTotal ) =
                        if isNothing form.executionDate then
                            emptySection

                        else if form.immediateResult == Just True then
                            randomBloodSugarResultInputAndTask language
                                configPerformed.setRandomBloodSugarResultMsg
                                form.sugarCount

                        else
                            ( [ viewCustomLabel language Translate.LaboratoryTaskResultsHelper "." "label" ]
                            , 0
                            , 0
                            )
                in
                ( testPrerequisitesSection ++ performedTestSection ++ testResultSection
                , performedTestTasksCompleted + testPrerequisitesTasksCompleted + testResultTasksCompleted
                , performedTestTasksTotal + testPrerequisitesTasksTotal + testResultTasksTotal
                )

            else
                emptySection

        emptySection =
            ( [], 0, 0 )
    in
    ( div [ class "ui form laboratory urine-dipstick" ] <|
        [ viewCustomLabel language (Translate.LaboratoryTaskLabel TaskRandomBloodSugarTest) "" "label header"
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
        [ viewCustomLabel language (Translate.LaboratoryTaskLabel task) "" "label header"
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
        [ viewCustomLabel language (Translate.LaboratoryTaskLabel task) "" "label header"
        ]
            ++ inputs
    , tasksCompleted
    , tasksTotal
    )


viewHbA1cTestForm :
    Language
    -> NominalDate
    -> ContentAndTasksLaboratoryTestInitialConfig msg
    -> ContentAndTasksForPerformedLaboratoryTestConfig msg
    -> List NominalDate
    -> HbA1cTestForm msg
    -> ( Html msg, Int, Int )
viewHbA1cTestForm language currentDate configInitial configPerformed previousTestsDates form =
    let
        ( initialSection, initialTasksCompleted, initialTasksTotal ) =
            if List.isEmpty previousTestsDates then
                let
                    updateFunc =
                        \gotResultsPreviously form_ ->
                            let
                                executionNote =
                                    if not gotResultsPreviously then
                                        Just TestNoteToBeDoneAtHospital

                                    else
                                        Nothing
                            in
                            { form_
                                | gotResultsPreviously = Just gotResultsPreviously
                                , executionNote = executionNote
                                , executionNoteDirty = True
                                , executionDate = Nothing
                                , executionDateDirty = True
                                , hba1cResult = Nothing
                                , hba1cResultDirty = True
                            }

                    referral =
                        if form.gotResultsPreviously == Just False then
                            referralMessage

                        else
                            []
                in
                ( [ viewQuestionLabel language Translate.GotResultsPreviouslyQuestion
                  , viewBoolInput
                        language
                        form.gotResultsPreviously
                        (configPerformed.setHbA1cTestFormBoolInputMsg updateFunc)
                        "got-results-previously"
                        Nothing
                  ]
                    ++ referral
                , taskCompleted form.gotResultsPreviously
                , 1
                )

            else
                emptySection

        ( derivedSection, derivedTasksCompleted, derivedTasksTotal ) =
            if (not <| List.isEmpty previousTestsDates) || form.gotResultsPreviously == Just True then
                let
                    ( executionDateSection, executionDateTasksCompleted, executionDateTasksTotal ) =
                        let
                            executionDateForView =
                                Maybe.map formatDDMMYYYY form.executionDate
                                    |> Maybe.withDefault ""

                            dateSelectorConfig =
                                let
                                    dateTo =
                                        Date.add Days -1 currentDate
                                in
                                { select = configPerformed.setHbA1cTestExecutionDateMsg
                                , close = configPerformed.setHbA1cTestDateSelectorStateMsg Nothing
                                , dateFrom = Date.add Years -5 currentDate
                                , dateTo = dateTo
                                , dateDefault = Just dateTo
                                }
                        in
                        ( [ viewCustomLabel language Translate.HbA1cMostRecentTestResultInstruction "." "label instruction"
                          , viewLabel language <| Translate.LaboratoryTaskDate TaskHbA1cTest
                          , div
                                [ class "form-input date"
                                , onClick <| configPerformed.setHbA1cTestDateSelectorStateMsg (Just dateSelectorConfig)
                                ]
                                [ text executionDateForView
                                ]
                          , viewModal <| viewCalendarPopup language form.dateSelectorPopupState form.executionDate
                          ]
                        , taskCompleted form.executionDate
                        , 1
                        )

                    ( testResultSection, testResultTasksCompleted, testResultTasksTotal ) =
                        if isNothing form.executionDate then
                            emptySection

                        else
                            ( [ viewLabel language <| Translate.LaboratoryTaskResult TaskHbA1cTest
                              , viewMeasurementInput language
                                    form.hba1cResult
                                    configPerformed.setHbA1cTestResultMsg
                                    "hba1c-result"
                                    Translate.Percentage
                              ]
                            , taskCompleted form.hba1cResult
                            , 1
                            )

                    referral =
                        Maybe.map2
                            (\executionDate _ ->
                                if Date.diff Months executionDate currentDate >= 6 then
                                    referralMessage

                                else
                                    []
                            )
                            form.executionDate
                            form.hba1cResult
                            |> Maybe.withDefault []
                in
                ( executionDateSection ++ testResultSection ++ referral
                , executionDateTasksCompleted + testResultTasksCompleted
                , executionDateTasksTotal + testResultTasksTotal
                )

            else
                emptySection

        referralMessage =
            [ viewCustomLabel language Translate.ReferToHospitalForTesting "." "label referral" ]

        emptySection =
            ( [], 0, 0 )
    in
    ( div [ class "ui form laboratory hba1c" ] <|
        [ viewCustomLabel language (Translate.LaboratoryTaskLabel TaskHbA1cTest) "" "label header" ]
            ++ initialSection
            ++ derivedSection
    , initialTasksCompleted + derivedTasksCompleted
    , initialTasksTotal + derivedTasksTotal
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
                            [ viewCustomLabel language Translate.LaboratoryTaskResultsHelper "." "label" ]
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
                    let
                        updateFunc =
                            \value form_ ->
                                { form_
                                    | testPerformed = Just value
                                    , testPerformedDirty = True
                                    , testPerformedToday = Nothing
                                    , testPerformedTodayDirty = True
                                    , patientFasted = Nothing
                                    , immediateResult = Nothing
                                    , executionNote = Nothing
                                    , executionNoteDirty = True
                                    , executionDate = Nothing
                                    , executionDateDirty = True
                                    , sugarCount = Nothing
                                    , sugarCountDirty = True
                                }
                    in
                    { setBoolInputMsg = config.setRandomBloodSugarTestFormBoolInputMsg updateFunc
                    , setExecutionNoteMsg = config.setRandomBloodSugarTestExecutionNoteMsg
                    }

                TaskHIVPCRTest ->
                    { setBoolInputMsg = config.setHIVPCRTestFormBoolInputMsg boolInputUpdateFunc
                    , setExecutionNoteMsg = config.setHIVPCRTestExecutionNoteMsg
                    }

                TaskPregnancyTest ->
                    { setBoolInputMsg = config.setPregnancyTestFormBoolInputMsg boolInputUpdateFunc
                    , setExecutionNoteMsg = config.setPregnancyTestExecutionNoteMsg
                    }

                TaskCreatinineTest ->
                    { setBoolInputMsg = config.setCreatinineTestFormBoolInputMsg boolInputUpdateFunc
                    , setExecutionNoteMsg = config.setCreatinineTestExecutionNoteMsg
                    }

                TaskLiverFunctionTest ->
                    { setBoolInputMsg = config.setLiverFunctionTestFormBoolInputMsg boolInputUpdateFunc
                    , setExecutionNoteMsg = config.setLiverFunctionTestExecutionNoteMsg
                    }

                TaskLipidPanelTest ->
                    { setBoolInputMsg = config.setLipidPanelTestFormBoolInputMsg boolInputUpdateFunc
                    , setExecutionNoteMsg = config.setLipidPanelTestExecutionNoteMsg
                    }

                TaskHbA1cTest ->
                    -- Not in use, as this task got a proprietary form.
                    { setBoolInputMsg = always config.noOpMsg
                    , setExecutionNoteMsg = always config.noOpMsg
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

                    TaskPregnancyTest ->
                        { setBoolInputMsg = config.setPregnancyTestFormBoolInputMsg boolInputUpdateFunc
                        , setExecutionDateMsg = config.setPregnancyTestExecutionDateMsg
                        , setDateSelectorStateMsg = config.setPregnancyTestDateSelectorStateMsg
                        }

                    TaskCreatinineTest ->
                        { setBoolInputMsg = config.setCreatinineTestFormBoolInputMsg boolInputUpdateFunc
                        , setExecutionDateMsg = config.setCreatinineTestExecutionDateMsg
                        , setDateSelectorStateMsg = config.setCreatinineTestDateSelectorStateMsg
                        }

                    TaskLiverFunctionTest ->
                        { setBoolInputMsg = config.setLiverFunctionTestFormBoolInputMsg boolInputUpdateFunc
                        , setExecutionDateMsg = config.setLiverFunctionTestExecutionDateMsg
                        , setDateSelectorStateMsg = config.setLiverFunctionTestDateSelectorStateMsg
                        }

                    TaskLipidPanelTest ->
                        { setBoolInputMsg = config.setLipidPanelTestFormBoolInputMsg boolInputUpdateFunc
                        , setExecutionDateMsg = config.setLipidPanelTestExecutionDateMsg
                        , setDateSelectorStateMsg = config.setLipidPanelTestDateSelectorStateMsg
                        }

                    TaskHbA1cTest ->
                        -- Not in use, as this task got a proprietary form.
                        { setBoolInputMsg = always config.noOpMsg
                        , setExecutionDateMsg = always config.noOpMsg
                        , setDateSelectorStateMsg = always config.noOpMsg
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
                        ( (viewLabel language <| Translate.LaboratoryTaskDate task)
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

                TaskPregnancyTest ->
                    config.setPregnancyTestFormBoolInputMsg updateFunc

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


emptyContentAndTasksLaboratoryTestInitialConfig : msg -> ContentAndTasksLaboratoryTestInitialConfig msg
emptyContentAndTasksLaboratoryTestInitialConfig noOpMsg =
    { setHIVTestFormBoolInputMsg = \_ _ -> noOpMsg
    , setHIVTestExecutionNoteMsg = always noOpMsg
    , setHIVTestResultMsg = always noOpMsg
    , setSyphilisTestFormBoolInputMsg = \_ _ -> noOpMsg
    , setSyphilisTestExecutionNoteMsg = always noOpMsg
    , setHepatitisBTestFormBoolInputMsg = \_ _ -> noOpMsg
    , setHepatitisBTestExecutionNoteMsg = always noOpMsg
    , setMalariaTestFormBoolInputMsg = \_ _ -> noOpMsg
    , setMalariaTestExecutionNoteMsg = always noOpMsg
    , setMalariaTestResultMsg = always noOpMsg
    , setBloodGpRsTestFormBoolInputMsg = \_ _ -> noOpMsg
    , setBloodGpRsTestExecutionNoteMsg = always noOpMsg
    , setUrineDipstickTestFormBoolInputMsg = \_ _ -> noOpMsg
    , setUrineDipstickTestExecutionNoteMsg = always noOpMsg
    , setUrineDipstickTestVariantMsg = always noOpMsg
    , setHemoglobinTestFormBoolInputMsg = \_ _ -> noOpMsg
    , setHemoglobinTestExecutionNoteMsg = always noOpMsg
    , setRandomBloodSugarTestFormBoolInputMsg = \_ _ -> noOpMsg
    , setRandomBloodSugarTestExecutionNoteMsg = always noOpMsg
    , setHIVPCRTestFormBoolInputMsg = \_ _ -> noOpMsg
    , setHIVPCRTestExecutionNoteMsg = always noOpMsg
    , setPregnancyTestFormBoolInputMsg = \_ _ -> noOpMsg
    , setPregnancyTestExecutionNoteMsg = always noOpMsg
    , setPregnancyTestResultMsg = always noOpMsg
    , setCreatinineTestFormBoolInputMsg = \_ _ -> noOpMsg
    , setCreatinineTestExecutionNoteMsg = always noOpMsg
    , setLiverFunctionTestFormBoolInputMsg = \_ _ -> noOpMsg
    , setLiverFunctionTestExecutionNoteMsg = always noOpMsg
    , setLipidPanelTestFormBoolInputMsg = \_ _ -> noOpMsg
    , setLipidPanelTestExecutionNoteMsg = always noOpMsg
    , setHbA1cTestFormBoolInputMsg = \_ _ -> noOpMsg
    , setHbA1cTestExecutionNoteMsg = always noOpMsg
    , noOpMsg = noOpMsg
    }


emptyContentAndTasksForPerformedLaboratoryTestConfig : msg -> ContentAndTasksForPerformedLaboratoryTestConfig msg
emptyContentAndTasksForPerformedLaboratoryTestConfig noOpMsg =
    { setHIVTestFormBoolInputMsg = \_ _ -> noOpMsg
    , setHIVTestExecutionDateMsg = always noOpMsg
    , setHIVTestDateSelectorStateMsg = always noOpMsg
    , setSyphilisTestFormBoolInputMsg = \_ _ -> noOpMsg
    , setSyphilisTestExecutionDateMsg = always noOpMsg
    , setSyphilisTestDateSelectorStateMsg = always noOpMsg
    , setHepatitisBTestFormBoolInputMsg = \_ _ -> noOpMsg
    , setHepatitisBTestExecutionDateMsg = always noOpMsg
    , setHepatitisBTestDateSelectorStateMsg = always noOpMsg
    , setMalariaTestFormBoolInputMsg = \_ _ -> noOpMsg
    , setMalariaTestExecutionDateMsg = always noOpMsg
    , setMalariaTestDateSelectorStateMsg = always noOpMsg
    , setBloodGpRsTestFormBoolInputMsg = \_ _ -> noOpMsg
    , setBloodGpRsTestExecutionDateMsg = always noOpMsg
    , setBloodGpRsTestDateSelectorStateMsg = always noOpMsg
    , setUrineDipstickTestFormBoolInputMsg = \_ _ -> noOpMsg
    , setUrineDipstickTestExecutionDateMsg = always noOpMsg
    , setUrineDipstickTestDateSelectorStateMsg = always noOpMsg
    , setHemoglobinTestFormBoolInputMsg = \_ _ -> noOpMsg
    , setHemoglobinTestExecutionDateMsg = always noOpMsg
    , setHemoglobinTestDateSelectorStateMsg = always noOpMsg
    , setRandomBloodSugarTestFormBoolInputMsg = \_ _ -> noOpMsg
    , setRandomBloodSugarTestExecutionDateMsg = always noOpMsg
    , setRandomBloodSugarTestDateSelectorStateMsg = always noOpMsg
    , setRandomBloodSugarResultMsg = always noOpMsg
    , setHIVPCRTestFormBoolInputMsg = \_ _ -> noOpMsg
    , setHIVPCRTestExecutionDateMsg = always noOpMsg
    , setHIVPCRTestDateSelectorStateMsg = always noOpMsg
    , setPregnancyTestFormBoolInputMsg = \_ _ -> noOpMsg
    , setPregnancyTestExecutionDateMsg = always noOpMsg
    , setPregnancyTestDateSelectorStateMsg = always noOpMsg
    , setCreatinineTestFormBoolInputMsg = \_ _ -> noOpMsg
    , setCreatinineTestExecutionDateMsg = always noOpMsg
    , setCreatinineTestDateSelectorStateMsg = always noOpMsg
    , setLiverFunctionTestFormBoolInputMsg = \_ _ -> noOpMsg
    , setLiverFunctionTestExecutionDateMsg = always noOpMsg
    , setLiverFunctionTestDateSelectorStateMsg = always noOpMsg
    , setLipidPanelTestFormBoolInputMsg = \_ _ -> noOpMsg
    , setLipidPanelTestExecutionDateMsg = always noOpMsg
    , setLipidPanelTestDateSelectorStateMsg = always noOpMsg
    , setHbA1cTestFormBoolInputMsg = \_ _ -> noOpMsg
    , setHbA1cTestExecutionDateMsg = always noOpMsg
    , setHbA1cTestDateSelectorStateMsg = always noOpMsg
    , setHbA1cTestResultMsg = always noOpMsg
    , noOpMsg = noOpMsg
    }


laboratoryTaskIconClass : LaboratoryTask -> String
laboratoryTaskIconClass task =
    case task of
        TaskHIVTest ->
            "laboratory-hiv"

        TaskSyphilisTest ->
            "laboratory-syphilis"

        TaskHepatitisBTest ->
            "laboratory-hepatitis-b"

        TaskMalariaTest ->
            "laboratory-malaria-testing"

        TaskBloodGpRsTest ->
            "laboratory-blood-group"

        TaskUrineDipstickTest ->
            "laboratory-urine-dipstick"

        TaskHemoglobinTest ->
            "laboratory-hemoglobin"

        TaskRandomBloodSugarTest ->
            "laboratory-blood-sugar"

        TaskHIVPCRTest ->
            "laboratory-hiv"

        TaskCompletePreviousTests ->
            "laboratory-history"

        TaskPregnancyTest ->
            "pregnancy"

        TaskCreatinineTest ->
            "creatinine"

        TaskLiverFunctionTest ->
            "liver-function"

        TaskLipidPanelTest ->
            "lipid-panel"

        TaskHbA1cTest ->
            "hba1c"


hepatitisBResultFormWithDefault : HepatitisBResultForm encounterId -> Maybe (HepatitisBTestValue encounterId) -> HepatitisBResultForm encounterId
hepatitisBResultFormWithDefault form saved =
    saved
        |> unwrap
            form
            (\value ->
                { executionNote = or form.executionNote (Just value.executionNote)
                , executionDate = or form.executionDate value.executionDate
                , testResult = or form.testResult value.testResult
                , originatingEncounter = or form.originatingEncounter value.originatingEncounter
                }
            )


toHepatitisBResultsValueWithDefault : Maybe (HepatitisBTestValue encounterId) -> HepatitisBResultForm encounterId -> Maybe (HepatitisBTestValue encounterId)
toHepatitisBResultsValueWithDefault saved form =
    hepatitisBResultFormWithDefault form saved
        |> toHepatitisBResultsValue


toHepatitisBResultsValue : HepatitisBResultForm encounterId -> Maybe (HepatitisBTestValue encounterId)
toHepatitisBResultsValue form =
    Maybe.map
        (\executionNote ->
            { executionNote = executionNote
            , executionDate = form.executionDate
            , testResult = form.testResult
            , originatingEncounter = form.originatingEncounter
            }
        )
        form.executionNote


syphilisResultFormWithDefault : SyphilisResultForm encounterId -> Maybe (SyphilisTestValue encounterId) -> SyphilisResultForm encounterId
syphilisResultFormWithDefault form saved =
    saved
        |> unwrap
            form
            (\value ->
                { executionNote = or form.executionNote (Just value.executionNote)
                , executionDate = or form.executionDate value.executionDate
                , testResult = or form.testResult value.testResult
                , symptoms = maybeValueConsideringIsDirtyField form.symptomsDirty form.symptoms (Maybe.map EverySet.toList value.symptoms)
                , symptomsDirty = form.symptomsDirty
                , originatingEncounter = or form.originatingEncounter value.originatingEncounter
                }
            )


toSyphilisResultValueWithDefault : Maybe (SyphilisTestValue encounterId) -> SyphilisResultForm encounterId -> Maybe (SyphilisTestValue encounterId)
toSyphilisResultValueWithDefault saved form =
    syphilisResultFormWithDefault form saved
        |> toSyphilisResultValue


toSyphilisResultValue : SyphilisResultForm encounterId -> Maybe (SyphilisTestValue encounterId)
toSyphilisResultValue form =
    Maybe.map
        (\executionNote ->
            { executionNote = executionNote
            , executionDate = form.executionDate
            , testResult = form.testResult
            , symptoms = Maybe.map EverySet.fromList form.symptoms
            , originatingEncounter = form.originatingEncounter
            }
        )
        form.executionNote


bloodGpRsResultFormWithDefault : BloodGpRsResultForm encounterId -> Maybe (BloodGpRsTestValue encounterId) -> BloodGpRsResultForm encounterId
bloodGpRsResultFormWithDefault form saved =
    saved
        |> unwrap
            form
            (\value ->
                { executionNote = or form.executionNote (Just value.executionNote)
                , executionDate = or form.executionDate value.executionDate
                , bloodGroup = or form.bloodGroup value.bloodGroup
                , rhesus = or form.rhesus value.rhesus
                , originatingEncounter = or form.originatingEncounter value.originatingEncounter
                }
            )


toBloodGpRsResultsValueWithDefault : Maybe (BloodGpRsTestValue encounterId) -> BloodGpRsResultForm encounterId -> Maybe (BloodGpRsTestValue encounterId)
toBloodGpRsResultsValueWithDefault saved form =
    bloodGpRsResultFormWithDefault form saved
        |> toBloodGpRsResultsValue


toBloodGpRsResultsValue : BloodGpRsResultForm encounterId -> Maybe (BloodGpRsTestValue encounterId)
toBloodGpRsResultsValue form =
    Maybe.map
        (\executionNote ->
            { executionNote = executionNote
            , executionDate = form.executionDate
            , bloodGroup = form.bloodGroup
            , rhesus = form.rhesus
            , originatingEncounter = form.originatingEncounter
            }
        )
        form.executionNote


hemoglobinResultFormWithDefault : HemoglobinResultForm -> Maybe HemoglobinTestValue -> HemoglobinResultForm
hemoglobinResultFormWithDefault form saved =
    saved
        |> unwrap
            form
            (\value ->
                { executionNote = or form.executionNote (Just value.executionNote)
                , executionDate = or form.executionDate value.executionDate
                , hemoglobinCount = or form.hemoglobinCount value.hemoglobinCount
                }
            )


toHemoglobinResultsValueWithDefault : Maybe HemoglobinTestValue -> HemoglobinResultForm -> Maybe HemoglobinTestValue
toHemoglobinResultsValueWithDefault saved form =
    hemoglobinResultFormWithDefault form saved
        |> toHemoglobinResultsValue


toHemoglobinResultsValue : HemoglobinResultForm -> Maybe HemoglobinTestValue
toHemoglobinResultsValue form =
    Maybe.map
        (\executionNote ->
            { executionNote = executionNote
            , executionDate = form.executionDate
            , hemoglobinCount = form.hemoglobinCount
            }
        )
        form.executionNote


randomBloodSugarResultFormWithDefault : RandomBloodSugarResultForm encounterId -> Maybe (RandomBloodSugarTestValue encounterId) -> RandomBloodSugarResultForm encounterId
randomBloodSugarResultFormWithDefault form saved =
    saved
        |> unwrap
            form
            (\value ->
                { executionNote = or form.executionNote (Just value.executionNote)
                , executionDate = or form.executionDate value.executionDate
                , testPrerequisites = or form.testPrerequisites value.testPrerequisites
                , sugarCount = or form.sugarCount value.sugarCount
                , originatingEncounter = or form.originatingEncounter value.originatingEncounter
                }
            )


toRandomBloodSugarResultsValueWithDefault : Maybe (RandomBloodSugarTestValue encounterId) -> RandomBloodSugarResultForm encounterId -> Maybe (RandomBloodSugarTestValue encounterId)
toRandomBloodSugarResultsValueWithDefault saved form =
    randomBloodSugarResultFormWithDefault form saved
        |> toRandomBloodSugarResultsValue


toRandomBloodSugarResultsValue : RandomBloodSugarResultForm encounterId -> Maybe (RandomBloodSugarTestValue encounterId)
toRandomBloodSugarResultsValue form =
    Maybe.map
        (\executionNote ->
            { executionNote = executionNote
            , executionDate = form.executionDate
            , testPrerequisites = form.testPrerequisites
            , sugarCount = form.sugarCount
            , originatingEncounter = form.originatingEncounter
            }
        )
        form.executionNote


urineDipstickResultFormWithDefault : UrineDipstickResultForm -> Maybe UrineDipstickTestValue -> UrineDipstickResultForm
urineDipstickResultFormWithDefault form saved =
    saved
        |> unwrap
            form
            (\value ->
                { testVariant = or form.testVariant value.testVariant
                , executionNote = or form.executionNote (Just value.executionNote)
                , executionDate = or form.executionDate value.executionDate
                , protein = or form.protein value.protein
                , ph = or form.ph value.ph
                , glucose = or form.glucose value.glucose
                , leukocytes = or form.leukocytes value.leukocytes
                , nitrite = or form.nitrite value.nitrite
                , urobilinogen = or form.urobilinogen value.urobilinogen
                , haemoglobin = or form.haemoglobin value.haemoglobin
                , ketone = or form.ketone value.ketone
                , bilirubin = or form.bilirubin value.bilirubin
                }
            )


toUrineDipstickResultsValueWithDefault : Maybe UrineDipstickTestValue -> UrineDipstickResultForm -> Maybe UrineDipstickTestValue
toUrineDipstickResultsValueWithDefault saved form =
    urineDipstickResultFormWithDefault form saved
        |> toUrineDipstickResultsValue


toUrineDipstickResultsValue : UrineDipstickResultForm -> Maybe UrineDipstickTestValue
toUrineDipstickResultsValue form =
    Maybe.map
        (\executionNote ->
            { testVariant = form.testVariant
            , executionNote = executionNote
            , executionDate = form.executionDate
            , protein = form.protein
            , ph = form.ph
            , glucose = form.glucose
            , leukocytes = form.leukocytes
            , nitrite = form.nitrite
            , urobilinogen = form.urobilinogen
            , haemoglobin = form.haemoglobin
            , ketone = form.ketone
            , bilirubin = form.bilirubin
            }
        )
        form.executionNote


hivPCRResultFormWithDefault : HIVPCRResultForm -> Maybe HIVPCRTestValue -> HIVPCRResultForm
hivPCRResultFormWithDefault form saved =
    saved
        |> unwrap
            form
            (\value ->
                { executionNote = or form.executionNote (Just value.executionNote)
                , executionDate = or form.executionDate value.executionDate
                , hivViralLoadStatus = or form.hivViralLoadStatus value.hivViralLoadStatus
                , hivViralLoad = or form.hivViralLoad value.hivViralLoad
                }
            )


toHIVPCRRResultsValueWithDefault : Maybe HIVPCRTestValue -> HIVPCRResultForm -> Maybe HIVPCRTestValue
toHIVPCRRResultsValueWithDefault saved form =
    hivPCRResultFormWithDefault form saved
        |> toHIVPCRRResultsValue


toHIVPCRRResultsValue : HIVPCRResultForm -> Maybe HIVPCRTestValue
toHIVPCRRResultsValue form =
    Maybe.map
        (\executionNote ->
            { executionNote = executionNote
            , executionDate = form.executionDate
            , hivViralLoadStatus = form.hivViralLoadStatus
            , hivViralLoad = form.hivViralLoad
            }
        )
        form.executionNote


creatinineResultFormWithDefault : CreatinineResultForm -> Maybe CreatinineTestValue -> CreatinineResultForm
creatinineResultFormWithDefault form saved =
    saved
        |> unwrap
            form
            (\value ->
                { executionNote = or form.executionNote (Just value.executionNote)
                , executionDate = or form.executionDate value.executionDate
                , creatinineResult = or form.creatinineResult value.creatinineResult
                , bunResult = or form.bunResult value.bunResult
                }
            )


toCreatinineResultsValueWithDefault : Maybe CreatinineTestValue -> CreatinineResultForm -> Maybe CreatinineTestValue
toCreatinineResultsValueWithDefault saved form =
    creatinineResultFormWithDefault form saved
        |> toCreatinineResultsValue


toCreatinineResultsValue : CreatinineResultForm -> Maybe CreatinineTestValue
toCreatinineResultsValue form =
    Maybe.map
        (\executionNote ->
            { executionNote = executionNote
            , executionDate = form.executionDate
            , creatinineResult = form.creatinineResult
            , bunResult = form.bunResult
            }
        )
        form.executionNote


liverFunctionResultFormWithDefault : LiverFunctionResultForm -> Maybe LiverFunctionTestValue -> LiverFunctionResultForm
liverFunctionResultFormWithDefault form saved =
    saved
        |> unwrap
            form
            (\value ->
                { executionNote = or form.executionNote (Just value.executionNote)
                , executionDate = or form.executionDate value.executionDate
                , altResult = or form.altResult value.altResult
                , astResult = or form.astResult value.astResult
                }
            )


toLiverFunctionResultsValueWithDefault : Maybe LiverFunctionTestValue -> LiverFunctionResultForm -> Maybe LiverFunctionTestValue
toLiverFunctionResultsValueWithDefault saved form =
    liverFunctionResultFormWithDefault form saved
        |> toLiverFunctionResultsValue


toLiverFunctionResultsValue : LiverFunctionResultForm -> Maybe LiverFunctionTestValue
toLiverFunctionResultsValue form =
    Maybe.map
        (\executionNote ->
            { executionNote = executionNote
            , executionDate = form.executionDate
            , altResult = form.altResult
            , astResult = form.astResult
            }
        )
        form.executionNote


lipidPanelResultFormWithDefault : LipidPanelResultForm -> Maybe LipidPanelTestValue -> LipidPanelResultForm
lipidPanelResultFormWithDefault form saved =
    saved
        |> unwrap
            form
            (\value ->
                let
                    unitOfMeasurement =
                        or form.unitOfMeasurement value.unitOfMeasurement

                    resultByValue ratio maybeResult =
                        Maybe.map2
                            (\result unit ->
                                case unit of
                                    UnitMmolL ->
                                        Round.roundNum 2 (result / ratio)

                                    UnitMgdL ->
                                        Round.roundNum 0 result
                            )
                            maybeResult
                            unitOfMeasurement
                in
                { executionNote = or form.executionNote (Just value.executionNote)
                , executionDate = or form.executionDate value.executionDate
                , unitOfMeasurement = unitOfMeasurement
                , totalCholesterolResult =
                    maybeValueConsideringIsDirtyField form.totalCholesterolResultDirty
                        form.totalCholesterolResult
                        (resultByValue cholesterolMmolLToMgdLRatio value.totalCholesterolResult)
                , totalCholesterolResultDirty = form.totalCholesterolResultDirty
                , ldlCholesterolResult =
                    maybeValueConsideringIsDirtyField form.ldlCholesterolResultDirty
                        form.ldlCholesterolResult
                        (resultByValue cholesterolMmolLToMgdLRatio value.ldlCholesterolResult)
                , ldlCholesterolResultDirty = form.ldlCholesterolResultDirty
                , hdlCholesterolResult =
                    maybeValueConsideringIsDirtyField form.hdlCholesterolResultDirty
                        form.hdlCholesterolResult
                        (resultByValue cholesterolMmolLToMgdLRatio value.hdlCholesterolResult)
                , hdlCholesterolResultDirty = form.hdlCholesterolResultDirty
                , triglyceridesResult =
                    maybeValueConsideringIsDirtyField form.triglyceridesResultDirty
                        form.triglyceridesResult
                        (resultByValue triglyceridesMmolLToMgdLRatio value.triglyceridesResult)
                , triglyceridesResultDirty = form.triglyceridesResultDirty
                }
            )


toLipidPanelResultsValueWithDefault : Maybe LipidPanelTestValue -> LipidPanelResultForm -> Maybe LipidPanelTestValue
toLipidPanelResultsValueWithDefault saved form =
    lipidPanelResultFormWithDefault form saved
        |> toLipidPanelResultsValue


toLipidPanelResultsValue : LipidPanelResultForm -> Maybe LipidPanelTestValue
toLipidPanelResultsValue form =
    Maybe.map
        (\executionNote ->
            let
                resultForBackend ratio maybeResult =
                    Maybe.map2
                        (\unitOfMeasurement result ->
                            case unitOfMeasurement of
                                UnitMmolL ->
                                    Round.roundNum 0 (result * ratio)

                                UnitMgdL ->
                                    result
                        )
                        form.unitOfMeasurement
                        maybeResult
            in
            { executionNote = executionNote
            , executionDate = form.executionDate
            , unitOfMeasurement = form.unitOfMeasurement
            , totalCholesterolResult = resultForBackend cholesterolMmolLToMgdLRatio form.totalCholesterolResult
            , ldlCholesterolResult = resultForBackend cholesterolMmolLToMgdLRatio form.ldlCholesterolResult
            , hdlCholesterolResult = resultForBackend cholesterolMmolLToMgdLRatio form.hdlCholesterolResult
            , triglyceridesResult = resultForBackend triglyceridesMmolLToMgdLRatio form.triglyceridesResult
            }
        )
        form.executionNote


cholesterolMmolLToMgdLRatio : Float
cholesterolMmolLToMgdLRatio =
    38.67


triglyceridesMmolLToMgdLRatio : Float
triglyceridesMmolLToMgdLRatio =
    88.57


hba1cTestFormWithDefault : HbA1cTestForm msg -> Maybe HbA1cTestValue -> HbA1cTestForm msg
hba1cTestFormWithDefault form saved =
    saved
        |> unwrap
            form
            (\value ->
                let
                    gotResultsPreviouslyByValue =
                        isJust value.executionDate
                in
                { gotResultsPreviously = or form.gotResultsPreviously (Just gotResultsPreviouslyByValue)
                , executionNote = valueConsideringIsDirtyField form.executionNoteDirty form.executionNote value.executionNote
                , executionNoteDirty = form.executionNoteDirty
                , executionDate = maybeValueConsideringIsDirtyField form.executionDateDirty form.executionDate value.executionDate
                , executionDateDirty = form.executionDateDirty
                , dateSelectorPopupState = form.dateSelectorPopupState
                , hba1cResult = maybeValueConsideringIsDirtyField form.hba1cResultDirty form.hba1cResult value.hba1cResult
                , hba1cResultDirty = form.hba1cResultDirty
                }
            )


toHbA1cTestValueWithDefault : Maybe HbA1cTestValue -> HbA1cTestForm msg -> Maybe HbA1cTestValue
toHbA1cTestValueWithDefault saved form =
    hba1cTestFormWithDefault form saved
        |> toHbA1cTestValue


toHbA1cTestValue : HbA1cTestForm msg -> Maybe HbA1cTestValue
toHbA1cTestValue form =
    Maybe.map
        (\executionNote ->
            { executionNote = executionNote
            , executionDate = form.executionDate
            , hba1cResult = form.hba1cResult
            }
        )
        form.executionNote


syphilisResultFormAndTasks :
    Language
    -> NominalDate
    -> (IllnessSymptom -> msg)
    -> (String -> msg)
    -> SyphilisResultForm encounterId
    -> ( Html msg, Int, Int )
syphilisResultFormAndTasks language currentDate setIllnessSymptomMsg setSyphilisTestResultMsg form =
    let
        ( testResultSection, testResultTasksCompleted, testResultTasksTotal ) =
            let
                ( symptomsSection, symptomsTasksCompleted, symptomsTasksTotal ) =
                    if form.testResult == Just TestPositive then
                        ( [ viewLabel language Translate.SelectIllnessSymptoms
                          , viewCheckBoxMultipleSelectInput language
                                [ IllnessSymptomHeadache
                                , IllnessSymptomVisionChanges
                                , IllnessSymptomRash
                                , IllnessSymptomPainlessUlcerMouth
                                , IllnessSymptomPainlessUlcerGenitals
                                ]
                                []
                                (form.symptoms |> Maybe.withDefault [])
                                (Just NoIllnessSymptoms)
                                setIllnessSymptomMsg
                                Translate.IllnessSymptom
                          ]
                        , taskCompleted form.symptoms
                        , 1
                        )

                    else
                        ( [], 0, 0 )
            in
            ( viewSelectInput language
                (Translate.LaboratoryTaskResult TaskSyphilisTest)
                form.testResult
                Translate.TestResult
                testResultToString
                [ TestPositive, TestNegative, TestIndeterminate ]
                setSyphilisTestResultMsg
                ++ symptomsSection
            , taskCompleted form.testResult + symptomsTasksCompleted
            , 1 + symptomsTasksTotal
            )
    in
    ( div [ class "ui form laboratory prenatal-test-result" ] <|
        resultFormHeaderSection language currentDate form.executionDate TaskSyphilisTest
            ++ testResultSection
    , testResultTasksCompleted
    , testResultTasksTotal
    )


hepatitisBResultFormAndTasks :
    Language
    -> NominalDate
    -> (String -> msg)
    -> HepatitisBResultForm encounterId
    -> ( Html msg, Int, Int )
hepatitisBResultFormAndTasks language currentDate setHepatitisBTestResultMsg form =
    let
        ( testResultSection, testResultTasksCompleted, testResultTasksTotal ) =
            let
                emptyOption =
                    if isNothing form.testResult then
                        emptySelectOption True

                    else
                        emptyNode
            in
            ( viewSelectInput language
                (Translate.LaboratoryTaskResult TaskHepatitisBTest)
                form.testResult
                Translate.TestResult
                testResultToString
                [ TestPositive, TestNegative, TestIndeterminate ]
                setHepatitisBTestResultMsg
            , taskCompleted form.testResult
            , 1
            )
    in
    ( div [ class "ui form laboratory prenatal-test-result" ] <|
        resultFormHeaderSection language currentDate form.executionDate TaskHepatitisBTest
            ++ testResultSection
    , testResultTasksCompleted
    , testResultTasksTotal
    )


bloodGpRsResultFormAndTasks :
    Language
    -> NominalDate
    -> (String -> msg)
    -> (String -> msg)
    -> BloodGpRsResultForm encounterId
    -> ( Html msg, Int, Int )
bloodGpRsResultFormAndTasks language currentDate setBloodGroupMsg setRhesusMsg form =
    let
        ( testResultSection, testResultTasksCompleted, testResultTasksTotal ) =
            ( viewSelectInput language
                Translate.LaboratoryBloodGroupTestResult
                form.bloodGroup
                Translate.LaboratoryBloodGroup
                bloodGroupToString
                [ BloodGroupA, BloodGroupB, BloodGroupAB, BloodGroupO ]
                setBloodGroupMsg
                ++ viewSelectInput language
                    Translate.LaboratoryRhesusTestResult
                    form.rhesus
                    Translate.LaboratoryRhesus
                    rhesusToString
                    [ RhesusPositive, RhesusNegative ]
                    setRhesusMsg
            , taskCompleted form.bloodGroup + taskCompleted form.rhesus
            , 2
            )
    in
    ( div [ class "ui form laboratory blood-group-result" ] <|
        resultFormHeaderSection language currentDate form.executionDate TaskBloodGpRsTest
            ++ testResultSection
    , testResultTasksCompleted
    , testResultTasksTotal
    )


urineDipstickResultFormAndTasks :
    Language
    -> NominalDate
    -> (String -> msg)
    -> (String -> msg)
    -> (String -> msg)
    -> (String -> msg)
    -> (String -> msg)
    -> (String -> msg)
    -> (String -> msg)
    -> (String -> msg)
    -> (String -> msg)
    -> UrineDipstickResultForm
    -> ( Html msg, Int, Int )
urineDipstickResultFormAndTasks language currentDate setProteinMsg setPHMsg setGlucoseMsg setLeukocytesMsg setNitriteMsg setUrobilinogenMsg setHaemoglobinMsg setKetoneMsg setBilirubinMsg form =
    let
        ( testResultSection, testResultTasksCompleted, testResultTasksTotal ) =
            Maybe.map
                (\testVariant ->
                    let
                        ( commonSection, commonTasksCompleted, commonTasksTotal ) =
                            ( viewSelectInput language
                                Translate.LaboratoryProteinTestResult
                                form.protein
                                Translate.LaboratoryProteinValue
                                proteinValueToString
                                [ Protein0
                                , ProteinPlus1
                                , ProteinPlus2
                                , ProteinPlus3
                                , ProteinPlus4
                                ]
                                setProteinMsg
                                ++ viewSelectInput language
                                    Translate.LaboratoryPHTestResult
                                    form.ph
                                    Translate.LaboratoryPHValue
                                    phValueToString
                                    [ Ph40
                                    , Ph45
                                    , Ph50
                                    , Ph60
                                    , Ph65
                                    , Ph70
                                    , Ph75
                                    , Ph80
                                    , Ph85
                                    ]
                                    setPHMsg
                                ++ viewSelectInput language
                                    Translate.LaboratoryGlucoseTestResult
                                    form.glucose
                                    Translate.LaboratoryGlucoseValue
                                    glucoseValueToString
                                    [ Glucose0
                                    , GlucosePlus1
                                    , GlucosePlus2
                                    , GlucosePlus3
                                    , GlucosePlus4
                                    ]
                                    setGlucoseMsg
                            , taskCompleted form.protein + taskCompleted form.ph + taskCompleted form.glucose
                            , 3
                            )
                    in
                    case testVariant of
                        VariantShortTest ->
                            ( commonSection, commonTasksCompleted, commonTasksTotal )

                        VariantLongTest ->
                            ( commonSection
                                ++ viewSelectInput language
                                    Translate.LaboratoryLeukocytesTestResult
                                    form.leukocytes
                                    Translate.LaboratoryLeukocytesValue
                                    leukocytesValueToString
                                    [ LeukocytesNegative
                                    , LeukocytesSmall
                                    , LeukocytesMedium
                                    , LeukocytesLarge
                                    ]
                                    setLeukocytesMsg
                                ++ viewSelectInput language
                                    Translate.LaboratoryNitriteTestResult
                                    form.nitrite
                                    Translate.LaboratoryNitriteValue
                                    nitriteValueToString
                                    [ NitriteNegative
                                    , NitritePlus
                                    , NitritePlusPlus
                                    ]
                                    setNitriteMsg
                                ++ viewSelectInput language
                                    Translate.LaboratoryUrobilinogenTestResult
                                    form.urobilinogen
                                    Translate.LaboratoryUrobilinogenValue
                                    urobilinogenValueToString
                                    [ Urobilinogen002
                                    , Urobilinogen10
                                    , Urobilinogen20
                                    , Urobilinogen40
                                    , Urobilinogen80
                                    ]
                                    setUrobilinogenMsg
                                ++ viewSelectInput language
                                    Translate.LaboratoryHaemoglobinTestResult
                                    form.haemoglobin
                                    Translate.LaboratoryHaemoglobinValue
                                    haemoglobinValueToString
                                    [ HaemoglobinNegative
                                    , HaemoglobinNonHemolyzedTrace
                                    , HaemoglobinNonHemolyzedModerate
                                    , HaemoglobinHemolyzedTrace
                                    , HaemoglobinSmall
                                    , HaemoglobinModerate
                                    , HaemoglobinLarge
                                    ]
                                    setHaemoglobinMsg
                                ++ viewSelectInput language
                                    Translate.LaboratoryKetoneTestResult
                                    form.ketone
                                    Translate.LaboratoryKetoneValue
                                    ketoneValueToString
                                    [ KetoneNegative
                                    , Ketone5
                                    , Ketone10
                                    , Ketone15
                                    , Ketone40
                                    , Ketone80
                                    , Ketone100
                                    ]
                                    setKetoneMsg
                                ++ viewSelectInput language
                                    Translate.LaboratoryBilirubinTestResult
                                    form.bilirubin
                                    Translate.LaboratoryBilirubinValue
                                    bilirubinValueToString
                                    [ BilirubinNegative
                                    , BilirubinSmall
                                    , BilirubinMedium
                                    , BilirubinLarge
                                    ]
                                    setBilirubinMsg
                            , commonTasksCompleted
                                + taskCompleted form.leukocytes
                                + taskCompleted form.nitrite
                                + taskCompleted form.urobilinogen
                                + taskCompleted form.haemoglobin
                                + taskCompleted form.ketone
                                + taskCompleted form.bilirubin
                            , commonTasksTotal + 6
                            )
                )
                form.testVariant
                |> Maybe.withDefault ( [], 0, 0 )
    in
    ( div [ class "ui form laboratory urine-dipstick-result" ] <|
        resultFormHeaderSection language currentDate form.executionDate TaskUrineDipstickTest
            ++ testResultSection
    , testResultTasksCompleted
    , testResultTasksTotal
    )


hemoglobinResultFormAndTasks :
    Language
    -> NominalDate
    -> (String -> msg)
    -> HemoglobinResultForm
    -> ( Html msg, Int, Int )
hemoglobinResultFormAndTasks language currentDate setHemoglobinMsg form =
    let
        ( testResultSection, testResultTasksCompleted, testResultTasksTotal ) =
            ( [ viewLabel language Translate.LaboratoryHemoglobinTestResult
              , viewMeasurementInput language
                    form.hemoglobinCount
                    setHemoglobinMsg
                    "hemoglobin-count"
                    Translate.UnitGramsPerDeciliter
              ]
            , taskCompleted form.hemoglobinCount
            , 1
            )
    in
    ( div [ class "ui form laboratory hemoglobin-result" ] <|
        resultFormHeaderSection language currentDate form.executionDate TaskHemoglobinTest
            ++ testResultSection
    , testResultTasksCompleted
    , testResultTasksTotal
    )


randomBloodSugarResultFormAndTasks :
    Language
    -> NominalDate
    -> (String -> msg)
    -> RandomBloodSugarResultForm encounterId
    -> ( Html msg, Int, Int )
randomBloodSugarResultFormAndTasks language currentDate setRandomBloodSugarMsg form =
    let
        ( testResultSection, testResultTasksCompleted, testResultTasksTotal ) =
            randomBloodSugarResultInputAndTask language setRandomBloodSugarMsg form.sugarCount
    in
    ( div [ class "ui form laboratory random-blood-sugar-result" ] <|
        resultFormHeaderSection language currentDate form.executionDate TaskRandomBloodSugarTest
            ++ testResultSection
    , testResultTasksCompleted
    , testResultTasksTotal
    )


randomBloodSugarResultInputAndTask :
    Language
    -> (String -> msg)
    -> Maybe Float
    -> ( List (Html msg), Int, Int )
randomBloodSugarResultInputAndTask language setRandomBloodSugarMsg sugarCount =
    ( [ viewLabel language Translate.LaboratoryRandomBloodSugarTestResult
      , viewMeasurementInput language
            sugarCount
            setRandomBloodSugarMsg
            "sugar-count"
            Translate.UnitMilliGramsPerDeciliter
      ]
    , taskCompleted sugarCount
    , 1
    )


hivPCRResultFormAndTasks :
    Language
    -> NominalDate
    -> (String -> msg)
    -> (Bool -> msg)
    -> HIVPCRResultForm
    -> ( Html msg, Int, Int )
hivPCRResultFormAndTasks language currentDate setHIVViralLoadMsg setHIVViralLoadUndetectableMsg form =
    let
        ( testResultSection, testResultTasksCompleted, testResultTasksTotal ) =
            let
                ( derivedSection, derivedTasksCompleted, derivedTasksTotal ) =
                    if form.hivViralLoadStatus == Just ViralLoadDetectable then
                        ( [ viewLabel language Translate.LaboratoryHIVPCRTestResult
                          , viewMeasurementInput language
                                form.hivViralLoad
                                setHIVViralLoadMsg
                                "hiv-viral-load"
                                Translate.UnitCopiesPerMM3
                          ]
                        , taskCompleted form.hivViralLoad
                        , 1
                        )

                    else
                        ( [], 0, 0 )
            in
            ( [ viewQuestionLabel language Translate.LaboratoryHIVPCRViralLoadStatusQuestion
              , viewBoolInput language
                    (Maybe.map ((==) ViralLoadUndetectable) form.hivViralLoadStatus)
                    setHIVViralLoadUndetectableMsg
                    "hiv-level-undetectable"
                    Nothing
              ]
                ++ derivedSection
            , taskCompleted form.hivViralLoadStatus + derivedTasksCompleted
            , 1 + derivedTasksTotal
            )
    in
    ( div [ class "ui form laboratory hiv-prc-result" ] <|
        resultFormHeaderSection language currentDate form.executionDate TaskHIVPCRTest
            ++ testResultSection
    , testResultTasksCompleted
    , testResultTasksTotal
    )


creatinineResultFormAndTasks :
    Language
    -> NominalDate
    -> (String -> msg)
    -> (String -> msg)
    -> CreatinineResultForm
    -> ( Html msg, Int, Int )
creatinineResultFormAndTasks language currentDate setCreatinineResultMsg setBUNResultMsg form =
    let
        ( testResultSection, testResultTasksCompleted, testResultTasksTotal ) =
            ( [ viewLabel language Translate.LaboratoryCreatinineCreatinineResult
              , viewMeasurementInput language
                    form.creatinineResult
                    setCreatinineResultMsg
                    "creatinine-result"
                    Translate.UnitMilliGramsPerDeciliter
              , viewLabel language Translate.LaboratoryCreatinineBUNResult
              , viewMeasurementInput language
                    form.bunResult
                    setBUNResultMsg
                    "bun-result"
                    Translate.UnitMilliGramsPerDeciliter
              ]
            , taskCompleted form.creatinineResult + taskCompleted form.bunResult
            , 2
            )
    in
    ( div [ class "ui form laboratory creatinine-result" ] <|
        resultFormHeaderSection language currentDate form.executionDate TaskCreatinineTest
            ++ testResultSection
    , testResultTasksCompleted
    , testResultTasksTotal
    )


liverFunctionResultFormAndTasks :
    Language
    -> NominalDate
    -> (String -> msg)
    -> (String -> msg)
    -> LiverFunctionResultForm
    -> ( Html msg, Int, Int )
liverFunctionResultFormAndTasks language currentDate setAltResultMsg setAstResultMsg form =
    let
        ( testResultSection, testResultTasksCompleted, testResultTasksTotal ) =
            ( [ viewLabel language Translate.LaboratoryLiverFunctionAltResult
              , viewMeasurementInput language
                    form.altResult
                    setAltResultMsg
                    "alt-result"
                    Translate.UnitInternationalUnitsPerLiter
              , viewLabel language Translate.LaboratoryLiverFunctionAstResult
              , viewMeasurementInput language
                    form.astResult
                    setAstResultMsg
                    "ast-result"
                    Translate.UnitInternationalUnitsPerLiter
              ]
            , taskCompleted form.altResult + taskCompleted form.astResult
            , 2
            )
    in
    ( div [ class "ui form laboratory liver-function-result" ] <|
        resultFormHeaderSection language currentDate form.executionDate TaskLiverFunctionTest
            ++ testResultSection
    , testResultTasksCompleted
    , testResultTasksTotal
    )


lipidPanelResultFormAndTasks :
    Language
    -> NominalDate
    -> (String -> msg)
    -> (String -> msg)
    -> (String -> msg)
    -> (String -> msg)
    -> (String -> msg)
    -> LipidPanelResultForm
    -> ( Html msg, Int, Int )
lipidPanelResultFormAndTasks language currentDate setUnitOfMeasurementMsg setTotalCholesterolResultMsg setLDLCholesterolResultMsg setHDLCholesterolResultMsg setTriglyceridesResultMsg form =
    let
        ( testResultSection, testResultTasksCompleted, testResultTasksTotal ) =
            let
                ( derivedSection, derivedTasksCompleted, derivedTasksTotal ) =
                    Maybe.map
                        (\unitOfMeasurement ->
                            ( [ viewLabel language Translate.LaboratoryLipidPanelTotalCholesterolLabel
                              , viewMeasurementInput language
                                    form.totalCholesterolResult
                                    setTotalCholesterolResultMsg
                                    "total-cholesterol"
                                    (Translate.UnitOfMeasurement unitOfMeasurement)
                              , viewLabel language Translate.LaboratoryLipidPanelLDLCholesterolLabel
                              , viewMeasurementInput language
                                    form.ldlCholesterolResult
                                    setLDLCholesterolResultMsg
                                    "ldl"
                                    (Translate.UnitOfMeasurement unitOfMeasurement)
                              , viewLabel language Translate.LaboratoryLipidPanelHDLCholesterolLabel
                              , viewMeasurementInput language
                                    form.hdlCholesterolResult
                                    setHDLCholesterolResultMsg
                                    "hdl"
                                    (Translate.UnitOfMeasurement unitOfMeasurement)
                              , viewLabel language Translate.LaboratoryLipidPanelTriglyceridesLabel
                              , viewMeasurementInput language
                                    form.triglyceridesResult
                                    setTriglyceridesResultMsg
                                    "triglycerides"
                                    (Translate.UnitOfMeasurement unitOfMeasurement)
                              ]
                            , taskCompleted form.totalCholesterolResult
                                + taskCompleted form.ldlCholesterolResult
                                + taskCompleted form.hdlCholesterolResult
                                + taskCompleted form.triglyceridesResult
                            , 4
                            )
                        )
                        form.unitOfMeasurement
                        |> Maybe.withDefault ( [], 0, 0 )
            in
            ( [ viewQuestionLabel language Translate.LaboratoryLipidPanelUnitOfMeasurementQuestion
              , option
                    [ value ""
                    , selected (form.unitOfMeasurement == Nothing)
                    ]
                    [ text "" ]
                    :: ([ UnitMmolL, UnitMgdL ]
                            |> List.map
                                (\unit ->
                                    option
                                        [ value (unitOfMeasurementToString unit)
                                        , selected (form.unitOfMeasurement == Just unit)
                                        ]
                                        [ text <| translate language <| Translate.UnitOfMeasurement unit ]
                                )
                       )
                    |> select [ onInput setUnitOfMeasurementMsg, class "form-input select unit-of-measurement" ]
              ]
                ++ derivedSection
            , taskCompleted form.unitOfMeasurement + derivedTasksCompleted
            , 1 + derivedTasksTotal
            )
    in
    ( div [ class "ui form laboratory lipid-panel-result" ] <|
        resultFormHeaderSection language currentDate form.executionDate TaskLipidPanelTest
            ++ testResultSection
    , testResultTasksCompleted
    , testResultTasksTotal
    )


resultFormHeaderSection : Language -> NominalDate -> Maybe NominalDate -> LaboratoryTask -> List (Html msg)
resultFormHeaderSection language currentDate executionDate task =
    let
        executionDateSection =
            Maybe.map
                (\date ->
                    [ viewLabel language <| Translate.LaboratoryTaskDate task
                    , p [ class "execution-date" ] [ text <| formatDDMMYYYY date ]
                    ]
                )
                executionDate
                |> Maybe.withDefault []
    in
    viewCustomLabel language (Translate.LaboratoryTaskLabel task) "" "label header"
        :: executionDateSection


testPerformedByValue : Maybe { a | executionNote : TestExecutionNote } -> Bool
testPerformedByValue =
    Maybe.map (.executionNote >> testPerformedByExecutionNote)
        >> Maybe.withDefault False


testPerformedByExecutionNote : TestExecutionNote -> Bool
testPerformedByExecutionNote executionNote =
    List.member executionNote [ TestNoteRunToday, TestNoteRunPreviously ]


expectRandomBloodSugarResultTask : RandomBloodSugarTestValue encounterId -> Bool
expectRandomBloodSugarResultTask value =
    let
        -- It's possible to enter the result immediatly (and not from
        -- Case management).
        -- If this is the case, we do not expect to see results task.
        immediateResult =
            Maybe.map (EverySet.member PrerequisiteImmediateResult) value.testPrerequisites
                |> Maybe.withDefault False
    in
    not immediateResult
        && testPerformedByExecutionNote value.executionNote


viewSelectInput :
    Language
    -> TranslationId
    -> Maybe a
    -> (a -> TranslationId)
    -> (a -> String)
    -> List a
    -> (String -> msg)
    -> List (Html msg)
viewSelectInput language labelTransId formValue valueTransId valueToStringFunc valuesList setMsg =
    [ viewLabel language labelTransId
    , emptyOptionForSelect formValue
        :: List.map
            (\item ->
                option
                    [ value (valueToStringFunc item)
                    , selected (formValue == Just item)
                    ]
                    [ text <| translate language <| valueTransId item ]
            )
            valuesList
        |> select
            [ onInput setMsg
            , class "form-input select"
            ]
    ]


emptyOptionForSelect : Maybe a -> Html any
emptyOptionForSelect value =
    if isNothing value then
        emptySelectOption True

    else
        emptyNode


fromNCDAValue : Maybe NCDAValue -> NCDAForm
fromNCDAValue saved =
    { step = Nothing
    , bornWithBirthDefect = Maybe.map (.signs >> EverySet.member NCDABornWithBirthDefect) saved
    , breastfedForSixMonths = Maybe.map (.signs >> EverySet.member NCDABreastfedForSixMonths) saved
    , appropriateComplementaryFeeding = Maybe.map (.signs >> EverySet.member NCDAAppropriateComplementaryFeeding) saved
    , ongeraMNP = Maybe.map (.signs >> EverySet.member NCDAOngeraMNP) saved
    , fiveFoodGroups = Maybe.map (.signs >> EverySet.member NCDAFiveFoodGroups) saved
    , mealFrequency6to8Months = Maybe.map (.signs >> EverySet.member NCDAMealFrequency6to8Months) saved
    , mealFrequency9to11Months = Maybe.map (.signs >> EverySet.member NCDAMealFrequency9to11Months) saved
    , mealFrequency12MonthsOrMore = Maybe.map (.signs >> EverySet.member NCDAMealFrequency12MonthsOrMore) saved
    , supportChildWithDisability = Maybe.map (.signs >> EverySet.member NCDASupportChildWithDisability) saved
    , conditionalCashTransfer = Maybe.map (.signs >> EverySet.member NCDAConditionalCashTransfer) saved
    , conditionalFoodItems = Maybe.map (.signs >> EverySet.member NCDAConditionalFoodItems) saved
    , hasCleanWater = Maybe.map (.signs >> EverySet.member NCDAHasCleanWater) saved
    , hasHandwashingFacility = Maybe.map (.signs >> EverySet.member NCDAHasHandwashingFacility) saved
    , hasToilets = Maybe.map (.signs >> EverySet.member NCDAHasToilets) saved
    , hasKitchenGarden = Maybe.map (.signs >> EverySet.member NCDAHasKitchenGarden) saved
    , regularPrenatalVisits = Maybe.map (.signs >> EverySet.member NCDARegularPrenatalVisits) saved
    , ironSupplementsDuringPregnancy = Maybe.map (.signs >> EverySet.member NCDAIronSupplementsDuringPregnancy) saved
    , insecticideTreatedBednetsDuringPregnancy = Maybe.map (.signs >> EverySet.member NCDAInsecticideTreatedBednetsDuringPregnancy) saved
    , birthWeight = Maybe.andThen .birthWeight saved
    }


ncdaFormWithDefault : NCDAForm -> Maybe NCDAValue -> NCDAForm
ncdaFormWithDefault form saved =
    saved
        |> unwrap
            form
            (\value ->
                { step = form.step
                , bornWithBirthDefect = or form.bornWithBirthDefect (EverySet.member NCDABornWithBirthDefect value.signs |> Just)
                , breastfedForSixMonths = or form.breastfedForSixMonths (EverySet.member NCDABreastfedForSixMonths value.signs |> Just)
                , appropriateComplementaryFeeding = or form.appropriateComplementaryFeeding (EverySet.member NCDAAppropriateComplementaryFeeding value.signs |> Just)
                , ongeraMNP = or form.ongeraMNP (EverySet.member NCDAOngeraMNP value.signs |> Just)
                , fiveFoodGroups = or form.fiveFoodGroups (EverySet.member NCDAFiveFoodGroups value.signs |> Just)
                , mealFrequency6to8Months = or form.mealFrequency6to8Months (EverySet.member NCDAMealFrequency6to8Months value.signs |> Just)
                , mealFrequency9to11Months = or form.mealFrequency9to11Months (EverySet.member NCDAMealFrequency9to11Months value.signs |> Just)
                , mealFrequency12MonthsOrMore = or form.mealFrequency12MonthsOrMore (EverySet.member NCDAMealFrequency12MonthsOrMore value.signs |> Just)
                , supportChildWithDisability = or form.supportChildWithDisability (EverySet.member NCDASupportChildWithDisability value.signs |> Just)
                , conditionalCashTransfer = or form.conditionalCashTransfer (EverySet.member NCDAConditionalCashTransfer value.signs |> Just)
                , conditionalFoodItems = or form.conditionalFoodItems (EverySet.member NCDAConditionalFoodItems value.signs |> Just)
                , hasCleanWater = or form.hasCleanWater (EverySet.member NCDAHasCleanWater value.signs |> Just)
                , hasHandwashingFacility = or form.hasHandwashingFacility (EverySet.member NCDAHasHandwashingFacility value.signs |> Just)
                , hasToilets = or form.hasToilets (EverySet.member NCDAHasToilets value.signs |> Just)
                , hasKitchenGarden = or form.hasKitchenGarden (EverySet.member NCDAHasKitchenGarden value.signs |> Just)
                , regularPrenatalVisits = or form.regularPrenatalVisits (EverySet.member NCDARegularPrenatalVisits value.signs |> Just)
                , ironSupplementsDuringPregnancy = or form.ironSupplementsDuringPregnancy (EverySet.member NCDAIronSupplementsDuringPregnancy value.signs |> Just)
                , insecticideTreatedBednetsDuringPregnancy = or form.insecticideTreatedBednetsDuringPregnancy (EverySet.member NCDAInsecticideTreatedBednetsDuringPregnancy value.signs |> Just)
                , birthWeight = or form.birthWeight value.birthWeight
                }
            )


toNCDAValueWithDefault : Maybe NCDAValue -> NCDAForm -> Maybe NCDAValue
toNCDAValueWithDefault saved form =
    ncdaFormWithDefault form saved
        |> toNCDAValue


toNCDAValue : NCDAForm -> Maybe NCDAValue
toNCDAValue form =
    let
        signs =
            [ ifNullableTrue NCDABornWithBirthDefect form.bornWithBirthDefect
            , ifNullableTrue NCDABreastfedForSixMonths form.breastfedForSixMonths
            , ifNullableTrue NCDAAppropriateComplementaryFeeding form.appropriateComplementaryFeeding
            , ifNullableTrue NCDAOngeraMNP form.ongeraMNP
            , ifNullableTrue NCDAFiveFoodGroups form.fiveFoodGroups
            , ifNullableTrue NCDAMealFrequency6to8Months form.mealFrequency6to8Months
            , ifNullableTrue NCDAMealFrequency9to11Months form.mealFrequency9to11Months
            , ifNullableTrue NCDAMealFrequency12MonthsOrMore form.mealFrequency12MonthsOrMore
            , ifNullableTrue NCDASupportChildWithDisability form.supportChildWithDisability
            , ifNullableTrue NCDAConditionalCashTransfer form.conditionalCashTransfer
            , ifNullableTrue NCDAConditionalFoodItems form.conditionalFoodItems
            , ifNullableTrue NCDAHasCleanWater form.hasCleanWater
            , ifNullableTrue NCDAHasHandwashingFacility form.hasHandwashingFacility
            , ifNullableTrue NCDAHasToilets form.hasToilets
            , ifNullableTrue NCDAHasKitchenGarden form.hasKitchenGarden
            , ifNullableTrue NCDARegularPrenatalVisits form.regularPrenatalVisits
            , ifNullableTrue NCDAIronSupplementsDuringPregnancy form.ironSupplementsDuringPregnancy
            , ifNullableTrue NCDAInsecticideTreatedBednetsDuringPregnancy form.insecticideTreatedBednetsDuringPregnancy
            ]
                |> Maybe.Extra.combine
                |> Maybe.map (List.foldl EverySet.union EverySet.empty >> ifEverySetEmpty NoNCDASigns)
    in
    Maybe.map NCDAValue signs
        |> andMap (Just form.birthWeight)


{-| Whether to expect a counseling activity is not just a yes/no question,
since we'd also like to know **which** sort of counseling activity to expect.
I suppose we could parameterize the `Counseling` activity by
`CounselingTiming`. However, that would be awkward in its own way, since we
also don't want more than one in each session.

So, we'll try it this way for now. We'll return `Nothing` if no kind of
counseling activity is expected, and `Just CounselingTiming` if one is
expected.

-}
expectCounselingActivity : EditableSession -> PersonId -> Maybe CounselingTiming
expectCounselingActivity session childId =
    let
        -- First, we check our current value. If we have a counseling session
        -- stored in the backend, or we've already got a local edit, then we
        -- use that.  This has two benefits. First, its a kind of optimization,
        -- since we're basically caching our conclusion about whether to
        -- showing the counseling activity or not. Second, it provides some UI
        -- stability ...  once we show the counseling activity and the user
        -- checks some boxes, it ensures that we'll definitely keep showing
        -- that one, and not switch to something else.
        cachedTiming =
            getChildMeasurementData childId session
                |> LocalData.toMaybe
                |> Maybe.andThen
                    (mapMeasurementData .counselingSession
                        >> currentValue
                        >> Maybe.map (.value >> Tuple.first)
                    )

        -- All the counseling session records from the past
        historical =
            getChildHistoricalMeasurements childId session.offlineSession
                |> LocalData.map .counselingSessions
                |> LocalData.withDefault Dict.empty

        -- Have we ever completed a counseling session of the specified type?
        completed timing =
            historical
                |> Dict.toList
                |> List.any
                    (\( _, counseling ) -> Tuple.first counseling.value == timing)

        -- How long ago did we complete a session of the specified type?
        completedDaysAgo timing =
            historical
                |> Dict.filter (\_ counseling -> Tuple.first counseling.value == timing)
                |> Dict.toList
                |> List.head
                |> Maybe.map (\( _, counseling ) -> diffDays counseling.dateMeasured session.offlineSession.session.startDate)

        -- How old will the child be as of the scheduled date of the session?
        -- (All of our date calculations are in days here).
        --
        -- It simplifies the rest of the calculation if we avoid making this a
        -- `Maybe`. We've got bigger problems if the session doesn't actually
        -- contain the child, so it should be safe to default the age to 0.
        age =
            getChild childId session.offlineSession
                |> Maybe.andThen
                    (\child ->
                        Maybe.map
                            (\birthDate -> diffDays birthDate session.offlineSession.session.startDate)
                            child.birthDate
                    )
                |> Maybe.withDefault 0

        -- We don't necessarily know when the next session will be scheduled,
        -- so we work on the assumption that it will be no more than 6 weeks
        -- from this session (so, 42 days).
        maximumSessionGap =
            42

        -- For the reminder, which isn't as critical, we apply the normal
        -- session gap of 32 days. This reduces the frequence of cases where we
        -- issue the reminder super-early, at the cost of some cases where we
        -- might issue no reminder (which is less serious).
        normalSessionGap =
            32

        -- To compute a two-month gap, we use one normal and one maximum
        twoMonthGap =
            normalSessionGap + maximumSessionGap

        -- To compute a three month gap, we use two normals and one maximum
        threeMonthGap =
            (normalSessionGap * 2) + maximumSessionGap

        -- In how many days (from the session date) will the child be 2 years
        -- old?
        daysUntilTwoYearsOld =
            (365 * 2) - age

        -- In how many days (from the session date) will the child be 1 year
        -- old?
        daysUntilOneYearOld =
            365 - age

        -- If we don't have a value already, we apply our basic logic, but
        -- lazily, so we make this a function. Here's a summary of our design
        -- goals, which end up having a number of parts.
        --
        -- - Definitely show the counseling activity before the relevant
        --   anniversary, using the assumption that the next session will be no
        --   more than 6 weeks away.
        --
        -- - Try to avoid showing counseling activities with no reminders, but
        --   do it without a reminder if necessary.
        --
        -- - Once we show a reminder, always show the counseling activity in
        --   the next session, even if it now seems a bit early (to avoid double
        --   reminders).
        --
        -- - Always show the entry counseling if it hasn't been done, unless
        --   we've already reached exit counseling.
        --
        -- - Make sure that there is a bit of a delay between entry counseling
        --   and midpoint counseling (for cases where a baby starts late).
        checkTiming _ =
            if completed Exit then
                -- If exit counseling has been done, then we need no more
                -- counseling
                Nothing

            else if completed BeforeExit then
                -- If we've given the exit reminder, then show the exit
                -- counseling now, even if it seems a bit early.
                Just Exit

            else if daysUntilTwoYearsOld < maximumSessionGap then
                -- If we can't be sure we'll have another session before the
                -- baby is two, then show the exit counseling
                Just Exit

            else if not (completed Entry) then
                -- If we haven't done entry counseling, then we always need to
                -- do it
                Just Entry

            else if completed MidPoint then
                -- If we have already done the MidPoint counseling, then the
                -- only thing left to consider is whether to show the Exit
                -- reminder
                if daysUntilTwoYearsOld < twoMonthGap then
                    Just BeforeExit

                else
                    Nothing

            else if completed BeforeMidpoint then
                -- If we've given the midpoint warning, then show it, even if
                -- it seems a bit early now.
                Just MidPoint

            else if daysUntilOneYearOld < maximumSessionGap then
                -- If we can't be sure we'll have another session before the
                -- baby is one year old, we show the exit counseling. Except,
                -- we also check to see whether we've done entry counseling
                -- recently ...  so that we'll always have a bit of a gap.
                case completedDaysAgo Entry of
                    Just daysAgo ->
                        if daysAgo < threeMonthGap then
                            -- We're forcing the midpoint counseling to be
                            -- roungly 3 months after the entry counseling. So,
                            -- the ideal sequence would be:
                            --
                            -- entry -> Nothing -> Rminder MidPoint -> MidPoint
                            if daysAgo < twoMonthGap then
                                Nothing

                            else
                                Just BeforeMidpoint

                        else
                            Just MidPoint

                    Nothing ->
                        Just MidPoint

            else if daysUntilOneYearOld < twoMonthGap then
                -- If we think we'll do the midpoint counseling at the next
                -- session, show the reminder. Except, again, we try to force a
                -- bit of separation between Entry and the Midpoint.
                case completedDaysAgo Entry of
                    Just daysAgo ->
                        if daysAgo < twoMonthGap then
                            -- We're forcing the reminder for midpoint
                            -- counseling to be roughtly 2 months after the
                            -- entry counseling.
                            Nothing

                        else
                            Just BeforeMidpoint

                    Nothing ->
                        Just BeforeMidpoint

            else
                Nothing
    in
    cachedTiming
        |> Maybe.Extra.orElseLazy checkTiming


{-| Which participant forms would we expect this mother to consent to in this session?
-}
expectParticipantConsent : OfflineSession -> PersonId -> Dict ParticipantFormId ParticipantForm
expectParticipantConsent session motherId =
    let
        previouslyConsented =
            getMotherHistoricalMeasurements motherId session
                |> LocalData.map
                    (.consents
                        >> Dict.map (\_ consent -> consent.value.formId)
                        >> Dict.values
                        >> EverySet.fromList
                    )
                |> LocalData.withDefault EverySet.empty

        consentedAtCurrentSession =
            getMotherMeasurementData2 motherId session
                |> LocalData.map
                    (.current
                        >> .consent
                        >> Dict.map (\_ consent -> consent.value.formId)
                        >> Dict.values
                        >> EverySet.fromList
                    )
                |> LocalData.withDefault EverySet.empty

        consentedAtPreviousSessions =
            EverySet.diff previouslyConsented consentedAtCurrentSession
    in
    session.allParticipantForms
        |> Dict.filter (\id _ -> not (EverySet.member id consentedAtPreviousSessions))


resolveLabTestDate :
    NominalDate
    -> ({ v | executionNote : TestExecutionNote, executionDate : Maybe NominalDate } -> Bool)
    -> ({ v | executionNote : TestExecutionNote, executionDate : Maybe NominalDate } -> Bool)
    ->
        Maybe
            ( id
            , { m
                | dateMeasured : NominalDate
                , value : { v | executionNote : TestExecutionNote, executionDate : Maybe NominalDate }
              }
            )
    -> Maybe NominalDate
resolveLabTestDate currentDate resultsExistFunc resultsValidFunc measurement =
    let
        dateMeasured =
            -- Date on which test was recorded.
            -- Note that this is not the date when test was performed,
            -- because it's possible to set past date for that.
            -- We need the recorded date, because the logic says that
            -- test that will not have results set for over 35 days is expired.
            -- Can default to current date, because we use it only when there's
            -- measurement value, and this means that there must be dateMeasured set.
            Maybe.map (Tuple.second >> .dateMeasured) measurement
                |> Maybe.withDefault currentDate
    in
    getMeasurementValueFunc measurement
        |> Maybe.andThen
            (\value ->
                if testPerformedByExecutionNote value.executionNote then
                    if resultsExistFunc value && (not <| resultsValidFunc value) then
                        -- Entered result is not valid, therefore,
                        -- we treat the test as if it was not performed.
                        Nothing

                    else if (not <| resultsExistFunc value) && (Date.diff Days dateMeasured currentDate >= labExpirationPeriod) then
                        -- No results were entered for more than 35 days since the
                        -- day on which measurement was taken.
                        -- Test is considered expired, and is being ignored
                        -- (as if it was never performed).
                        Nothing

                    else
                        value.executionDate

                else
                    Nothing
            )


isTestResultValid : { a | testResult : Maybe TestResult } -> Bool
isTestResultValid =
    .testResult
        >> Maybe.map ((/=) TestIndeterminate)
        >> -- In case test result was not set yet, we consider
           -- it to be valid, because results for some test are
           -- updated after few hours, or even days.
           Maybe.withDefault True
