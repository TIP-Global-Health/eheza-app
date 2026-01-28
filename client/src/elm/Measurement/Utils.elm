module Measurement.Utils exposing (..)

import AssocList as Dict exposing (Dict)
import Backend.Counseling.Model exposing (CounselingTiming(..))
import Backend.Entities exposing (..)
import Backend.IndividualEncounterParticipant.Model
import Backend.Measurement.Model exposing (..)
import Backend.Measurement.Utils exposing (..)
import Backend.Model exposing (ModelIndexedDb)
import Backend.NutritionEncounter.Utils
    exposing
        ( getChildScoreboardEncountersForParticipant
        , getWellChildEncountersForParticipant
        , resolveIndividualNutritionValues
        , resolveIndividualWellChildValues
        )
import Backend.ParticipantConsent.Model exposing (ParticipantForm)
import Backend.Person.Model exposing (Person)
import Backend.Person.Utils exposing (ageInMonths)
import Backend.PrenatalEncounter.Model exposing (PrenatalEncounterType(..))
import Backend.Session.Model exposing (EditableSession, OfflineSession)
import Backend.Session.Utils exposing (getChild, getChildHistoricalMeasurements, getChildMeasurementData, getMotherHistoricalMeasurements, getMotherMeasurementData, getMotherMeasurementData2)
import Date exposing (Unit(..))
import DateSelector.Model exposing (DateSelectorConfig)
import DateSelector.SelectorPopup exposing (viewCalendarPopup)
import EverySet exposing (EverySet)
import Gizra.Html exposing (emptyNode, showIf)
import Gizra.NominalDate exposing (NominalDate, diffDays, formatDDMMYYYY)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import List.Extra
import LocalData
import Maybe.Extra exposing (andMap, isJust, isNothing, or, unwrap)
import Measurement.Model exposing (..)
import Pages.ChildScoreboard.Encounter.Model
import Pages.Session.Model
import Pages.Utils
    exposing
        ( concatInputsAndTasksSections
        , ifEverySetEmpty
        , ifNullableTrue
        , ifTrue
        , maybeToBoolTask
        , maybeValueConsideringIsDirtyField
        , taskCompleted
        , taskCompletedWithException
        , valueConsideringIsDirtyField
        , viewBoolInput
        , viewBoolInputReverted
        , viewCheckBoxMultipleSelectCustomInput
        , viewCheckBoxMultipleSelectInput
        , viewCheckBoxSelectInput
        , viewCustomLabel
        , viewCustomSelectListInput
        , viewInstructionsLabel
        , viewLabel
        , viewMeasurementInput
        , viewQuestionLabel
        , viewSelectListInput
        )
import Pages.WellChild.Encounter.Model
import RemoteData exposing (RemoteData(..), WebData)
import Round
import SyncManager.Model exposing (Site(..))
import Translate exposing (TranslationId, translate)
import Translate.Model exposing (Language)
import Utils.Html exposing (viewModal)
import Utils.NominalDate exposing (renderDate)


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


getInputConstraintsMuac : Site -> FloatInputConstraints
getInputConstraintsMuac site =
    case site of
        SiteBurundi ->
            { minVal = 50
            , maxVal = 999
            }

        _ ->
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
fromChildMeasurementData : Site -> MeasurementData ChildMeasurements -> ModelChild
fromChildMeasurementData site data =
    let
        fromData measuremntFunc mappingFunc =
            mapMeasurementData measuremntFunc data
                |> currentValue
                |> Maybe.map mappingFunc

        muacFromFloatFunc =
            case site of
                SiteBurundi ->
                    -- Value is stored in cm, but for Burundi, we need to
                    -- view it as mm. Therefore, multiplying by 10.
                    (*) 10 >> String.fromFloat

                _ ->
                    String.fromFloat
    in
    { height =
        fromData .height (.value >> getHeightValue >> String.fromFloat)
            |> Maybe.withDefault ""
    , muac =
        fromData .muac (.value >> muacValueFunc >> muacFromFloatFunc)
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
        fromData .followUp (.value >> Just >> fromNutritionFollowUpValue)
            |> Maybe.withDefault emptyNutritionFollowUpForm
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


getChildForm : Site -> PersonId -> Pages.Session.Model.Model -> EditableSession -> ModelChild
getChildForm site childId pages session =
    -- Could use `Maybe.withDefault` here instead, but then
    -- `fromChildMeasurementData` would get calculated every time
    case Dict.get childId pages.childForms of
        Just childForm ->
            childForm

        Nothing ->
            getChildMeasurementData childId session
                |> LocalData.unwrap
                    emptyModelChild
                    (fromChildMeasurementData site
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


followUpFormWithDefault : FollowUpForm -> Maybe FollowUpValue -> FollowUpForm
followUpFormWithDefault form saved =
    saved
        |> unwrap
            form
            (\value ->
                { option = or form.option (EverySet.toList value.options |> List.head)
                , resolutionDate = or form.resolutionDate value.resolutionDate
                }
            )


toFollowUpValueWithDefault : Maybe FollowUpValue -> FollowUpForm -> Maybe FollowUpValue
toFollowUpValueWithDefault saved form =
    followUpFormWithDefault form saved
        |> toFollowUpValue


toFollowUpValue : FollowUpForm -> Maybe FollowUpValue
toFollowUpValue form =
    Maybe.map
        (\options ->
            FollowUpValue options form.resolutionDate
        )
        (Maybe.map (List.singleton >> EverySet.fromList) form.option)


fromNutritionFollowUpValue : Maybe NutritionFollowUpValue -> NutritionFollowUpForm
fromNutritionFollowUpValue saved =
    { option = Maybe.andThen (.options >> EverySet.toList >> List.head) saved
    , assesment = Maybe.map .assesment saved
    , resolutionDate = Maybe.andThen .resolutionDate saved
    }


nutritionFollowUpFormWithDefault : NutritionFollowUpForm -> Maybe NutritionFollowUpValue -> NutritionFollowUpForm
nutritionFollowUpFormWithDefault form saved =
    saved
        |> unwrap
            form
            (\value ->
                { option = or form.option (EverySet.toList value.options |> List.head)
                , assesment = or form.assesment (Just value.assesment)
                , resolutionDate = or form.resolutionDate value.resolutionDate
                }
            )


toNutritionFollowUpValueWithDefault : Maybe NutritionFollowUpValue -> NutritionFollowUpForm -> Maybe NutritionFollowUpValue
toNutritionFollowUpValueWithDefault saved form =
    nutritionFollowUpFormWithDefault form saved
        |> toNutritionFollowUpValue


toNutritionFollowUpValue : NutritionFollowUpForm -> Maybe NutritionFollowUpValue
toNutritionFollowUpValue form =
    Maybe.map2
        (\options assesment ->
            NutritionFollowUpValue options form.resolutionDate assesment
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

                    MedicationAspirin reason ->
                        Just ( Aspirin, reason )

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


wasFirstDoseAdministeredWithin14DaysFromBirthByVaccinationForm : NominalDate -> VaccinationForm msg -> Bool
wasFirstDoseAdministeredWithin14DaysFromBirthByVaccinationForm birthDate form =
    Maybe.map2
        (\administeredDoses administrationDates ->
            if EverySet.member VaccineDoseFirst administeredDoses then
                let
                    firstDoseAdminstrationDate =
                        EverySet.toList administrationDates
                            |> List.sortWith Date.compare
                            |> List.head
                in
                Maybe.map
                    (\adminstrationDate ->
                        Date.diff Days birthDate adminstrationDate < 14
                    )
                    firstDoseAdminstrationDate
                    |> Maybe.withDefault False

            else
                False
        )
        form.administeredDoses
        form.administrationDates
        |> Maybe.withDefault False


vaccinationFormDynamicContentAndTasks :
    Language
    -> NominalDate
    -> Site
    -> VaccinationFormDynamicContentAndTasksConfig msg
    -> VaccineType
    -> VaccinationForm msg
    -> ( List (Html msg), Int, Int )
vaccinationFormDynamicContentAndTasks language currentDate site config vaccineType form =
    let
        dosesFromPreviousEncountersData =
            config.dosesFromPreviousEncountersData

        dosesFromCurrentEncounterData =
            config.dosesFromCurrentEncounterData

        allDosesGivenData =
            dosesFromPreviousEncountersData
                ++ dosesFromCurrentEncounterData

        lastDoseData =
            List.filter (\( _, date ) -> date /= currentDate)
                allDosesGivenData
                |> List.reverse
                |> List.head

        historySection =
            case form.viewMode of
                ViewModeInitial ->
                    let
                        noDoseGivenToday =
                            List.filter
                                (\( _, date ) ->
                                    date == currentDate
                                )
                                dosesFromCurrentEncounterData
                                |> List.isEmpty

                        doseAllowedForDeletion =
                            List.filter
                                (\( _, date ) ->
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
                    let
                        allDosesGiven =
                            List.map Tuple.first allDosesGivenData

                        doseGivenToday =
                            List.filter
                                (\( _, date ) ->
                                    date == currentDate
                                )
                                dosesFromCurrentEncounterData
                                |> List.head
                                |> Maybe.map Tuple.first

                        dosesMissing =
                            List.filter (\dose -> not <| List.member dose allDosesGiven)
                                config.expectedDoses
                    in
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
                                                        Translate.WellChildVaccineLabel site type_
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
                                            if form.updatePreviousVaccines == Just False && config.suggestDoseToday then
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
    let
        -- Use form.brittleHair if available, otherwise default to False (NormalHairHead)
        -- This allows NCD to submit user input while Prenatal defaults to Normal
        hairHeadValue =
            form.brittleHair |> Maybe.withDefault False
    in
    Maybe.map CorePhysicalExamValue (Just (toEverySet BrittleHairCPE NormalHairHead hairHeadValue))
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


treatmentReviewInputsAndTasks :
    Language
    -> NominalDate
    -> ((Bool -> OngoingTreatmentReviewForm -> OngoingTreatmentReviewForm) -> Bool -> msg)
    -> (ReasonForNotTaking -> msg)
    -> (String -> msg)
    -> (AdverseEvent -> msg)
    -> OngoingTreatmentReviewForm
    -> ( List (Html msg), List (Maybe Bool) )
treatmentReviewInputsAndTasks language currentDate setTreatmentReviewBoolInputMsg setReasonForNotTakingMsg setMissedDosesMsg setAdverseEventMsg form =
    treatmentReviewCustomReasonsForNotTakingInputsAndTasks language
        currentDate
        ( [ NotTakingAdverseEvent, NotTakingNoMoney ], [ NotTakingMemoryProblems, NotTakingOther ] )
        setTreatmentReviewBoolInputMsg
        setReasonForNotTakingMsg
        setMissedDosesMsg
        setAdverseEventMsg
        form


treatmentReviewCustomReasonsForNotTakingInputsAndTasks :
    Language
    -> NominalDate
    -> ( List ReasonForNotTaking, List ReasonForNotTaking )
    -> ((Bool -> OngoingTreatmentReviewForm -> OngoingTreatmentReviewForm) -> Bool -> msg)
    -> (ReasonForNotTaking -> msg)
    -> (String -> msg)
    -> (AdverseEvent -> msg)
    -> OngoingTreatmentReviewForm
    -> ( List (Html msg), List (Maybe Bool) )
treatmentReviewCustomReasonsForNotTakingInputsAndTasks language currentDate ( reasonForNotTakingLeft, reasonForNotTakingRight ) setTreatmentReviewBoolInputMsg setReasonForNotTakingMsg setMissedDosesMsg setAdverseEventMsg form =
    let
        ( takenAsPrescribedInputs, takenAsPrescribedTasks ) =
            let
                takenAsPrescribedUpdateFunc value form_ =
                    if value then
                        { form_ | takenAsPrescribed = Just True, reasonForNotTaking = Nothing, reasonForNotTakingDirty = True }

                    else
                        { form_ | takenAsPrescribed = Just False }

                ( reasonForNotTakingInput, reasonForNotTakingTask ) =
                    let
                        takenAsPrescribed =
                            Maybe.withDefault True form.takenAsPrescribed
                    in
                    if not takenAsPrescribed then
                        ( [ div [ class "ui grid" ]
                                [ div [ class "one wide column" ] []
                                , div [ class "fifteen wide column" ]
                                    [ viewQuestionLabel language Translate.WhyNot ]
                                ]
                          , viewCheckBoxSelectInput language
                                reasonForNotTakingLeft
                                reasonForNotTakingRight
                                form.reasonForNotTaking
                                setReasonForNotTakingMsg
                                Translate.ReasonForNotTaking
                          ]
                        , [ maybeToBoolTask form.reasonForNotTaking ]
                        )

                    else
                        ( [], [] )
            in
            ( [ viewQuestionLabel language Translate.MedicationTakenAsPrescribedQuestion
              , viewBoolInput
                    language
                    form.takenAsPrescribed
                    (setTreatmentReviewBoolInputMsg takenAsPrescribedUpdateFunc)
                    "taken-as-prescribed"
                    Nothing
              ]
                ++ reasonForNotTakingInput
            , form.takenAsPrescribed
                :: reasonForNotTakingTask
            )

        ( missedDosesInputs, missedDosesTasks ) =
            let
                missedDosesUpdateFunc value form_ =
                    if value then
                        { form_ | missedDoses = Just True }

                    else
                        { form_ | missedDoses = Just False, totalMissedDoses = Nothing, totalMissedDosesDirty = True }

                ( totalMissedDosesInput, totalMissedDosesTask ) =
                    let
                        missedDoses =
                            Maybe.withDefault False form.missedDoses
                    in
                    if missedDoses then
                        let
                            options =
                                List.repeat 21 ""
                                    |> List.indexedMap (\index _ -> index + 1)

                            missedDosesInput =
                                viewCustomSelectListInput form.totalMissedDoses
                                    options
                                    String.fromInt
                                    setMissedDosesMsg
                                    String.fromInt
                                    ""
                                    True
                        in
                        ( [ div [ class "ui grid" ]
                                [ div [ class "one wide column" ] []
                                , div [ class "four wide column" ]
                                    [ viewQuestionLabel language Translate.HowManyDoses ]
                                , div [ class "four wide column" ]
                                    [ missedDosesInput ]
                                ]
                          ]
                        , [ maybeToBoolTask form.totalMissedDoses ]
                        )

                    else
                        ( [], [] )
            in
            ( [ viewQuestionLabel language Translate.MedicationDosesMissedQuestion
              , viewBoolInputReverted
                    language
                    form.missedDoses
                    (setTreatmentReviewBoolInputMsg missedDosesUpdateFunc)
                    "missed-doses"
                    Nothing
              ]
                ++ totalMissedDosesInput
            , form.missedDoses :: totalMissedDosesTask
            )

        ( sideEffectsInputs, sideEffectsTasks ) =
            let
                sideEffectsUpdateFunc value form_ =
                    if value then
                        { form_ | sideEffects = Just value }

                    else
                        { form_ | sideEffects = Just value, adverseEvents = Nothing, adverseEventsDirty = True }

                ( adverseEventsInput, adverseEventsTask ) =
                    let
                        sideEffects =
                            Maybe.withDefault False form.sideEffects
                    in
                    if sideEffects then
                        ( [ div [ class "ui grid" ]
                                [ div [ class "one wide column" ] []
                                , div [ class "fifteen wide column" ]
                                    [ viewQuestionLabel language Translate.AcuteIllnessAdverseEventKindsQuestion ]
                                ]
                          , viewCheckBoxMultipleSelectInput language
                                [ AdverseEventRashOrItching
                                , AdverseEventFever
                                , AdverseEventDiarrhea
                                ]
                                [ AdverseEventVomiting
                                , AdverseEventFatigue
                                , AdverseEventOther
                                ]
                                (Maybe.withDefault [] form.adverseEvents)
                                Nothing
                                setAdverseEventMsg
                                Translate.AdverseEvent
                          ]
                        , [ maybeToBoolTask form.adverseEvents ]
                        )

                    else
                        ( [], [] )
            in
            ( [ viewQuestionLabel language Translate.MedicationCausesSideEffectsQuestion
              , viewBoolInputReverted
                    language
                    form.sideEffects
                    (setTreatmentReviewBoolInputMsg sideEffectsUpdateFunc)
                    "side-effects"
                    Nothing
              ]
                ++ adverseEventsInput
            , form.sideEffects :: adverseEventsTask
            )

        feelingBetterUpdateFunc value form_ =
            { form_ | feelingBetter = Just value }
    in
    ( takenAsPrescribedInputs
        ++ [ viewQuestionLabel language Translate.MedicationFeelBetterAfterTakingQuestion
           , viewBoolInput
                language
                form.feelingBetter
                (setTreatmentReviewBoolInputMsg feelingBetterUpdateFunc)
                "feeling-better"
                Nothing
           ]
        ++ missedDosesInputs
        ++ sideEffectsInputs
    , takenAsPrescribedTasks ++ [ form.feelingBetter ] ++ missedDosesTasks ++ sideEffectsTasks
    )


outsideCareFormInputsAndTasksDiagnoses :
    Language
    -> OutsideCareConfig diagnosis msg
    -> OutsideCareForm diagnosis
    -> ( List (Html msg), List (Maybe Bool) )
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
                , form.givenNewDiagnosis :: newDiagnosisTasks
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
    , form.seenAtAnotherFacility :: givenNewDiagnosisTasks
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



--- Labs tests Form <=> Value functions   START  ---


hivTestFormWithDefault : HIVTestForm msg -> Maybe HIVTestValue -> HIVTestForm msg
hivTestFormWithDefault form saved =
    saved
        |> unwrap
            form
            (\value ->
                let
                    knownAsPositiveValue =
                        value.executionNote == TestNoteKnownAsPositive

                    testPerformedValue =
                        testPerformedByExecutionNote value.executionNote

                    immediateResultValue =
                        Maybe.map (EverySet.member PrerequisiteImmediateResult)
                            value.testPrerequisites

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
                , immediateResult = or form.immediateResult immediateResultValue
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
            , testPrerequisites = testPrerequisitesByImmediateResult form.immediateResult
            , testResult = form.testResult
            , hivSigns = hivSigns
            }
        )
        form.executionNote


partnerHIVTestFormWithDefault : PartnerHIVTestForm -> Maybe PartnerHIVTestValue -> PartnerHIVTestForm
partnerHIVTestFormWithDefault form =
    unwrap
        form
        (\value ->
            let
                knownAsPositiveValue =
                    value.executionNote == TestNoteKnownAsPositive

                testPerformedValue =
                    testPerformedByExecutionNote value.executionNote

                immediateResultValue =
                    Maybe.map (EverySet.member PrerequisiteImmediateResult)
                        value.testPrerequisites

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
            , immediateResult = or form.immediateResult immediateResultValue
            , executionNote = valueConsideringIsDirtyField form.executionNoteDirty form.executionNote value.executionNote
            , executionNoteDirty = form.executionNoteDirty
            , executionDate = maybeValueConsideringIsDirtyField form.executionDateDirty form.executionDate value.executionDate
            , executionDateDirty = form.executionDateDirty
            , testResult = maybeValueConsideringIsDirtyField form.testResultDirty form.testResult value.testResult
            , testResultDirty = form.testResultDirty
            , partnerTakingARV = valueConsideringIsDirtyField form.partnerTakingARVDirty form.partnerTakingARV partnerTakingARVValue
            , partnerTakingARVDirty = form.partnerTakingARVDirty
            , partnerSurpressedViralLoad = valueConsideringIsDirtyField form.partnerSurpressedViralLoadDirty form.partnerSurpressedViralLoad partnerSurpressedViralLoadValue
            , partnerSurpressedViralLoadDirty = form.partnerSurpressedViralLoadDirty
            }
        )


toPartnerHIVTestValueWithDefault : Maybe PartnerHIVTestValue -> PartnerHIVTestForm -> Maybe PartnerHIVTestValue
toPartnerHIVTestValueWithDefault saved form =
    partnerHIVTestFormWithDefault form saved
        |> toPartnerHIVTestValue


toPartnerHIVTestValue : PartnerHIVTestForm -> Maybe PartnerHIVTestValue
toPartnerHIVTestValue form =
    Maybe.map
        (\executionNote ->
            let
                hivSigns =
                    [ ifNullableTrue PartnerTakingARV form.partnerTakingARV
                    , ifNullableTrue PartnerSurpressedViralLoad form.partnerSurpressedViralLoad
                    ]
                        |> Maybe.Extra.combine
                        |> Maybe.map (List.foldl EverySet.union EverySet.empty >> ifEverySetEmpty NoPrenatalHIVSign)
            in
            { executionNote = executionNote
            , executionDate = form.executionDate
            , testPrerequisites = testPrerequisitesByImmediateResult form.immediateResult
            , testResult = form.testResult
            , hivSigns = hivSigns
            }
        )
        form.executionNote


hivTestUniversalFormWithDefault : HIVTestUniversalForm -> Maybe HIVTestValue -> HIVTestUniversalForm
hivTestUniversalFormWithDefault form =
    unwrap
        form
        (\value ->
            let
                knownAsPositiveValue =
                    value.executionNote == TestNoteKnownAsPositive

                testPerformedValue =
                    testPerformedByExecutionNote value.executionNote

                immediateResultValue =
                    Maybe.map (EverySet.member PrerequisiteImmediateResult)
                        value.testPrerequisites

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
            , immediateResult = or form.immediateResult immediateResultValue
            , executionNote = valueConsideringIsDirtyField form.executionNoteDirty form.executionNote value.executionNote
            , executionNoteDirty = form.executionNoteDirty
            , executionDate = maybeValueConsideringIsDirtyField form.executionDateDirty form.executionDate value.executionDate
            , executionDateDirty = form.executionDateDirty
            , testResult = maybeValueConsideringIsDirtyField form.testResultDirty form.testResult value.testResult
            , testResultDirty = form.testResultDirty
            , hivProgramHC = valueConsideringIsDirtyField form.hivProgramHCDirty form.hivProgramHC hivProgramHCValue
            , hivProgramHCDirty = form.hivProgramHCDirty
            , partnerHIVPositive = valueConsideringIsDirtyField form.partnerHIVPositiveDirty form.partnerHIVPositive partnerHIVPositiveValue
            , partnerHIVPositiveDirty = form.partnerHIVPositiveDirty
            , partnerTakingARV = valueConsideringIsDirtyField form.partnerTakingARVDirty form.partnerTakingARV partnerTakingARVValue
            , partnerTakingARVDirty = form.partnerTakingARVDirty
            , partnerSurpressedViralLoad = valueConsideringIsDirtyField form.partnerSurpressedViralLoadDirty form.partnerSurpressedViralLoad partnerSurpressedViralLoadValue
            , partnerSurpressedViralLoadDirty = form.partnerSurpressedViralLoadDirty
            }
        )


toHIVTestValueUniversalWithDefault : Maybe HIVTestValue -> HIVTestUniversalForm -> Maybe HIVTestValue
toHIVTestValueUniversalWithDefault saved form =
    hivTestUniversalFormWithDefault form saved
        |> toHIVTestValueUniversal


toHIVTestValueUniversal : HIVTestUniversalForm -> Maybe HIVTestValue
toHIVTestValueUniversal form =
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
            , testPrerequisites = testPrerequisitesByImmediateResult form.immediateResult
            , testResult = form.testResult
            , hivSigns = hivSigns
            }
        )
        form.executionNote


malariaTestFormWithDefault : MalariaTestForm -> Maybe MalariaTestValue -> MalariaTestForm
malariaTestFormWithDefault form =
    unwrap
        form
        (\value ->
            let
                testPerformedValue =
                    testPerformedByExecutionNote value.executionNote

                immediateResultValue =
                    Maybe.map (EverySet.member PrerequisiteImmediateResult)
                        value.testPrerequisites

                bloodSmearTakenByValue =
                    bloodSmearResultSet value.bloodSmearResult
            in
            { testPerformed =
                valueConsideringIsDirtyField form.testPerformedDirty
                    form.testPerformed
                    testPerformedValue
            , testPerformedDirty = form.testPerformedDirty
            , immediateResult = or form.immediateResult immediateResultValue
            , executionNote =
                valueConsideringIsDirtyField form.executionNoteDirty
                    form.executionNote
                    value.executionNote
            , executionNoteDirty = form.executionNoteDirty
            , executionDate =
                maybeValueConsideringIsDirtyField form.executionDateDirty
                    form.executionDate
                    value.executionDate
            , executionDateDirty = form.executionDateDirty
            , testResult = maybeValueConsideringIsDirtyField form.testResultDirty form.testResult value.testResult
            , testResultDirty = form.testResultDirty
            , bloodSmearTaken =
                maybeValueConsideringIsDirtyField form.bloodSmearTakenDirty
                    form.bloodSmearTaken
                    (Just bloodSmearTakenByValue)
            , bloodSmearTakenDirty = form.bloodSmearTakenDirty
            , bloodSmearResult =
                maybeValueConsideringIsDirtyField form.bloodSmearResultDirty
                    form.bloodSmearResult
                    (Just value.bloodSmearResult)
            , bloodSmearResultDirty = form.bloodSmearResultDirty
            }
        )


bloodSmearResultSet : BloodSmearResult -> Bool
bloodSmearResultSet =
    bloodSmearResultNotSet >> not


bloodSmearResultNotSet : BloodSmearResult -> Bool
bloodSmearResultNotSet value =
    List.member value [ BloodSmearPendingInput, BloodSmearNotTaken ]


toMalariaTestValueWithDefault : Maybe MalariaTestValue -> MalariaTestForm -> Maybe MalariaTestValue
toMalariaTestValueWithDefault saved form =
    malariaTestFormWithDefault form saved
        |> toMalariaTestValue


toMalariaTestValue : MalariaTestForm -> Maybe MalariaTestValue
toMalariaTestValue form =
    Maybe.map
        (\executionNote ->
            let
                defaultBloodSmearResult =
                    if
                        (form.testPerformed == Just False)
                            && (form.bloodSmearTaken == Just True)
                            && (form.immediateResult == Just False)
                    then
                        BloodSmearPendingInput

                    else
                        BloodSmearNotTaken
            in
            { executionNote = executionNote
            , executionDate = form.executionDate
            , testPrerequisites = testPrerequisitesByImmediateResult form.immediateResult
            , testResult = form.testResult
            , bloodSmearResult = Maybe.withDefault defaultBloodSmearResult form.bloodSmearResult
            }
        )
        form.executionNote


urineDipstickFormWithDefault : UrineDipstickTestForm msg -> Maybe UrineDipstickTestValue -> UrineDipstickTestForm msg
urineDipstickFormWithDefault form saved =
    saved
        |> unwrap
            form
            (\value ->
                let
                    testPerformedValue =
                        testPerformedByExecutionNote value.executionNote

                    immediateResultValue =
                        Maybe.map (EverySet.member PrerequisiteImmediateResult)
                            value.testPrerequisites

                    testPerformedTodayFromValue =
                        value.executionNote == TestNoteRunToday
                in
                { testPerformed = valueConsideringIsDirtyField form.testPerformedDirty form.testPerformed testPerformedValue
                , testPerformedDirty = form.testPerformedDirty
                , immediateResult = or form.immediateResult immediateResultValue
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


toUrineDipstickTestValueWithDefault : Maybe UrineDipstickTestValue -> UrineDipstickTestForm msg -> Maybe UrineDipstickTestValue
toUrineDipstickTestValueWithDefault saved form =
    urineDipstickFormWithDefault form saved
        |> toUrineDipstickTestValue


toUrineDipstickTestValue : UrineDipstickTestForm msg -> Maybe UrineDipstickTestValue
toUrineDipstickTestValue form =
    Maybe.map
        (\executionNote ->
            { testVariant = form.testVariant
            , executionNote = executionNote
            , executionDate = form.executionDate
            , testPrerequisites = testPrerequisitesByImmediateResult form.immediateResult
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


urineDipstickUniversalFormWithDefault : UrineDipstickTestUniversalForm -> Maybe UrineDipstickTestValue -> UrineDipstickTestUniversalForm
urineDipstickUniversalFormWithDefault form saved =
    saved
        |> unwrap
            form
            (\value ->
                let
                    testPerformedValue =
                        testPerformedByExecutionNote value.executionNote

                    immediateResultValue =
                        Maybe.map (EverySet.member PrerequisiteImmediateResult)
                            value.testPrerequisites
                in
                { testPerformed = valueConsideringIsDirtyField form.testPerformedDirty form.testPerformed testPerformedValue
                , testPerformedDirty = form.testPerformedDirty
                , immediateResult = or form.immediateResult immediateResultValue
                , executionNote = valueConsideringIsDirtyField form.executionNoteDirty form.executionNote value.executionNote
                , executionNoteDirty = form.executionNoteDirty
                , executionDate = maybeValueConsideringIsDirtyField form.executionDateDirty form.executionDate value.executionDate
                , executionDateDirty = form.executionDateDirty
                , testVariant = maybeValueConsideringIsDirtyField form.testVariantDirty form.testVariant value.testVariant
                , testVariantDirty = form.testVariantDirty
                , protein = maybeValueConsideringIsDirtyField form.proteinDirty form.protein value.protein
                , proteinDirty = form.proteinDirty
                , ph = maybeValueConsideringIsDirtyField form.phDirty form.ph value.ph
                , phDirty = form.phDirty
                , glucose = maybeValueConsideringIsDirtyField form.glucoseDirty form.glucose value.glucose
                , glucoseDirty = form.glucoseDirty
                , leukocytes = maybeValueConsideringIsDirtyField form.leukocytesDirty form.leukocytes value.leukocytes
                , leukocytesDirty = form.leukocytesDirty
                , nitrite = maybeValueConsideringIsDirtyField form.nitriteDirty form.nitrite value.nitrite
                , nitriteDirty = form.nitriteDirty
                , urobilinogen = maybeValueConsideringIsDirtyField form.urobilinogenDirty form.urobilinogen value.urobilinogen
                , urobilinogenDirty = form.urobilinogenDirty
                , haemoglobin = maybeValueConsideringIsDirtyField form.haemoglobinDirty form.haemoglobin value.haemoglobin
                , haemoglobinDirty = form.haemoglobinDirty
                , ketone = maybeValueConsideringIsDirtyField form.ketoneDirty form.ketone value.ketone
                , ketoneDirty = form.ketoneDirty
                , bilirubin = maybeValueConsideringIsDirtyField form.bilirubinDirty form.bilirubin value.bilirubin
                , bilirubinDirty = form.bilirubinDirty
                }
            )


toUrineDipstickTestValueUniversalWithDefault : Maybe UrineDipstickTestValue -> UrineDipstickTestUniversalForm -> Maybe UrineDipstickTestValue
toUrineDipstickTestValueUniversalWithDefault saved form =
    urineDipstickUniversalFormWithDefault form saved
        |> toUrineDipstickTestValueUniversal


toUrineDipstickTestValueUniversal : UrineDipstickTestUniversalForm -> Maybe UrineDipstickTestValue
toUrineDipstickTestValueUniversal form =
    Maybe.map
        (\executionNote ->
            { testVariant = form.testVariant
            , executionNote = executionNote
            , executionDate = form.executionDate
            , testPrerequisites = testPrerequisitesByImmediateResult form.immediateResult
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


randomBloodSugarFormWithDefault : RandomBloodSugarTestForm msg -> Maybe (RandomBloodSugarTestValue encounterId) -> RandomBloodSugarTestForm msg
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


toRandomBloodSugarTestValueWithDefault : Maybe (RandomBloodSugarTestValue encounterId) -> RandomBloodSugarTestForm msg -> Maybe (RandomBloodSugarTestValue encounterId)
toRandomBloodSugarTestValueWithDefault saved form =
    randomBloodSugarFormWithDefault form saved
        |> toRandomBloodSugarTestValue


toRandomBloodSugarTestValue : RandomBloodSugarTestForm msg -> Maybe (RandomBloodSugarTestValue encounterId)
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


randomBloodSugarUniversalFormWithDefault : RandomBloodSugarTestUniversalForm -> Maybe (RandomBloodSugarTestValue encounterId) -> RandomBloodSugarTestUniversalForm
randomBloodSugarUniversalFormWithDefault form saved =
    saved
        |> unwrap
            form
            (\value ->
                let
                    testPerformedValue =
                        testPerformedByExecutionNote value.executionNote

                    patientFastedValue =
                        Maybe.map (EverySet.member PrerequisiteFastFor12h)
                            value.testPrerequisites

                    immediateResultValue =
                        Maybe.map (EverySet.member PrerequisiteImmediateResult)
                            value.testPrerequisites
                in
                { testPerformed = valueConsideringIsDirtyField form.testPerformedDirty form.testPerformed testPerformedValue
                , testPerformedDirty = form.testPerformedDirty
                , immediateResult = or form.immediateResult immediateResultValue
                , executionNote = valueConsideringIsDirtyField form.executionNoteDirty form.executionNote value.executionNote
                , executionNoteDirty = form.executionNoteDirty
                , executionDate = maybeValueConsideringIsDirtyField form.executionDateDirty form.executionDate value.executionDate
                , executionDateDirty = form.executionDateDirty
                , patientFasted = or form.patientFasted patientFastedValue
                , sugarCount = maybeValueConsideringIsDirtyField form.sugarCountDirty form.sugarCount value.sugarCount
                , sugarCountDirty = form.sugarCountDirty
                }
            )


toRandomBloodSugarTestValueUniversalWithDefault : Maybe (RandomBloodSugarTestValue encounterId) -> RandomBloodSugarTestUniversalForm -> Maybe (RandomBloodSugarTestValue encounterId)
toRandomBloodSugarTestValueUniversalWithDefault saved form =
    randomBloodSugarUniversalFormWithDefault form saved
        |> toRandomBloodSugarTestValueUniversal


toRandomBloodSugarTestValueUniversal : RandomBloodSugarTestUniversalForm -> Maybe (RandomBloodSugarTestValue encounterId)
toRandomBloodSugarTestValueUniversal form =
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


bloodGpRsTestFormWithDefault : BloodGpRsTestForm -> Maybe (BloodGpRsTestValue encounterId) -> BloodGpRsTestForm
bloodGpRsTestFormWithDefault form saved =
    saved
        |> unwrap
            form
            (\value ->
                let
                    testPerformedValue =
                        testPerformedByExecutionNote value.executionNote

                    immediateResultValue =
                        Maybe.map (EverySet.member PrerequisiteImmediateResult)
                            value.testPrerequisites
                in
                { testPerformed = valueConsideringIsDirtyField form.testPerformedDirty form.testPerformed testPerformedValue
                , testPerformedDirty = form.testPerformedDirty
                , immediateResult = or form.immediateResult immediateResultValue
                , executionNote = valueConsideringIsDirtyField form.executionNoteDirty form.executionNote value.executionNote
                , executionNoteDirty = form.executionNoteDirty
                , executionDate = maybeValueConsideringIsDirtyField form.executionDateDirty form.executionDate value.executionDate
                , executionDateDirty = form.executionDateDirty
                , bloodGroup = maybeValueConsideringIsDirtyField form.bloodGroupDirty form.bloodGroup value.bloodGroup
                , bloodGroupDirty = form.bloodGroupDirty
                , rhesus = maybeValueConsideringIsDirtyField form.rhesusDirty form.rhesus value.rhesus
                , rhesusDirty = form.rhesusDirty
                }
            )


toBloodGpRsTestValueWithDefault : Maybe (BloodGpRsTestValue encounterId) -> BloodGpRsTestForm -> Maybe (BloodGpRsTestValue encounterId)
toBloodGpRsTestValueWithDefault saved form =
    bloodGpRsTestFormWithDefault form saved
        |> toBloodGpRsTestValue


toBloodGpRsTestValue : BloodGpRsTestForm -> Maybe (BloodGpRsTestValue encounterId)
toBloodGpRsTestValue form =
    Maybe.map
        (\executionNote ->
            { executionNote = executionNote
            , executionDate = form.executionDate
            , testPrerequisites = testPrerequisitesByImmediateResult form.immediateResult
            , bloodGroup = form.bloodGroup
            , rhesus = form.rhesus
            , originatingEncounter = Nothing
            }
        )
        form.executionNote


hemoglobinTestFormWithDefault : HemoglobinTestForm -> Maybe HemoglobinTestValue -> HemoglobinTestForm
hemoglobinTestFormWithDefault form saved =
    saved
        |> unwrap
            form
            (\value ->
                let
                    testPerformedValue =
                        testPerformedByExecutionNote value.executionNote

                    immediateResultValue =
                        Maybe.map (EverySet.member PrerequisiteImmediateResult)
                            value.testPrerequisites
                in
                { testPerformed = valueConsideringIsDirtyField form.testPerformedDirty form.testPerformed testPerformedValue
                , testPerformedDirty = form.testPerformedDirty
                , immediateResult = or form.immediateResult immediateResultValue
                , executionNote = valueConsideringIsDirtyField form.executionNoteDirty form.executionNote value.executionNote
                , executionNoteDirty = form.executionNoteDirty
                , executionDate = maybeValueConsideringIsDirtyField form.executionDateDirty form.executionDate value.executionDate
                , executionDateDirty = form.executionDateDirty
                , hemoglobinCount = maybeValueConsideringIsDirtyField form.hemoglobinCountDirty form.hemoglobinCount value.hemoglobinCount
                , hemoglobinCountDirty = form.hemoglobinCountDirty
                }
            )


toHemoglobinTestValueWithDefault : Maybe HemoglobinTestValue -> HemoglobinTestForm -> Maybe HemoglobinTestValue
toHemoglobinTestValueWithDefault saved form =
    hemoglobinTestFormWithDefault form saved
        |> toHemoglobinTestValue


toHemoglobinTestValue : HemoglobinTestForm -> Maybe HemoglobinTestValue
toHemoglobinTestValue form =
    Maybe.map
        (\executionNote ->
            { executionNote = executionNote
            , executionDate = form.executionDate
            , testPrerequisites = testPrerequisitesByImmediateResult form.immediateResult
            , hemoglobinCount = form.hemoglobinCount
            }
        )
        form.executionNote


hepatitisBTestFormWithDefault : HepatitisBTestForm -> Maybe (HepatitisBTestValue encounterId) -> HepatitisBTestForm
hepatitisBTestFormWithDefault form saved =
    saved
        |> unwrap
            form
            (\value ->
                let
                    knownAsPositiveValue =
                        value.executionNote == TestNoteKnownAsPositive

                    testPerformedValue =
                        testPerformedByExecutionNote value.executionNote

                    immediateResultValue =
                        Maybe.map (EverySet.member PrerequisiteImmediateResult)
                            value.testPrerequisites
                in
                { knownAsPositive = or form.knownAsPositive (Just knownAsPositiveValue)
                , testPerformed = valueConsideringIsDirtyField form.testPerformedDirty form.testPerformed testPerformedValue
                , testPerformedDirty = form.testPerformedDirty
                , immediateResult = or form.immediateResult immediateResultValue
                , executionNote = valueConsideringIsDirtyField form.executionNoteDirty form.executionNote value.executionNote
                , executionNoteDirty = form.executionNoteDirty
                , executionDate = maybeValueConsideringIsDirtyField form.executionDateDirty form.executionDate value.executionDate
                , executionDateDirty = form.executionDateDirty
                , testResult = maybeValueConsideringIsDirtyField form.testResultDirty form.testResult value.testResult
                , testResultDirty = form.testResultDirty
                }
            )


toHepatitisBTestValueWithDefault : Maybe (HepatitisBTestValue encounterId) -> HepatitisBTestForm -> Maybe (HepatitisBTestValue encounterId)
toHepatitisBTestValueWithDefault saved form =
    hepatitisBTestFormWithDefault form saved
        |> toHepatitisBTestValue


toHepatitisBTestValue : HepatitisBTestForm -> Maybe (HepatitisBTestValue encounterId)
toHepatitisBTestValue form =
    Maybe.map
        (\executionNote ->
            { executionNote = executionNote
            , executionDate = form.executionDate
            , testPrerequisites = testPrerequisitesByImmediateResult form.immediateResult
            , testResult = form.testResult
            , originatingEncounter = Nothing
            }
        )
        form.executionNote


hivPCRTestFormWithDefault : HIVPCRTestForm -> Maybe HIVPCRTestValue -> HIVPCRTestForm
hivPCRTestFormWithDefault form saved =
    saved
        |> unwrap
            form
            (\value ->
                let
                    testPerformedValue =
                        testPerformedByExecutionNote value.executionNote

                    immediateResultValue =
                        Maybe.map (EverySet.member PrerequisiteImmediateResult)
                            value.testPrerequisites
                in
                { testPerformed = valueConsideringIsDirtyField form.testPerformedDirty form.testPerformed testPerformedValue
                , testPerformedDirty = form.testPerformedDirty
                , immediateResult = or form.immediateResult immediateResultValue
                , executionNote = valueConsideringIsDirtyField form.executionNoteDirty form.executionNote value.executionNote
                , executionNoteDirty = form.executionNoteDirty
                , executionDate = maybeValueConsideringIsDirtyField form.executionDateDirty form.executionDate value.executionDate
                , executionDateDirty = form.executionDateDirty
                , hivViralLoadStatus = maybeValueConsideringIsDirtyField form.hivViralLoadStatusDirty form.hivViralLoadStatus value.hivViralLoadStatus
                , hivViralLoadStatusDirty = form.hivViralLoadStatusDirty
                , hivViralLoad = maybeValueConsideringIsDirtyField form.hivViralLoadDirty form.hivViralLoad value.hivViralLoad
                , hivViralLoadDirty = form.hivViralLoadDirty
                }
            )


toHIVPCRTestValueWithDefault : Maybe HIVPCRTestValue -> HIVPCRTestForm -> Maybe HIVPCRTestValue
toHIVPCRTestValueWithDefault saved form =
    hivPCRTestFormWithDefault form saved
        |> toHIVPCRTestValue


toHIVPCRTestValue : HIVPCRTestForm -> Maybe HIVPCRTestValue
toHIVPCRTestValue form =
    Maybe.map
        (\executionNote ->
            { executionNote = executionNote
            , executionDate = form.executionDate
            , testPrerequisites = testPrerequisitesByImmediateResult form.immediateResult
            , hivViralLoadStatus = form.hivViralLoadStatus
            , hivViralLoad = form.hivViralLoad
            }
        )
        form.executionNote


syphilisTestFormWithDefault : SyphilisTestForm -> Maybe (SyphilisTestValue encounterId) -> SyphilisTestForm
syphilisTestFormWithDefault form saved =
    saved
        |> unwrap
            form
            (\value ->
                let
                    testPerformedValue =
                        testPerformedByExecutionNote value.executionNote

                    immediateResultValue =
                        Maybe.map (EverySet.member PrerequisiteImmediateResult)
                            value.testPrerequisites
                in
                { testPerformed = valueConsideringIsDirtyField form.testPerformedDirty form.testPerformed testPerformedValue
                , testPerformedDirty = form.testPerformedDirty
                , immediateResult = or form.immediateResult immediateResultValue
                , executionNote = valueConsideringIsDirtyField form.executionNoteDirty form.executionNote value.executionNote
                , executionNoteDirty = form.executionNoteDirty
                , executionDate = maybeValueConsideringIsDirtyField form.executionDateDirty form.executionDate value.executionDate
                , executionDateDirty = form.executionDateDirty
                , testResult = maybeValueConsideringIsDirtyField form.testResultDirty form.testResult value.testResult
                , testResultDirty = form.testResultDirty
                , symptoms = maybeValueConsideringIsDirtyField form.symptomsDirty form.symptoms (Maybe.map EverySet.toList value.symptoms)
                , symptomsDirty = form.symptomsDirty
                }
            )


toSyphilisTestValueWithDefault : Maybe (SyphilisTestValue encounterId) -> SyphilisTestForm -> Maybe (SyphilisTestValue encounterId)
toSyphilisTestValueWithDefault saved form =
    syphilisTestFormWithDefault form saved
        |> toSyphilisTestValue


toSyphilisTestValue : SyphilisTestForm -> Maybe (SyphilisTestValue encounterId)
toSyphilisTestValue form =
    Maybe.map
        (\executionNote ->
            { executionNote = executionNote
            , executionDate = form.executionDate
            , testPrerequisites = testPrerequisitesByImmediateResult form.immediateResult
            , testResult = form.testResult
            , symptoms = Maybe.map EverySet.fromList form.symptoms
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
                        value.executionNote == TestNoteKnownAsPositive

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
                        value.executionNote == TestNoteKnownAsPositive

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


toCreatinineTestValueWithEmptyResults : TestExecutionNote -> Maybe NominalDate -> CreatinineTestValue
toCreatinineTestValueWithEmptyResults note date =
    CreatinineTestValue note date Nothing Nothing


toLiverFunctionTestValueWithEmptyResults : TestExecutionNote -> Maybe NominalDate -> LiverFunctionTestValue
toLiverFunctionTestValueWithEmptyResults note date =
    LiverFunctionTestValue note date Nothing Nothing


toLipidPanelTestValueWithEmptyResults : TestExecutionNote -> Maybe NominalDate -> LipidPanelTestValue
toLipidPanelTestValueWithEmptyResults note date =
    LipidPanelTestValue note date Nothing Nothing Nothing Nothing Nothing


testPrerequisitesByImmediateResult : Maybe Bool -> Maybe (EverySet TestPrerequisite)
testPrerequisitesByImmediateResult =
    Maybe.map
        (\immediateResult ->
            if immediateResult then
                EverySet.singleton PrerequisiteImmediateResult

            else
                EverySet.singleton NoTestPrerequisites
        )



--- Labs tests Form <=> Value functions   END  ---
--- Labs tests View functions   START  ---


standardTestResultInputsAndTasks :
    Language
    -> (String -> msg)
    -> Maybe TestResult
    -> LaboratoryTask
    -> ( List (Html msg), Int, Int )
standardTestResultInputsAndTasks language setTestResultMsg testResult task =
    ( viewSelectInput language
        (Translate.LaboratoryTaskResult task)
        testResult
        Translate.TestResult
        testResultToString
        [ TestPositive, TestNegative, TestIndeterminate ]
        setTestResultMsg
    , taskCompleted testResult
    , 1
    )


prerequisiteByImmediateResultInputsAndTasks :
    Language
    -> (Bool -> msg)
    -> Maybe Bool
    -> ( List (Html msg), Int, Int )
prerequisiteByImmediateResultInputsAndTasks language setFormBoolInputMsg immediateResult =
    ( [ viewQuestionLabel language <| Translate.TestUniversalPrerequisiteQuestion PrerequisiteImmediateResult
      , viewBoolInput
            language
            immediateResult
            setFormBoolInputMsg
            "immediate-result"
            (Just ( Translate.PointOfCare, Translate.Lab ))
      ]
    , taskCompleted immediateResult
    , 1
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

                    ( testResultSection, testResultTasksCompleted, testResultTasksTotal ) =
                        if isNothing form.executionDate then
                            ( [], 0, 0 )

                        else
                            let
                                setTestResultMsg =
                                    case task of
                                        TaskHIVTest ->
                                            Just configInitial.setHIVTestResultMsg

                                        TaskMalariaTest ->
                                            Just configInitial.setMalariaTestResultMsg

                                        TaskPregnancyTest ->
                                            Just configInitial.setPregnancyTestResultMsg

                                        TaskPartnerHIVTest ->
                                            Just configInitial.setPartnerHIVTestResultMsg

                                        _ ->
                                            Nothing
                            in
                            Maybe.map
                                (\setResultMsg ->
                                    ( [ viewLabel language <| Translate.LaboratoryTaskResult task
                                      , viewCustomSelectListInput form.testResult
                                            [ TestPositive, TestNegative, TestIndeterminate ]
                                            testResultToString
                                            setResultMsg
                                            (Translate.TestResult >> translate language)
                                            "form-input select"
                                            (isNothing form.testResult)
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
        viewCustomLabel language (Translate.LaboratoryTaskLabel task) "" "label header"
            :: initialSection
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
        viewCustomLabel language (Translate.LaboratoryTaskLabel task) "" "label header"
            :: inputs
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

                                            ( partnerHIVStatusSection, partnerHIVStatusTasksCompleted, partnerHIVStatusTasksTotal ) =
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
                                            ++ partnerHIVStatusSection
                                        , taskCompleted form.partnerHIVPositive + partnerHIVStatusTasksCompleted
                                        , 1 + partnerHIVStatusTasksTotal
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
        viewCustomLabel language (Translate.LaboratoryTaskLabel TaskHIVTest) "" "label header"
            :: initialSection
            ++ derivedSection
    , initialTasksCompleted + derivedTasksCompleted
    , initialTasksTotal + derivedTasksTotal
    )


viewHIVTestUniversalForm :
    Language
    -> NominalDate
    -> ContentAndTasksLaboratoryUniversalTestInitialConfig msg
    -> ContentAndTasksForPerformedLaboratoryUniversalTestConfig msg
    -> TestResult
    -> HIVTestUniversalForm
    -> ( Html msg, Int, Int )
viewHIVTestUniversalForm language currentDate configInitial configPerformed partnerHIVTestResult form =
    let
        ( knownAsPositiveSection, knownAsPositiveTasksCompleted, knownAsPositiveTasksTotal ) =
            contentAndTasksLaboratoryUniversalTestKnownAsPositive language currentDate configInitial TaskHIVTest form

        ( derivedSection, derivedTasksCompleted, derivedTasksTotal ) =
            let
                emptySection =
                    ( [], 0, 0 )
            in
            if form.knownAsPositive == Just False then
                let
                    ( initialSection, initialTasksCompleted, initialTasksTotal ) =
                        contentAndTasksLaboratoryUniversalTestInitial language currentDate configInitial TaskHIVTest form
                in
                if form.testPerformed == Just True then
                    let
                        ( testPrerequisitesSection, testPrerequisitesTasksCompleted, testPrerequisitesTasksTotal ) =
                            prerequisiteByImmediateResultInputsAndTasks language
                                (configInitial.setHIVTestFormBoolInputMsg
                                    (\value form_ ->
                                        { form_
                                            | immediateResult = Just value
                                            , testResult = Nothing
                                            , testResultDirty = True
                                            , hivProgramHC = Nothing
                                            , hivProgramHCDirty = True
                                            , partnerHIVPositive = Nothing
                                            , partnerHIVPositiveDirty = True
                                            , partnerTakingARV = Nothing
                                            , partnerTakingARVDirty = True
                                            , partnerSurpressedViralLoad = Nothing
                                            , partnerSurpressedViralLoadDirty = True
                                        }
                                    )
                                )
                                form.immediateResult

                        ( testResultSection, testResultTasksCompleted, testResultTasksTotal ) =
                            if isNothing form.executionDate then
                                emptySection

                            else
                                Maybe.map
                                    (\immediateResult ->
                                        if immediateResult then
                                            let
                                                isLabTech =
                                                    -- Only nurses perform initial phase of prenatal encounter.
                                                    False
                                            in
                                            hivResultInputsAndTasks language
                                                isLabTech
                                                configPerformed.setHIVTestResultMsg
                                                configInitial.setHIVTestFormBoolInputMsg
                                                form.testResult
                                                partnerHIVTestResult
                                                form.hivProgramHC
                                                form.partnerHIVPositive
                                                form.partnerTakingARV
                                                form.partnerSurpressedViralLoad

                                        else
                                            ( [ viewCustomLabel language Translate.LaboratoryTaskResultsHelper "." "label" ]
                                            , 0
                                            , 0
                                            )
                                    )
                                    form.immediateResult
                                    |> Maybe.withDefault emptySection
                    in
                    ( initialSection ++ testPrerequisitesSection ++ testResultSection
                    , initialTasksCompleted + testPrerequisitesTasksCompleted + testResultTasksCompleted
                    , initialTasksTotal + testPrerequisitesTasksTotal + testResultTasksTotal
                    )

                else
                    ( initialSection, initialTasksCompleted, initialTasksTotal )

            else
                emptySection
    in
    ( div [ class "ui form laboratory hiv" ] <|
        viewCustomLabel language (Translate.LaboratoryTaskLabel TaskHIVTest) "" "label header"
            :: knownAsPositiveSection
            ++ derivedSection
    , knownAsPositiveTasksCompleted + derivedTasksCompleted
    , knownAsPositiveTasksTotal + derivedTasksTotal
    )


contentAndTasksLaboratoryResultConfirmation :
    Language
    -> NominalDate
    -> ContentAndTasksLaboratoryResultConfig msg encounterId
    -> LaboratoryTask
    ->
        { f
            | runConfirmedByLabTech : Maybe Bool
            , executionNote : Maybe TestExecutionNote
        }
    -> ( List (Html msg), Int, Int )
contentAndTasksLaboratoryResultConfirmation language currentDate config task form =
    let
        msgs =
            let
                resolveNote value =
                    if value then
                        Just TestNoteRunConfirmedByLabTech

                    else
                        Nothing
            in
            case task of
                TaskHIVTest ->
                    let
                        updateFunc =
                            \value form_ ->
                                { form_
                                    | runConfirmedByLabTech = Just value
                                    , executionNote = resolveNote value
                                    , executionNoteDirty = True
                                    , testResult = Nothing
                                    , testResultDirty = True
                                    , hivProgramHC = Nothing
                                    , hivProgramHCDirty = True
                                    , partnerHIVPositive = Nothing
                                    , partnerHIVPositiveDirty = True
                                    , partnerTakingARV = Nothing
                                    , partnerTakingARVDirty = True
                                    , partnerSurpressedViralLoad = Nothing
                                    , partnerSurpressedViralLoadDirty = True
                                }
                    in
                    { setBoolInputMsg = config.setHIVTestFormBoolInputMsg updateFunc
                    , setExecutionNoteMsg = config.setHIVTestExecutionNoteMsg
                    }

                TaskPartnerHIVTest ->
                    let
                        updateFunc =
                            \value form_ ->
                                { form_
                                    | runConfirmedByLabTech = Just value
                                    , executionNote = resolveNote value
                                    , executionNoteDirty = True
                                    , testResult = Nothing
                                    , testResultDirty = True
                                }
                    in
                    { setBoolInputMsg = config.setPartnerHIVTestFormBoolInputMsg updateFunc
                    , setExecutionNoteMsg = config.setPartnerHIVTestExecutionNoteMsg
                    }

                TaskSyphilisTest ->
                    let
                        updateFunc =
                            \value form_ ->
                                { form_
                                    | runConfirmedByLabTech = Just value
                                    , executionNote = resolveNote value
                                    , executionNoteDirty = True
                                    , testResult = Nothing
                                    , testResultDirty = True
                                    , symptoms = Nothing
                                    , symptomsDirty = True
                                }
                    in
                    { setBoolInputMsg = config.setSyphilisTestFormBoolInputMsg updateFunc
                    , setExecutionNoteMsg = config.setSyphilisTestExecutionNoteMsg
                    }

                TaskHepatitisBTest ->
                    let
                        updateFunc =
                            \value form_ ->
                                { form_
                                    | runConfirmedByLabTech = Just value
                                    , executionNote = resolveNote value
                                    , executionNoteDirty = True
                                    , testResult = Nothing
                                    , testResultDirty = True
                                }
                    in
                    { setBoolInputMsg = config.setHepatitisBTestFormBoolInputMsg updateFunc
                    , setExecutionNoteMsg = config.setHepatitisBTestExecutionNoteMsg
                    }

                TaskMalariaTest ->
                    let
                        updateFunc =
                            \value form_ ->
                                { form_
                                    | runConfirmedByLabTech = Just value
                                    , executionNote = resolveNote value
                                    , executionNoteDirty = True
                                    , testResult = Nothing
                                    , testResultDirty = True
                                    , bloodSmearResult = Nothing
                                    , bloodSmearResultDirty = True
                                }
                    in
                    { setBoolInputMsg = config.setMalariaTestFormBoolInputMsg updateFunc
                    , setExecutionNoteMsg = config.setMalariaTestExecutionNoteMsg
                    }

                TaskBloodGpRsTest ->
                    let
                        updateFunc =
                            \value form_ ->
                                { form_
                                    | runConfirmedByLabTech = Just value
                                    , executionNote = resolveNote value
                                    , executionNoteDirty = True
                                    , bloodGroup = Nothing
                                    , bloodGroupDirty = True
                                    , rhesus = Nothing
                                    , rhesusDirty = True
                                }
                    in
                    { setBoolInputMsg = config.setBloodGpRsTestFormBoolInputMsg updateFunc
                    , setExecutionNoteMsg = config.setBloodGpRsTestExecutionNoteMsg
                    }

                TaskUrineDipstickTest ->
                    let
                        updateFunc =
                            \value form_ ->
                                { form_
                                    | runConfirmedByLabTech = Just value
                                    , executionNote = resolveNote value
                                    , executionNoteDirty = True
                                    , protein = Nothing
                                    , proteinDirty = True
                                    , ph = Nothing
                                    , phDirty = True
                                    , glucose = Nothing
                                    , glucoseDirty = True
                                    , leukocytes = Nothing
                                    , leukocytesDirty = True
                                    , nitrite = Nothing
                                    , nitriteDirty = True
                                    , urobilinogen = Nothing
                                    , urobilinogenDirty = True
                                    , haemoglobin = Nothing
                                    , haemoglobinDirty = True
                                    , ketone = Nothing
                                    , ketoneDirty = True
                                    , bilirubin = Nothing
                                    , bilirubinDirty = True
                                }
                    in
                    { setBoolInputMsg = config.setUrineDipstickTestFormBoolInputMsg updateFunc
                    , setExecutionNoteMsg = config.setUrineDipstickTestExecutionNoteMsg
                    }

                TaskHemoglobinTest ->
                    let
                        updateFunc =
                            \value form_ ->
                                { form_
                                    | runConfirmedByLabTech = Just value
                                    , executionNote = resolveNote value
                                    , executionNoteDirty = True
                                    , hemoglobinCount = Nothing
                                    , hemoglobinCountDirty = True
                                }
                    in
                    { setBoolInputMsg = config.setHemoglobinTestFormBoolInputMsg updateFunc
                    , setExecutionNoteMsg = config.setHemoglobinTestExecutionNoteMsg
                    }

                TaskRandomBloodSugarTest ->
                    let
                        updateFunc =
                            \value form_ ->
                                { form_
                                    | runConfirmedByLabTech = Just value
                                    , executionNote = resolveNote value
                                    , executionNoteDirty = True
                                    , sugarCount = Nothing
                                    , sugarCountDirty = True
                                }
                    in
                    { setBoolInputMsg = config.setRandomBloodSugarTestFormBoolInputMsg updateFunc
                    , setExecutionNoteMsg = config.setRandomBloodSugarTestExecutionNoteMsg
                    }

                TaskHIVPCRTest ->
                    let
                        updateFunc =
                            \value form_ ->
                                { form_
                                    | runConfirmedByLabTech = Just value
                                    , executionNote = resolveNote value
                                    , executionNoteDirty = True
                                    , hivViralLoadStatus = Nothing
                                    , hivViralLoadStatusDirty = True
                                    , hivViralLoad = Nothing
                                    , hivViralLoadDirty = True
                                }
                    in
                    { setBoolInputMsg = config.setHIVPCRTestFormBoolInputMsg updateFunc
                    , setExecutionNoteMsg = config.setHIVPCRTestExecutionNoteMsg
                    }

                _ ->
                    -- Other tasks do not use this config, as they either
                    -- have only 'non universal form' variant, or a proprietary form.
                    { setBoolInputMsg = always config.noOpMsg
                    , setExecutionNoteMsg = always config.noOpMsg
                    }

        ( derivedSection, derivedTasksCompleted, derivedTasksTotal ) =
            Maybe.map
                (\runConfirmedByLabTech ->
                    if runConfirmedByLabTech then
                        ( [], 0, 0 )

                    else
                        let
                            rightOptions =
                                case task of
                                    TaskPartnerHIVTest ->
                                        [ TestNoteNoEquipment
                                        , TestNoteNotPresent
                                        , TestNoteNotIndicated
                                        ]

                                    _ ->
                                        [ TestNoteNoEquipment
                                        , TestNoteNotIndicated
                                        ]
                        in
                        ( [ div [ class "why-not" ]
                                [ viewQuestionLabel language Translate.WhyNot
                                , viewCheckBoxSelectInput language
                                    [ TestNoteLackOfReagents
                                    , TestNoteLackOfOtherSupplies
                                    , TestNoteBrokenEquipment
                                    ]
                                    rightOptions
                                    form.executionNote
                                    msgs.setExecutionNoteMsg
                                    Translate.TestExecutionNote
                                ]
                          ]
                        , taskCompleted form.executionNote
                        , 1
                        )
                )
                form.runConfirmedByLabTech
                |> Maybe.withDefault ( [], 0, 0 )
    in
    ( [ viewQuestionLabel language Translate.TestWillBePerformedTodayQuestion
      , viewBoolInput
            language
            form.runConfirmedByLabTech
            msgs.setBoolInputMsg
            "test-performed"
            Nothing
      ]
        ++ derivedSection
    , taskCompleted form.runConfirmedByLabTech + derivedTasksCompleted
    , 1 + derivedTasksTotal
    )


hivResultFormAndTasks :
    Language
    -> NominalDate
    -> Bool
    -> ContentAndTasksLaboratoryResultConfig msg encounterId
    -> (String -> msg)
    -> ((Bool -> HIVResultForm -> HIVResultForm) -> Bool -> msg)
    -> TestResult
    -> HIVResultForm
    -> ( Html msg, Int, Int )
hivResultFormAndTasks language currentDate isLabTech config setHIVTestResultMsg setHIVTestFormBoolInputMsg partnerHIVTestResult form =
    let
        ( confirmationSection, confirmationTasksCompleted, confirmationTasksTotal ) =
            if isLabTech then
                contentAndTasksLaboratoryResultConfirmation language currentDate config TaskHIVTest form

            else
                ( [], 0, 0 )

        ( testResultSection, testResultTasksCompleted, testResultTasksTotal ) =
            if not isLabTech || form.runConfirmedByLabTech == Just True then
                hivResultInputsAndTasks language
                    isLabTech
                    setHIVTestResultMsg
                    setHIVTestFormBoolInputMsg
                    form.testResult
                    partnerHIVTestResult
                    form.hivProgramHC
                    form.partnerHIVPositive
                    form.partnerTakingARV
                    form.partnerSurpressedViralLoad

            else
                ( [], 0, 0 )
    in
    ( div [ class "ui form laboratory prenatal-test-result" ] <|
        resultFormHeaderSection language currentDate form.executionDate TaskHIVTest
            ++ confirmationSection
            ++ testResultSection
    , confirmationTasksCompleted + testResultTasksCompleted
    , confirmationTasksTotal + testResultTasksTotal
    )


hivResultFollowUpsFormAndTasks :
    Language
    -> NominalDate
    -> ((Bool -> HIVResultForm -> HIVResultForm) -> Bool -> msg)
    -> TestResult
    -> HIVResultForm
    -> ( Html msg, Int, Int )
hivResultFollowUpsFormAndTasks language currentDate setHIVTestFormBoolInputMsg partnerHIVTestResult form =
    let
        ( followUpsSection, followUpsTasksCompleted, followUpsTasksTotal ) =
            hivResultFollowUpInputsAndTasks language
                setHIVTestFormBoolInputMsg
                form.testResult
                partnerHIVTestResult
                form.hivProgramHC
                form.partnerHIVPositive
                form.partnerTakingARV
                form.partnerSurpressedViralLoad
    in
    ( div [ class "ui form laboratory prenatal-test-result" ] <|
        resultFormHeaderSection language currentDate form.executionDate TaskHIVTest
            ++ followUpsSection
    , followUpsTasksCompleted
    , followUpsTasksTotal
    )


hivResultInputsAndTasks :
    Language
    -> Bool
    -> (String -> msg)
    ->
        ((Bool
          ->
            { f
                | hivProgramHC : Maybe Bool
                , hivProgramHCDirty : Bool
                , partnerHIVPositive : Maybe Bool
                , partnerHIVPositiveDirty : Bool
                , partnerSurpressedViralLoad : Maybe Bool
                , partnerSurpressedViralLoadDirty : Bool
                , partnerTakingARV : Maybe Bool
                , partnerTakingARVDirty : Bool
            }
          ->
            { f
                | hivProgramHC : Maybe Bool
                , hivProgramHCDirty : Bool
                , partnerHIVPositive : Maybe Bool
                , partnerHIVPositiveDirty : Bool
                , partnerSurpressedViralLoad : Maybe Bool
                , partnerSurpressedViralLoadDirty : Bool
                , partnerTakingARV : Maybe Bool
                , partnerTakingARVDirty : Bool
            }
         )
         -> Bool
         -> msg
        )
    -> Maybe TestResult
    -> TestResult
    -> Maybe Bool
    -> Maybe Bool
    -> Maybe Bool
    -> Maybe Bool
    -> ( List (Html msg), Int, Int )
hivResultInputsAndTasks language isLabTech setTestResultMsg setHIVTestFormBoolInputMsg testResult partnerHIVTestResult hivProgramHC partnerHIVPositive partnerTakingARV partnerSurpressedViralLoad =
    let
        ( testResultSection, testResultTasksCompleted, testResultTasksTotal ) =
            standardTestResultInputsAndTasks language setTestResultMsg testResult TaskHIVTest

        ( hivSignsSection, hivSignsTasksCompleted, hivSignsTasksTotal ) =
            if not isLabTech then
                hivResultFollowUpInputsAndTasks language
                    setHIVTestFormBoolInputMsg
                    testResult
                    partnerHIVTestResult
                    hivProgramHC
                    partnerHIVPositive
                    partnerTakingARV
                    partnerSurpressedViralLoad

            else
                ( [], 0, 0 )
    in
    ( testResultSection
        ++ hivSignsSection
    , testResultTasksCompleted + hivSignsTasksCompleted
    , testResultTasksTotal + hivSignsTasksTotal
    )


hivResultFollowUpInputsAndTasks :
    Language
    ->
        ((Bool
          ->
            { f
                | hivProgramHC : Maybe Bool
                , hivProgramHCDirty : Bool
                , partnerHIVPositive : Maybe Bool
                , partnerHIVPositiveDirty : Bool
                , partnerSurpressedViralLoad : Maybe Bool
                , partnerSurpressedViralLoadDirty : Bool
                , partnerTakingARV : Maybe Bool
                , partnerTakingARVDirty : Bool
            }
          ->
            { f
                | hivProgramHC : Maybe Bool
                , hivProgramHCDirty : Bool
                , partnerHIVPositive : Maybe Bool
                , partnerHIVPositiveDirty : Bool
                , partnerSurpressedViralLoad : Maybe Bool
                , partnerSurpressedViralLoadDirty : Bool
                , partnerTakingARV : Maybe Bool
                , partnerTakingARVDirty : Bool
            }
         )
         -> Bool
         -> msg
        )
    -> Maybe TestResult
    -> TestResult
    -> Maybe Bool
    -> Maybe Bool
    -> Maybe Bool
    -> Maybe Bool
    -> ( List (Html msg), Int, Int )
hivResultFollowUpInputsAndTasks language setHIVTestFormBoolInputMsg testResult partnerHIVTestResult hivProgramHC partnerHIVPositiveByForm partnerTakingARV partnerSurpressedViralLoad =
    let
        emptySection =
            ( [], 0, 0 )
    in
    Maybe.map
        (\testResult_ ->
            case testResult_ of
                TestPositive ->
                    let
                        updateFunc =
                            \value form_ ->
                                { form_ | hivProgramHC = Just value, hivProgramHCDirty = True }
                    in
                    ( [ viewQuestionLabel language <| Translate.PrenatalHIVSignQuestion HIVProgramHC
                      , viewBoolInput
                            language
                            hivProgramHC
                            (setHIVTestFormBoolInputMsg updateFunc)
                            "hiv-program"
                            Nothing
                      ]
                    , taskCompleted hivProgramHC
                    , 1
                    )

                TestNegative ->
                    let
                        ( partnerHIVPositiveSection, partnerHIVPositiveTasksCompleted, partnerHIVPositiveTasksTotal ) =
                            if partnerHIVTestResult == TestIndeterminate then
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
                                in
                                ( [ viewQuestionLabel language <| Translate.PrenatalHIVSignQuestion PartnerHIVPositive
                                  , viewBoolInput
                                        language
                                        partnerHIVPositiveByForm
                                        (setHIVTestFormBoolInputMsg partnerHIVPositiveUpdateFunc)
                                        "partner-hiv-positive"
                                        Nothing
                                  ]
                                , taskCompleted partnerHIVPositiveByForm
                                , 1
                                )

                            else
                                emptySection

                        ( partnerHIVStatusSection, partnerHIVStatusTasksCompleted, partnerHIVStatusTasksTotal ) =
                            if partnerHIVPositiveByForm == Just True then
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
                                        if partnerTakingARV == Just True then
                                            let
                                                partnerSurpressedViralLoadUpdateFunc =
                                                    \value form_ ->
                                                        { form_ | partnerSurpressedViralLoad = Just value, partnerSurpressedViralLoadDirty = True }
                                            in
                                            ( [ viewQuestionLabel language <| Translate.PrenatalHIVSignQuestion PartnerSurpressedViralLoad
                                              , viewBoolInput
                                                    language
                                                    partnerSurpressedViralLoad
                                                    (setHIVTestFormBoolInputMsg partnerSurpressedViralLoadUpdateFunc)
                                                    "partner-surpressed-viral-load"
                                                    Nothing
                                              ]
                                            , taskCompleted partnerSurpressedViralLoad
                                            , 1
                                            )

                                        else
                                            emptySection
                                in
                                ( [ viewQuestionLabel language <| Translate.PrenatalHIVSignQuestion PartnerTakingARV
                                  , viewBoolInput
                                        language
                                        partnerTakingARV
                                        (setHIVTestFormBoolInputMsg partnerTakingARVUpdateFunc)
                                        "partner-taking-arv"
                                        Nothing
                                  ]
                                    ++ partnerARVSection
                                , taskCompleted partnerTakingARV + partnerARVTasksCompleted
                                , 1 + partnerARVTasksTotal
                                )

                            else
                                emptySection
                    in
                    ( partnerHIVPositiveSection ++ partnerHIVStatusSection
                    , partnerHIVPositiveTasksCompleted + partnerHIVStatusTasksCompleted
                    , partnerHIVPositiveTasksTotal + partnerHIVStatusTasksTotal
                    )

                TestIndeterminate ->
                    emptySection
        )
        testResult
        |> Maybe.withDefault emptySection


viewMalariaTestForm :
    Language
    -> NominalDate
    -> ContentAndTasksLaboratoryUniversalTestInitialConfig msg
    -> ContentAndTasksForPerformedLaboratoryUniversalTestConfig msg
    -> MalariaTestForm
    -> ( Html msg, Int, Int )
viewMalariaTestForm language currentDate configInitial configPerformed form =
    let
        ( initialSection, initialTasksCompleted, initialTasksTotal ) =
            contentAndTasksLaboratoryUniversalTestInitial language currentDate configInitial TaskMalariaTest form

        ( derivedSection, derivedTasksCompleted, derivedTasksTotal ) =
            let
                emptySection =
                    ( [], 0, 0 )
            in
            if form.testPerformed == Just True then
                let
                    ( testPrerequisitesSection, testPrerequisitesTasksCompleted, testPrerequisitesTasksTotal ) =
                        prerequisiteByImmediateResultInputsAndTasks language
                            (configInitial.setMalariaTestFormBoolInputMsg
                                (\value form_ ->
                                    { form_
                                        | immediateResult = Just value
                                        , testResult = Nothing
                                        , testResultDirty = True
                                        , bloodSmearTaken = Nothing
                                        , bloodSmearTakenDirty = True
                                        , bloodSmearResult = Nothing
                                        , bloodSmearResultDirty = True
                                    }
                                )
                            )
                            form.immediateResult

                    ( testResultSection, testResultTasksCompleted, testResultTasksTotal ) =
                        if isNothing form.executionDate then
                            emptySection

                        else
                            Maybe.map
                                (\immediateResult ->
                                    if immediateResult then
                                        standardTestResultInputsAndTasks language
                                            configPerformed.setMalariaTestResultMsg
                                            form.testResult
                                            TaskMalariaTest

                                    else
                                        ( [ viewCustomLabel language Translate.LaboratoryTaskResultsHelper "." "label" ]
                                        , 0
                                        , 0
                                        )
                                )
                                form.immediateResult
                                |> Maybe.withDefault emptySection
                in
                ( testPrerequisitesSection ++ testResultSection
                , testPrerequisitesTasksCompleted + testResultTasksCompleted
                , testPrerequisitesTasksTotal + testResultTasksTotal
                )

            else if form.testPerformed == Just False && isJust form.executionNote then
                let
                    ( testPrerequisitesSection_, testPrerequisitesTasksCompleted_, testPrerequisitesTasksTotal_ ) =
                        prerequisiteByImmediateResultInputsAndTasks language
                            (configInitial.setMalariaTestFormBoolInputMsg
                                (\value form_ ->
                                    { form_
                                        | immediateResult = Just value
                                        , bloodSmearResult = Nothing
                                        , bloodSmearResultDirty = True
                                    }
                                )
                            )
                            form.immediateResult

                    ( bloodSmearSection, bloodSmearTasksCompleted, bloodSmearTasksTotal ) =
                        if form.bloodSmearTaken == Just True then
                            let
                                ( bloodSmearResultSection, bloodSmearResultTasksCompleted, bloodSmearResultTasksTotal ) =
                                    Maybe.map
                                        (\immediateResult ->
                                            if immediateResult then
                                                bloodSmearResultInputsAndTasks language
                                                    Translate.TestResultQuestion
                                                    configPerformed.setBloodSmearResultMsg
                                                    form.bloodSmearResult

                                            else
                                                ( [ viewCustomLabel language Translate.LaboratoryTaskResultsHelper "." "label" ]
                                                , 0
                                                , 0
                                                )
                                        )
                                        form.immediateResult
                                        |> Maybe.withDefault emptySection
                            in
                            ( testPrerequisitesSection_
                                ++ bloodSmearResultSection
                            , testPrerequisitesTasksCompleted_ + bloodSmearResultTasksCompleted
                            , testPrerequisitesTasksTotal_ + bloodSmearResultTasksTotal
                            )

                        else
                            emptySection

                    updateFunc =
                        \value form_ ->
                            { form_
                                | bloodSmearTaken = Just value
                                , bloodSmearTakenDirty = True
                                , bloodSmearResult = Nothing
                                , bloodSmearResultDirty = True
                            }
                in
                ( [ viewQuestionLabel language Translate.BloodSmearQuestion
                  , viewBoolInput
                        language
                        form.bloodSmearTaken
                        (configInitial.setMalariaTestFormBoolInputMsg updateFunc)
                        "got-results-previously"
                        Nothing
                  ]
                    ++ bloodSmearSection
                , taskCompleted form.bloodSmearTaken + bloodSmearTasksCompleted
                , 1 + bloodSmearTasksTotal
                )

            else
                emptySection
    in
    ( div [ class "ui form laboratory malaria" ] <|
        viewCustomLabel language (Translate.LaboratoryTaskLabel TaskMalariaTest) "" "label header"
            :: initialSection
            ++ derivedSection
    , initialTasksCompleted + derivedTasksCompleted
    , initialTasksTotal + derivedTasksTotal
    )


malariaResultFormAndTasks :
    Language
    -> NominalDate
    -> Bool
    -> ContentAndTasksLaboratoryResultConfig msg encounterId
    -> (String -> msg)
    -> (String -> msg)
    -> MalariaResultForm
    -> ( Html msg, Int, Int )
malariaResultFormAndTasks language currentDate isLabTech config setMalariaTestResultMsg setBloodSmearResultMsg form =
    let
        ( confirmationSection, confirmationTasksCompleted, confirmationTasksTotal ) =
            if isLabTech then
                contentAndTasksLaboratoryResultConfirmation language currentDate config TaskMalariaTest form

            else
                ( [], 0, 0 )

        ( testResultSection, testResultTasksCompleted, testResultTasksTotal ) =
            if not isLabTech || form.runConfirmedByLabTech == Just True then
                if form.bloodSmearTaken then
                    bloodSmearResultInputsAndTasks language
                        Translate.BloodSmearTestResult
                        setBloodSmearResultMsg
                        form.bloodSmearResult

                else
                    standardTestResultInputsAndTasks language setMalariaTestResultMsg form.testResult TaskMalariaTest

            else
                ( [], 0, 0 )
    in
    ( div [ class "ui form laboratory prenatal-test-result" ] <|
        resultFormHeaderSection language currentDate form.executionDate TaskMalariaTest
            ++ confirmationSection
            ++ testResultSection
    , confirmationTasksCompleted + testResultTasksCompleted
    , confirmationTasksTotal + testResultTasksTotal
    )


bloodSmearResultInputsAndTasks :
    Language
    -> TranslationId
    -> (String -> msg)
    -> Maybe BloodSmearResult
    -> ( List (Html msg), Int, Int )
bloodSmearResultInputsAndTasks language questionTransId setBloodSmearResultMsg bloodSmearResult =
    ( viewSelectInput language
        questionTransId
        bloodSmearResult
        Translate.BloodSmearResult
        bloodSmearResultToString
        [ BloodSmearNegative
        , BloodSmearPlus
        , BloodSmearPlusPlus
        , BloodSmearPlusPlusPlus
        ]
        setBloodSmearResultMsg
    , taskCompleted bloodSmearResult
    , 1
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
        viewCustomLabel language (Translate.LaboratoryTaskLabel TaskPregnancyTest) "" "label header"
            :: initialSection
            ++ derivedSection
    , initialTasksCompleted + derivedTasksCompleted
    , initialTasksTotal + derivedTasksTotal
    )


viewPartnerHIVTestForm :
    Language
    -> NominalDate
    -> ContentAndTasksLaboratoryUniversalTestInitialConfig msg
    -> ContentAndTasksForPerformedLaboratoryUniversalTestConfig msg
    -> PartnerHIVTestForm
    -> ( Html msg, Int, Int )
viewPartnerHIVTestForm language currentDate configInitial configPerformed form =
    let
        isLabTech =
            -- Only nurses perform initial phase of prenatal encounter.
            False

        ( knownAsPositiveSection, knownAsPositiveTasksCompleted, knownAsPositiveTasksTotal ) =
            contentAndTasksLaboratoryUniversalTestKnownAsPositive language currentDate configInitial TaskPartnerHIVTest form

        ( derivedSection, derivedTasksCompleted, derivedTasksTotal ) =
            let
                emptySection =
                    ( [], 0, 0 )
            in
            Maybe.map
                (\knownAsPositive ->
                    if knownAsPositive then
                        partnerARVViralLoadInputsAndTasks language
                            configInitial.setPartnerHIVTestFormBoolInputMsg
                            form.partnerTakingARV
                            form.partnerSurpressedViralLoad

                    else
                        let
                            ( initialSection, initialTasksCompleted, initialTasksTotal ) =
                                contentAndTasksLaboratoryUniversalTestInitial language currentDate configInitial TaskPartnerHIVTest form
                        in
                        if form.testPerformed == Just True then
                            let
                                ( testPrerequisitesSection, testPrerequisitesTasksCompleted, testPrerequisitesTasksTotal ) =
                                    prerequisiteByImmediateResultInputsAndTasks language
                                        (configInitial.setPartnerHIVTestFormBoolInputMsg
                                            (\value form_ ->
                                                { form_
                                                    | immediateResult = Just value
                                                    , testResult = Nothing
                                                    , testResultDirty = True
                                                }
                                            )
                                        )
                                        form.immediateResult

                                ( testResultSection, testResultTasksCompleted, testResultTasksTotal ) =
                                    if isNothing form.executionDate then
                                        emptySection

                                    else
                                        Maybe.map
                                            (\immediateResult ->
                                                if immediateResult == True then
                                                    partnerHIVResultInputsAndTasks language
                                                        isLabTech
                                                        configPerformed.setPartnerHIVTestResultMsg
                                                        configInitial.setPartnerHIVTestFormBoolInputMsg
                                                        form.testResult
                                                        form.partnerTakingARV
                                                        form.partnerSurpressedViralLoad

                                                else
                                                    ( [ viewCustomLabel language Translate.LaboratoryTaskResultsHelper "." "label" ]
                                                    , 0
                                                    , 0
                                                    )
                                            )
                                            form.immediateResult
                                            |> Maybe.withDefault emptySection
                            in
                            ( initialSection ++ testPrerequisitesSection ++ testResultSection
                            , initialTasksCompleted + testPrerequisitesTasksCompleted + testResultTasksCompleted
                            , initialTasksTotal + testPrerequisitesTasksTotal + testResultTasksTotal
                            )

                        else
                            ( initialSection, initialTasksCompleted, initialTasksTotal )
                )
                form.knownAsPositive
                |> Maybe.withDefault emptySection
    in
    ( div [ class "ui form laboratory urine-dipstick" ] <|
        viewCustomLabel language (Translate.LaboratoryTaskLabel TaskPartnerHIVTest) "" "label header"
            :: knownAsPositiveSection
            ++ derivedSection
    , knownAsPositiveTasksCompleted + derivedTasksCompleted
    , knownAsPositiveTasksTotal + derivedTasksTotal
    )


partnerARVViralLoadInputsAndTasks :
    Language
    ->
        ((Bool
          ->
            { f
                | partnerSurpressedViralLoad : Maybe Bool
                , partnerSurpressedViralLoadDirty : Bool
                , partnerTakingARV : Maybe Bool
                , partnerTakingARVDirty : Bool
            }
          ->
            { f
                | partnerSurpressedViralLoad : Maybe Bool
                , partnerSurpressedViralLoadDirty : Bool
                , partnerTakingARV : Maybe Bool
                , partnerTakingARVDirty : Bool
            }
         )
         -> Bool
         -> msg
        )
    -> Maybe Bool
    -> Maybe Bool
    -> ( List (Html msg), Int, Int )
partnerARVViralLoadInputsAndTasks language setPartnerHIVTestFormBoolInputMsg partnerTakingARV partnerSurpressedViralLoad =
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
            if partnerTakingARV == Just True then
                let
                    partnerSurpressedViralLoadUpdateFunc =
                        \value form_ ->
                            { form_ | partnerSurpressedViralLoad = Just value, partnerSurpressedViralLoadDirty = True }
                in
                ( [ viewQuestionLabel language <| Translate.PrenatalHIVSignQuestion PartnerSurpressedViralLoad
                  , viewBoolInput
                        language
                        partnerSurpressedViralLoad
                        (setPartnerHIVTestFormBoolInputMsg partnerSurpressedViralLoadUpdateFunc)
                        "partner-surpressed-viral-load"
                        Nothing
                  ]
                , taskCompleted partnerSurpressedViralLoad
                , 1
                )

            else
                ( [], 0, 0 )
    in
    ( [ viewQuestionLabel language <| Translate.PrenatalHIVSignQuestion PartnerTakingARV
      , viewBoolInput
            language
            partnerTakingARV
            (setPartnerHIVTestFormBoolInputMsg partnerTakingARVUpdateFunc)
            "partner-taking-arv"
            Nothing
      ]
        ++ partnerARVSection
    , taskCompleted partnerTakingARV + partnerARVTasksCompleted
    , 1 + partnerARVTasksTotal
    )


partnerHIVResultFormAndTasks :
    Language
    -> NominalDate
    -> Bool
    -> ContentAndTasksLaboratoryResultConfig msg encounterId
    -> (String -> msg)
    -> ((Bool -> PartnerHIVResultForm -> PartnerHIVResultForm) -> Bool -> msg)
    -> PartnerHIVResultForm
    -> ( Html msg, Int, Int )
partnerHIVResultFormAndTasks language currentDate isLabTech config setPartnerHIVTestResultMsg setFormBoolInputMsg form =
    let
        ( confirmationSection, confirmationTasksCompleted, confirmationTasksTotal ) =
            if isLabTech then
                contentAndTasksLaboratoryResultConfirmation language currentDate config TaskPartnerHIVTest form

            else
                ( [], 0, 0 )

        ( testResultSection, testResultTasksCompleted, testResultTasksTotal ) =
            if not isLabTech || form.runConfirmedByLabTech == Just True then
                partnerHIVResultInputsAndTasks language
                    isLabTech
                    setPartnerHIVTestResultMsg
                    setFormBoolInputMsg
                    form.testResult
                    form.partnerTakingARV
                    form.partnerSurpressedViralLoad

            else
                ( [], 0, 0 )
    in
    ( div [ class "ui form laboratory prenatal-test-result" ] <|
        resultFormHeaderSection language currentDate form.executionDate TaskPartnerHIVTest
            ++ confirmationSection
            ++ testResultSection
    , confirmationTasksCompleted + testResultTasksCompleted
    , confirmationTasksTotal + testResultTasksTotal
    )


partnerHIVResultFollowUpsFormAndTasks :
    Language
    -> NominalDate
    -> ((Bool -> PartnerHIVResultForm -> PartnerHIVResultForm) -> Bool -> msg)
    -> PartnerHIVResultForm
    -> ( Html msg, Int, Int )
partnerHIVResultFollowUpsFormAndTasks language currentDate setFormBoolInputMsg form =
    let
        ( followUpsSection, followUpsTasksCompleted, followUpsTasksTotal ) =
            partnerHIVResultFollowUpInputsAndTasks language
                setFormBoolInputMsg
                form.testResult
                form.partnerTakingARV
                form.partnerSurpressedViralLoad
    in
    ( div [ class "ui form laboratory prenatal-test-result" ] <|
        resultFormHeaderSection language currentDate form.executionDate TaskPartnerHIVTest
            ++ followUpsSection
    , followUpsTasksCompleted
    , followUpsTasksTotal
    )


partnerHIVResultInputsAndTasks :
    Language
    -> Bool
    -> (String -> msg)
    ->
        ((Bool
          ->
            { f
                | partnerSurpressedViralLoad : Maybe Bool
                , partnerSurpressedViralLoadDirty : Bool
                , partnerTakingARV : Maybe Bool
                , partnerTakingARVDirty : Bool
            }
          ->
            { f
                | partnerSurpressedViralLoad : Maybe Bool
                , partnerSurpressedViralLoadDirty : Bool
                , partnerTakingARV : Maybe Bool
                , partnerTakingARVDirty : Bool
            }
         )
         -> Bool
         -> msg
        )
    -> Maybe TestResult
    -> Maybe Bool
    -> Maybe Bool
    -> ( List (Html msg), Int, Int )
partnerHIVResultInputsAndTasks language isLabTech setTestResultMsg setFormBoolInputMsg testResult partnerTakingARV partnerSurpressedViralLoad =
    let
        ( testResultSection, testResultTasksCompleted, testResultTasksTotal ) =
            standardTestResultInputsAndTasks language setTestResultMsg testResult TaskPartnerHIVTest

        ( hivSignsSection, hivSignsTasksCompleted, hivSignsTasksTotal ) =
            if not isLabTech then
                partnerHIVResultFollowUpInputsAndTasks language
                    setFormBoolInputMsg
                    testResult
                    partnerTakingARV
                    partnerSurpressedViralLoad

            else
                ( [], 0, 0 )
    in
    ( testResultSection
        ++ hivSignsSection
    , testResultTasksCompleted + hivSignsTasksCompleted
    , testResultTasksTotal + hivSignsTasksTotal
    )


partnerHIVResultFollowUpInputsAndTasks :
    Language
    ->
        ((Bool
          ->
            { f
                | partnerSurpressedViralLoad : Maybe Bool
                , partnerSurpressedViralLoadDirty : Bool
                , partnerTakingARV : Maybe Bool
                , partnerTakingARVDirty : Bool
            }
          ->
            { f
                | partnerSurpressedViralLoad : Maybe Bool
                , partnerSurpressedViralLoadDirty : Bool
                , partnerTakingARV : Maybe Bool
                , partnerTakingARVDirty : Bool
            }
         )
         -> Bool
         -> msg
        )
    -> Maybe TestResult
    -> Maybe Bool
    -> Maybe Bool
    -> ( List (Html msg), Int, Int )
partnerHIVResultFollowUpInputsAndTasks language setFormBoolInputMsg testResult partnerTakingARV partnerSurpressedViralLoad =
    let
        emptySection =
            ( [], 0, 0 )
    in
    Maybe.map
        (\testResult_ ->
            case testResult_ of
                TestPositive ->
                    partnerARVViralLoadInputsAndTasks language
                        setFormBoolInputMsg
                        partnerTakingARV
                        partnerSurpressedViralLoad

                _ ->
                    emptySection
        )
        testResult
        |> Maybe.withDefault emptySection


viewUrineDipstickTestForm :
    Language
    -> NominalDate
    -> ContentAndTasksLaboratoryTestInitialConfig msg
    -> ContentAndTasksForPerformedLaboratoryTestConfig msg
    -> UrineDipstickTestForm msg
    -> ( Html msg, Int, Int )
viewUrineDipstickTestForm language currentDate configInitial configPerformed form =
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
        viewCustomLabel language (Translate.LaboratoryTaskLabel TaskUrineDipstickTest) "" "label header"
            :: initialSection
            ++ derivedSection
    , initialTasksCompleted + derivedTasksCompleted
    , initialTasksTotal + derivedTasksTotal
    )


viewUrineDipstickTestUniversalForm :
    Language
    -> NominalDate
    -> ContentAndTasksLaboratoryUniversalTestInitialConfig msg
    -> ContentAndTasksForPerformedLaboratoryUniversalTestConfig msg
    -> UrineDipstickTestUniversalForm
    -> ( Html msg, Int, Int )
viewUrineDipstickTestUniversalForm language currentDate configInitial configPerformed form =
    let
        ( initialSection, initialTasksCompleted, initialTasksTotal ) =
            contentAndTasksLaboratoryUniversalTestInitial language currentDate configInitial TaskUrineDipstickTest form

        ( derivedSection, derivedTasksCompleted, derivedTasksTotal ) =
            let
                emptySection =
                    ( [], 0, 0 )
            in
            if form.testPerformed == Just True then
                let
                    ( testPrerequisitesSection, testPrerequisitesTasksCompleted, testPrerequisitesTasksTotal ) =
                        prerequisiteByImmediateResultInputsAndTasks language
                            (configInitial.setUrineDipstickTestFormBoolInputMsg
                                (\value form_ ->
                                    { form_
                                        | immediateResult = Just value
                                        , testVariant = Nothing
                                        , testVariantDirty = True
                                        , protein = Nothing
                                        , proteinDirty = True
                                        , ph = Nothing
                                        , phDirty = True
                                        , glucose = Nothing
                                        , glucoseDirty = True
                                        , leukocytes = Nothing
                                        , leukocytesDirty = True
                                        , nitrite = Nothing
                                        , nitriteDirty = True
                                        , urobilinogen = Nothing
                                        , urobilinogenDirty = True
                                        , haemoglobin = Nothing
                                        , haemoglobinDirty = True
                                        , ketone = Nothing
                                        , ketoneDirty = True
                                        , bilirubin = Nothing
                                        , bilirubinDirty = True
                                    }
                                )
                            )
                            form.immediateResult

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

                    ( testResultSection, testResultTasksCompleted, testResultTasksTotal ) =
                        Maybe.map
                            (\immediateResult ->
                                if immediateResult then
                                    urineDipstickResultInputsAndTasks language
                                        configPerformed.setProteinMsg
                                        configPerformed.setPHMsg
                                        configPerformed.setGlucoseMsg
                                        configPerformed.setLeukocytesMsg
                                        configPerformed.setNitriteMsg
                                        configPerformed.setUrobilinogenMsg
                                        configPerformed.setHaemoglobinMsg
                                        configPerformed.setKetoneMsg
                                        configPerformed.setBilirubinMsg
                                        form.protein
                                        form.ph
                                        form.glucose
                                        form.leukocytes
                                        form.nitrite
                                        form.urobilinogen
                                        form.haemoglobin
                                        form.ketone
                                        form.bilirubin
                                        form.testVariant

                                else
                                    ( [ viewCustomLabel language Translate.LaboratoryTaskResultsHelper "." "label" ]
                                    , 0
                                    , 0
                                    )
                            )
                            form.immediateResult
                            |> Maybe.withDefault emptySection
                in
                ( testPrerequisitesSection ++ testVariantSection ++ testResultSection
                , testPrerequisitesTasksCompleted + testVariantTasksCompleted + testResultTasksCompleted
                , testPrerequisitesTasksTotal + testVariantTasksTotal + testResultTasksTotal
                )

            else
                emptySection
    in
    ( div [ class "ui form laboratory urine-dipstick" ] <|
        viewCustomLabel language (Translate.LaboratoryTaskLabel TaskUrineDipstickTest) "" "label header"
            :: initialSection
            ++ derivedSection
    , initialTasksCompleted + derivedTasksCompleted
    , initialTasksTotal + derivedTasksTotal
    )


urineDipstickResultFormAndTasks :
    Language
    -> NominalDate
    -> Bool
    -> ContentAndTasksLaboratoryResultConfig msg encounterId
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
urineDipstickResultFormAndTasks language currentDate isLabTech config setProteinMsg setPHMsg setGlucoseMsg setLeukocytesMsg setNitriteMsg setUrobilinogenMsg setHaemoglobinMsg setKetoneMsg setBilirubinMsg form =
    let
        ( confirmationSection, confirmationTasksCompleted, confirmationTasksTotal ) =
            if isLabTech then
                contentAndTasksLaboratoryResultConfirmation language currentDate config TaskUrineDipstickTest form

            else
                ( [], 0, 0 )

        ( testResultSection, testResultTasksCompleted, testResultTasksTotal ) =
            if not isLabTech || form.runConfirmedByLabTech == Just True then
                urineDipstickResultInputsAndTasks language
                    setProteinMsg
                    setPHMsg
                    setGlucoseMsg
                    setLeukocytesMsg
                    setNitriteMsg
                    setUrobilinogenMsg
                    setHaemoglobinMsg
                    setKetoneMsg
                    setBilirubinMsg
                    form.protein
                    form.ph
                    form.glucose
                    form.leukocytes
                    form.nitrite
                    form.urobilinogen
                    form.haemoglobin
                    form.ketone
                    form.bilirubin
                    form.testVariant

            else
                ( [], 0, 0 )
    in
    ( div [ class "ui form laboratory urine-dipstick-result" ] <|
        resultFormHeaderSection language currentDate form.executionDate TaskUrineDipstickTest
            ++ confirmationSection
            ++ testResultSection
    , confirmationTasksCompleted + testResultTasksCompleted
    , confirmationTasksTotal + testResultTasksTotal
    )


urineDipstickResultInputsAndTasks :
    Language
    -> (String -> msg)
    -> (String -> msg)
    -> (String -> msg)
    -> (String -> msg)
    -> (String -> msg)
    -> (String -> msg)
    -> (String -> msg)
    -> (String -> msg)
    -> (String -> msg)
    -> Maybe ProteinValue
    -> Maybe PHValue
    -> Maybe GlucoseValue
    -> Maybe LeukocytesValue
    -> Maybe NitriteValue
    -> Maybe UrobilinogenValue
    -> Maybe HaemoglobinValue
    -> Maybe KetoneValue
    -> Maybe BilirubinValue
    -> Maybe TestVariant
    -> ( List (Html msg), Int, Int )
urineDipstickResultInputsAndTasks language setProteinMsg setPHMsg setGlucoseMsg setLeukocytesMsg setNitriteMsg setUrobilinogenMsg setHaemoglobinMsg setKetoneMsg setBilirubinMsg protein ph glucose leukocytes nitrite urobilinogen haemoglobin ketone bilirubin =
    Maybe.map
        (\testVariant ->
            let
                ( commonSection, commonTasksCompleted, commonTasksTotal ) =
                    ( viewSelectInput language
                        Translate.LaboratoryProteinTestResult
                        protein
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
                            ph
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
                            glucose
                            Translate.LaboratoryGlucoseValue
                            glucoseValueToString
                            [ Glucose0
                            , GlucosePlus1
                            , GlucosePlus2
                            , GlucosePlus3
                            , GlucosePlus4
                            ]
                            setGlucoseMsg
                    , taskCompleted protein + taskCompleted ph + taskCompleted glucose
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
                            leukocytes
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
                            nitrite
                            Translate.LaboratoryNitriteValue
                            nitriteValueToString
                            [ NitriteNegative
                            , NitritePlus
                            , NitritePlusPlus
                            ]
                            setNitriteMsg
                        ++ viewSelectInput language
                            Translate.LaboratoryUrobilinogenTestResult
                            urobilinogen
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
                            haemoglobin
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
                            ketone
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
                            bilirubin
                            Translate.LaboratoryBilirubinValue
                            bilirubinValueToString
                            [ BilirubinNegative
                            , BilirubinSmall
                            , BilirubinMedium
                            , BilirubinLarge
                            ]
                            setBilirubinMsg
                    , commonTasksCompleted
                        + taskCompleted leukocytes
                        + taskCompleted nitrite
                        + taskCompleted urobilinogen
                        + taskCompleted haemoglobin
                        + taskCompleted ketone
                        + taskCompleted bilirubin
                    , commonTasksTotal + 6
                    )
        )
        >> Maybe.withDefault ( [], 0, 0 )


viewRandomBloodSugarTestForm :
    Language
    -> NominalDate
    -> ContentAndTasksLaboratoryTestInitialConfig msg
    -> ContentAndTasksForPerformedLaboratoryTestConfig msg
    -> RandomBloodSugarTestForm msg
    -> ( Html msg, Int, Int )
viewRandomBloodSugarTestForm language currentDate configInitial configPerformed form =
    let
        ( initialSection, initialTasksCompleted, initialTasksTotal ) =
            contentAndTasksLaboratoryTestInitial language currentDate configInitial TaskRandomBloodSugarTest form

        ( derivedSection, derivedTasksCompleted, derivedTasksTotal ) =
            let
                emptySection =
                    ( [], 0, 0 )
            in
            if form.testPerformed == Just True then
                let
                    ( performedTestSection, performedTestTasksCompleted, performedTestTasksTotal ) =
                        contentAndTasksForPerformedLaboratoryTest language currentDate configPerformed TaskRandomBloodSugarTest form

                    ( testPrerequisitesSection, testPrerequisitesTasksCompleted, testPrerequisitesTasksTotal ) =
                        randomBloodSugarTestPrerequisitesInputsAndTasks language
                            configInitial.setRandomBloodSugarTestFormBoolInputMsg
                            form.immediateResult
                            form.patientFasted

                    ( testResultSection, testResultTasksCompleted, testResultTasksTotal ) =
                        if isNothing form.executionDate then
                            emptySection

                        else
                            Maybe.map
                                (\immediateResult ->
                                    if immediateResult then
                                        randomBloodSugarResultInputsAndTasks language
                                            configPerformed.setRandomBloodSugarResultMsg
                                            form.sugarCount

                                    else
                                        ( [ viewCustomLabel language Translate.LaboratoryTaskResultsHelper "." "label" ]
                                        , 0
                                        , 0
                                        )
                                )
                                form.immediateResult
                                |> Maybe.withDefault emptySection
                in
                ( testPrerequisitesSection ++ performedTestSection ++ testResultSection
                , performedTestTasksCompleted + testPrerequisitesTasksCompleted + testResultTasksCompleted
                , performedTestTasksTotal + testPrerequisitesTasksTotal + testResultTasksTotal
                )

            else
                emptySection
    in
    ( div [ class "ui form laboratory urine-dipstick" ] <|
        viewCustomLabel language (Translate.LaboratoryTaskLabel TaskRandomBloodSugarTest) "" "label header"
            :: initialSection
            ++ derivedSection
    , initialTasksCompleted + derivedTasksCompleted
    , initialTasksTotal + derivedTasksTotal
    )


viewRandomBloodSugarTestUniversalForm :
    Language
    -> NominalDate
    -> ContentAndTasksLaboratoryUniversalTestInitialConfig msg
    -> ContentAndTasksForPerformedLaboratoryUniversalTestConfig msg
    -> RandomBloodSugarTestUniversalForm
    -> ( Html msg, Int, Int )
viewRandomBloodSugarTestUniversalForm language currentDate configInitial configPerformed form =
    let
        ( initialSection, initialTasksCompleted, initialTasksTotal ) =
            contentAndTasksLaboratoryUniversalTestInitial language currentDate configInitial TaskRandomBloodSugarTest form

        ( derivedSection, derivedTasksCompleted, derivedTasksTotal ) =
            let
                emptySection =
                    ( [], 0, 0 )
            in
            if form.testPerformed == Just True then
                let
                    ( testPrerequisitesSection, testPrerequisitesTasksCompleted, testPrerequisitesTasksTotal ) =
                        randomBloodSugarTestPrerequisitesInputsAndTasks language
                            configInitial.setRandomBloodSugarTestFormBoolInputMsg
                            form.immediateResult
                            form.patientFasted

                    ( testResultSection, testResultTasksCompleted, testResultTasksTotal ) =
                        Maybe.map
                            (\immediateResult ->
                                if immediateResult then
                                    randomBloodSugarResultInputsAndTasks language
                                        configPerformed.setRandomBloodSugarResultMsg
                                        form.sugarCount

                                else
                                    ( [ viewCustomLabel language Translate.LaboratoryTaskResultsHelper "." "label" ]
                                    , 0
                                    , 0
                                    )
                            )
                            form.immediateResult
                            |> Maybe.withDefault emptySection
                in
                ( testPrerequisitesSection ++ testResultSection
                , testPrerequisitesTasksCompleted + testResultTasksCompleted
                , testPrerequisitesTasksTotal + testResultTasksTotal
                )

            else
                emptySection
    in
    ( div [ class "ui form laboratory urine-dipstick" ] <|
        viewCustomLabel language (Translate.LaboratoryTaskLabel TaskRandomBloodSugarTest) "" "label header"
            :: initialSection
            ++ derivedSection
    , initialTasksCompleted + derivedTasksCompleted
    , initialTasksTotal + derivedTasksTotal
    )


randomBloodSugarTestPrerequisitesInputsAndTasks language setRandomBloodSugarTestFormBoolInputMsg immediateResult patientFasted =
    let
        ( immediateResultSection, immediateResultTasksCompleted, immediateResultTasksTotal ) =
            prerequisiteByImmediateResultInputsAndTasks language
                (setRandomBloodSugarTestFormBoolInputMsg
                    (\value form_ ->
                        { form_
                            | immediateResult = Just value
                            , sugarCount = Nothing
                            , sugarCountDirty = True
                        }
                    )
                )
                immediateResult

        ( patientFastedSection, patientFastedTasksCompleted, patientFastedTasksTotal ) =
            ( [ viewQuestionLabel language <| Translate.TestPrerequisiteQuestion PrerequisiteFastFor12h
              , viewBoolInput
                    language
                    patientFasted
                    (setRandomBloodSugarTestFormBoolInputMsg
                        (\value form_ -> { form_ | patientFasted = Just value })
                    )
                    "patient-fasted"
                    Nothing
              ]
            , taskCompleted patientFasted
            , 1
            )
    in
    ( immediateResultSection ++ patientFastedSection
    , immediateResultTasksCompleted + patientFastedTasksCompleted
    , immediateResultTasksTotal + patientFastedTasksTotal
    )


randomBloodSugarResultFormAndTasks :
    Language
    -> NominalDate
    -> Bool
    -> ContentAndTasksLaboratoryResultConfig msg encounterId
    -> (String -> msg)
    -> RandomBloodSugarResultForm encounterId
    -> ( Html msg, Int, Int )
randomBloodSugarResultFormAndTasks language currentDate isLabTech config setRandomBloodSugarMsg form =
    let
        ( confirmationSection, confirmationTasksCompleted, confirmationTasksTotal ) =
            if isLabTech then
                contentAndTasksLaboratoryResultConfirmation language currentDate config TaskRandomBloodSugarTest form

            else
                ( [], 0, 0 )

        ( testResultSection, testResultTasksCompleted, testResultTasksTotal ) =
            if not isLabTech || form.runConfirmedByLabTech == Just True then
                randomBloodSugarResultInputsAndTasks language setRandomBloodSugarMsg form.sugarCount

            else
                ( [], 0, 0 )
    in
    ( div [ class "ui form laboratory random-blood-sugar-result" ] <|
        resultFormHeaderSection language currentDate form.executionDate TaskRandomBloodSugarTest
            ++ confirmationSection
            ++ testResultSection
    , confirmationTasksCompleted + testResultTasksCompleted
    , confirmationTasksTotal + testResultTasksTotal
    )


randomBloodSugarResultInputsAndTasks :
    Language
    -> (String -> msg)
    -> Maybe Float
    -> ( List (Html msg), Int, Int )
randomBloodSugarResultInputsAndTasks language setRandomBloodSugarMsg sugarCount =
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


viewBloodGpRsTestForm :
    Language
    -> NominalDate
    -> ContentAndTasksLaboratoryUniversalTestInitialConfig msg
    -> ContentAndTasksForPerformedLaboratoryUniversalTestConfig msg
    -> BloodGpRsTestForm
    -> ( Html msg, Int, Int )
viewBloodGpRsTestForm language currentDate configInitial configPerformed form =
    let
        ( initialSection, initialTasksCompleted, initialTasksTotal ) =
            contentAndTasksLaboratoryUniversalTestInitial language currentDate configInitial TaskBloodGpRsTest form

        ( derivedSection, derivedTasksCompleted, derivedTasksTotal ) =
            let
                emptySection =
                    ( [], 0, 0 )
            in
            if form.testPerformed == Just True then
                let
                    ( testPrerequisitesSection, testPrerequisitesTasksCompleted, testPrerequisitesTasksTotal ) =
                        prerequisiteByImmediateResultInputsAndTasks language
                            (configInitial.setBloodGpRsTestFormBoolInputMsg
                                (\value form_ ->
                                    { form_
                                        | immediateResult = Just value
                                        , bloodGroup = Nothing
                                        , bloodGroupDirty = True
                                        , rhesus = Nothing
                                        , rhesusDirty = True
                                    }
                                )
                            )
                            form.immediateResult

                    ( testResultSection, testResultTasksCompleted, testResultTasksTotal ) =
                        if isNothing form.executionDate then
                            emptySection

                        else
                            Maybe.map
                                (\immediateResult ->
                                    if immediateResult then
                                        bloodGpRsResultInputsAndTasks language
                                            configPerformed.setBloodGroupMsg
                                            configPerformed.setRhesusMsg
                                            form.bloodGroup
                                            form.rhesus

                                    else
                                        ( [ viewCustomLabel language Translate.LaboratoryTaskResultsHelper "." "label" ]
                                        , 0
                                        , 0
                                        )
                                )
                                form.immediateResult
                                |> Maybe.withDefault emptySection
                in
                ( testPrerequisitesSection ++ testResultSection
                , testPrerequisitesTasksCompleted + testResultTasksCompleted
                , testPrerequisitesTasksTotal + testResultTasksTotal
                )

            else
                emptySection
    in
    ( div [ class "ui form laboratory urine-dipstick" ] <|
        viewCustomLabel language (Translate.LaboratoryTaskLabel TaskBloodGpRsTest) "" "label header"
            :: initialSection
            ++ derivedSection
    , initialTasksCompleted + derivedTasksCompleted
    , initialTasksTotal + derivedTasksTotal
    )


bloodGpRsResultFormAndTasks :
    Language
    -> NominalDate
    -> Bool
    -> ContentAndTasksLaboratoryResultConfig msg encounterId
    -> (String -> msg)
    -> (String -> msg)
    -> BloodGpRsResultForm encounterId
    -> ( Html msg, Int, Int )
bloodGpRsResultFormAndTasks language currentDate isLabTech config setBloodGroupMsg setRhesusMsg form =
    let
        ( confirmationSection, confirmationTasksCompleted, confirmationTasksTotal ) =
            if isLabTech then
                contentAndTasksLaboratoryResultConfirmation language currentDate config TaskBloodGpRsTest form

            else
                ( [], 0, 0 )

        ( testResultSection, testResultTasksCompleted, testResultTasksTotal ) =
            if not isLabTech || form.runConfirmedByLabTech == Just True then
                bloodGpRsResultInputsAndTasks language
                    setBloodGroupMsg
                    setRhesusMsg
                    form.bloodGroup
                    form.rhesus

            else
                ( [], 0, 0 )
    in
    ( div [ class "ui form laboratory blood-group-result" ] <|
        resultFormHeaderSection language currentDate form.executionDate TaskBloodGpRsTest
            ++ confirmationSection
            ++ testResultSection
    , confirmationTasksCompleted + testResultTasksCompleted
    , confirmationTasksTotal + testResultTasksTotal
    )


bloodGpRsResultInputsAndTasks :
    Language
    -> (String -> msg)
    -> (String -> msg)
    -> Maybe BloodGroup
    -> Maybe Rhesus
    -> ( List (Html msg), Int, Int )
bloodGpRsResultInputsAndTasks language setBloodGroupMsg setRhesusMsg bloodGroup rhesus =
    ( viewSelectInput language
        Translate.LaboratoryBloodGroupTestResult
        bloodGroup
        Translate.LaboratoryBloodGroup
        bloodGroupToString
        [ BloodGroupA, BloodGroupB, BloodGroupAB, BloodGroupO ]
        setBloodGroupMsg
        ++ viewSelectInput language
            Translate.LaboratoryRhesusTestResult
            rhesus
            Translate.LaboratoryRhesus
            rhesusToString
            [ RhesusPositive, RhesusNegative ]
            setRhesusMsg
    , taskCompleted bloodGroup + taskCompleted rhesus
    , 2
    )


viewHemoglobinTestForm :
    Language
    -> NominalDate
    -> ContentAndTasksLaboratoryUniversalTestInitialConfig msg
    -> ContentAndTasksForPerformedLaboratoryUniversalTestConfig msg
    -> HemoglobinTestForm
    -> ( Html msg, Int, Int )
viewHemoglobinTestForm language currentDate configInitial configPerformed form =
    let
        ( initialSection, initialTasksCompleted, initialTasksTotal ) =
            contentAndTasksLaboratoryUniversalTestInitial language currentDate configInitial TaskHemoglobinTest form

        ( derivedSection, derivedTasksCompleted, derivedTasksTotal ) =
            let
                emptySection =
                    ( [], 0, 0 )
            in
            if form.testPerformed == Just True then
                let
                    ( testPrerequisitesSection, testPrerequisitesTasksCompleted, testPrerequisitesTasksTotal ) =
                        prerequisiteByImmediateResultInputsAndTasks language
                            (configInitial.setHemoglobinTestFormBoolInputMsg
                                (\value form_ ->
                                    { form_
                                        | immediateResult = Just value
                                        , hemoglobinCount = Nothing
                                        , hemoglobinCountDirty = True
                                    }
                                )
                            )
                            form.immediateResult

                    ( testResultSection, testResultTasksCompleted, testResultTasksTotal ) =
                        if isNothing form.executionDate then
                            emptySection

                        else
                            Maybe.map
                                (\immediateResult ->
                                    if immediateResult then
                                        hemoglobinResultInputsAndTasks language
                                            configPerformed.setHemoglobinCountMsg
                                            form.hemoglobinCount

                                    else
                                        ( [ viewCustomLabel language Translate.LaboratoryTaskResultsHelper "." "label" ]
                                        , 0
                                        , 0
                                        )
                                )
                                form.immediateResult
                                |> Maybe.withDefault emptySection
                in
                ( testPrerequisitesSection ++ testResultSection
                , testPrerequisitesTasksCompleted + testResultTasksCompleted
                , testPrerequisitesTasksTotal + testResultTasksTotal
                )

            else
                emptySection
    in
    ( div [ class "ui form laboratory urine-dipstick" ] <|
        viewCustomLabel language (Translate.LaboratoryTaskLabel TaskHemoglobinTest) "" "label header"
            :: initialSection
            ++ derivedSection
    , initialTasksCompleted + derivedTasksCompleted
    , initialTasksTotal + derivedTasksTotal
    )


hemoglobinResultFormAndTasks :
    Language
    -> NominalDate
    -> Bool
    -> ContentAndTasksLaboratoryResultConfig msg encounterId
    -> (String -> msg)
    -> HemoglobinResultForm
    -> ( Html msg, Int, Int )
hemoglobinResultFormAndTasks language currentDate isLabTech config setHemoglobinCountMsg form =
    let
        ( confirmationSection, confirmationTasksCompleted, confirmationTasksTotal ) =
            if isLabTech then
                contentAndTasksLaboratoryResultConfirmation language currentDate config TaskHemoglobinTest form

            else
                ( [], 0, 0 )

        ( testResultSection, testResultTasksCompleted, testResultTasksTotal ) =
            if not isLabTech || form.runConfirmedByLabTech == Just True then
                hemoglobinResultInputsAndTasks language
                    setHemoglobinCountMsg
                    form.hemoglobinCount

            else
                ( [], 0, 0 )
    in
    ( div [ class "ui form laboratory hemoglobin-result" ] <|
        resultFormHeaderSection language currentDate form.executionDate TaskHemoglobinTest
            ++ confirmationSection
            ++ testResultSection
    , confirmationTasksCompleted + testResultTasksCompleted
    , confirmationTasksTotal + testResultTasksTotal
    )


hemoglobinResultInputsAndTasks :
    Language
    -> (String -> msg)
    -> Maybe Float
    -> ( List (Html msg), Int, Int )
hemoglobinResultInputsAndTasks language setHemoglobinCountMsg hemoglobinCount =
    ( [ viewLabel language Translate.LaboratoryHemoglobinTestResult
      , viewMeasurementInput language
            hemoglobinCount
            setHemoglobinCountMsg
            "hemoglobin-count"
            Translate.UnitGramsPerDeciliter
      ]
    , taskCompleted hemoglobinCount
    , 1
    )


viewHepatitisBTestForm :
    Language
    -> NominalDate
    -> ContentAndTasksLaboratoryUniversalTestInitialConfig msg
    -> ContentAndTasksForPerformedLaboratoryUniversalTestConfig msg
    -> HepatitisBTestForm
    -> ( Html msg, Int, Int )
viewHepatitisBTestForm language currentDate configInitial configPerformed form =
    let
        ( knownAsPositiveSection, knownAsPositiveTasksCompleted, knownAsPositiveTasksTotal ) =
            contentAndTasksLaboratoryUniversalTestKnownAsPositive language currentDate configInitial TaskHepatitisBTest form

        ( derivedSection, derivedTasksCompleted, derivedTasksTotal ) =
            let
                emptySection =
                    ( [], 0, 0 )
            in
            if form.knownAsPositive == Just False then
                let
                    ( initialSection, initialTasksCompleted, initialTasksTotal ) =
                        contentAndTasksLaboratoryUniversalTestInitial language currentDate configInitial TaskHepatitisBTest form
                in
                if form.testPerformed == Just True then
                    let
                        ( testPrerequisitesSection, testPrerequisitesTasksCompleted, testPrerequisitesTasksTotal ) =
                            prerequisiteByImmediateResultInputsAndTasks language
                                (configInitial.setHepatitisBTestFormBoolInputMsg
                                    (\value form_ ->
                                        { form_
                                            | immediateResult = Just value
                                            , testResult = Nothing
                                            , testResultDirty = True
                                        }
                                    )
                                )
                                form.immediateResult

                        ( testResultSection, testResultTasksCompleted, testResultTasksTotal ) =
                            if isNothing form.executionDate then
                                emptySection

                            else
                                Maybe.map
                                    (\immediateResult ->
                                        if immediateResult then
                                            standardTestResultInputsAndTasks language
                                                configPerformed.setHepatitisBTestResultMsg
                                                form.testResult
                                                TaskHepatitisBTest

                                        else
                                            ( [ viewCustomLabel language Translate.LaboratoryTaskResultsHelper "." "label" ]
                                            , 0
                                            , 0
                                            )
                                    )
                                    form.immediateResult
                                    |> Maybe.withDefault emptySection
                    in
                    ( initialSection ++ testPrerequisitesSection ++ testResultSection
                    , initialTasksCompleted + testPrerequisitesTasksCompleted + testResultTasksCompleted
                    , initialTasksTotal + testPrerequisitesTasksTotal + testResultTasksTotal
                    )

                else
                    ( initialSection, initialTasksCompleted, initialTasksTotal )

            else
                emptySection
    in
    ( div [ class "ui form laboratory urine-dipstick" ] <|
        viewCustomLabel language (Translate.LaboratoryTaskLabel TaskHepatitisBTest) "" "label header"
            :: knownAsPositiveSection
            ++ derivedSection
    , knownAsPositiveTasksCompleted + derivedTasksCompleted
    , knownAsPositiveTasksTotal + derivedTasksTotal
    )


hepatitisBResultFormAndTasks :
    Language
    -> NominalDate
    -> Bool
    -> ContentAndTasksLaboratoryResultConfig msg encounterId
    -> (String -> msg)
    -> HepatitisBResultForm encounterId
    -> ( Html msg, Int, Int )
hepatitisBResultFormAndTasks language currentDate isLabTech config setHepatitisBTestResultMsg form =
    let
        ( confirmationSection, confirmationTasksCompleted, confirmationTasksTotal ) =
            if isLabTech then
                contentAndTasksLaboratoryResultConfirmation language currentDate config TaskHepatitisBTest form

            else
                ( [], 0, 0 )

        ( testResultSection, testResultTasksCompleted, testResultTasksTotal ) =
            if not isLabTech || form.runConfirmedByLabTech == Just True then
                standardTestResultInputsAndTasks language
                    setHepatitisBTestResultMsg
                    form.testResult
                    TaskHepatitisBTest

            else
                ( [], 0, 0 )
    in
    ( div [ class "ui form laboratory prenatal-test-result" ] <|
        resultFormHeaderSection language currentDate form.executionDate TaskHepatitisBTest
            ++ confirmationSection
            ++ testResultSection
    , confirmationTasksCompleted + testResultTasksCompleted
    , confirmationTasksTotal + testResultTasksTotal
    )


viewHIVPCRTestForm :
    Language
    -> NominalDate
    -> ContentAndTasksLaboratoryUniversalTestInitialConfig msg
    -> ContentAndTasksForPerformedLaboratoryUniversalTestConfig msg
    -> HIVPCRTestForm
    -> ( Html msg, Int, Int )
viewHIVPCRTestForm language currentDate configInitial configPerformed form =
    let
        ( initialSection, initialTasksCompleted, initialTasksTotal ) =
            contentAndTasksLaboratoryUniversalTestInitial language currentDate configInitial TaskHIVPCRTest form

        ( derivedSection, derivedTasksCompleted, derivedTasksTotal ) =
            let
                emptySection =
                    ( [], 0, 0 )
            in
            if form.testPerformed == Just True then
                let
                    ( testPrerequisitesSection, testPrerequisitesTasksCompleted, testPrerequisitesTasksTotal ) =
                        prerequisiteByImmediateResultInputsAndTasks language
                            (configInitial.setHIVPCRTestFormBoolInputMsg
                                (\value form_ ->
                                    { form_
                                        | immediateResult = Just value
                                        , hivViralLoadStatus = Nothing
                                        , hivViralLoadStatusDirty = True
                                        , hivViralLoad = Nothing
                                        , hivViralLoadDirty = True
                                    }
                                )
                            )
                            form.immediateResult

                    ( testResultSection, testResultTasksCompleted, testResultTasksTotal ) =
                        if isNothing form.executionDate then
                            emptySection

                        else
                            Maybe.map
                                (\immediateResult ->
                                    if immediateResult then
                                        hivPCRResultInputsAndTasks language
                                            configPerformed.setHIVViralLoadMsg
                                            configPerformed.setHIVViralLoadUndetectableMsg
                                            form.hivViralLoadStatus
                                            form.hivViralLoad

                                    else
                                        ( [ viewCustomLabel language Translate.LaboratoryTaskResultsHelper "." "label" ]
                                        , 0
                                        , 0
                                        )
                                )
                                form.immediateResult
                                |> Maybe.withDefault emptySection
                in
                ( testPrerequisitesSection ++ testResultSection
                , testPrerequisitesTasksCompleted + testResultTasksCompleted
                , testPrerequisitesTasksTotal + testResultTasksTotal
                )

            else
                emptySection
    in
    ( div [ class "ui form laboratory urine-dipstick" ] <|
        viewCustomLabel language (Translate.LaboratoryTaskLabel TaskHIVPCRTest) "" "label header"
            :: initialSection
            ++ derivedSection
    , initialTasksCompleted + derivedTasksCompleted
    , initialTasksTotal + derivedTasksTotal
    )


hivPCRResultFormAndTasks :
    Language
    -> NominalDate
    -> Bool
    -> ContentAndTasksLaboratoryResultConfig msg encounterId
    -> (String -> msg)
    -> (Bool -> msg)
    -> HIVPCRResultForm
    -> ( Html msg, Int, Int )
hivPCRResultFormAndTasks language currentDate isLabTech config setHIVViralLoadMsg setHIVViralLoadUndetectableMsg form =
    let
        ( confirmationSection, confirmationTasksCompleted, confirmationTasksTotal ) =
            if isLabTech then
                contentAndTasksLaboratoryResultConfirmation language currentDate config TaskHIVPCRTest form

            else
                ( [], 0, 0 )

        ( testResultSection, testResultTasksCompleted, testResultTasksTotal ) =
            if not isLabTech || form.runConfirmedByLabTech == Just True then
                hivPCRResultInputsAndTasks language
                    setHIVViralLoadMsg
                    setHIVViralLoadUndetectableMsg
                    form.hivViralLoadStatus
                    form.hivViralLoad

            else
                ( [], 0, 0 )
    in
    ( div [ class "ui form laboratory hiv-prc-result" ] <|
        resultFormHeaderSection language currentDate form.executionDate TaskHIVPCRTest
            ++ confirmationSection
            ++ testResultSection
    , confirmationTasksCompleted + testResultTasksCompleted
    , confirmationTasksTotal + testResultTasksTotal
    )


hivPCRResultInputsAndTasks :
    Language
    -> (String -> msg)
    -> (Bool -> msg)
    -> Maybe ViralLoadStatus
    -> Maybe Float
    -> ( List (Html msg), Int, Int )
hivPCRResultInputsAndTasks language setHIVViralLoadMsg setHIVViralLoadUndetectableMsg hivViralLoadStatus hivViralLoad =
    let
        ( derivedSection, derivedTasksCompleted, derivedTasksTotal ) =
            if hivViralLoadStatus == Just ViralLoadDetectable then
                ( [ viewLabel language Translate.LaboratoryHIVPCRTestResult
                  , viewMeasurementInput language
                        hivViralLoad
                        setHIVViralLoadMsg
                        "hiv-viral-load"
                        Translate.UnitCopiesPerMM3
                  ]
                , taskCompleted hivViralLoad
                , 1
                )

            else
                ( [], 0, 0 )
    in
    ( [ viewQuestionLabel language Translate.LaboratoryHIVPCRViralLoadStatusQuestion
      , viewBoolInput language
            (Maybe.map ((==) ViralLoadUndetectable) hivViralLoadStatus)
            setHIVViralLoadUndetectableMsg
            "hiv-level-undetectable"
            Nothing
      ]
        ++ derivedSection
    , taskCompleted hivViralLoadStatus + derivedTasksCompleted
    , 1 + derivedTasksTotal
    )


viewSyphilisTestForm :
    Language
    -> NominalDate
    -> ContentAndTasksLaboratoryUniversalTestInitialConfig msg
    -> ContentAndTasksForPerformedLaboratoryUniversalTestConfig msg
    -> SyphilisTestForm
    -> ( Html msg, Int, Int )
viewSyphilisTestForm language currentDate configInitial configPerformed form =
    let
        ( initialSection, initialTasksCompleted, initialTasksTotal ) =
            contentAndTasksLaboratoryUniversalTestInitial language currentDate configInitial TaskSyphilisTest form

        ( derivedSection, derivedTasksCompleted, derivedTasksTotal ) =
            let
                emptySection =
                    ( [], 0, 0 )
            in
            if form.testPerformed == Just True then
                let
                    ( testPrerequisitesSection, testPrerequisitesTasksCompleted, testPrerequisitesTasksTotal ) =
                        prerequisiteByImmediateResultInputsAndTasks language
                            (configInitial.setSyphilisTestFormBoolInputMsg
                                (\value form_ ->
                                    { form_
                                        | immediateResult = Just value
                                        , testResult = Nothing
                                        , testResultDirty = True
                                        , symptoms = Nothing
                                        , symptomsDirty = True
                                    }
                                )
                            )
                            form.immediateResult

                    ( testResultSection, testResultTasksCompleted, testResultTasksTotal ) =
                        if isNothing form.executionDate then
                            emptySection

                        else
                            Maybe.map
                                (\immediateResult ->
                                    if immediateResult then
                                        let
                                            isLabTech =
                                                -- Only nurses perform initial phase of prenatal encounter.
                                                False
                                        in
                                        syphilisResultInputsAndTasks language
                                            isLabTech
                                            configPerformed.setSyphilisTestResultMsg
                                            configPerformed.setIllnessSymptomMsg
                                            form.testResult
                                            form.symptoms

                                    else
                                        ( [ viewCustomLabel language Translate.LaboratoryTaskResultsHelper "." "label" ]
                                        , 0
                                        , 0
                                        )
                                )
                                form.immediateResult
                                |> Maybe.withDefault emptySection
                in
                ( testPrerequisitesSection ++ testResultSection
                , testPrerequisitesTasksCompleted + testResultTasksCompleted
                , testPrerequisitesTasksTotal + testResultTasksTotal
                )

            else
                emptySection
    in
    ( div [ class "ui form laboratory urine-dipstick" ] <|
        viewCustomLabel language (Translate.LaboratoryTaskLabel TaskSyphilisTest) "" "label header"
            :: initialSection
            ++ derivedSection
    , initialTasksCompleted + derivedTasksCompleted
    , initialTasksTotal + derivedTasksTotal
    )


syphilisResultFormAndTasks :
    Language
    -> NominalDate
    -> Bool
    -> ContentAndTasksLaboratoryResultConfig msg encounterId
    -> (String -> msg)
    -> (IllnessSymptom -> msg)
    -> SyphilisResultForm encounterId
    -> ( Html msg, Int, Int )
syphilisResultFormAndTasks language currentDate isLabTech config setTestResultMsg setIllnessSymptomMsg form =
    let
        ( confirmationSection, confirmationTasksCompleted, confirmationTasksTotal ) =
            if isLabTech then
                contentAndTasksLaboratoryResultConfirmation language currentDate config TaskSyphilisTest form

            else
                ( [], 0, 0 )

        ( testResultSection, testResultTasksCompleted, testResultTasksTotal ) =
            if not isLabTech || form.runConfirmedByLabTech == Just True then
                syphilisResultInputsAndTasks language
                    isLabTech
                    setTestResultMsg
                    setIllnessSymptomMsg
                    form.testResult
                    form.symptoms

            else
                ( [], 0, 0 )
    in
    ( div [ class "ui form laboratory prenatal-test-result" ] <|
        resultFormHeaderSection language currentDate form.executionDate TaskSyphilisTest
            ++ confirmationSection
            ++ testResultSection
    , confirmationTasksCompleted + testResultTasksCompleted
    , confirmationTasksTotal + testResultTasksTotal
    )


syphilisResultFollowUpsFormAndTasks :
    Language
    -> NominalDate
    -> (IllnessSymptom -> msg)
    -> SyphilisResultForm encounterId
    -> ( Html msg, Int, Int )
syphilisResultFollowUpsFormAndTasks language currentDate setIllnessSymptomMsg form =
    let
        ( followUpsSection, followUpsTasksCompleted, followUpsTasksTotal ) =
            syphilisResultFollowUpInputsAndTasks language
                setIllnessSymptomMsg
                form.symptoms
    in
    ( div [ class "ui form laboratory prenatal-test-result" ] <|
        resultFormHeaderSection language currentDate form.executionDate TaskSyphilisTest
            ++ followUpsSection
    , followUpsTasksCompleted
    , followUpsTasksTotal
    )


syphilisResultInputsAndTasks :
    Language
    -> Bool
    -> (String -> msg)
    -> (IllnessSymptom -> msg)
    -> Maybe TestResult
    -> Maybe (List IllnessSymptom)
    -> ( List (Html msg), Int, Int )
syphilisResultInputsAndTasks language isLabTech setTestResultMsg setIllnessSymptomMsg testResult symptoms =
    let
        ( testResultSection, testResultTasksCompleted, testResultTasksTotal ) =
            standardTestResultInputsAndTasks language setTestResultMsg testResult TaskSyphilisTest

        ( symptomsSection, symptomsTasksCompleted, symptomsTasksTotal ) =
            if testResult == Just TestPositive && not isLabTech then
                syphilisResultFollowUpInputsAndTasks language setIllnessSymptomMsg symptoms

            else
                ( [], 0, 0 )
    in
    ( testResultSection
        ++ symptomsSection
    , testResultTasksCompleted + symptomsTasksCompleted
    , testResultTasksTotal + symptomsTasksTotal
    )


syphilisResultFollowUpInputsAndTasks :
    Language
    -> (IllnessSymptom -> msg)
    -> Maybe (List IllnessSymptom)
    -> ( List (Html msg), Int, Int )
syphilisResultFollowUpInputsAndTasks language setIllnessSymptomMsg symptoms =
    ( [ viewLabel language Translate.SelectIllnessSymptoms
      , viewCheckBoxMultipleSelectInput language
            [ IllnessSymptomHeadache
            , IllnessSymptomVisionChanges
            , IllnessSymptomRash
            , IllnessSymptomPainlessUlcerMouth
            , IllnessSymptomPainlessUlcerGenitals
            ]
            []
            (Maybe.withDefault [] symptoms)
            (Just NoIllnessSymptoms)
            setIllnessSymptomMsg
            Translate.IllnessSymptom
      ]
    , taskCompleted symptoms
    , 1
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
        viewCustomLabel language (Translate.LaboratoryTaskLabel TaskHbA1cTest) "" "label header"
            :: initialSection
            ++ derivedSection
    , initialTasksCompleted + derivedTasksCompleted
    , initialTasksTotal + derivedTasksTotal
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


contentAndTasksLaboratoryUniversalTestKnownAsPositive :
    Language
    -> NominalDate
    -> ContentAndTasksLaboratoryUniversalTestInitialConfig msg
    -> LaboratoryTask
    -> { f | knownAsPositive : Maybe Bool }
    -> ( List (Html msg), Int, Int )
contentAndTasksLaboratoryUniversalTestKnownAsPositive language currentDate config task form =
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
                    , executionNote = executionNote
                    , executionNoteDirty = True
                    , executionDate = Nothing
                    , executionDateDirty = True
                }

        setMsg =
            case task of
                TaskHIVTest ->
                    config.setHIVTestFormBoolInputMsg updateFunc

                TaskPartnerHIVTest ->
                    let
                        partnerHIVTestUpdateFunc =
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
                                    , executionNote = executionNote
                                    , executionNoteDirty = True
                                    , executionDate = Nothing
                                    , executionDateDirty = True
                                    , partnerTakingARV = Nothing
                                    , partnerTakingARVDirty = True
                                    , partnerSurpressedViralLoad = Nothing
                                    , partnerSurpressedViralLoadDirty = True
                                }
                    in
                    config.setPartnerHIVTestFormBoolInputMsg partnerHIVTestUpdateFunc

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

                TaskUrineDipstickTest ->
                    { setBoolInputMsg = config.setUrineDipstickTestFormBoolInputMsg boolInputUpdateFunc
                    , setExecutionNoteMsg = config.setUrineDipstickTestExecutionNoteMsg
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

                _ ->
                    -- Other tasks do not use this config, as they either
                    -- have only 'universal form' variant, or a proprietary form.
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


contentAndTasksLaboratoryUniversalTestInitial :
    Language
    -> NominalDate
    -> ContentAndTasksLaboratoryUniversalTestInitialConfig msg
    -> LaboratoryTask
    ->
        { f
            | testPerformed : Maybe Bool
            , executionNote : Maybe TestExecutionNote
        }
    -> ( List (Html msg), Int, Int )
contentAndTasksLaboratoryUniversalTestInitial language currentDate config task form =
    let
        msgs =
            let
                resolveNoteAndDate value =
                    if value then
                        ( Just TestNoteRunToday, Just currentDate )

                    else
                        ( Nothing, Nothing )
            in
            case task of
                TaskHIVTest ->
                    let
                        updateFunc =
                            \value form_ ->
                                let
                                    ( executionNote, executionDate ) =
                                        resolveNoteAndDate value
                                in
                                { form_
                                    | testPerformed = Just value
                                    , testPerformedDirty = True
                                    , immediateResult = Nothing
                                    , executionNote = executionNote
                                    , executionNoteDirty = True
                                    , executionDate = executionDate
                                    , executionDateDirty = True
                                    , testResult = Nothing
                                    , testResultDirty = True
                                    , hivProgramHC = Nothing
                                    , hivProgramHCDirty = True
                                    , partnerHIVPositive = Nothing
                                    , partnerHIVPositiveDirty = True
                                    , partnerTakingARV = Nothing
                                    , partnerTakingARVDirty = True
                                    , partnerSurpressedViralLoad = Nothing
                                    , partnerSurpressedViralLoadDirty = True
                                }
                    in
                    { setBoolInputMsg = config.setHIVTestFormBoolInputMsg updateFunc
                    , setExecutionNoteMsg = config.setHIVTestExecutionNoteMsg
                    }

                TaskSyphilisTest ->
                    let
                        updateFunc =
                            \value form_ ->
                                let
                                    ( executionNote, executionDate ) =
                                        resolveNoteAndDate value
                                in
                                { form_
                                    | testPerformed = Just value
                                    , testPerformedDirty = True
                                    , immediateResult = Nothing
                                    , executionNote = executionNote
                                    , executionNoteDirty = True
                                    , executionDate = executionDate
                                    , executionDateDirty = True
                                    , testResult = Nothing
                                    , testResultDirty = True
                                    , symptoms = Nothing
                                    , symptomsDirty = True
                                }
                    in
                    { setBoolInputMsg = config.setSyphilisTestFormBoolInputMsg updateFunc
                    , setExecutionNoteMsg = config.setSyphilisTestExecutionNoteMsg
                    }

                TaskHepatitisBTest ->
                    let
                        updateFunc =
                            \value form_ ->
                                let
                                    ( executionNote, executionDate ) =
                                        resolveNoteAndDate value
                                in
                                { form_
                                    | testPerformed = Just value
                                    , testPerformedDirty = True
                                    , immediateResult = Nothing
                                    , executionNote = executionNote
                                    , executionNoteDirty = True
                                    , executionDate = executionDate
                                    , executionDateDirty = True
                                    , testResult = Nothing
                                    , testResultDirty = True
                                }
                    in
                    { setBoolInputMsg = config.setHepatitisBTestFormBoolInputMsg updateFunc
                    , setExecutionNoteMsg = config.setHepatitisBTestExecutionNoteMsg
                    }

                TaskMalariaTest ->
                    let
                        updateFunc =
                            \value form_ ->
                                let
                                    ( executionNote, executionDate ) =
                                        resolveNoteAndDate value
                                in
                                { form_
                                    | testPerformed = Just value
                                    , testPerformedDirty = True
                                    , immediateResult = Nothing
                                    , executionNote = executionNote
                                    , executionNoteDirty = True
                                    , executionDate = executionDate
                                    , executionDateDirty = True
                                    , testResult = Nothing
                                    , testResultDirty = True
                                    , bloodSmearTaken = Nothing
                                    , bloodSmearTakenDirty = True
                                    , bloodSmearResult = Nothing
                                    , bloodSmearResultDirty = True
                                }
                    in
                    { setBoolInputMsg = config.setMalariaTestFormBoolInputMsg updateFunc
                    , setExecutionNoteMsg = config.setMalariaTestExecutionNoteMsg
                    }

                TaskBloodGpRsTest ->
                    let
                        updateFunc =
                            \value form_ ->
                                let
                                    ( executionNote, executionDate ) =
                                        resolveNoteAndDate value
                                in
                                { form_
                                    | testPerformed = Just value
                                    , testPerformedDirty = True
                                    , immediateResult = Nothing
                                    , executionNote = executionNote
                                    , executionNoteDirty = True
                                    , executionDate = executionDate
                                    , executionDateDirty = True
                                    , bloodGroup = Nothing
                                    , bloodGroupDirty = True
                                    , rhesus = Nothing
                                    , rhesusDirty = True
                                }
                    in
                    { setBoolInputMsg = config.setBloodGpRsTestFormBoolInputMsg updateFunc
                    , setExecutionNoteMsg = config.setBloodGpRsTestExecutionNoteMsg
                    }

                TaskUrineDipstickTest ->
                    let
                        updateFunc =
                            \value form_ ->
                                let
                                    ( executionNote, executionDate ) =
                                        resolveNoteAndDate value
                                in
                                { form_
                                    | testPerformed = Just value
                                    , testPerformedDirty = True
                                    , immediateResult = Nothing
                                    , executionNote = executionNote
                                    , executionNoteDirty = True
                                    , executionDate = executionDate
                                    , executionDateDirty = True
                                    , testVariant = Nothing
                                    , testVariantDirty = True
                                    , protein = Nothing
                                    , proteinDirty = True
                                    , ph = Nothing
                                    , phDirty = True
                                    , glucose = Nothing
                                    , glucoseDirty = True
                                    , leukocytes = Nothing
                                    , leukocytesDirty = True
                                    , nitrite = Nothing
                                    , nitriteDirty = True
                                    , urobilinogen = Nothing
                                    , urobilinogenDirty = True
                                    , haemoglobin = Nothing
                                    , haemoglobinDirty = True
                                    , ketone = Nothing
                                    , ketoneDirty = True
                                    , bilirubin = Nothing
                                    , bilirubinDirty = True
                                }
                    in
                    { setBoolInputMsg = config.setUrineDipstickTestFormBoolInputMsg updateFunc
                    , setExecutionNoteMsg = config.setUrineDipstickTestExecutionNoteMsg
                    }

                TaskHemoglobinTest ->
                    let
                        updateFunc =
                            \value form_ ->
                                let
                                    ( executionNote, executionDate ) =
                                        resolveNoteAndDate value
                                in
                                { form_
                                    | testPerformed = Just value
                                    , testPerformedDirty = True
                                    , immediateResult = Nothing
                                    , executionNote = executionNote
                                    , executionNoteDirty = True
                                    , executionDate = executionDate
                                    , executionDateDirty = True
                                    , hemoglobinCount = Nothing
                                    , hemoglobinCountDirty = True
                                }
                    in
                    { setBoolInputMsg = config.setHemoglobinTestFormBoolInputMsg updateFunc
                    , setExecutionNoteMsg = config.setHemoglobinTestExecutionNoteMsg
                    }

                TaskRandomBloodSugarTest ->
                    let
                        updateFunc =
                            \value form_ ->
                                let
                                    ( executionNote, executionDate ) =
                                        resolveNoteAndDate value
                                in
                                { form_
                                    | testPerformed = Just value
                                    , testPerformedDirty = True
                                    , immediateResult = Nothing
                                    , executionNote = executionNote
                                    , executionNoteDirty = True
                                    , executionDate = executionDate
                                    , executionDateDirty = True
                                    , patientFasted = Nothing
                                    , sugarCount = Nothing
                                    , sugarCountDirty = True
                                }
                    in
                    { setBoolInputMsg = config.setRandomBloodSugarTestFormBoolInputMsg updateFunc
                    , setExecutionNoteMsg = config.setRandomBloodSugarTestExecutionNoteMsg
                    }

                TaskHIVPCRTest ->
                    let
                        updateFunc =
                            \value form_ ->
                                let
                                    ( executionNote, executionDate ) =
                                        resolveNoteAndDate value
                                in
                                { form_
                                    | testPerformed = Just value
                                    , testPerformedDirty = True
                                    , immediateResult = Nothing
                                    , executionNote = executionNote
                                    , executionNoteDirty = True
                                    , executionDate = executionDate
                                    , executionDateDirty = True
                                    , hivViralLoadStatus = Nothing
                                    , hivViralLoadStatusDirty = True
                                    , hivViralLoad = Nothing
                                    , hivViralLoadDirty = True
                                }
                    in
                    { setBoolInputMsg = config.setHIVPCRTestFormBoolInputMsg updateFunc
                    , setExecutionNoteMsg = config.setHIVPCRTestExecutionNoteMsg
                    }

                TaskPartnerHIVTest ->
                    let
                        updateFunc =
                            \value form_ ->
                                let
                                    ( executionNote, executionDate ) =
                                        resolveNoteAndDate value
                                in
                                { form_
                                    | testPerformed = Just value
                                    , testPerformedDirty = True
                                    , immediateResult = Nothing
                                    , executionNote = executionNote
                                    , executionNoteDirty = True
                                    , executionDate = executionDate
                                    , executionDateDirty = True
                                    , testResult = Nothing
                                    , testResultDirty = True
                                }
                    in
                    { setBoolInputMsg = config.setPartnerHIVTestFormBoolInputMsg updateFunc
                    , setExecutionNoteMsg = config.setPartnerHIVTestExecutionNoteMsg
                    }

                _ ->
                    -- Other tasks do not use this config, as they either
                    -- have only 'non universal form' variant, or a proprietary form.
                    { setBoolInputMsg = always config.noOpMsg
                    , setExecutionNoteMsg = always config.noOpMsg
                    }

        ( derivedSection, derivedTasksCompleted, derivedTasksTotal ) =
            Maybe.map
                (\testPerformed ->
                    if testPerformed then
                        ( [], 0, 0 )

                    else
                        let
                            rightOptions =
                                case task of
                                    TaskPartnerHIVTest ->
                                        [ TestNoteNoEquipment
                                        , TestNoteNotPresent
                                        , TestNoteNotIndicated
                                        ]

                                    _ ->
                                        [ TestNoteNoEquipment
                                        , TestNoteNotIndicated
                                        ]
                        in
                        ( [ div [ class "why-not" ]
                                [ viewQuestionLabel language Translate.WhyNot
                                , viewCheckBoxSelectInput language
                                    [ TestNoteLackOfReagents
                                    , TestNoteLackOfOtherSupplies
                                    , TestNoteBrokenEquipment
                                    ]
                                    rightOptions
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
    ( [ viewQuestionLabel language Translate.TestWillBePerformedTodayQuestion
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
                            if value then
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

                    TaskRandomBloodSugarTest ->
                        { setBoolInputMsg = config.setRandomBloodSugarTestFormBoolInputMsg boolInputUpdateFunc
                        , setExecutionDateMsg = config.setRandomBloodSugarTestExecutionDateMsg
                        , setDateSelectorStateMsg = config.setRandomBloodSugarTestDateSelectorStateMsg
                        }

                    TaskUrineDipstickTest ->
                        { setBoolInputMsg = config.setUrineDipstickTestFormBoolInputMsg boolInputUpdateFunc
                        , setExecutionDateMsg = config.setUrineDipstickTestExecutionDateMsg
                        , setDateSelectorStateMsg = config.setUrineDipstickTestDateSelectorStateMsg
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

                    _ ->
                        -- Other tasks do not use this config, as they either
                        -- have only 'universal form' variant, or a proprietary form.
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
    , setBloodSmearResultMsg = always noOpMsg
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
    , setPartnerHIVTestFormBoolInputMsg = \_ _ -> noOpMsg
    , setPartnerHIVTestExecutionNoteMsg = always noOpMsg
    , setPartnerHIVTestResultMsg = always noOpMsg
    , noOpMsg = noOpMsg
    }


emptyContentAndTasksLaboratoryUniversalTestInitialConfig : msg -> ContentAndTasksLaboratoryUniversalTestInitialConfig msg
emptyContentAndTasksLaboratoryUniversalTestInitialConfig noOpMsg =
    { setHIVTestFormBoolInputMsg = \_ _ -> noOpMsg
    , setHIVTestExecutionNoteMsg = always noOpMsg
    , setSyphilisTestFormBoolInputMsg = \_ _ -> noOpMsg
    , setSyphilisTestExecutionNoteMsg = always noOpMsg
    , setHepatitisBTestFormBoolInputMsg = \_ _ -> noOpMsg
    , setHepatitisBTestExecutionNoteMsg = always noOpMsg
    , setMalariaTestFormBoolInputMsg = \_ _ -> noOpMsg
    , setMalariaTestExecutionNoteMsg = always noOpMsg
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
    , setPartnerHIVTestFormBoolInputMsg = \_ _ -> noOpMsg
    , setPartnerHIVTestExecutionNoteMsg = always noOpMsg
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
    , setPartnerHIVTestFormBoolInputMsg = \_ _ -> noOpMsg
    , setPartnerHIVTestExecutionDateMsg = always noOpMsg
    , setPartnerHIVTestDateSelectorStateMsg = always noOpMsg
    , noOpMsg = noOpMsg
    }


emptyContentAndTasksForPerformedLaboratoryUniversalTestConfig : msg -> ContentAndTasksForPerformedLaboratoryUniversalTestConfig msg
emptyContentAndTasksForPerformedLaboratoryUniversalTestConfig noOpMsg =
    { setHIVTestResultMsg = always noOpMsg
    , setSyphilisTestResultMsg = always noOpMsg
    , setIllnessSymptomMsg = always noOpMsg
    , setHepatitisBTestResultMsg = always noOpMsg
    , setMalariaTestResultMsg = always noOpMsg
    , setBloodSmearResultMsg = always noOpMsg
    , setBloodGroupMsg = always noOpMsg
    , setRhesusMsg = always noOpMsg
    , setProteinMsg = always noOpMsg
    , setPHMsg = always noOpMsg
    , setGlucoseMsg = always noOpMsg
    , setLeukocytesMsg = always noOpMsg
    , setNitriteMsg = always noOpMsg
    , setUrobilinogenMsg = always noOpMsg
    , setHaemoglobinMsg = always noOpMsg
    , setKetoneMsg = always noOpMsg
    , setBilirubinMsg = always noOpMsg
    , setHemoglobinCountMsg = always noOpMsg
    , setRandomBloodSugarResultMsg = always noOpMsg
    , setHIVViralLoadMsg = always noOpMsg
    , setHIVViralLoadUndetectableMsg = always noOpMsg
    , setPartnerHIVTestResultMsg = always noOpMsg
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


emptyContentAndTasksLaboratoryResultConfig : msg -> ContentAndTasksLaboratoryResultConfig msg encounterId
emptyContentAndTasksLaboratoryResultConfig noOpMsg =
    { setHIVTestFormBoolInputMsg = \_ _ -> noOpMsg
    , setHIVTestExecutionNoteMsg = always noOpMsg
    , setPartnerHIVTestFormBoolInputMsg = \_ _ -> noOpMsg
    , setPartnerHIVTestExecutionNoteMsg = always noOpMsg
    , setSyphilisTestFormBoolInputMsg = \_ _ -> noOpMsg
    , setSyphilisTestExecutionNoteMsg = always noOpMsg
    , setHepatitisBTestFormBoolInputMsg = \_ _ -> noOpMsg
    , setHepatitisBTestExecutionNoteMsg = always noOpMsg
    , setMalariaTestFormBoolInputMsg = \_ _ -> noOpMsg
    , setMalariaTestExecutionNoteMsg = always noOpMsg
    , setBloodGpRsTestFormBoolInputMsg = \_ _ -> noOpMsg
    , setBloodGpRsTestExecutionNoteMsg = always noOpMsg
    , setUrineDipstickTestFormBoolInputMsg = \_ _ -> noOpMsg
    , setUrineDipstickTestExecutionNoteMsg = always noOpMsg
    , setHemoglobinTestFormBoolInputMsg = \_ _ -> noOpMsg
    , setHemoglobinTestExecutionNoteMsg = always noOpMsg
    , setRandomBloodSugarTestFormBoolInputMsg = \_ _ -> noOpMsg
    , setRandomBloodSugarTestExecutionNoteMsg = always noOpMsg
    , setHIVPCRTestFormBoolInputMsg = \_ _ -> noOpMsg
    , setHIVPCRTestExecutionNoteMsg = always noOpMsg

    -- , setPregnancyTestFormBoolInputMsg = \_ _ -> noOpMsg
    -- , setPregnancyTestExecutionNoteMsg = always noOpMsg
    -- , setPregnancyTestResultMsg = always noOpMsg
    -- , setCreatinineTestFormBoolInputMsg = \_ _ -> noOpMsg
    -- , setCreatinineTestExecutionNoteMsg = always noOpMsg
    -- , setLiverFunctionTestFormBoolInputMsg = \_ _ -> noOpMsg
    -- , setLiverFunctionTestExecutionNoteMsg = always noOpMsg
    -- , setLipidPanelTestFormBoolInputMsg = \_ _ -> noOpMsg
    -- , setLipidPanelTestExecutionNoteMsg = always noOpMsg
    -- , setHbA1cTestFormBoolInputMsg = \_ _ -> noOpMsg
    -- , setHbA1cTestExecutionNoteMsg = always noOpMsg
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

        TaskPartnerHIVTest ->
            "laboratory-hiv"


hepatitisBResultFormWithDefault : HepatitisBResultForm encounterId -> Maybe (HepatitisBTestValue encounterId) -> HepatitisBResultForm encounterId
hepatitisBResultFormWithDefault form saved =
    saved
        |> unwrap
            form
            (\value ->
                let
                    runConfirmedByLabTechFromValue =
                        resolveRunConfirmedByLabTechFromValue value
                in
                { runConfirmedByLabTech = or form.runConfirmedByLabTech runConfirmedByLabTechFromValue
                , executionNote =
                    maybeValueConsideringIsDirtyField form.executionNoteDirty
                        form.executionNote
                        (Just value.executionNote)
                , executionNoteDirty = form.executionNoteDirty
                , executionDate = or form.executionDate value.executionDate
                , testPrerequisites = or form.testPrerequisites value.testPrerequisites
                , testResult =
                    maybeValueConsideringIsDirtyField form.testResultDirty
                        form.testResult
                        value.testResult
                , testResultDirty = form.testResultDirty
                , originatingEncounter = or form.originatingEncounter value.originatingEncounter
                }
            )


toHepatitisBResultValueWithDefault : Maybe (HepatitisBTestValue encounterId) -> HepatitisBResultForm encounterId -> Maybe (HepatitisBTestValue encounterId)
toHepatitisBResultValueWithDefault saved form =
    hepatitisBResultFormWithDefault form saved
        |> toHepatitisBResultValue


toHepatitisBResultValue : HepatitisBResultForm encounterId -> Maybe (HepatitisBTestValue encounterId)
toHepatitisBResultValue form =
    Maybe.map
        (\executionNote ->
            let
                executionNoteConsideringLabTech =
                    if form.runConfirmedByLabTech == Just True then
                        TestNoteRunConfirmedByLabTech

                    else
                        executionNote
            in
            { executionNote = executionNoteConsideringLabTech
            , executionDate = form.executionDate
            , testPrerequisites = form.testPrerequisites
            , testResult = form.testResult
            , originatingEncounter = form.originatingEncounter
            }
        )
        form.executionNote


malariaResultFormWithDefault : MalariaResultForm -> Maybe MalariaTestValue -> MalariaResultForm
malariaResultFormWithDefault form saved =
    saved
        |> unwrap
            form
            (\value ->
                let
                    runConfirmedByLabTechFromValue =
                        resolveRunConfirmedByLabTechFromValue value

                    bloodSmearTakenByValue =
                        List.member value.bloodSmearResult
                            [ BloodSmearNegative
                            , BloodSmearPlus
                            , BloodSmearPlusPlus
                            , BloodSmearPlusPlusPlus
                            , BloodSmearPendingInput
                            ]

                    -- If we have an indication that Blood Smear test was
                    -- ordered on initail phase, empty it's value.
                    bloodSmearResultByValue =
                        if value.bloodSmearResult == BloodSmearPendingInput then
                            Nothing

                        else
                            Just value.bloodSmearResult
                in
                { runConfirmedByLabTech = or form.runConfirmedByLabTech runConfirmedByLabTechFromValue
                , executionNote =
                    maybeValueConsideringIsDirtyField form.executionNoteDirty
                        form.executionNote
                        (Just value.executionNote)
                , executionNoteDirty = form.executionNoteDirty
                , executionDate = or form.executionDate value.executionDate
                , testPrerequisites = or form.testPrerequisites value.testPrerequisites
                , testResult =
                    maybeValueConsideringIsDirtyField form.testResultDirty
                        form.testResult
                        value.testResult
                , testResultDirty = form.testResultDirty
                , bloodSmearTaken = bloodSmearTakenByValue
                , bloodSmearResult =
                    maybeValueConsideringIsDirtyField form.bloodSmearResultDirty
                        form.bloodSmearResult
                        bloodSmearResultByValue
                , bloodSmearResultDirty = form.bloodSmearResultDirty
                }
            )


toMalariaResultValueWithDefault : Maybe MalariaTestValue -> MalariaResultForm -> Maybe MalariaTestValue
toMalariaResultValueWithDefault saved form =
    malariaResultFormWithDefault form saved
        |> toMalariaResultValue


toMalariaResultValue : MalariaResultForm -> Maybe MalariaTestValue
toMalariaResultValue form =
    Maybe.map
        (\executionNote ->
            let
                executionNoteConsideringLabTech =
                    if form.runConfirmedByLabTech == Just True then
                        TestNoteRunConfirmedByLabTech

                    else
                        executionNote
            in
            { executionNote = executionNoteConsideringLabTech
            , executionDate = form.executionDate
            , testPrerequisites = form.testPrerequisites
            , testResult = form.testResult
            , bloodSmearResult = Maybe.withDefault BloodSmearNotTaken form.bloodSmearResult
            }
        )
        form.executionNote


syphilisResultFormWithDefault : SyphilisResultForm encounterId -> Maybe (SyphilisTestValue encounterId) -> SyphilisResultForm encounterId
syphilisResultFormWithDefault form saved =
    saved
        |> unwrap
            form
            (\value ->
                let
                    runConfirmedByLabTechFromValue =
                        resolveRunConfirmedByLabTechFromValue value

                    symptomsByValue =
                        Maybe.andThen
                            (\symptoms_ ->
                                if EverySet.member IllnessSymptomPendingInput symptoms_ then
                                    -- When 'pending input' is set, we know it's an indecation
                                    -- that symptoms are yet to be set (since it was Lab tech
                                    -- filling the results of the test).
                                    Nothing

                                else
                                    Just <| EverySet.toList symptoms_
                            )
                            value.symptoms
                in
                { runConfirmedByLabTech = or form.runConfirmedByLabTech runConfirmedByLabTechFromValue
                , executionNote =
                    maybeValueConsideringIsDirtyField form.executionNoteDirty
                        form.executionNote
                        (Just value.executionNote)
                , executionNoteDirty = form.executionNoteDirty
                , executionDate = or form.executionDate value.executionDate
                , testPrerequisites = or form.testPrerequisites value.testPrerequisites
                , testResult =
                    maybeValueConsideringIsDirtyField form.testResultDirty
                        form.testResult
                        value.testResult
                , testResultDirty = form.testResultDirty
                , symptoms = maybeValueConsideringIsDirtyField form.symptomsDirty form.symptoms symptomsByValue
                , symptomsDirty = form.symptomsDirty
                , originatingEncounter = or form.originatingEncounter value.originatingEncounter
                }
            )


toSyphilisResultValueWithDefault : Bool -> Maybe (SyphilisTestValue encounterId) -> SyphilisResultForm encounterId -> Maybe (SyphilisTestValue encounterId)
toSyphilisResultValueWithDefault isLabTech saved form =
    syphilisResultFormWithDefault form saved
        |> toSyphilisResultValue isLabTech


toSyphilisResultValue : Bool -> SyphilisResultForm encounterId -> Maybe (SyphilisTestValue encounterId)
toSyphilisResultValue isLabTech form =
    Maybe.map
        (\executionNote ->
            let
                executionNoteConsideringLabTech =
                    if form.runConfirmedByLabTech == Just True then
                        TestNoteRunConfirmedByLabTech

                    else
                        executionNote

                symptoms =
                    if isLabTech then
                        -- Lab technician can activate this function only from
                        -- recurrent phase, where they set lab results.
                        -- symptoms field is used for follow-up question after
                        -- result is entered, and can not be set by lab tach.
                        -- Therefore, we can safely set it to 'input pending'
                        -- value, indicating that nurse will have to fill it in
                        -- the follow up questions.
                        Just <| EverySet.singleton IllnessSymptomPendingInput

                    else
                        Maybe.map EverySet.fromList form.symptoms
            in
            { executionNote = executionNoteConsideringLabTech
            , executionDate = form.executionDate
            , testPrerequisites = form.testPrerequisites
            , testResult = form.testResult
            , symptoms = symptoms
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
                let
                    runConfirmedByLabTechFromValue =
                        resolveRunConfirmedByLabTechFromValue value
                in
                { runConfirmedByLabTech = or form.runConfirmedByLabTech runConfirmedByLabTechFromValue
                , executionNote =
                    maybeValueConsideringIsDirtyField form.executionNoteDirty
                        form.executionNote
                        (Just value.executionNote)
                , executionNoteDirty = form.executionNoteDirty
                , executionDate = or form.executionDate value.executionDate
                , testPrerequisites = or form.testPrerequisites value.testPrerequisites
                , bloodGroup =
                    maybeValueConsideringIsDirtyField form.bloodGroupDirty
                        form.bloodGroup
                        value.bloodGroup
                , bloodGroupDirty = form.bloodGroupDirty
                , rhesus =
                    maybeValueConsideringIsDirtyField form.rhesusDirty
                        form.rhesus
                        value.rhesus
                , rhesusDirty = form.rhesusDirty
                , originatingEncounter = or form.originatingEncounter value.originatingEncounter
                }
            )


toBloodGpRsResultValueWithDefault : Maybe (BloodGpRsTestValue encounterId) -> BloodGpRsResultForm encounterId -> Maybe (BloodGpRsTestValue encounterId)
toBloodGpRsResultValueWithDefault saved form =
    bloodGpRsResultFormWithDefault form saved
        |> toBloodGpRsResultValue


toBloodGpRsResultValue : BloodGpRsResultForm encounterId -> Maybe (BloodGpRsTestValue encounterId)
toBloodGpRsResultValue form =
    Maybe.map
        (\executionNote ->
            let
                executionNoteConsideringLabTech =
                    if form.runConfirmedByLabTech == Just True then
                        TestNoteRunConfirmedByLabTech

                    else
                        executionNote
            in
            { executionNote = executionNoteConsideringLabTech
            , executionDate = form.executionDate
            , testPrerequisites = form.testPrerequisites
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
                let
                    runConfirmedByLabTechFromValue =
                        resolveRunConfirmedByLabTechFromValue value
                in
                { runConfirmedByLabTech = or form.runConfirmedByLabTech runConfirmedByLabTechFromValue
                , executionNote =
                    maybeValueConsideringIsDirtyField form.executionNoteDirty
                        form.executionNote
                        (Just value.executionNote)
                , executionNoteDirty = form.executionNoteDirty
                , executionDate = or form.executionDate value.executionDate
                , testPrerequisites = or form.testPrerequisites value.testPrerequisites
                , hemoglobinCount =
                    maybeValueConsideringIsDirtyField form.hemoglobinCountDirty
                        form.hemoglobinCount
                        value.hemoglobinCount
                , hemoglobinCountDirty = form.hemoglobinCountDirty
                }
            )


toHemoglobinResultValueWithDefault : Maybe HemoglobinTestValue -> HemoglobinResultForm -> Maybe HemoglobinTestValue
toHemoglobinResultValueWithDefault saved form =
    hemoglobinResultFormWithDefault form saved
        |> toHemoglobinResultValue


toHemoglobinResultValue : HemoglobinResultForm -> Maybe HemoglobinTestValue
toHemoglobinResultValue form =
    Maybe.map
        (\executionNote ->
            let
                executionNoteConsideringLabTech =
                    if form.runConfirmedByLabTech == Just True then
                        TestNoteRunConfirmedByLabTech

                    else
                        executionNote
            in
            { executionNote = executionNoteConsideringLabTech
            , executionDate = form.executionDate
            , testPrerequisites = form.testPrerequisites
            , hemoglobinCount = form.hemoglobinCount
            }
        )
        form.executionNote


randomBloodSugarResultFormWithDefault :
    RandomBloodSugarResultForm encounterId
    -> Maybe (RandomBloodSugarTestValue encounterId)
    -> RandomBloodSugarResultForm encounterId
randomBloodSugarResultFormWithDefault form saved =
    saved
        |> unwrap
            form
            (\value ->
                let
                    runConfirmedByLabTechFromValue =
                        resolveRunConfirmedByLabTechFromValue value
                in
                { runConfirmedByLabTech = or form.runConfirmedByLabTech runConfirmedByLabTechFromValue
                , executionNote =
                    maybeValueConsideringIsDirtyField form.executionNoteDirty
                        form.executionNote
                        (Just value.executionNote)
                , executionNoteDirty = form.executionNoteDirty
                , executionDate = or form.executionDate value.executionDate
                , testPrerequisites = or form.testPrerequisites value.testPrerequisites
                , sugarCount = maybeValueConsideringIsDirtyField form.sugarCountDirty form.sugarCount value.sugarCount
                , sugarCountDirty = form.sugarCountDirty
                , originatingEncounter = or form.originatingEncounter value.originatingEncounter
                }
            )


toRandomBloodSugarResultValueWithDefault : Maybe (RandomBloodSugarTestValue encounterId) -> RandomBloodSugarResultForm encounterId -> Maybe (RandomBloodSugarTestValue encounterId)
toRandomBloodSugarResultValueWithDefault saved form =
    randomBloodSugarResultFormWithDefault form saved
        |> toRandomBloodSugarResultValue


toRandomBloodSugarResultValue : RandomBloodSugarResultForm encounterId -> Maybe (RandomBloodSugarTestValue encounterId)
toRandomBloodSugarResultValue form =
    Maybe.map
        (\executionNote ->
            let
                executionNoteConsideringLabTech =
                    if form.runConfirmedByLabTech == Just True then
                        TestNoteRunConfirmedByLabTech

                    else
                        executionNote
            in
            { executionNote = executionNoteConsideringLabTech
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
                let
                    runConfirmedByLabTechFromValue =
                        resolveRunConfirmedByLabTechFromValue value
                in
                { runConfirmedByLabTech = or form.runConfirmedByLabTech runConfirmedByLabTechFromValue
                , executionNote =
                    maybeValueConsideringIsDirtyField form.executionNoteDirty
                        form.executionNote
                        (Just value.executionNote)
                , executionNoteDirty = form.executionNoteDirty
                , testVariant = or form.testVariant value.testVariant
                , executionDate = or form.executionDate value.executionDate
                , testPrerequisites = or form.testPrerequisites value.testPrerequisites
                , protein = maybeValueConsideringIsDirtyField form.proteinDirty form.protein value.protein
                , proteinDirty = form.proteinDirty
                , ph = maybeValueConsideringIsDirtyField form.phDirty form.ph value.ph
                , phDirty = form.phDirty
                , glucose = maybeValueConsideringIsDirtyField form.glucoseDirty form.glucose value.glucose
                , glucoseDirty = form.glucoseDirty
                , leukocytes = maybeValueConsideringIsDirtyField form.leukocytesDirty form.leukocytes value.leukocytes
                , leukocytesDirty = form.leukocytesDirty
                , nitrite = maybeValueConsideringIsDirtyField form.nitriteDirty form.nitrite value.nitrite
                , nitriteDirty = form.nitriteDirty
                , urobilinogen = maybeValueConsideringIsDirtyField form.urobilinogenDirty form.urobilinogen value.urobilinogen
                , urobilinogenDirty = form.urobilinogenDirty
                , haemoglobin = maybeValueConsideringIsDirtyField form.haemoglobinDirty form.haemoglobin value.haemoglobin
                , haemoglobinDirty = form.haemoglobinDirty
                , ketone = maybeValueConsideringIsDirtyField form.ketoneDirty form.ketone value.ketone
                , ketoneDirty = form.ketoneDirty
                , bilirubin = maybeValueConsideringIsDirtyField form.bilirubinDirty form.bilirubin value.bilirubin
                , bilirubinDirty = form.bilirubinDirty
                }
            )


toUrineDipstickResultValueWithDefault : Maybe UrineDipstickTestValue -> UrineDipstickResultForm -> Maybe UrineDipstickTestValue
toUrineDipstickResultValueWithDefault saved form =
    urineDipstickResultFormWithDefault form saved
        |> toUrineDipstickResultValue


toUrineDipstickResultValue : UrineDipstickResultForm -> Maybe UrineDipstickTestValue
toUrineDipstickResultValue form =
    Maybe.map
        (\executionNote ->
            let
                executionNoteConsideringLabTech =
                    if form.runConfirmedByLabTech == Just True then
                        TestNoteRunConfirmedByLabTech

                    else
                        executionNote
            in
            { executionNote = executionNoteConsideringLabTech
            , testVariant = form.testVariant
            , executionDate = form.executionDate
            , testPrerequisites = form.testPrerequisites
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
                let
                    runConfirmedByLabTechFromValue =
                        resolveRunConfirmedByLabTechFromValue value
                in
                { runConfirmedByLabTech = or form.runConfirmedByLabTech runConfirmedByLabTechFromValue
                , executionNote =
                    maybeValueConsideringIsDirtyField form.executionNoteDirty
                        form.executionNote
                        (Just value.executionNote)
                , executionNoteDirty = form.executionNoteDirty
                , executionDate = or form.executionDate value.executionDate
                , testPrerequisites = or form.testPrerequisites value.testPrerequisites
                , hivViralLoadStatus =
                    maybeValueConsideringIsDirtyField form.hivViralLoadStatusDirty
                        form.hivViralLoadStatus
                        value.hivViralLoadStatus
                , hivViralLoadStatusDirty = form.hivViralLoadStatusDirty
                , hivViralLoad =
                    maybeValueConsideringIsDirtyField form.hivViralLoadDirty
                        form.hivViralLoad
                        value.hivViralLoad
                , hivViralLoadDirty = form.hivViralLoadDirty
                }
            )


toHIVPCRResultValueWithDefault : Maybe HIVPCRTestValue -> HIVPCRResultForm -> Maybe HIVPCRTestValue
toHIVPCRResultValueWithDefault saved form =
    hivPCRResultFormWithDefault form saved
        |> toHIVPCRResultValue


toHIVPCRResultValue : HIVPCRResultForm -> Maybe HIVPCRTestValue
toHIVPCRResultValue form =
    Maybe.map
        (\executionNote ->
            let
                executionNoteConsideringLabTech =
                    if form.runConfirmedByLabTech == Just True then
                        TestNoteRunConfirmedByLabTech

                    else
                        executionNote
            in
            { executionNote = executionNoteConsideringLabTech
            , executionDate = form.executionDate
            , testPrerequisites = form.testPrerequisites
            , hivViralLoadStatus = form.hivViralLoadStatus
            , hivViralLoad = form.hivViralLoad
            }
        )
        form.executionNote


resolveRunConfirmedByLabTechFromValue : { v | executionNote : TestExecutionNote } -> Maybe Bool
resolveRunConfirmedByLabTechFromValue value =
    if value.executionNote == TestNoteRunConfirmedByLabTech then
        Just True

    else if testNotPerformedByWhyNotAtExecutionNote value.executionNote then
        Just False

    else
        Nothing


hivResultFormWithDefault : HIVResultForm -> Maybe HIVTestValue -> HIVResultForm
hivResultFormWithDefault form =
    unwrap
        form
        (\value ->
            let
                runConfirmedByLabTechFromValue =
                    resolveRunConfirmedByLabTechFromValue value

                signsByValue =
                    Maybe.map
                        (\signs ->
                            if EverySet.member PrenatalHIVSignPendingInput signs then
                                -- When 'pending input' is set, we know it's an indecation
                                -- that signs are yet to be set (since it was Lab tech
                                -- filling the results of the test).
                                { hivProgramHCValue = Nothing
                                , partnerHIVPositiveValue = Nothing
                                , partnerTakingARVValue = Nothing
                                , partnerSurpressedViralLoadValue = Nothing
                                }

                            else
                                { hivProgramHCValue = EverySet.member HIVProgramHC signs |> Just
                                , partnerHIVPositiveValue = EverySet.member PartnerHIVPositive signs |> Just
                                , partnerTakingARVValue = EverySet.member PartnerTakingARV signs |> Just
                                , partnerSurpressedViralLoadValue = EverySet.member PartnerSurpressedViralLoad signs |> Just
                                }
                        )
                        value.hivSigns
                        |> Maybe.withDefault
                            { hivProgramHCValue = Nothing
                            , partnerHIVPositiveValue = Nothing
                            , partnerTakingARVValue = Nothing
                            , partnerSurpressedViralLoadValue = Nothing
                            }
            in
            { runConfirmedByLabTech = or form.runConfirmedByLabTech runConfirmedByLabTechFromValue
            , executionNote =
                maybeValueConsideringIsDirtyField form.executionNoteDirty
                    form.executionNote
                    (Just value.executionNote)
            , executionNoteDirty = form.executionNoteDirty
            , executionDate = or form.executionDate value.executionDate
            , testPrerequisites = or form.testPrerequisites value.testPrerequisites
            , testResult =
                maybeValueConsideringIsDirtyField form.testResultDirty
                    form.testResult
                    value.testResult
            , testResultDirty = form.testResultDirty
            , hivProgramHC =
                maybeValueConsideringIsDirtyField form.hivProgramHCDirty
                    form.hivProgramHC
                    signsByValue.hivProgramHCValue
            , hivProgramHCDirty = form.hivProgramHCDirty
            , partnerHIVPositive =
                maybeValueConsideringIsDirtyField form.partnerHIVPositiveDirty
                    form.partnerHIVPositive
                    signsByValue.partnerHIVPositiveValue
            , partnerHIVPositiveDirty = form.partnerHIVPositiveDirty
            , partnerTakingARV =
                maybeValueConsideringIsDirtyField form.partnerTakingARVDirty
                    form.partnerTakingARV
                    signsByValue.partnerTakingARVValue
            , partnerTakingARVDirty = form.partnerTakingARVDirty
            , partnerSurpressedViralLoad =
                maybeValueConsideringIsDirtyField form.partnerSurpressedViralLoadDirty
                    form.partnerSurpressedViralLoad
                    signsByValue.partnerSurpressedViralLoadValue
            , partnerSurpressedViralLoadDirty = form.partnerSurpressedViralLoadDirty
            }
        )


toHIVResultValueWithDefault : Bool -> Maybe HIVTestValue -> HIVResultForm -> Maybe HIVTestValue
toHIVResultValueWithDefault isLabTech saved form =
    hivResultFormWithDefault form saved
        |> toHIVResultValue isLabTech


toHIVResultValue : Bool -> HIVResultForm -> Maybe HIVTestValue
toHIVResultValue isLabTech form =
    Maybe.map
        (\executionNote ->
            let
                executionNoteConsideringLabTech =
                    if form.runConfirmedByLabTech == Just True then
                        TestNoteRunConfirmedByLabTech

                    else
                        executionNote

                hivSigns =
                    if isLabTech then
                        -- Lab technician can activate this function only from
                        -- recurrent phase, where they set lab results.
                        -- hivSigns field is used for follow-up questions after
                        -- result is entered, and can not be set by lab tech.
                        -- Therefore, we can safely set it to 'input pending'
                        -- value, indicating that nurse will have to fill it in
                        -- the follow up questions.
                        Just <| EverySet.singleton PrenatalHIVSignPendingInput

                    else
                        [ ifNullableTrue HIVProgramHC form.hivProgramHC
                        , ifNullableTrue PartnerHIVPositive form.partnerHIVPositive
                        , ifNullableTrue PartnerTakingARV form.partnerTakingARV
                        , ifNullableTrue PartnerSurpressedViralLoad form.partnerSurpressedViralLoad
                        ]
                            |> Maybe.Extra.combine
                            |> Maybe.map (List.foldl EverySet.union EverySet.empty >> ifEverySetEmpty NoPrenatalHIVSign)
            in
            { executionNote = executionNoteConsideringLabTech
            , executionDate = form.executionDate
            , testPrerequisites = form.testPrerequisites
            , testResult = form.testResult
            , hivSigns = hivSigns
            }
        )
        form.executionNote


partnerHIVResultFormWithDefault : PartnerHIVResultForm -> Maybe PartnerHIVTestValue -> PartnerHIVResultForm
partnerHIVResultFormWithDefault form saved =
    saved
        |> unwrap
            form
            (\value ->
                let
                    runConfirmedByLabTechFromValue =
                        resolveRunConfirmedByLabTechFromValue value

                    signsByValue =
                        Maybe.map
                            (\signs ->
                                if EverySet.member PrenatalHIVSignPendingInput signs then
                                    -- When 'pending input' is set, we know it's an indecation
                                    -- that signs are yet to be set (since it was Lab tech
                                    -- filling the results of the test).
                                    { partnerTakingARVValue = Nothing
                                    , partnerSurpressedViralLoadValue = Nothing
                                    }

                                else
                                    { partnerTakingARVValue = EverySet.member PartnerTakingARV signs |> Just
                                    , partnerSurpressedViralLoadValue = EverySet.member PartnerSurpressedViralLoad signs |> Just
                                    }
                            )
                            value.hivSigns
                            |> Maybe.withDefault
                                { partnerTakingARVValue = Nothing
                                , partnerSurpressedViralLoadValue = Nothing
                                }
                in
                { runConfirmedByLabTech = or form.runConfirmedByLabTech runConfirmedByLabTechFromValue
                , executionNote =
                    maybeValueConsideringIsDirtyField form.executionNoteDirty
                        form.executionNote
                        (Just value.executionNote)
                , executionNoteDirty = form.executionNoteDirty
                , executionDate = or form.executionDate value.executionDate
                , testPrerequisites = or form.testPrerequisites value.testPrerequisites
                , testResult =
                    maybeValueConsideringIsDirtyField form.testResultDirty
                        form.testResult
                        value.testResult
                , testResultDirty = form.testResultDirty
                , partnerTakingARV =
                    maybeValueConsideringIsDirtyField form.partnerTakingARVDirty
                        form.partnerTakingARV
                        signsByValue.partnerTakingARVValue
                , partnerTakingARVDirty = form.partnerTakingARVDirty
                , partnerSurpressedViralLoad =
                    maybeValueConsideringIsDirtyField form.partnerSurpressedViralLoadDirty
                        form.partnerSurpressedViralLoad
                        signsByValue.partnerSurpressedViralLoadValue
                , partnerSurpressedViralLoadDirty = form.partnerSurpressedViralLoadDirty
                }
            )


toPartnerHIVResultValueWithDefault : Bool -> Maybe PartnerHIVTestValue -> PartnerHIVResultForm -> Maybe PartnerHIVTestValue
toPartnerHIVResultValueWithDefault isLabTech saved form =
    partnerHIVResultFormWithDefault form saved
        |> toPartnerHIVResultValue isLabTech


toPartnerHIVResultValue : Bool -> PartnerHIVResultForm -> Maybe PartnerHIVTestValue
toPartnerHIVResultValue isLabTech form =
    Maybe.map
        (\executionNote ->
            let
                executionNoteConsideringLabTech =
                    if form.runConfirmedByLabTech == Just True then
                        TestNoteRunConfirmedByLabTech

                    else
                        executionNote

                hivSigns =
                    if isLabTech then
                        -- Lab technician can activate this function only from
                        -- recurrent phase, where they set lab results.
                        -- hivSigns field is used for follow-up questions after
                        -- result is entered, and can not be set by lab tech.
                        -- Therefore, we can safely set it to 'input pending'
                        -- value, indicating that nurse will have to fill it in
                        -- the follow up questions.
                        Just <| EverySet.singleton PrenatalHIVSignPendingInput

                    else
                        [ ifNullableTrue PartnerTakingARV form.partnerTakingARV
                        , ifNullableTrue PartnerSurpressedViralLoad form.partnerSurpressedViralLoad
                        ]
                            |> Maybe.Extra.combine
                            |> Maybe.map (List.foldl EverySet.union EverySet.empty >> ifEverySetEmpty NoPrenatalHIVSign)
            in
            { executionNote = executionNoteConsideringLabTech
            , executionDate = form.executionDate
            , testPrerequisites = form.testPrerequisites
            , testResult = form.testResult
            , hivSigns = hivSigns
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


toCreatinineResultValueWithDefault : Maybe CreatinineTestValue -> CreatinineResultForm -> Maybe CreatinineTestValue
toCreatinineResultValueWithDefault saved form =
    creatinineResultFormWithDefault form saved
        |> toCreatinineResultValue


toCreatinineResultValue : CreatinineResultForm -> Maybe CreatinineTestValue
toCreatinineResultValue form =
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


toLiverFunctionResultValueWithDefault : Maybe LiverFunctionTestValue -> LiverFunctionResultForm -> Maybe LiverFunctionTestValue
toLiverFunctionResultValueWithDefault saved form =
    liverFunctionResultFormWithDefault form saved
        |> toLiverFunctionResultValue


toLiverFunctionResultValue : LiverFunctionResultForm -> Maybe LiverFunctionTestValue
toLiverFunctionResultValue form =
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


toLipidPanelResultValueWithDefault : Maybe LipidPanelTestValue -> LipidPanelResultForm -> Maybe LipidPanelTestValue
toLipidPanelResultValueWithDefault saved form =
    lipidPanelResultFormWithDefault form saved
        |> toLipidPanelResultValue


toLipidPanelResultValue : LipidPanelResultForm -> Maybe LipidPanelTestValue
toLipidPanelResultValue form =
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
              , viewSelectListInput language
                    form.unitOfMeasurement
                    [ UnitMmolL, UnitMgdL ]
                    unitOfMeasurementToString
                    setUnitOfMeasurementMsg
                    Translate.UnitOfMeasurement
                    "select unit-of-measurement"
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
    List.member executionNote [ TestNoteRunToday, TestNoteRunPreviously, TestNoteRunConfirmedByLabTech ]


testNotPerformedByWhyNotAtExecutionNote : TestExecutionNote -> Bool
testNotPerformedByWhyNotAtExecutionNote executionNote =
    List.member executionNote
        [ TestNoteLackOfReagents
        , TestNoteLackOfOtherSupplies
        , TestNoteNoEquipment
        , TestNoteBrokenEquipment
        , TestNoteNotIndicated
        ]


expectUniversalTestResultTask : { v | testPrerequisites : Maybe (EverySet TestPrerequisite), executionNote : TestExecutionNote } -> Bool
expectUniversalTestResultTask value =
    -- It's possible to enter the result immediatly (and not from
    -- Case management).
    -- If this is the case, we do not expect to see results task.
    Maybe.map (EverySet.member PrerequisiteImmediateResult >> not) value.testPrerequisites
        |> Maybe.withDefault False


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
    , viewCustomSelectListInput formValue
        valuesList
        valueToStringFunc
        setMsg
        (valueTransId >> translate language)
        "form-input select"
        (isNothing formValue)
    ]


fromNCDAValue : Maybe NCDAValue -> NCDAForm
fromNCDAValue saved =
    let
        ancVisitsDates =
            Maybe.map .ancVisitsDates saved

        updateANCVisits =
            Maybe.map (EverySet.isEmpty >> not)
                ancVisitsDates
    in
    { step = Nothing
    , updateANCVisits = updateANCVisits
    , ancVisitsDates = ancVisitsDates
    , appropriateComplementaryFeeding = Maybe.map (.signs >> EverySet.member AppropriateComplementaryFeeding) saved
    , bornWithBirthDefect = Maybe.map (.signs >> EverySet.member BornWithBirthDefect) saved
    , breastfedForSixMonths = Maybe.map (.signs >> EverySet.member BreastfedForSixMonths) saved
    , conditionalFoodItems = Maybe.map (.signs >> EverySet.member ConditionalFoodItems) saved
    , fiveFoodGroups = Maybe.map (.signs >> EverySet.member FiveFoodGroups) saved
    , hasCleanWater = Maybe.map (.signs >> EverySet.member HasCleanWater) saved
    , hasHandwashingFacility = Maybe.map (.signs >> EverySet.member HasHandwashingFacility) saved
    , hasToilets = Maybe.map (.signs >> EverySet.member HasToilets) saved
    , hasKitchenGarden = Maybe.map (.signs >> EverySet.member HasKitchenGarden) saved

    -- New:
    , beneficiaryCashTransfer = Maybe.map (.signs >> EverySet.member BeneficiaryCashTransfer) saved
    , childGotDiarrhea = Maybe.map (.signs >> EverySet.member ChildGotDiarrhea) saved
    , childReceivesFBF = Maybe.map (.signs >> EverySet.member ChildReceivesFBF) saved
    , childTakingFBF = Maybe.map (.signs >> EverySet.member ChildTakingFBF) saved
    , childReceivesVitaminA = Maybe.andThen .receivesVitaminA saved
    , childReceivesDewormer = Maybe.map (.signs >> EverySet.member ChildReceivesDewormer) saved
    , childReceivesECD = Maybe.map (.signs >> EverySet.member ChildReceivesECD) saved
    , childWithDisability = Maybe.map (.signs >> EverySet.member ChildWithDisability) saved
    , ongeraMNP = Maybe.map (.signs >> EverySet.member OngeraMNP) saved
    , insecticideTreatedBednets = Maybe.map (.signs >> EverySet.member InsecticideTreatedBednets) saved
    , childBehindOnVaccination = Maybe.map (.signs >> EverySet.member ChildBehindOnVaccination) saved
    , receivingCashTransfer = Maybe.map (.signs >> EverySet.member ReceivingCashTransfer) saved
    , receivingSupport = Maybe.map (.signs >> EverySet.member ReceivingSupport) saved
    , supplementsDuringPregnancy = Maybe.map (.signs >> EverySet.member SupplementsDuringPregnancy) saved
    , takenSupplementsPerGuidance = Maybe.map (.signs >> EverySet.member TakenSupplementsPerGuidance) saved
    , treatedForAcuteMalnutrition = Maybe.map (.signs >> EverySet.member TreatedForAcuteMalnutrition) saved
    , takingOngeraMNP = Maybe.map (.signs >> EverySet.member TakingOngeraMNP) saved
    , mealsAtRecommendedTimes = Maybe.map (.signs >> EverySet.member MealsAtRecommendedTimes) saved
    , birthWeight = Maybe.andThen .birthWeight saved

    -- Nutrition measurements.
    , stuntingLevel = Maybe.andThen .stuntingLevel saved
    , stuntingLevelNotTaken = Maybe.map (.stuntingLevel >> isNothing) saved
    , weight = Maybe.andThen .weight saved
    , weightNotTaken = Maybe.map (.weight >> isNothing) saved
    , muac = Maybe.andThen .muac saved
    , muacNotTaken = Maybe.map (.muac >> isNothing) saved
    , showsEdemaSigns = Maybe.map (.signs >> EverySet.member ShowsEdemaSigns) saved
    }


ncdaFormWithDefault : NCDAForm -> Maybe NCDAValue -> NCDAForm
ncdaFormWithDefault form saved =
    saved
        |> unwrap
            form
            (\value ->
                let
                    updateANCVisits =
                        EverySet.isEmpty value.ancVisitsDates |> not
                in
                { step = form.step
                , updateANCVisits = or form.updateANCVisits (Just updateANCVisits)
                , ancVisitsDates = or form.ancVisitsDates (Just value.ancVisitsDates)
                , appropriateComplementaryFeeding = or form.appropriateComplementaryFeeding (EverySet.member AppropriateComplementaryFeeding value.signs |> Just)
                , bornWithBirthDefect = or form.bornWithBirthDefect (EverySet.member BornWithBirthDefect value.signs |> Just)
                , breastfedForSixMonths = or form.breastfedForSixMonths (EverySet.member BreastfedForSixMonths value.signs |> Just)
                , conditionalFoodItems = or form.conditionalFoodItems (EverySet.member ConditionalFoodItems value.signs |> Just)
                , fiveFoodGroups = or form.fiveFoodGroups (EverySet.member FiveFoodGroups value.signs |> Just)
                , hasCleanWater = or form.hasCleanWater (EverySet.member HasCleanWater value.signs |> Just)
                , hasHandwashingFacility = or form.hasHandwashingFacility (EverySet.member HasHandwashingFacility value.signs |> Just)
                , hasToilets = or form.hasToilets (EverySet.member HasToilets value.signs |> Just)
                , hasKitchenGarden = or form.hasKitchenGarden (EverySet.member HasKitchenGarden value.signs |> Just)

                -- New:
                , beneficiaryCashTransfer = or form.beneficiaryCashTransfer (EverySet.member BeneficiaryCashTransfer value.signs |> Just)
                , childGotDiarrhea = or form.childGotDiarrhea (EverySet.member ChildGotDiarrhea value.signs |> Just)
                , childReceivesFBF = or form.childReceivesFBF (EverySet.member ChildReceivesFBF value.signs |> Just)
                , childTakingFBF = or form.childTakingFBF (EverySet.member ChildTakingFBF value.signs |> Just)
                , childReceivesVitaminA = or form.childReceivesVitaminA value.receivesVitaminA
                , childReceivesDewormer = or form.childReceivesDewormer (EverySet.member ChildReceivesDewormer value.signs |> Just)
                , childReceivesECD = or form.childReceivesECD (EverySet.member ChildReceivesECD value.signs |> Just)
                , childWithDisability = or form.childWithDisability (EverySet.member ChildWithDisability value.signs |> Just)
                , ongeraMNP = or form.ongeraMNP (EverySet.member OngeraMNP value.signs |> Just)
                , insecticideTreatedBednets = or form.insecticideTreatedBednets (EverySet.member InsecticideTreatedBednets value.signs |> Just)
                , childBehindOnVaccination = or form.childBehindOnVaccination (EverySet.member ChildBehindOnVaccination value.signs |> Just)
                , receivingCashTransfer = or form.receivingCashTransfer (EverySet.member ReceivingCashTransfer value.signs |> Just)
                , receivingSupport = or form.receivingSupport (EverySet.member ReceivingSupport value.signs |> Just)
                , supplementsDuringPregnancy = or form.supplementsDuringPregnancy (EverySet.member SupplementsDuringPregnancy value.signs |> Just)
                , takenSupplementsPerGuidance = or form.takenSupplementsPerGuidance (EverySet.member TakenSupplementsPerGuidance value.signs |> Just)
                , treatedForAcuteMalnutrition = or form.treatedForAcuteMalnutrition (EverySet.member TreatedForAcuteMalnutrition value.signs |> Just)
                , takingOngeraMNP = or form.takingOngeraMNP (EverySet.member TakingOngeraMNP value.signs |> Just)
                , mealsAtRecommendedTimes = or form.mealsAtRecommendedTimes (EverySet.member MealsAtRecommendedTimes value.signs |> Just)
                , birthWeight = or form.birthWeight value.birthWeight

                -- Nutrition measurements.
                , stuntingLevel = or form.stuntingLevel value.stuntingLevel
                , stuntingLevelNotTaken = or form.stuntingLevelNotTaken (isNothing value.stuntingLevel |> Just)
                , weight = or form.weight value.weight
                , weightNotTaken = or form.weightNotTaken (isNothing value.weight |> Just)
                , muac = or form.muac value.muac
                , muacNotTaken = or form.muacNotTaken (isNothing value.muac |> Just)
                , showsEdemaSigns = or form.showsEdemaSigns (EverySet.member ShowsEdemaSigns value.signs |> Just)
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
            [ ifNullableTrue AppropriateComplementaryFeeding form.appropriateComplementaryFeeding
            , ifNullableTrue BornWithBirthDefect form.bornWithBirthDefect
            , ifNullableTrue BreastfedForSixMonths form.breastfedForSixMonths
            , ifNullableTrue ConditionalFoodItems form.conditionalFoodItems
            , ifNullableTrue FiveFoodGroups form.fiveFoodGroups
            , ifNullableTrue HasCleanWater form.hasCleanWater
            , ifNullableTrue HasHandwashingFacility form.hasHandwashingFacility
            , ifNullableTrue HasToilets form.hasToilets
            , ifNullableTrue HasKitchenGarden form.hasKitchenGarden

            -- New:
            , ifNullableTrue BeneficiaryCashTransfer form.beneficiaryCashTransfer
            , ifNullableTrue ChildGotDiarrhea form.childGotDiarrhea
            , ifNullableTrue ChildReceivesFBF form.childReceivesFBF
            , ifNullableTrue ChildTakingFBF form.childTakingFBF
            , ifNullableTrue ChildReceivesDewormer form.childReceivesDewormer
            , ifNullableTrue ChildReceivesECD form.childReceivesECD
            , ifNullableTrue ChildWithDisability form.childWithDisability
            , ifNullableTrue OngeraMNP form.ongeraMNP
            , ifNullableTrue InsecticideTreatedBednets form.insecticideTreatedBednets
            , ifNullableTrue ChildBehindOnVaccination form.childBehindOnVaccination
            , ifNullableTrue ReceivingCashTransfer form.receivingCashTransfer
            , ifNullableTrue ReceivingSupport form.receivingSupport
            , ifNullableTrue SupplementsDuringPregnancy form.supplementsDuringPregnancy
            , ifNullableTrue TakenSupplementsPerGuidance form.takenSupplementsPerGuidance
            , ifNullableTrue TreatedForAcuteMalnutrition form.treatedForAcuteMalnutrition
            , ifNullableTrue TakingOngeraMNP form.takingOngeraMNP
            , ifNullableTrue MealsAtRecommendedTimes form.mealsAtRecommendedTimes
            , ifNullableTrue ShowsEdemaSigns form.showsEdemaSigns
            ]
                |> Maybe.Extra.combine
                |> Maybe.map (List.foldl EverySet.union EverySet.empty >> ifEverySetEmpty NoNCDASigns)

        ancVisitsDates =
            Maybe.withDefault EverySet.empty form.ancVisitsDates
    in
    Maybe.map NCDAValue signs
        |> andMap (Just form.birthWeight)
        |> andMap (Just ancVisitsDates)
        |> andMap (Just form.childReceivesVitaminA)
        |> andMap (Just form.stuntingLevel)
        |> andMap (Just form.weight)
        |> andMap (Just form.muac)


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
    getMeasurementValueFunc measurement
        |> Maybe.andThen
            (\value ->
                if testPerformedByExecutionNote value.executionNote then
                    let
                        resultsExist =
                            resultsExistFunc value
                    in
                    if resultsExist && (not <| resultsValidFunc value) then
                        -- Entered result is not valid, therefore,
                        -- we treat the test as if it was not performed.
                        Nothing

                    else
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
                        if not resultsExist && (Date.diff Days dateMeasured currentDate >= labExpirationPeriod) then
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


resolveChildANCPregnancyData : PersonId -> ModelIndexedDb -> ( Maybe NominalDate, EverySet NominalDate )
resolveChildANCPregnancyData childId db =
    Dict.get childId db.pregnancyByNewborn
        |> Maybe.andThen RemoteData.toMaybe
        |> Maybe.Extra.join
        |> Maybe.map
            (\( participantId, participant ) ->
                let
                    encountersDates =
                        Dict.get participantId db.prenatalEncountersByParticipant
                            |> Maybe.andThen RemoteData.toMaybe
                            |> Maybe.map
                                (Dict.values
                                    >> List.filterMap
                                        (\encounter ->
                                            if not <| List.member encounter.encounterType [ NursePostpartumEncounter, ChwPostpartumEncounter ] then
                                                Just encounter.startDate

                                            else
                                                Nothing
                                        )
                                    >> EverySet.fromList
                                )
                            |> Maybe.withDefault EverySet.empty
                in
                ( participant.eddDate, encountersDates )
            )
        |> Maybe.withDefault ( Nothing, EverySet.empty )


childDiagnosedWithMalnutrition : PersonId -> ModelIndexedDb -> Bool
childDiagnosedWithMalnutrition childId db =
    let
        individualParticipants =
            Dict.get childId db.individualParticipantsByPerson
                |> Maybe.andThen RemoteData.toMaybe
                |> Maybe.map Dict.toList
                |> Maybe.withDefault []

        individualWellChildParticipantId =
            List.filter
                (\( _, participant ) ->
                    participant.encounterType == Backend.IndividualEncounterParticipant.Model.WellChildEncounter
                )
                individualParticipants
                |> List.head
                |> Maybe.map Tuple.first

        individualNutritionParticipantId =
            List.filter
                (\( _, participant ) ->
                    participant.encounterType == Backend.IndividualEncounterParticipant.Model.NutritionEncounter
                )
                individualParticipants
                |> List.head
                |> Maybe.map Tuple.first

        groupNutritionMeasurements =
            Dict.get childId db.childMeasurements
                |> Maybe.andThen RemoteData.toMaybe
                |> Maybe.withDefault emptyChildMeasurementList

        individualNutritionMeasurements =
            Maybe.map
                (\participantId ->
                    Backend.Measurement.Utils.generatePreviousMeasurements
                        Backend.NutritionEncounter.Utils.getNutritionEncountersForParticipant
                        .nutritionMeasurements
                        Nothing
                        participantId
                        db
                )
                individualNutritionParticipantId
                |> Maybe.withDefault []
                |> List.map (Tuple.second >> Tuple.second)

        individualWellChildMeasurements =
            Maybe.map
                (\participantId ->
                    Backend.Measurement.Utils.generatePreviousMeasurements
                        getWellChildEncountersForParticipant
                        .wellChildMeasurements
                        Nothing
                        participantId
                        db
                )
                individualWellChildParticipantId
                |> Maybe.withDefault []
                |> List.map (Tuple.second >> Tuple.second)

        groupNutritionAssessment =
            generateGroupNutritionAssessmentEntries groupNutritionMeasurements

        individualNutritionAssessment =
            generateIndividualNutritionAssessmentEntries individualNutritionMeasurements

        individuaWellChildAssessment =
            generateIndividualNutritionAssessmentEntries individualWellChildMeasurements
    in
    individualNutritionAssessment
        ++ groupNutritionAssessment
        ++ individuaWellChildAssessment
        |> List.any
            (\( _, assessments ) ->
                List.member AssesmentAcuteMalnutritionModerate assessments
                    || List.member AssesmentAcuteMalnutritionSevere assessments
            )


resoloveLastScheduledImmunizationVisitDate : PersonId -> ModelIndexedDb -> Maybe NominalDate
resoloveLastScheduledImmunizationVisitDate childId db =
    let
        individualParticipants =
            Dict.get childId db.individualParticipantsByPerson
                |> Maybe.andThen RemoteData.toMaybe
                |> Maybe.map Dict.toList
                |> Maybe.withDefault []

        individualWellChildParticipantId =
            List.filter
                (\( _, participant ) ->
                    participant.encounterType == Backend.IndividualEncounterParticipant.Model.WellChildEncounter
                )
                individualParticipants
                |> List.head
                |> Maybe.map Tuple.first

        individualWellChildMeasurements =
            Maybe.map
                (\participantId ->
                    Backend.Measurement.Utils.generatePreviousMeasurements
                        getWellChildEncountersForParticipant
                        .wellChildMeasurements
                        Nothing
                        participantId
                        db
                )
                individualWellChildParticipantId
                |> Maybe.withDefault []
                |> List.map (Tuple.second >> Tuple.second)
    in
    List.filterMap
        (.nextVisit
            >> getMeasurementValueFunc
            >> Maybe.andThen .immunisationDate
        )
        individualWellChildMeasurements
        |> -- Since generatePreviousMeasurements sorts DESC the list,
           -- we know that at head of list we got immunization
           -- visit date that was scheduled last.
           List.head


isBehindOnVaccinationsByProgress : NominalDate -> Site -> PersonId -> ModelIndexedDb -> Bool
isBehindOnVaccinationsByProgress currentDate site childId db =
    let
        individualParticipants =
            Dict.get childId db.individualParticipantsByPerson
                |> Maybe.andThen RemoteData.toMaybe
                |> Maybe.map Dict.toList
                |> Maybe.withDefault []

        individualWellChildParticipantId =
            List.filter
                (\( _, participant ) ->
                    participant.encounterType == Backend.IndividualEncounterParticipant.Model.WellChildEncounter
                )
                individualParticipants
                |> List.head
                |> Maybe.map Tuple.first
    in
    if isJust individualWellChildParticipantId then
        behindOnVaccinationsByProgressFromWellChild currentDate site individualWellChildParticipantId db

    else
        let
            individualChildScoreboardParticipantId =
                List.filter
                    (\( _, participant ) ->
                        participant.encounterType == Backend.IndividualEncounterParticipant.Model.ChildScoreboardEncounter
                    )
                    individualParticipants
                    |> List.head
                    |> Maybe.map Tuple.first
        in
        behindOnVaccinationsByProgressFromChildScoreboard currentDate site individualChildScoreboardParticipantId db


behindOnVaccinationsByProgressFromWellChild : NominalDate -> Site -> Maybe IndividualEncounterParticipantId -> ModelIndexedDb -> Bool
behindOnVaccinationsByProgressFromWellChild currentDate site individualWellChildParticipantId db =
    let
        lastWellChildEncounterId =
            Maybe.andThen
                (getWellChildEncountersForParticipant db
                    >> List.map Tuple.first
                    >> List.head
                )
                individualWellChildParticipantId

        maybeAssembled =
            Maybe.andThen
                (\id ->
                    generateAssembledDataForWellChild site id db
                        |> RemoteData.toMaybe
                )
                lastWellChildEncounterId
    in
    Maybe.map
        (\assembled ->
            behindOnVaccinationsByProgress currentDate
                site
                assembled.person
                assembled.vaccinationProgress
        )
        maybeAssembled
        |> Maybe.withDefault False


behindOnVaccinationsByProgressFromChildScoreboard : NominalDate -> Site -> Maybe IndividualEncounterParticipantId -> ModelIndexedDb -> Bool
behindOnVaccinationsByProgressFromChildScoreboard currentDate site individualChildScoreboardParticipantId db =
    let
        lastChildScoreboardEncounterId =
            Maybe.andThen
                (getChildScoreboardEncountersForParticipant db
                    >> List.map Tuple.first
                    >> List.head
                )
                individualChildScoreboardParticipantId

        maybeAssembled =
            Maybe.andThen
                (\id ->
                    generateAssembledDataForChildScoreboard site id db
                        |> RemoteData.toMaybe
                )
                lastChildScoreboardEncounterId
    in
    Maybe.map
        (\assembled ->
            behindOnVaccinationsByProgress currentDate
                site
                assembled.person
                assembled.vaccinationProgress
        )
        maybeAssembled
        |> Maybe.withDefault False


{-| Here we look at vaccinations given during previous encounters.
So that we know the state up until current encounter (where additional VaccinationStatus
may have been recorded.)
-}
behindOnVaccinationsByHistory :
    NominalDate
    -> Site
    -> Person
    -> VaccinationProgressDict
    -> VaccinationProgressDict
    -> Bool
behindOnVaccinationsByHistory currentDate site person vaccinationHistory vaccinationProgress =
    generateSuggestedVaccinations currentDate
        site
        person
        vaccinationHistory
        vaccinationProgress
        |> List.isEmpty
        |> not


{-| Here we look at vaccinations given at previous encounters AND at
current encounter, so we know the state at current moment.
-}
behindOnVaccinationsByProgress :
    NominalDate
    -> Site
    -> Person
    -> VaccinationProgressDict
    -> Bool
behindOnVaccinationsByProgress currentDate site person vaccinationProgress =
    generateSuggestedVaccinations currentDate
        site
        person
        vaccinationProgress
        vaccinationProgress
        |> List.isEmpty
        |> not


resolveNCDASteps : NominalDate -> Person -> Bool -> Bool -> List NCDAStep
resolveNCDASteps currentDate person ncdaNeverFilled atHealthCenter =
    List.filter (expectNCDAStep currentDate person ncdaNeverFilled atHealthCenter) ncdaSteps


expectNCDAStep : NominalDate -> Person -> Bool -> Bool -> NCDAStep -> Bool
expectNCDAStep currentDate person ncdaNeverFilled atHealthCenter task =
    case task of
        -- If NCDA was filled before, for sure it included answers to
        -- needed questions. Since questions at this step are to be asked
        -- only once, we know it can be skipped.
        NCDAStepAntenatalCare ->
            ncdaNeverFilled

        NCDAStepNutritionAssessment ->
            not atHealthCenter

        NCDAStepNutritionBehavior ->
            ageInMonths currentDate person
                |> Maybe.map (\ageMonths -> ageMonths >= 6)
                |> Maybe.withDefault False

        -- All other tasks are shown always.
        _ ->
            True


ncdaSteps : List NCDAStep
ncdaSteps =
    [ NCDAStepAntenatalCare
    , NCDAStepUniversalInterventions
    , NCDAStepNutritionBehavior
    , NCDAStepNutritionAssessment
    , NCDAStepTargetedInterventions
    , NCDAStepInfrastructureEnvironment
    ]


generateSuggestedVaccinations :
    NominalDate
    -> Site
    -> Person
    -> VaccinationProgressDict
    -> VaccinationProgressDict
    -> List ( WellChildVaccineType, VaccineDose )
generateSuggestedVaccinations currentDate site person vaccinationHistory vaccinationProgress =
    let
        initialOpvAdministered =
            wasInitialOpvAdministeredByVaccinationProgress person.birthDate vaccinationHistory
    in
    allVaccineTypes site
        |> List.filter (expectVaccineForPerson currentDate site person initialOpvAdministered vaccinationProgress)
        |> List.filterMap
            (\vaccineType ->
                let
                    suggestedDose =
                        case latestVaccinationDataForVaccine vaccinationHistory vaccineType of
                            Just ( lastDoseAdministered, lastDoseDate ) ->
                                nextDoseForVaccine currentDate
                                    site
                                    person
                                    lastDoseDate
                                    initialOpvAdministered
                                    lastDoseAdministered
                                    vaccineType

                            Nothing ->
                                Just VaccineDoseFirst
                in
                Maybe.map (\nextDose -> ( vaccineType, nextDose )) suggestedDose
            )


wasInitialOpvAdministeredByVaccinationProgress : Maybe NominalDate -> VaccinationProgressDict -> Bool
wasInitialOpvAdministeredByVaccinationProgress birthDate vaccinationProgress =
    let
        firstDoseAdminstrationDate =
            Dict.get VaccineOPV vaccinationProgress
                |> Maybe.andThen (Dict.get VaccineDoseFirst)
    in
    Maybe.map2
        (\adminstrationDate birthDate_ ->
            Date.diff Days birthDate_ adminstrationDate < 14
        )
        firstDoseAdminstrationDate
        birthDate
        |> Maybe.withDefault False


{-| For each type of vaccine, we generate next dose and administration date.
If there's no need for future vaccination, Nothing is returned.
-}
generateFutureVaccinationsData :
    NominalDate
    -> Site
    -> Maybe NominalDate
    -> Gender
    -> Bool
    -> VaccinationProgressDict
    -> List ( WellChildVaccineType, Maybe ( VaccineDose, NominalDate ) )
generateFutureVaccinationsData currentDate site birthDate gender scheduleFirstDoseForToday vaccinationProgress =
    let
        initialOpvAdministered =
            wasInitialOpvAdministeredByVaccinationProgress birthDate vaccinationProgress
    in
    allVaccineTypesByGender site gender
        |> List.map
            (\vaccineType ->
                let
                    nextVaccinationData =
                        case latestVaccinationDataForVaccine vaccinationProgress vaccineType of
                            Just ( lastDoseAdministered, lastDoseDate ) ->
                                nextVaccinationDataForVaccine site
                                    birthDate
                                    vaccineType
                                    initialOpvAdministered
                                    lastDoseDate
                                    lastDoseAdministered

                            Nothing ->
                                -- There were no vaccination so far, so
                                -- we offer first dose for today.
                                let
                                    initialDate =
                                        Maybe.map
                                            (\birthDate_ ->
                                                initialVaccinationDateByBirthDate site
                                                    birthDate_
                                                    initialOpvAdministered
                                                    vaccinationProgress
                                                    ( vaccineType, VaccineDoseFirst )
                                            )
                                            birthDate
                                            |> Maybe.withDefault currentDate

                                    vaccinationDate =
                                        if scheduleFirstDoseForToday then
                                            Date.max initialDate currentDate

                                        else
                                            initialDate
                                in
                                Just ( VaccineDoseFirst, vaccinationDate )
                in
                -- Getting Nothing at nextVaccinationData indicates that
                -- vacination cycle is completed for this vaccine.
                ( vaccineType, nextVaccinationData )
            )


{-| Check if the first dose of vaccine may be administered to the person on the limit date.
-}
expectVaccineForPerson : NominalDate -> Site -> Person -> Bool -> VaccinationProgressDict -> WellChildVaccineType -> Bool
expectVaccineForPerson limitDate site person initialOpvAdministered vaccinationProgress vaccineType =
    expectVaccineDoseForPerson limitDate site person initialOpvAdministered vaccinationProgress ( vaccineType, VaccineDoseFirst )


{-| Check if a dose of vaccine may be administered to a person on the limit date.
For example, to check if the dose of vaccine may be administered today, we set
limit date to current date. If we want to check in one year, we set the limit date
to current date + 1 year.
-}
expectVaccineDoseForPerson : NominalDate -> Site -> Person -> Bool -> VaccinationProgressDict -> ( WellChildVaccineType, VaccineDose ) -> Bool
expectVaccineDoseForPerson limitDate site person initialOpvAdministered vaccinationProgress ( vaccineType, vaccineDose ) =
    Maybe.map
        (\birthDate ->
            let
                expectedDate =
                    initialVaccinationDateByBirthDate site
                        birthDate
                        initialOpvAdministered
                        vaccinationProgress
                        ( vaccineType, vaccineDose )

                compared =
                    Date.compare expectedDate limitDate

                genderCondition =
                    if vaccineType == VaccineHPV then
                        person.gender == Female

                    else
                        True
            in
            (compared == LT || compared == EQ) && genderCondition
        )
        person.birthDate
        |> Maybe.withDefault False


initialVaccinationDateByBirthDate :
    Site
    -> NominalDate
    -> Bool
    -> VaccinationProgressDict
    -> ( WellChildVaccineType, VaccineDose )
    -> NominalDate
initialVaccinationDateByBirthDate site birthDate initialOpvAdministered vaccinationProgress ( vaccineType, vaccineDose ) =
    let
        dosesInterval =
            vaccineDoseToComparable vaccineDose - 1

        ( interval, unit ) =
            getIntervalForVaccine site vaccineType

        never =
            Date.add Years 999 birthDate
    in
    case vaccineType of
        VaccineBCG ->
            birthDate

        VaccineOPV ->
            case vaccineDose of
                VaccineDoseFirst ->
                    birthDate

                _ ->
                    if initialOpvAdministered then
                        -- Second dose is given starting from age of 6 weeks.
                        Date.add Weeks 6 birthDate
                            |> Date.add unit ((dosesInterval - 1) * interval)

                    else
                        -- Second dose is given starting from age of 10 weeks.
                        Date.add Weeks 6 birthDate
                            |> Date.add unit (dosesInterval * interval)

        VaccineDTP ->
            Date.add Weeks 6 birthDate
                |> Date.add unit (dosesInterval * interval)

        VaccineDTPStandalone ->
            -- All 3 dosed of DTP were given, it has passed
            -- at least 28 days since third dose, and, child
            -- is at last 18 months old.
            Dict.get VaccineOPV vaccinationProgress
                |> Maybe.andThen (Dict.get VaccineDoseThird)
                |> Maybe.map
                    (\thirdDoseDate ->
                        let
                            fourWeeksAfterThirdDTPDose =
                                Date.add Days 28 thirdDoseDate

                            dateWhen18MonthsOld =
                                Date.add Months 18 birthDate
                        in
                        if Date.compare fourWeeksAfterThirdDTPDose dateWhen18MonthsOld == GT then
                            fourWeeksAfterThirdDTPDose

                        else
                            dateWhen18MonthsOld
                    )
                |> Maybe.withDefault never

        VaccinePCV13 ->
            Date.add Weeks 6 birthDate
                |> Date.add unit (dosesInterval * interval)

        VaccineRotarix ->
            Date.add Weeks 6 birthDate
                |> Date.add unit (dosesInterval * interval)

        VaccineIPV ->
            case site of
                SiteRwanda ->
                    case vaccineDose of
                        VaccineDoseFirst ->
                            Date.add Weeks 14 birthDate

                        -- At Rwanda site, we got second dose scheduled on the latter
                        -- between age of 36 weeks, and 4 weeks after first dose was administered.
                        VaccineDoseSecond ->
                            let
                                dateWhen9MonthsOld =
                                    Date.add Weeks 36 birthDate
                            in
                            Dict.get VaccineIPV vaccinationProgress
                                |> Maybe.andThen (Dict.get VaccineDoseFirst)
                                |> Maybe.map
                                    (\firstDoseDate ->
                                        let
                                            fourWeeksAfterFirstDose =
                                                Date.add Days 28 firstDoseDate
                                        in
                                        if Date.compare fourWeeksAfterFirstDose dateWhen9MonthsOld == GT then
                                            fourWeeksAfterFirstDose

                                        else
                                            dateWhen9MonthsOld
                                    )
                                |> Maybe.withDefault
                                    -- We default to 9 months, because it's first date when second dose is allowed at.
                                    -- This is needed when filling in vaccination history.
                                    dateWhen9MonthsOld

                        _ ->
                            never

                _ ->
                    Date.add Weeks 14 birthDate
                        |> Date.add unit (dosesInterval * interval)

        VaccineMR ->
            Date.add Weeks 36 birthDate
                |> Date.add unit (dosesInterval * interval)

        VaccineHPV ->
            Date.add Years 12 birthDate
                |> Date.add unit (dosesInterval * interval)


latestVaccinationDataForVaccine : VaccinationProgressDict -> WellChildVaccineType -> Maybe ( VaccineDose, NominalDate )
latestVaccinationDataForVaccine vaccinationsData vaccineType =
    Dict.get vaccineType vaccinationsData
        |> Maybe.andThen
            (Dict.toList
                >> List.sortBy (Tuple.first >> vaccineDoseToComparable)
                >> List.reverse
                >> List.head
            )


nextVaccinationDataForVaccine :
    Site
    -> Maybe NominalDate
    -> WellChildVaccineType
    -> Bool
    -> NominalDate
    -> VaccineDose
    -> Maybe ( VaccineDose, NominalDate )
nextVaccinationDataForVaccine site maybeBirthDate vaccineType initialOpvAdministered lastDoseDate lastDoseAdministered =
    Maybe.andThen
        (\birthDate ->
            if getLastDoseForVaccine site initialOpvAdministered vaccineType == lastDoseAdministered then
                Nothing

            else
                getNextVaccineDose lastDoseAdministered
                    |> Maybe.map
                        (\dose ->
                            let
                                ( interval, unit ) =
                                    getIntervalForVaccine site vaccineType
                            in
                            if
                                -- Initial OPV dose is supposed to be given before child turns 2 weeks old.
                                -- Second dose of OPV is given at 6 weeks, so it's not straight forward by interval.
                                vaccineType == VaccineOPV && initialOpvAdministered && dose == VaccineDoseSecond
                            then
                                let
                                    ageOfSixWeeks =
                                        Date.add Weeks 6 birthDate

                                    secondDoseByInterval =
                                        Date.add unit interval lastDoseDate
                                in
                                ( dose
                                , if Date.compare ageOfSixWeeks secondDoseByInterval == GT then
                                    ageOfSixWeeks

                                  else
                                    secondDoseByInterval
                                )

                            else if
                                (site == SiteRwanda)
                                    && (vaccineType == VaccineIPV)
                                    && (dose == VaccineDoseSecond)
                            then
                                let
                                    fourWeeksAfterFirstDose =
                                        Date.add Days 28 lastDoseDate

                                    dateWhen9MonthsOld =
                                        Date.add Weeks 36 birthDate
                                in
                                ( dose
                                , if Date.compare fourWeeksAfterFirstDose dateWhen9MonthsOld == GT then
                                    fourWeeksAfterFirstDose

                                  else
                                    dateWhen9MonthsOld
                                )

                            else
                                ( dose, Date.add unit interval lastDoseDate )
                        )
        )
        maybeBirthDate


nextDoseForVaccine : NominalDate -> Site -> Person -> NominalDate -> Bool -> VaccineDose -> WellChildVaccineType -> Maybe VaccineDose
nextDoseForVaccine currentDate site person lastDoseDate initialOpvAdministered lastDoseAdministered vaccineType =
    nextVaccinationDataForVaccine site person.birthDate vaccineType initialOpvAdministered lastDoseDate lastDoseAdministered
        |> Maybe.andThen
            (\( dose, dueDate ) ->
                if Date.compare dueDate currentDate == GT then
                    Nothing

                else
                    Just dose
            )


immunisationTaskToVaccineType : ImmunisationTask -> Maybe WellChildVaccineType
immunisationTaskToVaccineType task =
    case task of
        TaskBCG ->
            Just VaccineBCG

        TaskDTP ->
            Just VaccineDTP

        TaskDTPStandalone ->
            Just VaccineDTPStandalone

        TaskHPV ->
            Just VaccineHPV

        TaskIPV ->
            Just VaccineIPV

        TaskMR ->
            Just VaccineMR

        TaskOPV ->
            Just VaccineOPV

        TaskPCV13 ->
            Just VaccinePCV13

        TaskRotarix ->
            Just VaccineRotarix

        TaskOverview ->
            Nothing


getAllDosesForVaccine : Site -> Bool -> WellChildVaccineType -> List VaccineDose
getAllDosesForVaccine site initialOpvAdministered vaccineType =
    let
        lastDose =
            getLastDoseForVaccine site initialOpvAdministered vaccineType
    in
    List.filterMap
        (\dose ->
            if vaccineDoseToComparable dose <= vaccineDoseToComparable lastDose then
                Just dose

            else
                Nothing
        )
        allVaccineDoses


getLastDoseForVaccine : Site -> Bool -> WellChildVaccineType -> VaccineDose
getLastDoseForVaccine site initialOpvAdministered vaccineType =
    case vaccineType of
        VaccineBCG ->
            VaccineDoseFirst

        VaccineOPV ->
            if initialOpvAdministered then
                VaccineDoseFourth

            else
                VaccineDoseThird

        VaccineDTP ->
            VaccineDoseThird

        VaccineDTPStandalone ->
            VaccineDoseFirst

        VaccinePCV13 ->
            VaccineDoseThird

        VaccineRotarix ->
            VaccineDoseSecond

        VaccineIPV ->
            case site of
                SiteRwanda ->
                    VaccineDoseSecond

                _ ->
                    VaccineDoseFirst

        VaccineMR ->
            VaccineDoseSecond

        VaccineHPV ->
            VaccineDoseSecond


getIntervalForVaccine : Site -> WellChildVaccineType -> ( Int, Unit )
getIntervalForVaccine site vaccineType =
    case vaccineType of
        VaccineBCG ->
            ( 0, Days )

        VaccineOPV ->
            ( 4, Weeks )

        VaccineDTP ->
            ( 4, Weeks )

        VaccineDTPStandalone ->
            ( 0, Days )

        VaccinePCV13 ->
            ( 4, Weeks )

        VaccineRotarix ->
            ( 4, Weeks )

        -- So far, there was only single IPV dose.
        -- Since https://github.com/TIP-Global-Health/eheza-app/issues/1426,
        -- at Rwanda site, we got second dose scheduled on the latter
        -- between age of 36 weeks, and 4 weeks after first dose was administered.
        -- This requirement is not reflected here. Instead, it's defined as
        -- special case at appropriate spots in code (which use getIntervalForVaccine).
        VaccineIPV ->
            ( 0, Days )

        VaccineMR ->
            case site of
                SiteBurundi ->
                    ( 9, Months )

                _ ->
                    ( 6, Months )

        VaccineHPV ->
            ( 6, Months )


allVaccineTypesByGender : Site -> Gender -> List WellChildVaccineType
allVaccineTypesByGender site gender =
    allVaccineTypes site
        |> List.filter
            (\vaccineType ->
                case vaccineType of
                    VaccineHPV ->
                        gender == Female

                    _ ->
                        True
            )


allVaccineTypes : Site -> List WellChildVaccineType
allVaccineTypes site =
    let
        common =
            [ VaccineBCG
            , VaccineOPV
            , VaccineDTP
            , VaccinePCV13
            , VaccineRotarix
            , VaccineIPV
            , VaccineMR
            ]
    in
    case site of
        SiteBurundi ->
            common ++ [ VaccineDTPStandalone ]

        _ ->
            common ++ [ VaccineHPV ]


allVaccineDoses : List VaccineDose
allVaccineDoses =
    [ VaccineDoseFirst, VaccineDoseSecond, VaccineDoseThird, VaccineDoseFourth ]


getPreviousMeasurements : List ( NominalDate, ( id, a ) ) -> List a
getPreviousMeasurements =
    List.map (Tuple.second >> Tuple.second)


mergeVaccinationProgressDicts : VaccinationProgressDict -> VaccinationProgressDict -> VaccinationProgressDict
mergeVaccinationProgressDicts dict1 dict2 =
    Dict.merge
        (\vaccineType dosesDict -> Dict.insert vaccineType dosesDict)
        (\vaccineType dosesDict1 dosesDict2 ->
            Dict.merge
                (\dose date -> Dict.insert dose date)
                (\dose date1 date2 ->
                    if Date.compare date1 date2 == GT then
                        Dict.insert dose date1

                    else
                        Dict.insert dose date2
                )
                (\dose date -> Dict.insert dose date)
                dosesDict1
                dosesDict2
                Dict.empty
                |> Dict.insert vaccineType
        )
        (\vaccineType dosesDict -> Dict.insert vaccineType dosesDict)
        dict1
        dict2
        Dict.empty


generateGroupNutritionAssessmentEntries : ChildMeasurementList -> List ( NominalDate, List NutritionAssessment )
generateGroupNutritionAssessmentEntries measurementList =
    let
        assessmentsFromNutrition =
            Dict.values measurementList.nutritions
                |> List.filterMap
                    (\nutrition -> filterNutritionAssessmentsFromNutritionValue nutrition.dateMeasured nutrition.value)

        assessmentsFromFollowUp =
            Dict.values measurementList.followUp
                |> List.filterMap
                    (\followUp -> filterNutritionAssessmentsFromFollowUpValue followUp.dateMeasured followUp.value)
    in
    mergeNutritionAssessmentEntries
        assessmentsFromNutrition
        assessmentsFromFollowUp


generateIndividualNutritionAssessmentEntries :
    List
        { c
            | nutrition :
                Maybe
                    ( id1
                    , { v1
                        | dateMeasured : NominalDate
                        , value : NutritionValue
                      }
                    )
            , followUp :
                Maybe
                    ( id2
                    , { v2
                        | dateMeasured : NominalDate
                        , value : NutritionFollowUpValue
                      }
                    )
        }
    -> List ( NominalDate, List NutritionAssessment )
generateIndividualNutritionAssessmentEntries measurementList =
    let
        assessmentsFromNutrition =
            List.map
                (\measurements ->
                    Maybe.map2 filterNutritionAssessmentsFromNutritionValue
                        (getMeasurementDateMeasuredFunc measurements.nutrition)
                        (getMeasurementValueFunc measurements.nutrition)
                        |> Maybe.Extra.join
                )
                measurementList
                |> Maybe.Extra.values

        assessmentsFromFollowUp =
            List.map
                (\measurements ->
                    Maybe.map2 filterNutritionAssessmentsFromFollowUpValue
                        (getMeasurementDateMeasuredFunc measurements.followUp)
                        (getMeasurementValueFunc measurements.followUp)
                        |> Maybe.Extra.join
                )
                measurementList
                |> Maybe.Extra.values
    in
    mergeNutritionAssessmentEntries
        assessmentsFromNutrition
        assessmentsFromFollowUp


filterNutritionAssessmentsFromNutritionValue : NominalDate -> NutritionValue -> Maybe ( NominalDate, List NutritionAssessment )
filterNutritionAssessmentsFromNutritionValue dateMeasured value =
    let
        assesments =
            EverySet.toList value.assesment
                |> List.filterMap
                    (\assesment ->
                        case assesment of
                            NoNutritionAssessment ->
                                Nothing

                            AssesmentMalnutritionSigns _ ->
                                Just <| AssesmentMalnutritionSigns (EverySet.toList value.signs)

                            _ ->
                                Just assesment
                    )
    in
    if List.isEmpty assesments then
        Nothing

    else
        Just ( dateMeasured, assesments )


filterNutritionAssessmentsFromFollowUpValue : NominalDate -> NutritionFollowUpValue -> Maybe ( NominalDate, List NutritionAssessment )
filterNutritionAssessmentsFromFollowUpValue dateMeasured value =
    let
        assesments =
            EverySet.toList value.assesment
                |> List.filterMap
                    (\assesment ->
                        case assesment of
                            NoNutritionAssessment ->
                                Nothing

                            _ ->
                                Just assesment
                    )
    in
    if List.isEmpty assesments then
        Nothing

    else
        Just ( dateMeasured, assesments )


mergeNutritionAssessmentEntries :
    List ( NominalDate, List NutritionAssessment )
    -> List ( NominalDate, List NutritionAssessment )
    -> List ( NominalDate, List NutritionAssessment )
mergeNutritionAssessmentEntries list1 list2 =
    Dict.merge
        (\date assessments -> Dict.insert date assessments)
        (\date assessments1 assessments2 ->
            Dict.insert date
                ((assessments1 ++ assessments2)
                    |> EverySet.fromList
                    |> EverySet.toList
                )
        )
        (\date assessments -> Dict.insert date assessments)
        (Dict.fromList list1)
        (Dict.fromList list2)
        Dict.empty
        |> Dict.toList


generateAssembledDataForWellChild : Site -> WellChildEncounterId -> ModelIndexedDb -> WebData Pages.WellChild.Encounter.Model.AssembledData
generateAssembledDataForWellChild site id db =
    let
        encounter =
            Dict.get id db.wellChildEncounters
                |> Maybe.withDefault NotAsked

        measurements =
            Dict.get id db.wellChildMeasurements
                |> Maybe.withDefault NotAsked

        participant =
            encounter
                |> RemoteData.andThen
                    (\encounter_ ->
                        Dict.get encounter_.participant db.individualParticipants
                            |> Maybe.withDefault NotAsked
                    )

        person =
            participant
                |> RemoteData.andThen
                    (\participant_ ->
                        Dict.get participant_.person db.people
                            |> Maybe.withDefault NotAsked
                    )

        previousMeasurementsWithDates =
            RemoteData.toMaybe encounter
                |> Maybe.map
                    (\encounter_ ->
                        Backend.Measurement.Utils.generatePreviousMeasurements
                            getWellChildEncountersForParticipant
                            .wellChildMeasurements
                            (Just id)
                            encounter_.participant
                            db
                    )
                |> Maybe.withDefault []

        assembledWithEmptyVaccinationDicts =
            RemoteData.map Pages.WellChild.Encounter.Model.AssembledData (Success id)
                |> RemoteData.andMap encounter
                |> RemoteData.andMap participant
                |> RemoteData.andMap person
                |> RemoteData.andMap measurements
                |> RemoteData.andMap (Success previousMeasurementsWithDates)
                |> RemoteData.andMap (Success Dict.empty)
                |> RemoteData.andMap (Success Dict.empty)
    in
    RemoteData.map
        (\assembled ->
            let
                ( vaccinationHistory, vaccinationProgress ) =
                    generateVaccinationProgressDictsForWellChild site assembled db
            in
            { assembled | vaccinationHistory = vaccinationHistory, vaccinationProgress = vaccinationProgress }
        )
        assembledWithEmptyVaccinationDicts


generateVaccinationProgressDictsForWellChild : Site -> Pages.WellChild.Encounter.Model.AssembledData -> ModelIndexedDb -> ( VaccinationProgressDict, VaccinationProgressDict )
generateVaccinationProgressDictsForWellChild site assembled db =
    let
        previousMeasurements =
            getPreviousMeasurements assembled.previousMeasurementsWithDates

        vaccinationProgressByChildScoreboard =
            let
                individualParticipants =
                    Dict.get assembled.participant.person db.individualParticipantsByPerson
                        |> Maybe.andThen RemoteData.toMaybe
                        |> Maybe.map Dict.toList
                        |> Maybe.withDefault []

                individualChildScoreboardParticipantId =
                    List.filter
                        (Tuple.second
                            >> .encounterType
                            >> (==) Backend.IndividualEncounterParticipant.Model.ChildScoreboardEncounter
                        )
                        individualParticipants
                        |> List.head
                        |> Maybe.map Tuple.first
            in
            Maybe.map (generateVaccinationProgressDictByChildScoreboard site db)
                individualChildScoreboardParticipantId
                |> Maybe.withDefault Dict.empty

        vaccinationHistory =
            generateVaccinationProgressForWellChild site assembled.person previousMeasurements

        vaccinationProgress =
            assembled.measurements
                :: previousMeasurements
                |> generateVaccinationProgressForWellChild site assembled.person
    in
    ( mergeVaccinationProgressDicts
        vaccinationHistory
        vaccinationProgressByChildScoreboard
    , mergeVaccinationProgressDicts
        vaccinationProgress
        vaccinationProgressByChildScoreboard
    )


generateAssembledDataForChildScoreboard : Site -> ChildScoreboardEncounterId -> ModelIndexedDb -> WebData Pages.ChildScoreboard.Encounter.Model.AssembledData
generateAssembledDataForChildScoreboard site id db =
    let
        encounter =
            Dict.get id db.childScoreboardEncounters
                |> Maybe.withDefault NotAsked

        measurements =
            Dict.get id db.childScoreboardMeasurements
                |> Maybe.withDefault NotAsked

        participant =
            encounter
                |> RemoteData.andThen
                    (\encounter_ ->
                        Dict.get encounter_.participant db.individualParticipants
                            |> Maybe.withDefault NotAsked
                    )

        person =
            participant
                |> RemoteData.andThen
                    (\participant_ ->
                        Dict.get participant_.person db.people
                            |> Maybe.withDefault NotAsked
                    )

        previousMeasurementsWithDates =
            RemoteData.toMaybe encounter
                |> Maybe.map
                    (\encounter_ ->
                        Backend.Measurement.Utils.generatePreviousMeasurements
                            getChildScoreboardEncountersForParticipant
                            .childScoreboardMeasurements
                            (Just id)
                            encounter_.participant
                            db
                    )
                |> Maybe.withDefault []

        assembledWithEmptyVaccinationDicts =
            RemoteData.map Pages.ChildScoreboard.Encounter.Model.AssembledData (Success id)
                |> RemoteData.andMap encounter
                |> RemoteData.andMap participant
                |> RemoteData.andMap person
                |> RemoteData.andMap measurements
                |> RemoteData.andMap (Success previousMeasurementsWithDates)
                |> RemoteData.andMap (Success Dict.empty)
                |> RemoteData.andMap (Success Dict.empty)
    in
    RemoteData.map
        (\assembled ->
            let
                ( vaccinationHistory, vaccinationProgress ) =
                    generateVaccinationProgressDictsForChildScoreboard site assembled db
            in
            { assembled | vaccinationHistory = vaccinationHistory, vaccinationProgress = vaccinationProgress }
        )
        assembledWithEmptyVaccinationDicts


generateVaccinationProgressDictsForChildScoreboard : Site -> Pages.ChildScoreboard.Encounter.Model.AssembledData -> ModelIndexedDb -> ( VaccinationProgressDict, VaccinationProgressDict )
generateVaccinationProgressDictsForChildScoreboard site assembled db =
    let
        previousMeasurements =
            getPreviousMeasurements assembled.previousMeasurementsWithDates

        vaccinationProgressByWellChild =
            let
                individualParticipants =
                    Dict.get assembled.participant.person db.individualParticipantsByPerson
                        |> Maybe.andThen RemoteData.toMaybe
                        |> Maybe.map Dict.toList
                        |> Maybe.withDefault []

                individualWellChildParticipantId =
                    List.filter
                        (Tuple.second
                            >> .encounterType
                            >> (==) Backend.IndividualEncounterParticipant.Model.WellChildEncounter
                        )
                        individualParticipants
                        |> List.head
                        |> Maybe.map Tuple.first
            in
            Maybe.map (generateVaccinationProgressDictByWellChild site assembled.person db)
                individualWellChildParticipantId
                |> Maybe.withDefault Dict.empty

        vaccinationHistory =
            generateVaccinationProgressForChildScoreboard site previousMeasurements

        vaccinationProgress =
            assembled.measurements
                :: previousMeasurements
                |> generateVaccinationProgressForChildScoreboard site
    in
    ( mergeVaccinationProgressDicts
        vaccinationHistory
        vaccinationProgressByWellChild
    , mergeVaccinationProgressDicts
        vaccinationProgress
        vaccinationProgressByWellChild
    )


generateVaccinationProgressDictByWellChild : Site -> Person -> ModelIndexedDb -> IndividualEncounterParticipantId -> VaccinationProgressDict
generateVaccinationProgressDictByWellChild site person db participantId =
    Backend.Measurement.Utils.generatePreviousMeasurements
        Backend.NutritionEncounter.Utils.getWellChildEncountersForParticipant
        .wellChildMeasurements
        Nothing
        participantId
        db
        |> getPreviousMeasurements
        |> generateVaccinationProgressForWellChild site person


generateVaccinationProgressDictByChildScoreboard : Site -> ModelIndexedDb -> IndividualEncounterParticipantId -> VaccinationProgressDict
generateVaccinationProgressDictByChildScoreboard site db participantId =
    Backend.Measurement.Utils.generatePreviousMeasurements
        Backend.NutritionEncounter.Utils.getChildScoreboardEncountersForParticipant
        .childScoreboardMeasurements
        Nothing
        participantId
        db
        |> getPreviousMeasurements
        |> generateVaccinationProgressForChildScoreboard site


generateVaccinationProgressForWellChild : Site -> Person -> List WellChildMeasurements -> VaccinationProgressDict
generateVaccinationProgressForWellChild site person measurements =
    let
        bcgImmunisationsProgress =
            List.filterMap (.bcgImmunisation >> getMeasurementValueFunc)
                measurements
                |> generateVaccinationProgressForVaccine

        dtpImmunisationsProgress =
            List.filterMap (.dtpImmunisation >> getMeasurementValueFunc)
                measurements
                |> generateVaccinationProgressForVaccine

        dtpStandaloneEntry =
            -- This entry shall appear after 3 doses of DTP were given.
            if isJust <| Dict.get VaccineDoseThird dtpImmunisationsProgress then
                let
                    dtpStandaloneImmunisations =
                        List.filterMap (.dtpStandaloneImmunisation >> getMeasurementValueFunc)
                            measurements
                in
                [ ( VaccineDTPStandalone, generateVaccinationProgressForVaccine dtpStandaloneImmunisations ) ]

            else
                []

        ipvImmunisationsProgress =
            List.filterMap (.ipvImmunisation >> getMeasurementValueFunc)
                measurements
                |> generateVaccinationProgressForVaccine

        mrImmunisationsProgress =
            List.filterMap (.mrImmunisation >> getMeasurementValueFunc)
                measurements
                |> generateVaccinationProgressForVaccine

        opvImmunisationsProgress =
            List.filterMap (.opvImmunisation >> getMeasurementValueFunc)
                measurements
                |> generateVaccinationProgressForVaccine

        pcv13ImmunisationsProgress =
            List.filterMap (.pcv13Immunisation >> getMeasurementValueFunc)
                measurements
                |> generateVaccinationProgressForVaccine

        rotarixImmunisationsProgress =
            List.filterMap (.rotarixImmunisation >> getMeasurementValueFunc)
                measurements
                |> generateVaccinationProgressForVaccine

        hpvEntry =
            if person.gender == Female then
                let
                    hpvImmunisations =
                        List.filterMap (.hpvImmunisation >> getMeasurementValueFunc)
                            measurements
                in
                [ ( VaccineHPV, generateVaccinationProgressForVaccine hpvImmunisations ) ]

            else
                []

        vaccineTypesForSite =
            allVaccineTypes site
    in
    [ ( VaccineBCG, bcgImmunisationsProgress )
    , ( VaccineOPV, opvImmunisationsProgress )
    , ( VaccineDTP, dtpImmunisationsProgress )
    , ( VaccinePCV13, pcv13ImmunisationsProgress )
    , ( VaccineRotarix, rotarixImmunisationsProgress )
    , ( VaccineIPV, ipvImmunisationsProgress )
    , ( VaccineMR, mrImmunisationsProgress )
    ]
        ++ dtpStandaloneEntry
        ++ hpvEntry
        |> List.filter
            (\( vaccineType, _ ) ->
                List.member vaccineType vaccineTypesForSite
            )
        |> Dict.fromList


generateVaccinationProgressForChildScoreboard : Site -> List ChildScoreboardMeasurements -> VaccinationProgressDict
generateVaccinationProgressForChildScoreboard site measurements =
    let
        bcgImmunisationsProgress =
            List.filterMap (.bcgImmunisation >> getMeasurementValueFunc)
                measurements
                |> generateVaccinationProgressForVaccine

        dtpImmunisationsProgress =
            List.filterMap (.dtpImmunisation >> getMeasurementValueFunc)
                measurements
                |> generateVaccinationProgressForVaccine

        -- This entry shall appear after 3 doses of DTP were given.
        dtpStandaloneEntry =
            if isJust <| Dict.get VaccineDoseThird dtpImmunisationsProgress then
                let
                    dtpStandaloneImmunisations =
                        List.filterMap (.dtpStandaloneImmunisation >> getMeasurementValueFunc)
                            measurements
                in
                [ ( VaccineDTPStandalone, generateVaccinationProgressForVaccine dtpStandaloneImmunisations ) ]

            else
                []

        ipvImmunisationsProgress =
            List.filterMap (.ipvImmunisation >> getMeasurementValueFunc)
                measurements
                |> generateVaccinationProgressForVaccine

        mrImmunisationsProgress =
            List.filterMap (.mrImmunisation >> getMeasurementValueFunc)
                measurements
                |> generateVaccinationProgressForVaccine

        opvImmunisationsProgress =
            List.filterMap (.opvImmunisation >> getMeasurementValueFunc)
                measurements
                |> generateVaccinationProgressForVaccine

        pcv13ImmunisationsProgress =
            List.filterMap (.pcv13Immunisation >> getMeasurementValueFunc)
                measurements
                |> generateVaccinationProgressForVaccine

        rotarixImmunisationsProgress =
            List.filterMap (.rotarixImmunisation >> getMeasurementValueFunc)
                measurements
                |> generateVaccinationProgressForVaccine

        vaccineTypesForSite =
            allVaccineTypes site
    in
    [ ( VaccineBCG, bcgImmunisationsProgress )
    , ( VaccineOPV, opvImmunisationsProgress )
    , ( VaccineDTP, dtpImmunisationsProgress )
    , ( VaccinePCV13, pcv13ImmunisationsProgress )
    , ( VaccineRotarix, rotarixImmunisationsProgress )
    , ( VaccineIPV, ipvImmunisationsProgress )
    , ( VaccineMR, mrImmunisationsProgress )
    ]
        ++ dtpStandaloneEntry
        |> List.filter
            (\( vaccineType, _ ) ->
                List.member vaccineType vaccineTypesForSite
            )
        |> Dict.fromList


muacMeasurementIsOff : Maybe MuacInCm -> Bool
muacMeasurementIsOff =
    -- MUAC is not green.
    Maybe.map (muacIndication >> (/=) ColorAlertGreen)
        >> Maybe.withDefault False


nutritionFeedingFormWithDefault : NutritionFeedingForm -> Maybe NutritionFeedingValue -> NutritionFeedingForm
nutritionFeedingFormWithDefault form saved =
    saved
        |> unwrap
            form
            (\value ->
                { receiveSupplement = or form.receiveSupplement (EverySet.member ReceiveSupplement value.signs |> Just)
                , rationPresentAtHome = or form.rationPresentAtHome (EverySet.member RationPresentAtHome value.signs |> Just)
                , enoughTillNextSession = or form.enoughTillNextSession (EverySet.member EnoughTillNextSession value.signs |> Just)
                , supplementShared = or form.supplementShared (EverySet.member SupplementShared value.signs |> Just)
                , encouragedToEat = or form.encouragedToEat (EverySet.member EncouragedToEat value.signs |> Just)
                , refusingToEat = or form.refusingToEat (EverySet.member RefusingToEat value.signs |> Just)
                , breastfeeding = or form.breastfeeding (EverySet.member FeedingSignBreastfeeding value.signs |> Just)
                , cleanWaterAvailable = or form.cleanWaterAvailable (EverySet.member CleanWaterAvailable value.signs |> Just)
                , eatenWithWater = or form.eatenWithWater (EverySet.member EatenWithWater value.signs |> Just)
                , supplementType = or form.supplementType (Just value.supplementType)
                , sachetsPerDay = or form.sachetsPerDay (Just value.sachetsPerDay)
                }
            )


toNutritionFeedingValueWithDefault : Maybe NutritionFeedingValue -> NutritionFeedingForm -> Maybe NutritionFeedingValue
toNutritionFeedingValueWithDefault saved form =
    nutritionFeedingFormWithDefault form saved
        |> toNutritionFeedingValue


toNutritionFeedingValue : NutritionFeedingForm -> Maybe NutritionFeedingValue
toNutritionFeedingValue form =
    let
        signs =
            [ ifNullableTrue ReceiveSupplement form.receiveSupplement
            , ifNullableTrue RationPresentAtHome form.rationPresentAtHome
            , ifNullableTrue EnoughTillNextSession form.enoughTillNextSession
            , ifNullableTrue SupplementShared form.supplementShared
            , ifNullableTrue EncouragedToEat form.encouragedToEat
            , ifNullableTrue RefusingToEat form.refusingToEat
            , ifNullableTrue FeedingSignBreastfeeding form.breastfeeding
            , ifNullableTrue CleanWaterAvailable form.cleanWaterAvailable
            , ifNullableTrue EatenWithWater form.eatenWithWater
            ]
                |> Maybe.Extra.combine
                |> Maybe.map (List.foldl EverySet.union EverySet.empty >> ifEverySetEmpty NoNutritionFeedingSigns)

        supplementType =
            form.supplementType
                |> Maybe.withDefault NoNutritionSupplementType
                |> Just

        sachetsPerDay =
            form.sachetsPerDay
                |> Maybe.withDefault 0
                |> Just
    in
    Maybe.map NutritionFeedingValue signs
        |> andMap supplementType
        |> andMap sachetsPerDay


nutritionHygieneFormWithDefault : NutritionHygieneForm -> Maybe NutritionHygieneValue -> NutritionHygieneForm
nutritionHygieneFormWithDefault form saved =
    saved
        |> unwrap
            form
            (\value ->
                { soapInTheHouse = or form.soapInTheHouse (EverySet.member SoapInTheHouse value.signs |> Just)
                , washHandsBeforeFeeding = or form.washHandsBeforeFeeding (EverySet.member WashHandsBeforeFeeding value.signs |> Just)
                , foodCovered = or form.foodCovered (EverySet.member FoodCovered value.signs |> Just)
                , mainWaterSource = or form.mainWaterSource (Just value.mainWaterSource)
                , waterPreparationOption = or form.waterPreparationOption (Just value.waterPreparationOption)
                }
            )


toNutritionHygieneValueWithDefault : Maybe NutritionHygieneValue -> NutritionHygieneForm -> Maybe NutritionHygieneValue
toNutritionHygieneValueWithDefault saved form =
    nutritionHygieneFormWithDefault form saved
        |> toNutritionHygieneValue


toNutritionHygieneValue : NutritionHygieneForm -> Maybe NutritionHygieneValue
toNutritionHygieneValue form =
    let
        signs =
            [ ifNullableTrue SoapInTheHouse form.soapInTheHouse
            , ifNullableTrue WashHandsBeforeFeeding form.washHandsBeforeFeeding
            , ifNullableTrue FoodCovered form.foodCovered
            ]
                |> Maybe.Extra.combine
                |> Maybe.map (List.foldl EverySet.union EverySet.empty >> ifEverySetEmpty NoNutritionHygieneSigns)
    in
    Maybe.map NutritionHygieneValue signs
        |> andMap form.mainWaterSource
        |> andMap form.waterPreparationOption


nutritionFoodSecurityFormWithDefault : NutritionFoodSecurityForm -> Maybe NutritionFoodSecurityValue -> NutritionFoodSecurityForm
nutritionFoodSecurityFormWithDefault form saved =
    saved
        |> unwrap
            form
            (\value ->
                { householdGotFood = or form.householdGotFood (EverySet.member HouseholdGotFood value.signs |> Just)
                , mainIncomeSource = or form.mainIncomeSource (Just value.mainIncomeSource)
                }
            )


toNutritionFoodSecurityValueWithDefault : Maybe NutritionFoodSecurityValue -> NutritionFoodSecurityForm -> Maybe NutritionFoodSecurityValue
toNutritionFoodSecurityValueWithDefault saved form =
    nutritionFoodSecurityFormWithDefault form saved
        |> toNutritionFoodSecurityValue


toNutritionFoodSecurityValue : NutritionFoodSecurityForm -> Maybe NutritionFoodSecurityValue
toNutritionFoodSecurityValue form =
    let
        signs =
            [ ifNullableTrue HouseholdGotFood form.householdGotFood
            ]
                |> Maybe.Extra.combine
                |> Maybe.map (List.foldl EverySet.union EverySet.empty >> ifEverySetEmpty NoNutritionFoodSecuritySigns)
    in
    Maybe.map NutritionFoodSecurityValue signs
        |> andMap form.mainIncomeSource


nutritionCaringFormWithDefault : NutritionCaringForm -> Maybe NutritionCaringValue -> NutritionCaringForm
nutritionCaringFormWithDefault form saved =
    saved
        |> unwrap
            form
            (\value ->
                { parentHealth = or form.parentHealth (EverySet.member ParentsAliveHealthy value.signs |> Just)
                , childClean = or form.childClean (EverySet.member ChildClean value.signs |> Just)
                , caringOption = or form.caringOption (value.caringOption |> Just)
                }
            )


toNutritionCaringValueWithDefault : Maybe NutritionCaringValue -> NutritionCaringForm -> Maybe NutritionCaringValue
toNutritionCaringValueWithDefault saved form =
    nutritionCaringFormWithDefault form saved
        |> toNutritionCaringValue


toNutritionCaringValue : NutritionCaringForm -> Maybe NutritionCaringValue
toNutritionCaringValue form =
    let
        signs =
            [ ifNullableTrue ParentsAliveHealthy form.parentHealth
            , ifNullableTrue ChildClean form.childClean
            ]
                |> Maybe.Extra.combine
                |> Maybe.map (List.foldl EverySet.union EverySet.empty >> ifEverySetEmpty NoCaringSigns)
    in
    Maybe.map NutritionCaringValue signs
        |> andMap form.caringOption


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


fromListWithDefaultValue : a -> Maybe (List a) -> EverySet a
fromListWithDefaultValue default maybeList =
    case maybeList of
        Just list ->
            EverySet.fromList list |> ifEverySetEmpty default

        Nothing ->
            EverySet.singleton default


fromAdministrationNote : Maybe AdministrationNote -> MedicationAdministrationForm
fromAdministrationNote saved =
    Maybe.map
        (\administrationNote ->
            let
                ( medicationAdministered, reasonForNonAdministration ) =
                    if administrationNote == AdministeredToday then
                        ( Just True, Nothing )

                    else
                        ( Just False, Just administrationNote )
            in
            MedicationAdministrationForm medicationAdministered reasonForNonAdministration
        )
        saved
        |> Maybe.withDefault emptyMedicationAdministrationForm


medicationAdministrationFormWithDefault : MedicationAdministrationForm -> Maybe AdministrationNote -> MedicationAdministrationForm
medicationAdministrationFormWithDefault form saved =
    let
        fromSavedForm =
            fromAdministrationNote saved
    in
    { medicationAdministered = or form.medicationAdministered fromSavedForm.medicationAdministered
    , reasonForNonAdministration = or form.reasonForNonAdministration fromSavedForm.reasonForNonAdministration
    }


toAdministrationNoteWithDefault : Maybe AdministrationNote -> MedicationAdministrationForm -> Maybe AdministrationNote
toAdministrationNoteWithDefault saved form =
    medicationAdministrationFormWithDefault form saved
        |> toAdministrationNote


toAdministrationNote : MedicationAdministrationForm -> Maybe AdministrationNote
toAdministrationNote form =
    form.medicationAdministered
        |> Maybe.andThen
            (\medicationAdministered ->
                if medicationAdministered then
                    Just AdministeredToday

                else
                    form.reasonForNonAdministration
            )


medicationAdministrationFormInputsAndTasks :
    Language
    -> NominalDate
    -> Person
    -> MedicationAdministrationFormConfig msg
    -> MedicationAdministrationForm
    -> ( List (Html msg), List (Maybe Bool) )
medicationAdministrationFormInputsAndTasks language currentDate person config form =
    let
        instructions =
            config.resolveDosageAndIconFunc language currentDate person
                |> Maybe.map
                    (\( dosage, icon, helper ) ->
                        [ h2 [] [ text <| translate language Translate.ActionsToTake ++ ":" ]
                        , div [ class "instructions" ]
                            [ viewAdministeredMedicationCustomLabel language
                                Translate.Administer
                                (Translate.MedicationDistributionSign config.medication)
                                ""
                                icon
                                dosage
                                Nothing
                            , div [ class "prescription" ] [ text <| helper ++ "." ]
                            ]
                        ]
                    )
                |> Maybe.withDefault []

        questions =
            concatInputsAndTasksSections
                [ ( [ viewAdministeredMedicationQuestion language (Translate.MedicationDistributionSign config.medication)
                    , viewBoolInput
                        language
                        form.medicationAdministered
                        config.setMedicationAdministeredMsg
                        ""
                        Nothing
                    ]
                  , [ form.medicationAdministered ]
                  )
                , derivedQuestion
                ]

        derivedQuestion =
            if form.medicationAdministered == Just False then
                ( [ viewQuestionLabel language Translate.WhyNot
                  , viewCheckBoxSelectInput language
                        [ NonAdministrationLackOfStock, NonAdministrationKnownAllergy, NonAdministrationPatientUnableToAfford ]
                        [ NonAdministrationPatientDeclined, NonAdministrationOther ]
                        form.reasonForNonAdministration
                        config.setReasonForNonAdministration
                        Translate.AdministrationNote
                  ]
                , [ maybeToBoolTask form.reasonForNonAdministration ]
                )

            else
                ( [], [] )
    in
    concatInputsAndTasksSections
        [ ( instructions
          , []
          )
        , questions
        ]


viewAdministeredMedicationCustomLabel : Language -> TranslationId -> TranslationId -> String -> String -> String -> Maybe NominalDate -> Html any
viewAdministeredMedicationCustomLabel language administerTranslationId medicineTranslationId medicineSuffix iconClass suffix maybeDate =
    let
        message =
            div [] <|
                [ text <| translate language administerTranslationId
                , text ": "
                , span [ class "medicine" ] [ text <| translate language medicineTranslationId ++ medicineSuffix ]
                ]
                    ++ renderDatePart language maybeDate
                    ++ [ text <| " " ++ suffix ]
    in
    viewInstructionsLabel iconClass message


viewAdministeredMedicationQuestion : Language -> TranslationId -> Html any
viewAdministeredMedicationQuestion language medicineTranslationId =
    div [ class "label" ]
        [ text <|
            translate language Translate.AdministeredMedicationQuestion
                ++ " "
                ++ translate language medicineTranslationId
                ++ " "
                ++ translate language Translate.ToThePatient
                ++ "?"
        ]


viewReinforceAdherenceQuestion : Language -> TranslationId -> Html any
viewReinforceAdherenceQuestion language medicineTranslationId =
    div [ class "label" ]
        [ text <|
            translate language Translate.ReinforceAdherenceQuestion
                ++ " "
                ++ translate language medicineTranslationId
                ++ "?"
        ]


renderDatePart : Language -> Maybe NominalDate -> List (Html any)
renderDatePart language maybeDate =
    maybeDate
        |> Maybe.map (\date -> [ span [ class "date" ] [ text <| " (" ++ renderDate language date ++ ")" ] ])
        |> Maybe.withDefault []
