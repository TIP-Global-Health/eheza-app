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
        ( ifEverySetEmpty
        , ifNullableTrue
        , ifTrue
        , maybeValueConsideringIsDirtyField
        , taskCompleted
        , taskCompletedWithException
        , valueConsideringIsDirtyField
        , viewBoolInput
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
