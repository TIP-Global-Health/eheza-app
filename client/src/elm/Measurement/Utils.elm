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
import EverySet exposing (EverySet)
import Gizra.NominalDate exposing (NominalDate)
import LocalData
import Maybe.Extra exposing (andMap, or, unwrap)
import Measurement.Model exposing (..)
import Pages.Session.Model
import Pages.Utils exposing (ifEverySetEmpty, ifNullableTrue, ifTrue, valueConsideringIsDirtyField)


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
        fromData .height (.value >> heightValueFunc >> String.fromFloat)
            |> Maybe.withDefault ""
    , muac =
        fromData .muac (.value >> muacValueFunc >> String.fromFloat)
            |> Maybe.withDefault ""
    , nutritionSigns =
        fromData .nutrition .value
            |> Maybe.withDefault EverySet.empty
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
    { height = Maybe.map heightValueFunc saved
    , heightDirty = False
    }


heightFormWithDefault : HeightForm -> Maybe HeightInCm -> HeightForm
heightFormWithDefault form saved =
    saved
        |> unwrap
            form
            (\value ->
                { height = valueConsideringIsDirtyField form.heightDirty form.height (value |> heightValueFunc)
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


fromNutritionValue : Maybe (EverySet ChildNutritionSign) -> NutritionForm
fromNutritionValue saved =
    { signs = Maybe.map EverySet.toList saved }


nutritionFormWithDefault : NutritionForm -> Maybe (EverySet ChildNutritionSign) -> NutritionForm
nutritionFormWithDefault form saved =
    saved
        |> unwrap
            form
            (\value ->
                { signs = or form.signs (EverySet.toList value |> Just) }
            )


toNutritionValueWithDefault : Maybe (EverySet ChildNutritionSign) -> NutritionForm -> Maybe (EverySet ChildNutritionSign)
toNutritionValueWithDefault saved form =
    nutritionFormWithDefault form saved
        |> toNutritionValue


toNutritionValue : NutritionForm -> Maybe (EverySet ChildNutritionSign)
toNutritionValue form =
    Maybe.map (EverySet.fromList >> ifEverySetEmpty NormalChildNutrition) form.signs


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
    }


followUpFormWithDefault : FollowUpForm -> Maybe FollowUpValue -> FollowUpForm
followUpFormWithDefault form saved =
    saved
        |> unwrap
            form
            (\value ->
                { option = or form.option (EverySet.toList value.options |> List.head)
                , assesment = or form.assesment (Just value.assesment)
                }
            )


toFollowUpValueWithDefault : Maybe FollowUpValue -> FollowUpForm -> Maybe FollowUpValue
toFollowUpValueWithDefault saved form =
    followUpFormWithDefault form saved
        |> toFollowUpValue


toFollowUpValue : FollowUpForm -> Maybe FollowUpValue
toFollowUpValue form =
    let
        options =
            form.option
                |> Maybe.map (List.singleton >> EverySet.fromList)
    in
    Maybe.map FollowUpValue options
        |> andMap form.assesment


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
            [ Maybe.map (ifTrue HandReferrerForm) form.handReferralForm
            , Maybe.map (ifTrue ReferToHealthCenter) form.referToHealthCenter
            , ifNullableTrue PrenatalAccompanyToHC form.accompanyToHealthCenter
            ]
                |> Maybe.Extra.combine
                |> Maybe.map (List.foldl EverySet.union EverySet.empty >> ifEverySetEmpty NoSendToHCSigns)

        reasonForNotSendingToHC =
            form.reasonForNotSendingToHC
                |> Maybe.withDefault NoReasonForNotSendingToHC
                |> Just
    in
    Maybe.map SendToHCValue signs
        |> andMap reasonForNotSendingToHC


allNextStepsTasks : List NextStepsTask
allNextStepsTasks =
    [ NextStepContributingFactors, NextStepsHealthEducation, NextStepsSendToHC, NextStepFollowUp ]


fromBasicVitalsForm : Maybe BasicVitalsValue -> BasicVitalsForm
fromBasicVitalsForm saved =
    { respiratoryRate = Maybe.map .respiratoryRate saved
    , respiratoryRateDirty = False
    , bodyTemperature = Maybe.map .bodyTemperature saved
    , bodyTemperatureDirty = False
    }


vitalsBasicFormWithDefault : BasicVitalsForm -> Maybe BasicVitalsValue -> BasicVitalsForm
vitalsBasicFormWithDefault form saved =
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


toBasicVitalsValueWithDefault : Maybe BasicVitalsValue -> BasicVitalsForm -> Maybe BasicVitalsValue
toBasicVitalsValueWithDefault saved form =
    vitalsBasicFormWithDefault form saved
        |> toBasicVitalsValue


toBasicVitalsValue : BasicVitalsForm -> Maybe BasicVitalsValue
toBasicVitalsValue form =
    Maybe.map BasicVitalsValue form.respiratoryRate
        |> andMap form.bodyTemperature
