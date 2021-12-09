module Pages.PrenatalActivity.Utils exposing (..)

import AssocList as Dict exposing (Dict)
import Backend.Measurement.Model exposing (..)
import Backend.Measurement.Utils exposing (getMeasurementValueFunc, heightValueFunc, muacIndication, muacValueFunc, weightValueFunc)
import Backend.Model exposing (ModelIndexedDb)
import Backend.Person.Model exposing (Person)
import Backend.PrenatalActivity.Model exposing (..)
import Backend.PrenatalEncounter.Model exposing (PrenatalEncounterType(..))
import EverySet exposing (EverySet)
import Gizra.NominalDate exposing (NominalDate, diffDays, diffWeeks)
import Html exposing (Html)
import Maybe.Extra exposing (andMap, isJust, isNothing, or, unwrap)
import Measurement.Model exposing (VitalsForm)
import Measurement.Utils exposing (sendToHCFormWithDefault, vitalsFormWithDefault)
import Pages.PrenatalActivity.Model exposing (..)
import Pages.PrenatalEncounter.Model exposing (AssembledData)
import Pages.Utils
    exposing
        ( ifEverySetEmpty
        , ifNullableTrue
        , ifTrue
        , taskAllCompleted
        , taskCompleted
        , valueConsideringIsDirtyField
        , viewBoolInput
        , viewQuestionLabel
        )
import Translate exposing (Language, translate)


isFirstEncounter : AssembledData -> Bool
isFirstEncounter assembled =
    List.isEmpty assembled.nursePreviousMeasurementsWithDates


expectActivity : NominalDate -> AssembledData -> PrenatalActivity -> Bool
expectActivity currentDate assembled activity =
    case assembled.encounter.encounterType of
        NurseEncounter ->
            case activity of
                PregnancyDating ->
                    -- Only show on first encounter
                    isFirstEncounter assembled

                History ->
                    True

                Examination ->
                    True

                FamilyPlanning ->
                    True

                Backend.PrenatalActivity.Model.MalariaPrevention ->
                    assembled.nursePreviousMeasurementsWithDates
                        |> List.filter
                            (\( _, measurements ) ->
                                measurements.malariaPrevention
                                    |> Maybe.map (Tuple.second >> .value >> EverySet.member MosquitoNet)
                                    |> Maybe.withDefault False
                            )
                        |> List.isEmpty

                Medication ->
                    True

                DangerSigns ->
                    True

                Laboratory ->
                    -- Always True, as there are sub activities that should be
                    -- presented on every encounter.
                    True

                PrenatalPhoto ->
                    expectPrenatalPhoto currentDate assembled

                -- Unique Chw activities.
                _ ->
                    False

        ChwFirstEncounter ->
            case activity of
                PregnancyDating ->
                    -- Do not show, if patient already visited health center.
                    isFirstEncounter assembled

                Laboratory ->
                    -- Do not show, if patient already visited health center.
                    isFirstEncounter assembled

                DangerSigns ->
                    True

                Backend.PrenatalActivity.Model.HealthEducation ->
                    activityCompleted currentDate assembled DangerSigns
                        && noDangerSigns assembled

                NextSteps ->
                    mandatoryActivitiesForNextStepsCompleted currentDate assembled

                -- Unique nurse activities.
                _ ->
                    False

        ChwSecondEncounter ->
            case activity of
                DangerSigns ->
                    True

                BirthPlan ->
                    activityCompleted currentDate assembled DangerSigns
                        && noDangerSigns assembled

                Backend.PrenatalActivity.Model.HealthEducation ->
                    activityCompleted currentDate assembled DangerSigns
                        && noDangerSigns assembled

                NextSteps ->
                    mandatoryActivitiesForNextStepsCompleted currentDate assembled

                -- Unique nurse activities.
                _ ->
                    False

        ChwThirdPlusEncounter ->
            case activity of
                DangerSigns ->
                    True

                Backend.PrenatalActivity.Model.HealthEducation ->
                    activityCompleted currentDate assembled DangerSigns
                        && noDangerSigns assembled

                NextSteps ->
                    mandatoryActivitiesForNextStepsCompleted currentDate assembled

                -- Unique nurse activities.
                _ ->
                    False

        ChwPostpartumEncounter ->
            case activity of
                PregnancyOutcome ->
                    True

                DangerSigns ->
                    True

                NextSteps ->
                    mandatoryActivitiesForNextStepsCompleted currentDate assembled

                -- Unique nurse activities.
                _ ->
                    False


activityCompleted : NominalDate -> AssembledData -> PrenatalActivity -> Bool
activityCompleted currentDate assembled activity =
    case activity of
        PregnancyDating ->
            isJust assembled.measurements.lastMenstrualPeriod

        History ->
            if isFirstEncounter assembled then
                -- First antenatal encounter - all tasks should be completed
                isJust assembled.measurements.obstetricHistory
                    && isJust assembled.measurements.obstetricHistoryStep2
                    && isJust assembled.measurements.medicalHistory
                    && isJust assembled.measurements.socialHistory

            else
                -- Subsequent antenatal encounter - only Social history task
                -- needs to be completed.
                isJust assembled.measurements.socialHistory

        Examination ->
            isJust assembled.measurements.vitals
                && isJust assembled.measurements.nutrition
                && isJust assembled.measurements.corePhysicalExam
                && isJust assembled.measurements.obstetricalExam
                && isJust assembled.measurements.breastExam

        FamilyPlanning ->
            isJust assembled.measurements.familyPlanning

        Backend.PrenatalActivity.Model.MalariaPrevention ->
            isJust assembled.measurements.malariaPrevention

        Medication ->
            isJust assembled.measurements.medication

        DangerSigns ->
            isJust assembled.measurements.dangerSigns

        PrenatalPhoto ->
            isJust assembled.measurements.prenatalPhoto

        Laboratory ->
            if assembled.encounter.encounterType == NurseEncounter then
                List.all (laboratoryTaskCompleted currentDate assembled) laboratoryTasks

            else
                -- CHW only got one lab on first encounter
                isJust assembled.measurements.pregnancyTest

        Backend.PrenatalActivity.Model.HealthEducation ->
            isJust assembled.measurements.healthEducation

        BirthPlan ->
            isJust assembled.measurements.birthPlan

        NextSteps ->
            let
                nextStepsTasks =
                    resolveNextStepsTasks currentDate assembled
            in
            case nextStepsTasks of
                [ NextStepsAppointmentConfirmation, NextStepsFollowUp ] ->
                    isJust assembled.measurements.appointmentConfirmation && isJust assembled.measurements.followUp

                [ NextStepsSendToHC, NextStepsFollowUp ] ->
                    isJust assembled.measurements.sendToHC && isJust assembled.measurements.followUp

                [ NextStepsHealthEducation, NextStepsNewbornEnrolment ] ->
                    isJust assembled.measurements.healthEducation
                        && isJust assembled.participant.newborn

                [ NextStepsSendToHC, NextStepsFollowUp, NextStepsHealthEducation, NextStepsNewbornEnrolment ] ->
                    isJust assembled.measurements.sendToHC
                        && isJust assembled.measurements.followUp
                        && isJust assembled.measurements.healthEducation
                        && isJust assembled.participant.newborn

                _ ->
                    False

        PregnancyOutcome ->
            isJust assembled.participant.dateConcluded


resolveNextStepsTasks : NominalDate -> AssembledData -> List NextStepsTask
resolveNextStepsTasks currentDate data =
    case data.encounter.encounterType of
        -- We should never get here, as Next Steps
        -- displayed only for CHW.
        NurseEncounter ->
            []

        _ ->
            -- The order is important. Do not change.
            [ NextStepsAppointmentConfirmation, NextStepsSendToHC, NextStepsFollowUp, NextStepsHealthEducation, NextStepsNewbornEnrolment ]
                |> List.filter (expectNextStepsTasks currentDate data)


expectNextStepsTasks : NominalDate -> AssembledData -> NextStepsTask -> Bool
expectNextStepsTasks currentDate data task =
    let
        dangerSigns =
            dangerSignsPresent data
    in
    case task of
        NextStepsAppointmentConfirmation ->
            not dangerSigns && data.encounter.encounterType /= ChwPostpartumEncounter

        NextStepsFollowUp ->
            case data.encounter.encounterType of
                ChwPostpartumEncounter ->
                    dangerSigns

                _ ->
                    True

        NextStepsSendToHC ->
            dangerSigns

        NextStepsHealthEducation ->
            data.encounter.encounterType == ChwPostpartumEncounter

        NextStepsNewbornEnrolment ->
            data.encounter.encounterType == ChwPostpartumEncounter


mandatoryActivitiesForNextStepsCompleted : NominalDate -> AssembledData -> Bool
mandatoryActivitiesForNextStepsCompleted currentDate data =
    case data.encounter.encounterType of
        NurseEncounter ->
            -- There're no mandatory activities for nurse encounters.
            True

        ChwFirstEncounter ->
            let
                commonMandatoryActivitiesCompleted =
                    ((not <| expectActivity currentDate data PregnancyDating) || activityCompleted currentDate data PregnancyDating)
                        && ((not <| expectActivity currentDate data Laboratory) || activityCompleted currentDate data Laboratory)
                        && activityCompleted currentDate data DangerSigns
            in
            if dangerSignsPresent data then
                commonMandatoryActivitiesCompleted

            else
                commonMandatoryActivitiesCompleted
                    && activityCompleted currentDate data Backend.PrenatalActivity.Model.HealthEducation

        ChwSecondEncounter ->
            let
                commonMandatoryActivitiesCompleted =
                    activityCompleted currentDate data DangerSigns
            in
            if dangerSignsPresent data then
                commonMandatoryActivitiesCompleted

            else
                commonMandatoryActivitiesCompleted
                    && activityCompleted currentDate data BirthPlan
                    && activityCompleted currentDate data Backend.PrenatalActivity.Model.HealthEducation

        ChwThirdPlusEncounter ->
            let
                commonMandatoryActivitiesCompleted =
                    activityCompleted currentDate data DangerSigns
            in
            if dangerSignsPresent data then
                commonMandatoryActivitiesCompleted

            else
                commonMandatoryActivitiesCompleted
                    && activityCompleted currentDate data Backend.PrenatalActivity.Model.HealthEducation

        ChwPostpartumEncounter ->
            activityCompleted currentDate data PregnancyOutcome
                && activityCompleted currentDate data DangerSigns


expectPrenatalPhoto : NominalDate -> AssembledData -> Bool
expectPrenatalPhoto currentDate data =
    let
        periods =
            -- Periods, where we want to have 1 photo:
            --  1. 12 weeks, or less.
            --  2. Between week 13 and week 27.
            --  3. Week 28, or more.
            [ [ (>) 13 ], [ (>) 28, (<=) 13 ], [ (<=) 28 ] ]

        nursePreviousMeasurements =
            List.map Tuple.second data.nursePreviousMeasurementsWithDates
    in
    data.globalLmpDate
        |> Maybe.map
            (\lmpDate ->
                let
                    currentWeek =
                        diffDays lmpDate currentDate // 7

                    conditionsForCurrentWeek =
                        periods
                            |> List.filter
                                (\periodConditions ->
                                    List.all (\condition -> condition currentWeek == True) periodConditions
                                )
                            |> List.head
                in
                conditionsForCurrentWeek
                    |> Maybe.map
                        (\conditions ->
                            -- There should be no encounters that are  within dates range,
                            -- that got a photo measurement.
                            data.nursePreviousMeasurementsWithDates
                                |> List.filterMap
                                    (\( encounterDate, measurements ) ->
                                        let
                                            encounterWeek =
                                                diffDays lmpDate encounterDate // 7
                                        in
                                        -- Encounter is within dates range, and it's has a photo measurement.
                                        if
                                            List.all (\condition -> condition encounterWeek == True) conditions
                                                && isJust measurements.prenatalPhoto
                                        then
                                            Just encounterDate

                                        else
                                            Nothing
                                    )
                                |> List.isEmpty
                        )
                    -- There are no period conditions, meaning we're not within required dates
                    -- range. Therefore, we do not allow photo activity.
                    |> Maybe.withDefault False
            )
        -- We do not allow photo activity when Lmp date is not known.
        |> Maybe.withDefault False


noDangerSigns : AssembledData -> Bool
noDangerSigns data =
    let
        getDangerSignsType getFunc =
            data.measurements.dangerSigns
                |> Maybe.map (Tuple.second >> .value >> getFunc >> EverySet.toList)
                |> Maybe.withDefault []

        dangerSignsEmpty emptySign signsList =
            List.isEmpty signsList || signsList == [ emptySign ]
    in
    case data.encounter.encounterType of
        ChwPostpartumEncounter ->
            let
                motherSignsEmpty =
                    getDangerSignsType .postpartumMother
                        |> dangerSignsEmpty NoPostpartumMotherDangerSigns

                childSignsEmpty =
                    getDangerSignsType .postpartumChild
                        |> dangerSignsEmpty NoPostpartumChildDangerSigns
            in
            motherSignsEmpty && childSignsEmpty

        _ ->
            getDangerSignsType .signs
                |> dangerSignsEmpty NoDangerSign


dangerSignsPresent : AssembledData -> Bool
dangerSignsPresent data =
    isJust data.measurements.dangerSigns && not (noDangerSigns data)


generateDangerSignsList : Language -> AssembledData -> List String
generateDangerSignsList language data =
    let
        getDangerSignsListForType getFunc translateFunc noSignsValue =
            data.measurements.dangerSigns
                |> Maybe.map
                    (Tuple.second
                        >> .value
                        >> getFunc
                        >> EverySet.toList
                        >> List.filter ((/=) noSignsValue)
                        >> List.map (translateFunc >> translate language)
                    )
                |> Maybe.withDefault []
    in
    case data.encounter.encounterType of
        ChwPostpartumEncounter ->
            let
                motherSigns =
                    getDangerSignsListForType .postpartumMother Translate.PostpartumMotherDangerSign NoPostpartumMotherDangerSigns

                childSigns =
                    getDangerSignsListForType .postpartumChild Translate.PostpartumChildDangerSign NoPostpartumChildDangerSigns
            in
            motherSigns ++ childSigns

        _ ->
            getDangerSignsListForType .signs Translate.DangerSign NoDangerSign


getMotherHeightMeasurement : PrenatalMeasurements -> Maybe HeightInCm
getMotherHeightMeasurement measurements =
    measurements.nutrition
        |> Maybe.map (Tuple.second >> .value >> .height)


generatePrenatalAssesment : AssembledData -> PrenatalAssesment
generatePrenatalAssesment assembled =
    if noDangerSigns assembled then
        AssesmentNormalPregnancy

    else
        AssesmentHighRiskPregnancy


healthEducationFormInputsAndTasks : Language -> AssembledData -> HealthEducationForm -> ( List (Html Msg), List (Maybe Bool) )
healthEducationFormInputsAndTasks language assembled healthEducationForm =
    let
        form =
            assembled.measurements.healthEducation
                |> getMeasurementValueFunc
                |> healthEducationFormWithDefault healthEducationForm

        healthEducationCompletedAtEncounter encounterType =
            assembled.chwPreviousMeasurementsWithDates
                |> List.filterMap
                    (\( _, encounterType_, measurements ) ->
                        if encounterType == encounterType_ then
                            Just measurements

                        else
                            Nothing
                    )
                -- There's a posibility to have more than one
                -- 'Third' enciunter, therefore, the check
                -- for ANY in list.
                |> List.any (.healthEducation >> isJust)

        firstEnconterInputs =
            [ expectationsInput, visitsReviewInput, warningSignsInput ]

        firstEnconterTasks =
            [ form.expectations, form.visitsReview, form.warningSigns ]

        secondEnconterInputs =
            [ hemorrhagingInput ]

        secondEnconterTasks =
            [ form.hemorrhaging ]

        thirdEnconterInputs =
            [ hemorrhagingInput, familyPlanningInput, breastfeedingInput ]

        thirdEnconterTasks =
            [ form.hemorrhaging, form.familyPlanning, form.breastfeeding ]

        postpartumEnconterInputs =
            [ breastfeedingInput, immunizationInput, hygieneInput ]

        postpartumEnconterTasks =
            [ form.breastfeeding, form.immunization, form.hygiene ]

        expectationsUpdateFunc value form_ =
            { form_ | expectations = Just value }

        setBoolInputMsg =
            case assembled.encounter.encounterType of
                ChwPostpartumEncounter ->
                    SetHealthEducationSubActivityBoolInput

                _ ->
                    SetHealthEducationBoolInput

        expectationsInput =
            [ viewQuestionLabel language <| Translate.PrenatalHealthEducationQuestion EducationExpectations
            , viewBoolInput
                language
                form.expectations
                (setBoolInputMsg expectationsUpdateFunc)
                "expectations"
                Nothing
            ]

        visitsReviewUpdateFunc value form_ =
            { form_ | visitsReview = Just value }

        visitsReviewInput =
            [ viewQuestionLabel language <| Translate.PrenatalHealthEducationQuestion EducationVisitsReview
            , viewBoolInput
                language
                form.visitsReview
                (setBoolInputMsg visitsReviewUpdateFunc)
                "visits-review"
                Nothing
            ]

        warningSignsUpdateFunc value form_ =
            { form_ | warningSigns = Just value }

        warningSignsInput =
            [ viewQuestionLabel language <| Translate.PrenatalHealthEducationQuestion EducationWarningSigns
            , viewBoolInput
                language
                form.warningSigns
                (setBoolInputMsg warningSignsUpdateFunc)
                "warning-signs"
                Nothing
            ]

        hemorrhagingUpdateFunc value form_ =
            { form_ | hemorrhaging = Just value }

        hemorrhagingInput =
            [ viewQuestionLabel language <| Translate.PrenatalHealthEducationQuestion EducationHemorrhaging
            , viewBoolInput
                language
                form.hemorrhaging
                (setBoolInputMsg hemorrhagingUpdateFunc)
                "hemorrhaging"
                Nothing
            ]

        familyPlanningUpdateFunc value form_ =
            { form_ | familyPlanning = Just value }

        familyPlanningInput =
            [ viewQuestionLabel language <| Translate.PrenatalHealthEducationQuestion EducationFamilyPlanning
            , viewBoolInput
                language
                form.familyPlanning
                (setBoolInputMsg familyPlanningUpdateFunc)
                "family-planning"
                Nothing
            ]

        breastfeedingUpdateFunc value form_ =
            { form_ | breastfeeding = Just value }

        breastfeedingInput =
            [ viewQuestionLabel language <| Translate.PrenatalHealthEducationQuestion EducationBreastfeeding
            , viewBoolInput
                language
                form.breastfeeding
                (setBoolInputMsg breastfeedingUpdateFunc)
                "breastfeeding"
                Nothing
            ]

        immunizationUpdateFunc value form_ =
            { form_ | immunization = Just value }

        immunizationInput =
            [ viewQuestionLabel language <| Translate.PrenatalHealthEducationQuestion EducationImmunization
            , viewBoolInput
                language
                form.immunization
                (setBoolInputMsg immunizationUpdateFunc)
                "immunization"
                Nothing
            ]

        hygieneUpdateFunc value form_ =
            { form_ | hygiene = Just value }

        hygieneInput =
            [ viewQuestionLabel language <| Translate.PrenatalHealthEducationQuestion EducationHygiene
            , viewBoolInput
                language
                form.hygiene
                (setBoolInputMsg hygieneUpdateFunc)
                "hygiene"
                Nothing
            ]

        ( inputsFromFirst, tasksFromFirst ) =
            if healthEducationCompletedAtFirst || healthEducationCompletedAtSecond || healthEducationCompletedAtThird then
                ( [], [] )

            else
                ( firstEnconterInputs, firstEnconterTasks )

        healthEducationCompletedAtFirst =
            healthEducationCompletedAtEncounter ChwFirstEncounter

        healthEducationCompletedAtSecond =
            healthEducationCompletedAtEncounter ChwSecondEncounter

        healthEducationCompletedAtThird =
            healthEducationCompletedAtEncounter ChwThirdPlusEncounter
    in
    -- For all encounter types but postpartum, if Health
    -- education was not completed at previous encounter,
    -- its inputs are added to next encounter.
    case assembled.encounter.encounterType of
        ChwFirstEncounter ->
            ( List.concat firstEnconterInputs
            , firstEnconterTasks
            )

        ChwSecondEncounter ->
            ( List.concat <| inputsFromFirst ++ secondEnconterInputs
            , tasksFromFirst ++ secondEnconterTasks
            )

        ChwThirdPlusEncounter ->
            -- Second encounter tasks reappear at third encounter anyway,
            -- so, we do not need to add them explicitly.
            ( List.concat <| inputsFromFirst ++ thirdEnconterInputs
            , tasksFromFirst ++ thirdEnconterTasks
            )

        ChwPostpartumEncounter ->
            ( List.concat postpartumEnconterInputs
            , postpartumEnconterTasks
            )

        -- We should never get here, as health
        -- education is presented only for CHW.
        NurseEncounter ->
            ( [], [] )


nextStepsTasksCompletedFromTotal :
    Language
    -> AssembledData
    -> NextStepsData
    -> NextStepsTask
    -> ( Int, Int )
nextStepsTasksCompletedFromTotal language assembled data task =
    case task of
        NextStepsAppointmentConfirmation ->
            let
                form =
                    assembled.measurements.appointmentConfirmation
                        |> getMeasurementValueFunc
                        |> appointmentConfirmationFormWithDefault data.appointmentConfirmationForm
            in
            ( taskCompleted form.appointmentDate
            , 1
            )

        NextStepsFollowUp ->
            let
                form =
                    assembled.measurements.followUp
                        |> getMeasurementValueFunc
                        |> followUpFormWithDefault data.followUpForm
            in
            ( taskCompleted form.option
            , 1
            )

        NextStepsSendToHC ->
            let
                form =
                    assembled.measurements.sendToHC
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
            ( reasonForNotSentActive + taskCompleted form.handReferralForm + taskCompleted form.accompanyToHealthCenter
            , reasonForNotSentCompleted + 2
            )

        NextStepsHealthEducation ->
            let
                form =
                    assembled.measurements.healthEducation
                        |> getMeasurementValueFunc
                        |> healthEducationFormWithDefault data.healthEducationForm

                tasksCompleted =
                    List.map taskCompleted tasks
                        |> List.sum

                ( _, tasks ) =
                    healthEducationFormInputsAndTasks language assembled data.healthEducationForm
            in
            ( tasksCompleted
            , List.length tasks
            )

        NextStepsNewbornEnrolment ->
            ( taskCompleted assembled.participant.newborn
            , 1
            )


{-| This is a convenience for cases where the form values ought to be redefined
to allow multiple values. So, it should go away eventually.
-}
toEverySet : a -> a -> Bool -> EverySet a
toEverySet presentValue absentValue present =
    if present then
        EverySet.singleton presentValue

    else
        EverySet.singleton absentValue


resolvePreviousValue : AssembledData -> (PrenatalMeasurements -> Maybe ( id, PrenatalMeasurement a )) -> (a -> b) -> Maybe b
resolvePreviousValue assembled measurementFunc valueFunc =
    assembled.nursePreviousMeasurementsWithDates
        |> List.filterMap
            (\( _, measurements ) ->
                measurementFunc measurements
                    |> Maybe.map (Tuple.second >> .value >> valueFunc)
            )
        |> List.reverse
        |> List.head


fromBreastExamValue : Maybe BreastExamValue -> BreastExamForm
fromBreastExamValue saved =
    { breast = Maybe.map (.exam >> EverySet.toList) saved
    , selfGuidance = Maybe.map .selfGuidance saved
    }


breastExamFormWithDefault : BreastExamForm -> Maybe BreastExamValue -> BreastExamForm
breastExamFormWithDefault form saved =
    saved
        |> unwrap
            form
            (\value ->
                { breast = or form.breast (value.exam |> EverySet.toList |> Just)
                , selfGuidance = or form.selfGuidance (Just value.selfGuidance)
                }
            )


toBreastExamValueWithDefault : Maybe BreastExamValue -> BreastExamForm -> Maybe BreastExamValue
toBreastExamValueWithDefault saved form =
    breastExamFormWithDefault form saved
        |> toBreastExamValue


toBreastExamValue : BreastExamForm -> Maybe BreastExamValue
toBreastExamValue form =
    -- The `EverySet.singleton` is temporary, until BresatExamForm is
    -- redefined to allow more than one.
    Maybe.map BreastExamValue (Maybe.map EverySet.fromList form.breast)
        |> andMap form.selfGuidance


fromCorePhysicalExamValue : Maybe CorePhysicalExamValue -> CorePhysicalExamForm
fromCorePhysicalExamValue saved =
    { brittleHair = Maybe.map (.hairHead >> EverySet.member BrittleHairCPE) saved
    , paleConjuctiva = Maybe.map (.eyes >> EverySet.member PaleConjuctiva) saved
    , neck = Maybe.map (.neck >> EverySet.toList) saved
    , heart = Maybe.andThen (.heart >> EverySet.toList >> List.head) saved
    , heartMurmur = Maybe.map .heartMurmur saved
    , lungs = Maybe.map (.lungs >> EverySet.toList) saved
    , abdomen = Maybe.map (.abdomen >> EverySet.toList) saved
    , hands = Maybe.map (.hands >> EverySet.toList) saved
    , legs = Maybe.map (.legs >> EverySet.toList) saved
    }


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


fromDangerSignsValue : Maybe DangerSignsValue -> DangerSignsForm
fromDangerSignsValue saved =
    { signs = Maybe.map (.signs >> EverySet.toList) saved
    , postpartumMother = Maybe.map (.postpartumMother >> EverySet.toList) saved
    , postpartumChild = Maybe.map (.postpartumChild >> EverySet.toList) saved
    }


dangerSignsFormWithDefault : DangerSignsForm -> Maybe DangerSignsValue -> DangerSignsForm
dangerSignsFormWithDefault form saved =
    saved
        |> unwrap
            form
            (\value ->
                { signs = or form.signs (EverySet.toList value.signs |> Just)
                , postpartumMother = or form.postpartumMother (EverySet.toList value.postpartumMother |> Just)
                , postpartumChild = or form.postpartumChild (EverySet.toList value.postpartumChild |> Just)
                }
            )


toDangerSignsValueWithDefault : Maybe DangerSignsValue -> DangerSignsForm -> Maybe DangerSignsValue
toDangerSignsValueWithDefault saved form =
    dangerSignsFormWithDefault form saved
        |> toDangerSignsValue


toDangerSignsValue : DangerSignsForm -> Maybe DangerSignsValue
toDangerSignsValue form =
    let
        signs =
            form.signs
                |> Maybe.withDefault [ NoDangerSign ]
                |> EverySet.fromList

        postpartumMother =
            form.postpartumMother
                |> Maybe.withDefault [ NoPostpartumMotherDangerSigns ]
                |> EverySet.fromList

        postpartumChild =
            form.postpartumChild
                |> Maybe.withDefault [ NoPostpartumChildDangerSigns ]
                |> EverySet.fromList
    in
    Just <| DangerSignsValue signs postpartumMother postpartumChild


fromLastMenstrualPeriodValue : Maybe LastMenstrualPeriodValue -> PregnancyDatingForm
fromLastMenstrualPeriodValue saved =
    { lmpRange = Nothing
    , lmpDate = Maybe.map .date saved
    , lmpDateConfident = Maybe.map .confident saved
    , chwLmpConfirmation = Maybe.map .confirmation saved
    , isDateSelectorOpen = False
    }


lastMenstrualPeriodFormWithDefault : PregnancyDatingForm -> Maybe LastMenstrualPeriodValue -> PregnancyDatingForm
lastMenstrualPeriodFormWithDefault form saved =
    saved
        |> unwrap
            form
            (\value ->
                { lmpRange = or form.lmpRange (Just SixMonth)
                , lmpDate = or form.lmpDate (Just value.date)
                , lmpDateConfident = or form.lmpDateConfident (Just value.confident)
                , chwLmpConfirmation = or form.chwLmpConfirmation (Just value.confirmation)
                , isDateSelectorOpen = form.isDateSelectorOpen
                }
            )


toLastMenstrualPeriodValueWithDefault : Maybe LastMenstrualPeriodValue -> PregnancyDatingForm -> Maybe LastMenstrualPeriodValue
toLastMenstrualPeriodValueWithDefault saved form =
    lastMenstrualPeriodFormWithDefault form saved
        |> toLastMenstrualPeriodValue


toLastMenstrualPeriodValue : PregnancyDatingForm -> Maybe LastMenstrualPeriodValue
toLastMenstrualPeriodValue form =
    let
        chwLmpConfirmation =
            Maybe.withDefault False form.chwLmpConfirmation
    in
    Maybe.map LastMenstrualPeriodValue form.lmpDate
        |> andMap form.lmpDateConfident
        |> andMap (Just chwLmpConfirmation)


fromMedicalHistoryValue : Maybe (EverySet MedicalHistorySign) -> MedicalHistoryForm
fromMedicalHistoryValue saved =
    { uterineMyoma = Maybe.map (EverySet.member UterineMyoma) saved
    , diabetes = Maybe.map (EverySet.member Diabetes) saved
    , cardiacDisease = Maybe.map (EverySet.member CardiacDisease) saved
    , renalDisease = Maybe.map (EverySet.member RenalDisease) saved
    , hypertensionBeforePregnancy = Maybe.map (EverySet.member HypertensionBeforePregnancy) saved
    , tuberculosisPast = Maybe.map (EverySet.member TuberculosisPast) saved
    , tuberculosisPresent = Maybe.map (EverySet.member TuberculosisPresent) saved
    , asthma = Maybe.map (EverySet.member Asthma) saved
    , bowedLegs = Maybe.map (EverySet.member BowedLegs) saved
    , hiv = Maybe.map (EverySet.member HIV) saved
    , mentalHealthHistory = Maybe.map (EverySet.member MentalHealthHistory) saved
    }


medicalHistoryFormWithDefault : MedicalHistoryForm -> Maybe (EverySet MedicalHistorySign) -> MedicalHistoryForm
medicalHistoryFormWithDefault form saved =
    saved
        |> unwrap
            form
            (\value ->
                { uterineMyoma = or form.uterineMyoma (EverySet.member UterineMyoma value |> Just)
                , diabetes = or form.diabetes (EverySet.member Diabetes value |> Just)
                , cardiacDisease = or form.cardiacDisease (EverySet.member CardiacDisease value |> Just)
                , renalDisease = or form.renalDisease (EverySet.member RenalDisease value |> Just)
                , hypertensionBeforePregnancy = or form.hypertensionBeforePregnancy (EverySet.member HypertensionBeforePregnancy value |> Just)
                , tuberculosisPast = or form.tuberculosisPast (EverySet.member TuberculosisPast value |> Just)
                , tuberculosisPresent = or form.tuberculosisPresent (EverySet.member TuberculosisPresent value |> Just)
                , asthma = or form.asthma (EverySet.member Asthma value |> Just)
                , bowedLegs = or form.bowedLegs (EverySet.member BowedLegs value |> Just)
                , hiv = or form.hiv (EverySet.member HIV value |> Just)
                , mentalHealthHistory = or form.mentalHealthHistory (EverySet.member MentalHealthHistory value |> Just)
                }
            )


toMedicalHistoryValueWithDefault : Maybe (EverySet MedicalHistorySign) -> MedicalHistoryForm -> Maybe (EverySet MedicalHistorySign)
toMedicalHistoryValueWithDefault saved form =
    medicalHistoryFormWithDefault form saved
        |> toMedicalHistoryValue


toMedicalHistoryValue : MedicalHistoryForm -> Maybe (EverySet MedicalHistorySign)
toMedicalHistoryValue form =
    [ Maybe.map (ifTrue UterineMyoma) form.uterineMyoma
    , Maybe.map (ifTrue Diabetes) form.diabetes
    , Maybe.map (ifTrue CardiacDisease) form.cardiacDisease
    , Maybe.map (ifTrue HypertensionBeforePregnancy) form.hypertensionBeforePregnancy
    , Maybe.map (ifTrue TuberculosisPast) form.tuberculosisPast
    , Maybe.map (ifTrue TuberculosisPresent) form.tuberculosisPresent
    , Maybe.map (ifTrue Asthma) form.asthma
    , Maybe.map (ifTrue BowedLegs) form.bowedLegs
    , Maybe.map (ifTrue HIV) form.hiv
    , Maybe.map (ifTrue MentalHealthHistory) form.mentalHealthHistory
    ]
        |> Maybe.Extra.combine
        |> Maybe.map (List.foldl EverySet.union EverySet.empty >> ifEverySetEmpty NoMedicalHistorySigns)


fromMedicationValue : Maybe (EverySet MedicationSign) -> MedicationForm
fromMedicationValue saved =
    { receivedIronFolicAcid = Maybe.map (EverySet.member IronAndFolicAcidSupplement) saved
    , receivedDewormingPill = Maybe.map (EverySet.member DewormingPill) saved
    }


medicationFormWithDefault : MedicationForm -> Maybe (EverySet MedicationSign) -> MedicationForm
medicationFormWithDefault form saved =
    saved
        |> unwrap
            form
            (\value ->
                { receivedIronFolicAcid = or form.receivedIronFolicAcid (EverySet.member IronAndFolicAcidSupplement value |> Just)
                , receivedDewormingPill = or form.receivedDewormingPill (EverySet.member DewormingPill value |> Just)
                }
            )


toMedicationValueWithDefault : Maybe (EverySet MedicationSign) -> MedicationForm -> Maybe (EverySet MedicationSign)
toMedicationValueWithDefault saved form =
    medicationFormWithDefault form saved
        |> toMedicationValue


toMedicationValue : MedicationForm -> Maybe (EverySet MedicationSign)
toMedicationValue form =
    [ Maybe.map (ifTrue IronAndFolicAcidSupplement) form.receivedIronFolicAcid
    , ifNullableTrue DewormingPill form.receivedDewormingPill
    ]
        |> Maybe.Extra.combine
        |> Maybe.map (List.foldl EverySet.union EverySet.empty >> ifEverySetEmpty NoMedication)


fromObstetricalExamValue : Maybe ObstetricalExamValue -> ObstetricalExamForm
fromObstetricalExamValue saved =
    { fundalHeight = Maybe.map (.fundalHeight >> heightValueFunc) saved
    , fundalHeightDirty = False
    , fetalPresentation = Maybe.map .fetalPresentation saved
    , fetalMovement = Maybe.map .fetalMovement saved
    , fetalHeartRate = Maybe.map .fetalHeartRate saved
    , fetalHeartRateDirty = False
    , cSectionScar = Maybe.map .cSectionScar saved
    }


obstetricalExamFormWithDefault : ObstetricalExamForm -> Maybe ObstetricalExamValue -> ObstetricalExamForm
obstetricalExamFormWithDefault form saved =
    saved
        |> unwrap
            form
            (\value ->
                { fundalHeight = valueConsideringIsDirtyField form.fundalHeightDirty form.fundalHeight (heightValueFunc value.fundalHeight)
                , fundalHeightDirty = form.fundalHeightDirty
                , fetalPresentation = or form.fetalPresentation (Just value.fetalPresentation)
                , fetalMovement = or form.fetalMovement (Just value.fetalMovement)
                , fetalHeartRate = valueConsideringIsDirtyField form.fetalHeartRateDirty form.fetalHeartRate value.fetalHeartRate
                , fetalHeartRateDirty = form.fetalHeartRateDirty
                , cSectionScar = or form.cSectionScar (Just value.cSectionScar)
                }
            )


toObstetricalExamValueWithDefault : Maybe ObstetricalExamValue -> ObstetricalExamForm -> Maybe ObstetricalExamValue
toObstetricalExamValueWithDefault saved form =
    obstetricalExamFormWithDefault form saved
        |> toObstetricalExamValue


toObstetricalExamValue : ObstetricalExamForm -> Maybe ObstetricalExamValue
toObstetricalExamValue form =
    Maybe.map ObstetricalExamValue (Maybe.map HeightInCm form.fundalHeight)
        |> andMap form.fetalPresentation
        |> andMap form.fetalMovement
        |> andMap form.fetalHeartRate
        |> andMap form.cSectionScar


fromObstetricHistoryValue : Maybe ObstetricHistoryValue -> ObstetricFormFirstStep
fromObstetricHistoryValue saved =
    { currentlyPregnant = Maybe.map .currentlyPregnant saved
    , termPregnancy = Maybe.map .termPregnancy saved
    , termPregnancyDirty = False
    , preTermPregnancy = Maybe.map .preTermPregnancy saved
    , preTermPregnancyDirty = False
    , stillbirthsAtTerm = Maybe.map .stillbirthsAtTerm saved
    , stillbirthsAtTermDirty = False
    , stillbirthsPreTerm = Maybe.map .stillbirthsPreTerm saved
    , stillbirthsPreTermDirty = False
    , abortions = Maybe.map .abortions saved
    , abortionsDirty = False
    , liveChildren = Maybe.map .liveChildren saved
    , liveChildrenDirty = False
    }


obstetricHistoryFormWithDefault : ObstetricFormFirstStep -> Maybe ObstetricHistoryValue -> ObstetricFormFirstStep
obstetricHistoryFormWithDefault form saved =
    saved
        |> unwrap
            form
            (\value ->
                { currentlyPregnant = or form.currentlyPregnant (Just value.currentlyPregnant)
                , termPregnancy = valueConsideringIsDirtyField form.termPregnancyDirty form.termPregnancy value.termPregnancy
                , termPregnancyDirty = form.termPregnancyDirty
                , preTermPregnancy = valueConsideringIsDirtyField form.preTermPregnancyDirty form.preTermPregnancy value.preTermPregnancy
                , preTermPregnancyDirty = form.preTermPregnancyDirty
                , stillbirthsAtTerm = valueConsideringIsDirtyField form.stillbirthsAtTermDirty form.stillbirthsAtTerm value.stillbirthsAtTerm
                , stillbirthsAtTermDirty = form.stillbirthsAtTermDirty
                , stillbirthsPreTerm = valueConsideringIsDirtyField form.stillbirthsPreTermDirty form.stillbirthsPreTerm value.stillbirthsPreTerm
                , stillbirthsPreTermDirty = form.stillbirthsPreTermDirty
                , abortions = valueConsideringIsDirtyField form.abortionsDirty form.abortions value.abortions
                , abortionsDirty = form.abortionsDirty
                , liveChildren = valueConsideringIsDirtyField form.liveChildrenDirty form.liveChildren value.liveChildren
                , liveChildrenDirty = form.liveChildrenDirty
                }
            )


toObstetricHistoryValueWithDefault : Maybe ObstetricHistoryValue -> ObstetricFormFirstStep -> Maybe ObstetricHistoryValue
toObstetricHistoryValueWithDefault saved form =
    obstetricHistoryFormWithDefault form saved
        |> toObstetricHistoryValue


toObstetricHistoryValue : ObstetricFormFirstStep -> Maybe ObstetricHistoryValue
toObstetricHistoryValue form =
    Maybe.map ObstetricHistoryValue form.currentlyPregnant
        |> andMap form.termPregnancy
        |> andMap form.preTermPregnancy
        |> andMap form.stillbirthsAtTerm
        |> andMap form.stillbirthsPreTerm
        |> andMap form.abortions
        |> andMap form.liveChildren


obstetricHistoryStep2FormWithDefault : ObstetricFormSecondStep -> Maybe ObstetricHistoryStep2Value -> ObstetricFormSecondStep
obstetricHistoryStep2FormWithDefault form saved =
    saved
        |> unwrap
            form
            (\value ->
                { cSections = valueConsideringIsDirtyField form.cSectionsDirty form.cSections value.cSections
                , cSectionsDirty = form.cSectionsDirty
                , cSectionInPreviousDelivery = or form.cSectionInPreviousDelivery (EverySet.member CSectionInPreviousDelivery value.previousDelivery |> Just)
                , cSectionReason = or form.cSectionReason (value.cSectionReason |> EverySet.toList |> List.head)
                , previousDeliveryPeriod = or form.previousDeliveryPeriod (value.previousDeliveryPeriod |> EverySet.toList |> List.head)
                , successiveAbortions = or form.successiveAbortions (EverySet.member SuccessiveAbortions value.obstetricHistory |> Just)
                , successivePrematureDeliveries = or form.successivePrematureDeliveries (EverySet.member SuccessivePrematureDeliveries value.obstetricHistory |> Just)
                , stillbornPreviousDelivery = or form.stillbornPreviousDelivery (EverySet.member StillbornPreviousDelivery value.previousDelivery |> Just)
                , babyDiedOnDayOfBirthPreviousDelivery = or form.babyDiedOnDayOfBirthPreviousDelivery (EverySet.member BabyDiedOnDayOfBirthPreviousDelivery value.previousDelivery |> Just)
                , partialPlacentaPreviousDelivery = or form.partialPlacentaPreviousDelivery (EverySet.member PartialPlacentaPreviousDelivery value.previousDelivery |> Just)
                , severeHemorrhagingPreviousDelivery = or form.severeHemorrhagingPreviousDelivery (EverySet.member SevereHemorrhagingPreviousDelivery value.previousDelivery |> Just)
                , preeclampsiaPreviousPregnancy = or form.preeclampsiaPreviousPregnancy (EverySet.member PreeclampsiaPreviousPregnancy value.obstetricHistory |> Just)
                , convulsionsPreviousDelivery = or form.convulsionsPreviousDelivery (EverySet.member ConvulsionsPreviousDelivery value.previousDelivery |> Just)
                , convulsionsAndUnconsciousPreviousDelivery = or form.convulsionsAndUnconsciousPreviousDelivery (EverySet.member ConvulsionsAndUnconsciousPreviousDelivery value.previousDelivery |> Just)
                , gestationalDiabetesPreviousPregnancy = or form.gestationalDiabetesPreviousPregnancy (EverySet.member GestationalDiabetesPreviousPregnancy value.obstetricHistory |> Just)
                , incompleteCervixPreviousPregnancy = or form.incompleteCervixPreviousPregnancy (EverySet.member IncompleteCervixPreviousPregnancy value.obstetricHistory |> Just)
                , rhNegative = or form.rhNegative (EverySet.member RhNegative value.obstetricHistory |> Just)
                }
            )


toObstetricHistoryStep2ValueWithDefault : Maybe ObstetricHistoryStep2Value -> ObstetricFormSecondStep -> Maybe ObstetricHistoryStep2Value
toObstetricHistoryStep2ValueWithDefault saved form =
    obstetricHistoryStep2FormWithDefault form saved
        |> toObstetricHistoryStep2Value


toObstetricHistoryStep2Value : ObstetricFormSecondStep -> Maybe ObstetricHistoryStep2Value
toObstetricHistoryStep2Value form =
    let
        previousDeliverySet =
            [ Maybe.map (ifTrue CSectionInPreviousDelivery) form.cSectionInPreviousDelivery
            , Maybe.map (ifTrue StillbornPreviousDelivery) form.stillbornPreviousDelivery
            , Maybe.map (ifTrue BabyDiedOnDayOfBirthPreviousDelivery) form.babyDiedOnDayOfBirthPreviousDelivery
            , Maybe.map (ifTrue PartialPlacentaPreviousDelivery) form.partialPlacentaPreviousDelivery
            , Maybe.map (ifTrue SevereHemorrhagingPreviousDelivery) form.severeHemorrhagingPreviousDelivery
            , Maybe.map (ifTrue ConvulsionsPreviousDelivery) form.convulsionsPreviousDelivery
            , Maybe.map (ifTrue ConvulsionsAndUnconsciousPreviousDelivery) form.convulsionsAndUnconsciousPreviousDelivery
            ]
                |> Maybe.Extra.combine
                |> Maybe.map (List.foldl EverySet.union EverySet.empty >> ifEverySetEmpty NoPreviousDeliverySign)

        obstetricHistorySet =
            [ Maybe.map (ifTrue SuccessiveAbortions) form.successiveAbortions
            , Maybe.map (ifTrue SuccessivePrematureDeliveries) form.successivePrematureDeliveries
            , Maybe.map (ifTrue PreeclampsiaPreviousPregnancy) form.preeclampsiaPreviousPregnancy
            , Maybe.map (ifTrue GestationalDiabetesPreviousPregnancy) form.gestationalDiabetesPreviousPregnancy
            , Maybe.map (ifTrue IncompleteCervixPreviousPregnancy) form.incompleteCervixPreviousPregnancy
            , Maybe.map (ifTrue RhNegative) form.rhNegative
            ]
                |> Maybe.Extra.combine
                |> Maybe.map (List.foldl EverySet.union EverySet.empty >> ifEverySetEmpty NoObstetricHistorySign)
    in
    Maybe.map ObstetricHistoryStep2Value form.cSections
        |> andMap (Maybe.map EverySet.singleton form.cSectionReason)
        |> andMap previousDeliverySet
        |> andMap (Maybe.map EverySet.singleton form.previousDeliveryPeriod)
        |> andMap obstetricHistorySet


fromFamilyPlanningValue : Maybe (EverySet FamilyPlanningSign) -> FamilyPlanningForm
fromFamilyPlanningValue saved =
    { signs = Maybe.map EverySet.toList saved }


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


fromPrenatalNutritionValue : Maybe PrenatalNutritionValue -> NutritionAssessmentForm
fromPrenatalNutritionValue saved =
    { height = Maybe.map (.height >> heightValueFunc) saved
    , heightDirty = False
    , weight = Maybe.map (.weight >> weightValueFunc) saved
    , weightDirty = False
    , muac = Maybe.map (.muac >> muacValueFunc) saved
    , muacDirty = False
    }


prenatalNutritionFormWithDefault : NutritionAssessmentForm -> Maybe PrenatalNutritionValue -> NutritionAssessmentForm
prenatalNutritionFormWithDefault form saved =
    saved
        |> unwrap
            form
            (\value ->
                { height = valueConsideringIsDirtyField form.heightDirty form.height (heightValueFunc value.height)
                , heightDirty = form.heightDirty
                , weight = valueConsideringIsDirtyField form.weightDirty form.weight (weightValueFunc value.weight)
                , weightDirty = form.weightDirty
                , muac = valueConsideringIsDirtyField form.muacDirty form.muac (muacValueFunc value.muac)
                , muacDirty = form.muacDirty
                }
            )


toPrenatalNutritionValueWithDefault : Maybe PrenatalNutritionValue -> NutritionAssessmentForm -> Maybe PrenatalNutritionValue
toPrenatalNutritionValueWithDefault saved form =
    prenatalNutritionFormWithDefault form saved
        |> toPrenatalNutritionValue


toPrenatalNutritionValue : NutritionAssessmentForm -> Maybe PrenatalNutritionValue
toPrenatalNutritionValue form =
    Maybe.map PrenatalNutritionValue (Maybe.map HeightInCm form.height)
        |> andMap (Maybe.map WeightInKg form.weight)
        |> andMap (Maybe.map MuacInCm form.muac)


fromMalariaPreventionValue : Maybe (EverySet MalariaPreventionSign) -> MalariaPreventionForm
fromMalariaPreventionValue saved =
    { receivedMosquitoNet = Maybe.map (EverySet.member MosquitoNet) saved
    }


malariaPreventionFormWithDefault : MalariaPreventionForm -> Maybe (EverySet MalariaPreventionSign) -> MalariaPreventionForm
malariaPreventionFormWithDefault form saved =
    saved
        |> unwrap
            form
            (\value ->
                { receivedMosquitoNet = or form.receivedMosquitoNet (EverySet.member MosquitoNet value |> Just)
                }
            )


toMalariaPreventionValueWithDefault : Maybe (EverySet MalariaPreventionSign) -> MalariaPreventionForm -> Maybe (EverySet MalariaPreventionSign)
toMalariaPreventionValueWithDefault saved form =
    malariaPreventionFormWithDefault form saved
        |> toMalariaPreventionValue


toMalariaPreventionValue : MalariaPreventionForm -> Maybe (EverySet MalariaPreventionSign)
toMalariaPreventionValue form =
    Maybe.map (toEverySet MosquitoNet NoMalariaPreventionSigns) form.receivedMosquitoNet


fromSocialHistoryValue : Maybe SocialHistoryValue -> SocialHistoryForm
fromSocialHistoryValue saved =
    { accompaniedByPartner = Maybe.map (.socialHistory >> EverySet.member AccompaniedByPartner) saved
    , partnerReceivedCounseling = Maybe.map (.socialHistory >> EverySet.member PartnerHivCounseling) saved
    , partnerReceivedTesting = Maybe.map (.hivTestingResult >> (==) NoHivTesting >> not) saved
    , partnerTestingResult = Maybe.map .hivTestingResult saved
    }


socialHistoryFormWithDefault : SocialHistoryForm -> Maybe SocialHistoryValue -> SocialHistoryForm
socialHistoryFormWithDefault form saved =
    saved
        |> unwrap
            form
            (\value ->
                { accompaniedByPartner = or form.accompaniedByPartner (EverySet.member AccompaniedByPartner value.socialHistory |> Just)
                , partnerReceivedCounseling = or form.partnerReceivedCounseling (EverySet.member PartnerHivCounseling value.socialHistory |> Just)
                , partnerReceivedTesting = or form.partnerReceivedTesting (value.hivTestingResult == NoHivTesting |> not |> Just)
                , partnerTestingResult = or form.partnerTestingResult (Just value.hivTestingResult)
                }
            )


toSocialHistoryValueWithDefault : Maybe SocialHistoryValue -> SocialHistoryForm -> Maybe SocialHistoryValue
toSocialHistoryValueWithDefault saved form =
    socialHistoryFormWithDefault form saved
        |> toSocialHistoryValue


toSocialHistoryValue : SocialHistoryForm -> Maybe SocialHistoryValue
toSocialHistoryValue form =
    let
        socialHistory =
            [ Maybe.map (ifTrue AccompaniedByPartner) form.accompaniedByPartner
            , ifNullableTrue PartnerHivCounseling form.partnerReceivedCounseling
            ]
                |> Maybe.Extra.combine
                |> Maybe.map (List.foldl EverySet.union EverySet.empty >> ifEverySetEmpty NoSocialHistorySign)
    in
    Maybe.map SocialHistoryValue socialHistory
        |> andMap form.partnerTestingResult


fromPregnancyTestValue : Maybe PregnancyTestResult -> PregnancyTestForm
fromPregnancyTestValue saved =
    { pregnancyTestResult = saved }


pregnancyTestFormWithDefault : PregnancyTestForm -> Maybe PregnancyTestResult -> PregnancyTestForm
pregnancyTestFormWithDefault form saved =
    saved
        |> unwrap
            form
            (\value ->
                let
                    formWithDefault =
                        fromPregnancyTestValue saved
                in
                { pregnancyTestResult = or form.pregnancyTestResult formWithDefault.pregnancyTestResult
                }
            )


toPregnancyTestValueWithDefault : Maybe PregnancyTestResult -> PregnancyTestForm -> Maybe PregnancyTestResult
toPregnancyTestValueWithDefault saved form =
    pregnancyTestFormWithDefault form saved
        |> (\form_ ->
                form_
           )
        |> toPregnancyTestValue


toPregnancyTestValue : PregnancyTestForm -> Maybe PregnancyTestResult
toPregnancyTestValue form =
    form.pregnancyTestResult


historyTasksCompletedFromTotal : AssembledData -> HistoryData -> HistoryTask -> ( Int, Int )
historyTasksCompletedFromTotal assembled data task =
    case task of
        Obstetric ->
            case data.obstetricHistoryStep of
                ObstetricHistoryFirstStep ->
                    let
                        formStep1_ =
                            assembled.measurements.obstetricHistory
                                |> getMeasurementValueFunc
                                |> obstetricHistoryFormWithDefault data.obstetricFormFirstStep

                        intInputs =
                            [ formStep1_.termPregnancy
                            , formStep1_.preTermPregnancy
                            , formStep1_.stillbirthsAtTerm
                            , formStep1_.stillbirthsPreTerm
                            , formStep1_.abortions
                            , formStep1_.liveChildren
                            ]
                    in
                    ( (intInputs
                        |> List.map taskCompleted
                        |> List.sum
                      )
                        + taskCompleted formStep1_.currentlyPregnant
                    , List.length intInputs + 1
                    )

                ObstetricHistorySecondStep ->
                    let
                        formStep2_ =
                            assembled.measurements.obstetricHistoryStep2
                                |> getMeasurementValueFunc
                                |> obstetricHistoryStep2FormWithDefault data.obstetricFormSecondStep

                        boolInputs =
                            [ formStep2_.cSectionInPreviousDelivery
                            , formStep2_.successiveAbortions
                            , formStep2_.successivePrematureDeliveries
                            , formStep2_.stillbornPreviousDelivery
                            , formStep2_.babyDiedOnDayOfBirthPreviousDelivery
                            , formStep2_.partialPlacentaPreviousDelivery
                            , formStep2_.severeHemorrhagingPreviousDelivery
                            , formStep2_.preeclampsiaPreviousPregnancy
                            , formStep2_.convulsionsPreviousDelivery
                            , formStep2_.convulsionsAndUnconsciousPreviousDelivery
                            , formStep2_.gestationalDiabetesPreviousPregnancy
                            , formStep2_.incompleteCervixPreviousPregnancy
                            , formStep2_.rhNegative
                            ]
                    in
                    ( (boolInputs
                        |> List.map taskCompleted
                        |> List.sum
                      )
                        + taskCompleted formStep2_.cSections
                        + taskCompleted formStep2_.cSectionReason
                        + taskCompleted formStep2_.previousDeliveryPeriod
                    , List.length boolInputs + 3
                    )

        Medical ->
            let
                medicalForm =
                    assembled.measurements.medicalHistory
                        |> getMeasurementValueFunc
                        |> medicalHistoryFormWithDefault data.medicalForm

                boolInputs =
                    [ medicalForm.uterineMyoma
                    , medicalForm.diabetes
                    , medicalForm.cardiacDisease
                    , medicalForm.renalDisease
                    , medicalForm.hypertensionBeforePregnancy
                    , medicalForm.tuberculosisPast
                    , medicalForm.tuberculosisPresent
                    , medicalForm.asthma
                    , medicalForm.bowedLegs
                    , medicalForm.hiv
                    ]
            in
            ( boolInputs
                |> List.map taskCompleted
                |> List.sum
            , List.length boolInputs
            )

        Social ->
            let
                socialForm =
                    assembled.measurements.socialHistory
                        |> getMeasurementValueFunc
                        |> socialHistoryFormWithDefault data.socialForm

                showCounselingQuestion =
                    assembled.nursePreviousMeasurementsWithDates
                        |> List.filter
                            (\( _, measurements ) ->
                                measurements.socialHistory
                                    |> Maybe.map (Tuple.second >> .value >> .socialHistory >> EverySet.member PartnerHivCounseling)
                                    |> Maybe.withDefault False
                            )
                        |> List.isEmpty

                partnerReceivedCounselingInput =
                    if showCounselingQuestion then
                        [ socialForm.partnerReceivedCounseling ]

                    else
                        []

                showTestingQuestions =
                    assembled.nursePreviousMeasurementsWithDates
                        |> List.filter
                            (\( _, measurements ) ->
                                measurements.socialHistory
                                    |> Maybe.map
                                        (\socialHistory ->
                                            let
                                                value =
                                                    Tuple.second socialHistory |> .value
                                            in
                                            (value.hivTestingResult == ResultHivPositive)
                                                || (value.hivTestingResult == ResultHivNegative)
                                        )
                                    |> Maybe.withDefault False
                            )
                        |> List.isEmpty

                partnerReceivedTestingInput =
                    if showTestingQuestions then
                        [ socialForm.partnerReceivedTesting ]

                    else
                        []

                boolInputs =
                    (socialForm.accompaniedByPartner
                        :: partnerReceivedCounselingInput
                    )
                        ++ partnerReceivedTestingInput

                listInputs =
                    if socialForm.partnerReceivedTesting == Just True then
                        [ socialForm.partnerTestingResult ]

                    else
                        []
            in
            ( (boolInputs |> List.map taskCompleted |> List.sum)
                + (listInputs |> List.map taskCompleted |> List.sum)
            , List.length boolInputs + List.length listInputs
            )


examinationTasksCompletedFromTotal : AssembledData -> ExaminationData -> Bool -> ExaminationTask -> ( Int, Int )
examinationTasksCompletedFromTotal assembled data firstEncounter task =
    case task of
        Vitals ->
            let
                form =
                    assembled.measurements.vitals
                        |> getMeasurementValueFunc
                        |> vitalsFormWithDefault data.vitalsForm
            in
            ( taskAllCompleted [ form.sysBloodPressure, form.diaBloodPressure ]
                + ([ Maybe.map (always ()) form.heartRate
                   , Maybe.map (always ()) form.respiratoryRate
                   , Maybe.map (always ()) form.bodyTemperature
                   ]
                    |> List.map taskCompleted
                    |> List.sum
                  )
            , 4
            )

        NutritionAssessment ->
            let
                hideHeightInput =
                    not firstEncounter

                form_ =
                    assembled.measurements.nutrition
                        |> getMeasurementValueFunc
                        |> prenatalNutritionFormWithDefault data.nutritionAssessmentForm

                form =
                    if hideHeightInput then
                        assembled.nursePreviousMeasurementsWithDates
                            |> List.head
                            |> Maybe.andThen (Tuple.second >> getMotherHeightMeasurement)
                            |> Maybe.map (\(HeightInCm height) -> { form_ | height = Just height })
                            |> Maybe.withDefault form_

                    else
                        form_

                tasks_ =
                    if hideHeightInput then
                        [ form.weight, form.muac ]

                    else
                        [ form.height, form.weight, form.muac ]

                tasksForBmi =
                    if hideHeightInput then
                        [ form.weight ]

                    else
                        [ form.height, form.weight ]
            in
            ( (List.map taskCompleted tasks_ |> List.sum)
                -- This is for BMI task, which is considered as completed
                -- when both height and weight are set.
                + taskAllCompleted tasksForBmi
            , List.length tasks_ + 1
            )

        CorePhysicalExam ->
            let
                form =
                    assembled.measurements.corePhysicalExam
                        |> getMeasurementValueFunc
                        |> corePhysicalExamFormWithDefault data.corePhysicalExamForm

                extremitiesTaskCompleted =
                    if isJust form.hands && isJust form.legs then
                        1

                    else
                        0
            in
            ( extremitiesTaskCompleted
                + taskCompleted form.neck
                + taskCompleted form.lungs
                + taskCompleted form.abdomen
                + taskCompleted form.heart
                + ([ form.brittleHair
                   , form.paleConjuctiva
                   ]
                    |> List.map taskCompleted
                    |> List.sum
                  )
            , 7
            )

        ObstetricalExam ->
            let
                form =
                    assembled.measurements.obstetricalExam
                        |> getMeasurementValueFunc
                        |> obstetricalExamFormWithDefault data.obstetricalExamForm
            in
            ( taskCompleted form.fetalPresentation
                + taskCompleted form.fetalMovement
                + taskCompleted form.cSectionScar
                + ([ Maybe.map (always ()) form.fundalHeight, Maybe.map (always ()) form.fetalHeartRate ]
                    |> List.map taskCompleted
                    |> List.sum
                  )
            , 5
            )

        BreastExam ->
            let
                form =
                    assembled.measurements.breastExam
                        |> getMeasurementValueFunc
                        |> breastExamFormWithDefault data.breastExamForm
            in
            ( taskCompleted form.breast + taskCompleted form.selfGuidance
            , 2
            )


socialHistoryHivTestingResultFromString : String -> Maybe SocialHistoryHivTestingResult
socialHistoryHivTestingResultFromString result =
    case result of
        "positive" ->
            Just ResultHivPositive

        "negative" ->
            Just ResultHivNegative

        "indeterminate" ->
            Just ResultHivIndeterminate

        "none" ->
            Just NoHivTesting

        _ ->
            Nothing


fromBirthPlanValue : Maybe BirthPlanValue -> BirthPlanForm
fromBirthPlanValue saved =
    { haveInsurance = Maybe.map (.signs >> EverySet.member Insurance) saved
    , boughtClothes = Maybe.map (.signs >> EverySet.member BoughtClothes) saved
    , caregiverAccompany = Maybe.map (.signs >> EverySet.member CaregiverAccompany) saved
    , savedMoney = Maybe.map (.signs >> EverySet.member SavedMoney) saved
    , haveTransportation = Maybe.map (.signs >> EverySet.member Transportation) saved
    , familyPlanning = Maybe.map (.familyPlanning >> EverySet.toList) saved
    }


birthPlanFormWithDefault : BirthPlanForm -> Maybe BirthPlanValue -> BirthPlanForm
birthPlanFormWithDefault form saved =
    saved
        |> unwrap
            form
            (\value ->
                { haveInsurance = or form.haveInsurance (EverySet.member Insurance value.signs |> Just)
                , boughtClothes = or form.boughtClothes (EverySet.member BoughtClothes value.signs |> Just)
                , caregiverAccompany = or form.caregiverAccompany (EverySet.member CaregiverAccompany value.signs |> Just)
                , savedMoney = or form.savedMoney (EverySet.member SavedMoney value.signs |> Just)
                , haveTransportation = or form.haveTransportation (EverySet.member Transportation value.signs |> Just)
                , familyPlanning = or form.familyPlanning (value.familyPlanning |> EverySet.toList |> Just)
                }
            )


toBirthPlanValueWithDefault : Maybe BirthPlanValue -> BirthPlanForm -> Maybe BirthPlanValue
toBirthPlanValueWithDefault saved form =
    birthPlanFormWithDefault form saved
        |> toBirthPlanValue


toBirthPlanValue : BirthPlanForm -> Maybe BirthPlanValue
toBirthPlanValue form =
    let
        signs =
            [ Maybe.map (ifTrue Insurance) form.haveInsurance
            , Maybe.map (ifTrue BoughtClothes) form.boughtClothes
            , Maybe.map (ifTrue CaregiverAccompany) form.caregiverAccompany
            , Maybe.map (ifTrue SavedMoney) form.savedMoney
            , Maybe.map (ifTrue Transportation) form.haveTransportation
            ]
                |> Maybe.Extra.combine
                |> Maybe.map (List.foldl EverySet.union EverySet.empty >> ifEverySetEmpty NoBirthPlan)
    in
    Maybe.map BirthPlanValue signs
        |> andMap (Maybe.map EverySet.fromList form.familyPlanning)


fromHealthEducationValue : Maybe (EverySet PrenatalHealthEducationSign) -> HealthEducationForm
fromHealthEducationValue saved =
    { expectations = Maybe.map (EverySet.member EducationExpectations) saved
    , visitsReview = Maybe.map (EverySet.member EducationVisitsReview) saved
    , warningSigns = Maybe.map (EverySet.member EducationWarningSigns) saved
    , hemorrhaging = Maybe.map (EverySet.member EducationHemorrhaging) saved
    , familyPlanning = Maybe.map (EverySet.member EducationFamilyPlanning) saved
    , breastfeeding = Maybe.map (EverySet.member EducationBreastfeeding) saved
    , immunization = Maybe.map (EverySet.member EducationImmunization) saved
    , hygiene = Maybe.map (EverySet.member EducationHygiene) saved
    }


healthEducationFormWithDefault : HealthEducationForm -> Maybe (EverySet PrenatalHealthEducationSign) -> HealthEducationForm
healthEducationFormWithDefault form saved =
    saved
        |> unwrap
            form
            (\signs ->
                { expectations = or form.expectations (EverySet.member EducationExpectations signs |> Just)
                , visitsReview = or form.visitsReview (EverySet.member EducationVisitsReview signs |> Just)
                , warningSigns = or form.warningSigns (EverySet.member EducationWarningSigns signs |> Just)
                , hemorrhaging = or form.hemorrhaging (EverySet.member EducationHemorrhaging signs |> Just)
                , familyPlanning = or form.familyPlanning (EverySet.member EducationFamilyPlanning signs |> Just)
                , breastfeeding = or form.breastfeeding (EverySet.member EducationBreastfeeding signs |> Just)
                , immunization = or form.immunization (EverySet.member EducationImmunization signs |> Just)
                , hygiene = or form.hygiene (EverySet.member EducationHygiene signs |> Just)
                }
            )


toHealthEducationValueWithDefault : Maybe (EverySet PrenatalHealthEducationSign) -> HealthEducationForm -> Maybe (EverySet PrenatalHealthEducationSign)
toHealthEducationValueWithDefault saved form =
    healthEducationFormWithDefault form saved
        |> toHealthEducationValue


toHealthEducationValue : HealthEducationForm -> Maybe (EverySet PrenatalHealthEducationSign)
toHealthEducationValue form =
    [ ifNullableTrue EducationExpectations form.expectations
    , ifNullableTrue EducationVisitsReview form.visitsReview
    , ifNullableTrue EducationWarningSigns form.warningSigns
    , ifNullableTrue EducationHemorrhaging form.hemorrhaging
    , ifNullableTrue EducationFamilyPlanning form.familyPlanning
    , ifNullableTrue EducationBreastfeeding form.breastfeeding
    , ifNullableTrue EducationImmunization form.immunization
    , ifNullableTrue EducationHygiene form.hygiene
    ]
        |> Maybe.Extra.combine
        |> Maybe.map (List.foldl EverySet.union EverySet.empty >> ifEverySetEmpty NoPrenatalHealthEducationSigns)


fromFollowUpValue : Maybe PrenatalFollowUpValue -> FollowUpForm
fromFollowUpValue saved =
    { option = Maybe.andThen (.options >> EverySet.toList >> List.head) saved
    , assesment = Maybe.map .assesment saved
    }


followUpFormWithDefault : FollowUpForm -> Maybe PrenatalFollowUpValue -> FollowUpForm
followUpFormWithDefault form saved =
    saved
        |> unwrap
            form
            (\value ->
                { option = or form.option (EverySet.toList value.options |> List.head)
                , assesment = or form.assesment (Just value.assesment)
                }
            )


toFollowUpValueWithDefault : Maybe PrenatalFollowUpValue -> FollowUpForm -> Maybe PrenatalFollowUpValue
toFollowUpValueWithDefault saved form =
    followUpFormWithDefault form saved
        |> toFollowUpValue


toFollowUpValue : FollowUpForm -> Maybe PrenatalFollowUpValue
toFollowUpValue form =
    let
        options =
            form.option
                |> Maybe.map (List.singleton >> EverySet.fromList)
    in
    Maybe.map PrenatalFollowUpValue options
        |> andMap form.assesment


fromAppointmentConfirmationValue : Maybe PrenatalAppointmentConfirmationValue -> AppointmentConfirmationForm
fromAppointmentConfirmationValue saved =
    { appointmentDate = Maybe.map .date saved
    , isDateSelectorOpen = False
    }


appointmentConfirmationFormWithDefault : AppointmentConfirmationForm -> Maybe PrenatalAppointmentConfirmationValue -> AppointmentConfirmationForm
appointmentConfirmationFormWithDefault form saved =
    saved
        |> unwrap
            form
            (\value ->
                { appointmentDate = or form.appointmentDate (Just value.date)
                , isDateSelectorOpen = form.isDateSelectorOpen
                }
            )


toAppointmentConfirmationValueWithDefault : Maybe PrenatalAppointmentConfirmationValue -> AppointmentConfirmationForm -> Maybe PrenatalAppointmentConfirmationValue
toAppointmentConfirmationValueWithDefault saved form =
    let
        form_ =
            appointmentConfirmationFormWithDefault form saved
    in
    toAppointmentConfirmationValue form_


toAppointmentConfirmationValue : AppointmentConfirmationForm -> Maybe PrenatalAppointmentConfirmationValue
toAppointmentConfirmationValue form =
    Maybe.map PrenatalAppointmentConfirmationValue form.appointmentDate


prenatalRDTFormWithDefault : PrenatalLabsRDTForm -> Maybe PrenatalLabsRDTValue -> PrenatalLabsRDTForm
prenatalRDTFormWithDefault form saved =
    saved
        |> unwrap
            form
            (\value ->
                let
                    testPerformedFromValue =
                        List.member value.executionNote [ TestNoteRunToday, TestNoteRunPreviously ]

                    testPerformedTodayFromValue =
                        value.executionNote == TestNoteRunToday
                in
                { testPerformed = or form.testPerformed (Just testPerformedFromValue)
                , testPerformedToday = valueConsideringIsDirtyField form.testPerformedTodayDirty form.testPerformedToday testPerformedTodayFromValue
                , testPerformedTodayDirty = form.testPerformedTodayDirty
                , executionNote = valueConsideringIsDirtyField form.executionNoteDirty form.executionNote value.executionNote
                , executionNoteDirty = form.executionNoteDirty
                , executionDate = or form.executionDate value.executionDate
                , testResult = or form.testResult value.testResult
                , isDateSelectorOpen = form.isDateSelectorOpen
                }
            )


toPrenatalRDTValueWithDefault : Maybe PrenatalLabsRDTValue -> PrenatalLabsRDTForm -> Maybe PrenatalLabsRDTValue
toPrenatalRDTValueWithDefault saved form =
    prenatalRDTFormWithDefault form saved
        |> toPrenatalRDTValue


toPrenatalRDTValue : PrenatalLabsRDTForm -> Maybe PrenatalLabsRDTValue
toPrenatalRDTValue form =
    Maybe.map
        (\executionNote ->
            { executionNote = executionNote
            , executionDate = form.executionDate
            , testResult = form.testResult
            }
        )
        form.executionNote


prenatalUrineDipstickFormWithDefault : PrenatalUrineDipstickForm -> Maybe PrenatalUrineDipstickTestValue -> PrenatalUrineDipstickForm
prenatalUrineDipstickFormWithDefault form saved =
    saved
        |> unwrap
            form
            (\value ->
                let
                    testPerformedFromValue =
                        List.member value.executionNote [ TestNoteRunToday, TestNoteRunPreviously ]

                    testPerformedTodayFromValue =
                        value.executionNote == TestNoteRunToday
                in
                { testPerformed = or form.testPerformed (Just testPerformedFromValue)
                , testPerformedToday = valueConsideringIsDirtyField form.testPerformedTodayDirty form.testPerformedToday testPerformedTodayFromValue
                , testPerformedTodayDirty = form.testPerformedTodayDirty
                , testVariant = or form.testVariant value.testVariant
                , executionNote = valueConsideringIsDirtyField form.executionNoteDirty form.executionNote value.executionNote
                , executionNoteDirty = form.executionNoteDirty
                , executionDate = or form.executionDate value.executionDate
                , isDateSelectorOpen = form.isDateSelectorOpen
                }
            )


toPrenatalUrineDipstickTestValueWithDefault : Maybe PrenatalUrineDipstickTestValue -> PrenatalUrineDipstickForm -> Maybe PrenatalUrineDipstickTestValue
toPrenatalUrineDipstickTestValueWithDefault saved form =
    prenatalUrineDipstickFormWithDefault form saved
        |> toPrenatalUrineDipstickTestValue


toPrenatalUrineDipstickTestValue : PrenatalUrineDipstickForm -> Maybe PrenatalUrineDipstickTestValue
toPrenatalUrineDipstickTestValue form =
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
            , specificGravity = Nothing
            , ketone = Nothing
            , bilirubin = Nothing
            }
        )
        form.executionNote


prenatalNonRDTFormWithDefault :
    PrenatalLabsNonRDTForm
    -> Maybe { value | executionNote : PrenatalTestExecutionNote, executionDate : Maybe NominalDate }
    -> PrenatalLabsNonRDTForm
prenatalNonRDTFormWithDefault form saved =
    saved
        |> unwrap
            form
            (\value ->
                let
                    testPerformedFromValue =
                        List.member value.executionNote [ TestNoteRunToday, TestNoteRunPreviously ]

                    testPerformedTodayFromValue =
                        value.executionNote == TestNoteRunToday
                in
                { testPerformed = or form.testPerformed (Just testPerformedFromValue)
                , testPerformedToday = valueConsideringIsDirtyField form.testPerformedTodayDirty form.testPerformedToday testPerformedTodayFromValue
                , testPerformedTodayDirty = form.testPerformedTodayDirty
                , executionNote = valueConsideringIsDirtyField form.executionNoteDirty form.executionNote value.executionNote
                , executionNoteDirty = form.executionNoteDirty
                , executionDate = or form.executionDate value.executionDate
                , isDateSelectorOpen = form.isDateSelectorOpen
                }
            )


toPrenatalNonRDTValueWithDefault :
    Maybe { value | executionNote : PrenatalTestExecutionNote, executionDate : Maybe NominalDate }
    -> (PrenatalTestExecutionNote -> Maybe NominalDate -> { value | executionNote : PrenatalTestExecutionNote, executionDate : Maybe NominalDate })
    -> PrenatalLabsNonRDTForm
    -> Maybe { value | executionNote : PrenatalTestExecutionNote, executionDate : Maybe NominalDate }
toPrenatalNonRDTValueWithDefault saved withEmptyResultsFunc form =
    let
        formWithDefault =
            prenatalNonRDTFormWithDefault form saved
    in
    Maybe.map (\executionNote -> withEmptyResultsFunc executionNote formWithDefault.executionDate)
        formWithDefault.executionNote


toHepatitisBTestValueWithEmptyResults : PrenatalTestExecutionNote -> Maybe NominalDate -> PrenatalHepatitisBTestValue
toHepatitisBTestValueWithEmptyResults note date =
    PrenatalHepatitisBTestValue note date Nothing


toSyphilisTestValueWithEmptyResults : PrenatalTestExecutionNote -> Maybe NominalDate -> PrenatalSyphilisTestValue
toSyphilisTestValueWithEmptyResults note date =
    PrenatalSyphilisTestValue note date Nothing


toHemoglobinTestValueWithEmptyResults : PrenatalTestExecutionNote -> Maybe NominalDate -> PrenatalHemoglobinTestValue
toHemoglobinTestValueWithEmptyResults note date =
    PrenatalHemoglobinTestValue note date Nothing


toRandomBloodSugarTestValueWithEmptyResults : PrenatalTestExecutionNote -> Maybe NominalDate -> PrenatalRandomBloodSugarTestValue
toRandomBloodSugarTestValueWithEmptyResults note date =
    PrenatalRandomBloodSugarTestValue note date Nothing


toBloodGpRsTestValueWithEmptyResults : PrenatalTestExecutionNote -> Maybe NominalDate -> PrenatalBloodGpRsTestValue
toBloodGpRsTestValueWithEmptyResults note date =
    PrenatalBloodGpRsTestValue note date Nothing Nothing


laboratoryTasks : List LaboratoryTask
laboratoryTasks =
    [ TaskHIVTest
    , TaskSyphilisTest
    , TaskHepatitisBTest
    , TaskMalariaTest
    , TaskBloodGpRsTest
    , TaskUrineDipstickTest
    , TaskHemoglobinTest
    , TaskRandomBloodSugarTest
    ]


laboratoryTaskCompleted : NominalDate -> AssembledData -> LaboratoryTask -> Bool
laboratoryTaskCompleted currentDate assembled task =
    let
        measurements =
            assembled.measurements

        taskExpected =
            expectLaboratoryTask currentDate assembled
    in
    case task of
        TaskHIVTest ->
            (not <| taskExpected TaskHIVTest) || isJust measurements.hivTest

        TaskSyphilisTest ->
            (not <| taskExpected TaskSyphilisTest) || isJust measurements.syphilisTest

        TaskHepatitisBTest ->
            (not <| taskExpected TaskHepatitisBTest) || isJust measurements.hepatitisBTest

        TaskMalariaTest ->
            (not <| taskExpected TaskMalariaTest) || isJust measurements.malariaTest

        TaskBloodGpRsTest ->
            (not <| taskExpected TaskBloodGpRsTest) || isJust measurements.bloodGpRsTest

        TaskUrineDipstickTest ->
            (not <| taskExpected TaskUrineDipstickTest) || isJust measurements.urineDipstickTest

        TaskHemoglobinTest ->
            (not <| taskExpected TaskHemoglobinTest) || isJust measurements.hemoglobinTest

        TaskRandomBloodSugarTest ->
            (not <| taskExpected TaskRandomBloodSugarTest) || isJust measurements.randomBloodSugarTest


expectLaboratoryTask : NominalDate -> AssembledData -> LaboratoryTask -> Bool
expectLaboratoryTask currentDate assembled task =
    if assembled.encounter.encounterType /= NurseEncounter then
        False

    else
        let
            testsDates =
                generatePreviousLaboratoryTestsDatesDict currentDate assembled

            isInitialTest test =
                Dict.get test testsDates
                    |> Maybe.map List.isEmpty
                    |> Maybe.withDefault True

            isRepeatingTestOnWeek week test =
                Maybe.map
                    (\lmpDate ->
                        if diffWeeks lmpDate currentDate < week then
                            isInitialTest test

                        else
                            let
                                lastTestWeek =
                                    Dict.get test testsDates
                                        |> Maybe.map (List.map (\testsDate -> diffWeeks testsDate currentDate))
                                        |> Maybe.withDefault []
                                        |> List.sort
                                        |> List.reverse
                                        |> List.head
                            in
                            Maybe.map (\testWeek -> testWeek < week) lastTestWeek
                                |> Maybe.withDefault True
                    )
                    assembled.globalLmpDate
                    |> Maybe.withDefault (isInitialTest test)
        in
        case task of
            TaskHIVTest ->
                isRepeatingTestOnWeek 38 TaskHIVTest

            TaskSyphilisTest ->
                isRepeatingTestOnWeek 38 TaskSyphilisTest

            TaskHepatitisBTest ->
                isRepeatingTestOnWeek 34 TaskHepatitisBTest

            TaskMalariaTest ->
                True

            TaskBloodGpRsTest ->
                isInitialTest TaskBloodGpRsTest

            TaskUrineDipstickTest ->
                True

            TaskHemoglobinTest ->
                True

            TaskRandomBloodSugarTest ->
                -- First ?? @todo
                isInitialTest TaskRandomBloodSugarTest


generatePreviousLaboratoryTestsDatesDict : NominalDate -> AssembledData -> Dict LaboratoryTask (List NominalDate)
generatePreviousLaboratoryTestsDatesDict currentDate assembled =
    let
        generateTestDates getFunc =
            List.filterMap
                (Tuple.second
                    >> getFunc
                    >> Maybe.map (Tuple.second >> .dateMeasured)
                )
                assembled.nursePreviousMeasurementsWithDates
    in
    [ ( TaskHIVTest, generateTestDates .hivTest )
    , ( TaskSyphilisTest, generateTestDates .syphilisTest )
    , ( TaskHepatitisBTest, generateTestDates .hepatitisBTest )
    , ( TaskMalariaTest, generateTestDates .malariaTest )
    , ( TaskBloodGpRsTest, generateTestDates .bloodGpRsTest )
    , ( TaskUrineDipstickTest, generateTestDates .urineDipstickTest )
    , ( TaskHemoglobinTest, generateTestDates .hemoglobinTest )
    , ( TaskRandomBloodSugarTest, generateTestDates .randomBloodSugarTest )
    ]
        |> Dict.fromList


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
