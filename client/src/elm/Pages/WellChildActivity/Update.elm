module Pages.WellChildActivity.Update exposing (update)

import App.Model
import App.Ports exposing (bindDropZone)
import AssocList as Dict
import Backend.Entities exposing (..)
import Backend.IndividualEncounterParticipant.Model
import Backend.Measurement.Model exposing (..)
import Backend.Measurement.Utils exposing (getMeasurementValueFunc)
import Backend.Model exposing (ModelIndexedDb)
import Backend.WellChildEncounter.Model exposing (EncounterWarning(..))
import Date
import EverySet exposing (EverySet)
import Gizra.NominalDate exposing (NominalDate)
import Gizra.Update exposing (sequenceExtra)
import Maybe.Extra exposing (isJust, isNothing, unwrap)
import Measurement.Model
    exposing
        ( HeightForm
        , MuacForm
        , NutritionForm
        , PhotoForm
        , WeightForm
        , emptyHeightForm
        , emptyMuacForm
        , emptyNutritionForm
        , emptyPhotoForm
        , emptyWeightForm
        )
import Measurement.Utils exposing (..)
import Pages.Page exposing (Page(..), UserPage(..))
import Pages.Utils exposing (setMultiSelectInputValue)
import Pages.WellChildActivity.Model exposing (..)
import Pages.WellChildActivity.Utils exposing (..)
import RemoteData exposing (RemoteData(..))
import Result exposing (Result)


update : NominalDate -> Bool -> WellChildEncounterId -> ModelIndexedDb -> Msg -> Model -> ( Model, Cmd Msg, List App.Model.Msg )
update currentDate isChw id db msg model =
    let
        resolveFormWithDefaults getMeasurementFunc formWithDefaultsFunc form =
            Dict.get id db.wellChildMeasurements
                |> Maybe.withDefault NotAsked
                |> RemoteData.toMaybe
                |> Maybe.map
                    (getMeasurementFunc
                        >> getMeasurementValueFunc
                        >> formWithDefaultsFunc form
                    )
                |> Maybe.withDefault form

        resolveVaccinationForm vaccineType form =
            Dict.get id db.wellChildMeasurements
                |> Maybe.withDefault NotAsked
                |> RemoteData.toMaybe
                |> Maybe.map
                    (getMeasurementByVaccineTypeFunc vaccineType
                        >> vaccinationFormWithDefault form
                    )
                |> Maybe.withDefault form

        generateNutritionAssessmentMsgs nextTask =
            Maybe.map (\task -> [ SetActiveNutritionAssessmentTask task ]) nextTask
                |> Maybe.withDefault [ SetActivePage <| UserPage <| WellChildEncounterPage id ]

        generateDangerSignsMsgs nextTask =
            Maybe.map (\task -> [ SetActiveDangerSignsTask task ]) nextTask
                |> Maybe.withDefault [ SetActivePage <| UserPage <| WellChildEncounterPage id ]

        generateMedicationMsgs nextTask =
            Maybe.map (\task -> [ SetActiveMedicationTask task ]) nextTask
                |> Maybe.withDefault [ SetActivePage <| UserPage <| WellChildEncounterPage id ]

        generateNextStepsMsgs nextTask =
            Maybe.map (\task -> [ SetActiveNextStepsTask task ]) nextTask
                |> Maybe.withDefault [ SetActivePage <| UserPage <| WellChildProgressReportPage id ]

        generateImmunisationMsgs nextTask =
            let
                defaultMsg =
                    if isChw then
                        SetActivePage <| UserPage <| WellChildEncounterPage id

                    else
                        SetActiveImmunisationTask TaskOverview
            in
            Maybe.map (\task -> [ SetActiveImmunisationTask task ]) nextTask
                |> Maybe.withDefault [ defaultMsg ]

        focusOnCalendar =
            App.Model.ScrollToElement "dropdown--content-container"
    in
    case msg of
        SetActivePage page ->
            ( model
            , Cmd.none
            , [ App.Model.SetActivePage page ]
            )

        SetWarningPopupState state ->
            ( { model | warningPopupState = state }, Cmd.none, [] )

        NoOp ->
            ( model
            , Cmd.none
            , []
            )

        SetExpectedDateConcluded value ->
            let
                updatedForm =
                    model.pregnancySummaryForm
                        |> (\form -> { form | expectedDateConcluded = Just value })
            in
            ( { model | pregnancySummaryForm = updatedForm }
            , Cmd.none
            , []
            )

        ToggleExpectedDateConcluded ->
            let
                updatedForm =
                    model.pregnancySummaryForm
                        |> (\form -> { form | isExpectedDateConcludedSelectorOpen = not form.isExpectedDateConcludedSelectorOpen })
            in
            ( { model | pregnancySummaryForm = updatedForm }
            , Cmd.none
            , [ focusOnCalendar ]
            )

        SetDeliveryComplicationsPresent value ->
            let
                updatedForm =
                    model.pregnancySummaryForm
                        |> (\form -> { form | deliveryComplicationsPresent = Just value, deliveryComplications = Nothing })
            in
            ( { model | pregnancySummaryForm = updatedForm }
            , Cmd.none
            , []
            )

        SetDeliveryComplication complication ->
            let
                form =
                    resolveFormWithDefaults .pregnancySummary pregnancySummaryFormWithDefault model.pregnancySummaryForm

                updatedForm =
                    setMultiSelectInputValue .deliveryComplications
                        (\complications -> { form | deliveryComplications = complications })
                        NoDeliveryComplications
                        complication
                        form
            in
            ( { model | pregnancySummaryForm = updatedForm }
            , Cmd.none
            , []
            )

        SavePregnancySummary personId saved ->
            let
                measurementId =
                    Maybe.map Tuple.first saved

                measurement =
                    getMeasurementValueFunc saved

                appMsgs =
                    model.pregnancySummaryForm
                        |> toPregnancySummaryValueWithDefault measurement
                        |> Maybe.map
                            (\value ->
                                [ Backend.WellChildEncounter.Model.SavePregnancySummary personId measurementId value
                                    |> Backend.Model.MsgWellChildEncounter id
                                    |> App.Model.MsgIndexedDb
                                , App.Model.SetActivePage <| UserPage <| WellChildEncounterPage id
                                ]
                            )
                        |> Maybe.withDefault []
            in
            ( model
            , Cmd.none
            , appMsgs
            )

        SetActiveDangerSignsTask task ->
            let
                updatedData =
                    model.dangerSignsData
                        |> (\data -> { data | activeTask = Just task })
            in
            ( { model | dangerSignsData = updatedData }
            , Cmd.none
            , []
            )

        SetSymptom symptom ->
            let
                form =
                    resolveFormWithDefaults .symptomsReview symptomsReviewFormWithDefault model.dangerSignsData.symptomsReviewForm

                updatedForm =
                    setMultiSelectInputValue .symptoms
                        (\symptoms -> { form | symptoms = symptoms })
                        NoWellChildSymptoms
                        symptom
                        form

                updatedData =
                    model.dangerSignsData
                        |> (\data -> { data | symptomsReviewForm = updatedForm })
            in
            ( { model | dangerSignsData = updatedData }
            , Cmd.none
            , []
            )

        SaveSymptomsReview personId saved nextTask_ ->
            let
                measurementId =
                    Maybe.map Tuple.first saved

                measurement =
                    getMeasurementValueFunc saved

                extraMsgs =
                    generateDangerSignsMsgs nextTask_

                appMsgs =
                    model.dangerSignsData.symptomsReviewForm
                        |> toSymptomsReviewValueWithDefault measurement
                        |> Maybe.map
                            (Backend.WellChildEncounter.Model.SaveSymptomsReview personId measurementId
                                >> Backend.Model.MsgWellChildEncounter id
                                >> App.Model.MsgIndexedDb
                                >> List.singleton
                            )
                        |> Maybe.withDefault []
            in
            ( model
            , Cmd.none
            , appMsgs
            )
                |> sequenceExtra (update currentDate isChw id db) extraMsgs

        SetVitalsResporatoryRate value ->
            let
                updatedForm =
                    model.dangerSignsData.vitalsForm
                        |> (\form ->
                                { form | respiratoryRate = String.toInt value, respiratoryRateDirty = True }
                           )

                updatedData =
                    model.dangerSignsData
                        |> (\data -> { data | vitalsForm = updatedForm })
            in
            ( { model | dangerSignsData = updatedData }
            , Cmd.none
            , []
            )

        SetVitalsBodyTemperature value ->
            let
                updatedForm =
                    model.dangerSignsData.vitalsForm
                        |> (\form ->
                                { form | bodyTemperature = String.toFloat value, bodyTemperatureDirty = True }
                           )

                updatedData =
                    model.dangerSignsData
                        |> (\data -> { data | vitalsForm = updatedForm })
            in
            ( { model | dangerSignsData = updatedData }
            , Cmd.none
            , []
            )

        SaveVitals personId saved nextTask_ ->
            let
                measurementId =
                    Maybe.map Tuple.first saved

                measurement =
                    getMeasurementValueFunc saved

                extraMsgs =
                    generateDangerSignsMsgs nextTask_

                appMsgs =
                    model.dangerSignsData.vitalsForm
                        |> toBasicVitalsValueWithDefault measurement
                        |> Maybe.map
                            (Backend.WellChildEncounter.Model.SaveVitals personId measurementId
                                >> Backend.Model.MsgWellChildEncounter id
                                >> App.Model.MsgIndexedDb
                                >> List.singleton
                            )
                        |> Maybe.withDefault []
            in
            ( model
            , Cmd.none
            , appMsgs
            )
                |> sequenceExtra (update currentDate isChw id db) extraMsgs

        SetActiveNutritionAssessmentTask task ->
            let
                updatedData =
                    model.nutritionAssessmentData
                        |> (\data -> { data | activeTask = Just task })
            in
            ( { model | nutritionAssessmentData = updatedData }
            , Cmd.none
            , []
            )

        SetHeight string ->
            let
                updatedForm =
                    model.nutritionAssessmentData.heightForm
                        |> (\form ->
                                { form | height = String.toFloat string, heightDirty = True }
                           )

                updatedData =
                    model.nutritionAssessmentData
                        |> (\data -> { data | heightForm = updatedForm })
            in
            ( { model | nutritionAssessmentData = updatedData }
            , Cmd.none
            , []
            )

        SaveHeight personId saved nextTask_ ->
            let
                measurementId =
                    Maybe.map Tuple.first saved

                measurement =
                    getMeasurementValueFunc saved

                extraMsgs =
                    generateNutritionAssessmentMsgs nextTask_

                appMsgs =
                    model.nutritionAssessmentData.heightForm
                        |> toHeightValueWithDefault measurement
                        |> Maybe.map
                            (Backend.WellChildEncounter.Model.SaveHeight personId measurementId
                                >> Backend.Model.MsgWellChildEncounter id
                                >> App.Model.MsgIndexedDb
                                >> List.singleton
                            )
                        |> Maybe.withDefault []
            in
            ( model
            , Cmd.none
            , appMsgs
            )
                |> sequenceExtra (update currentDate isChw id db) extraMsgs

        SetHeadCircumference string ->
            let
                updatedForm =
                    model.nutritionAssessmentData.headCircumferenceForm
                        |> (\form ->
                                { form | headCircumference = String.toFloat string, headCircumferenceDirty = True, measurementNotTaken = Just False }
                           )

                updatedData =
                    model.nutritionAssessmentData
                        |> (\data -> { data | headCircumferenceForm = updatedForm })
            in
            ( { model | nutritionAssessmentData = updatedData }
            , Cmd.none
            , []
            )

        ToggleHeadCircumferenceNotTaken ->
            let
                form =
                    model.nutritionAssessmentData.headCircumferenceForm

                notTaken =
                    Maybe.map not form.measurementNotTaken
                        |> Maybe.withDefault True

                headCircumference =
                    if notTaken then
                        Just 0

                    else
                        Nothing

                updatedForm =
                    { form | headCircumference = headCircumference, headCircumferenceDirty = True, measurementNotTaken = Just notTaken }

                updatedData =
                    model.nutritionAssessmentData
                        |> (\data -> { data | headCircumferenceForm = updatedForm })
            in
            ( { model | nutritionAssessmentData = updatedData }
            , Cmd.none
            , [ focusOnCalendar ]
            )

        CloseHeadCircumferencePopup personId saved nextTask_ ->
            let
                extraMsgs =
                    [ SetWarningPopupState Nothing
                    , SaveHeadCircumference personId saved nextTask_
                    ]
            in
            ( model
            , Cmd.none
            , []
            )
                |> sequenceExtra (update currentDate isChw id db) extraMsgs

        PreSaveHeadCircumference personId maybeZscore saved nextTask ->
            let
                warning =
                    Maybe.map
                        (\zscore ->
                            if zscore > 3 then
                                WarningHeadCircumferenceMacrocephaly

                            else if zscore < -3 then
                                WarningHeadCircumferenceMicrocephaly

                            else
                                NoHeadCircumferenceWarning
                        )
                        maybeZscore
                        |> Maybe.withDefault NoHeadCircumferenceWarning

                setEncounterWarningMsg =
                    [ Backend.WellChildEncounter.Model.SetWellChildEncounterWarning warning
                        |> Backend.Model.MsgWellChildEncounter id
                        |> App.Model.MsgIndexedDb
                    ]

                extraMsgs =
                    -- If there's a warning, we show warning popup.
                    -- Head Circumference will be saved once popup is closed.
                    -- If there's no warning, we execute Save here.
                    case warning of
                        WarningHeadCircumferenceMacrocephaly ->
                            [ PopupMacrocephaly personId saved nextTask
                                |> Just
                                |> SetWarningPopupState
                            ]

                        WarningHeadCircumferenceMicrocephaly ->
                            [ PopupMicrocephaly personId saved nextTask
                                |> Just
                                |> SetWarningPopupState
                            ]

                        _ ->
                            [ SaveHeadCircumference personId saved nextTask ]
            in
            ( model
            , Cmd.none
            , setEncounterWarningMsg
            )
                |> sequenceExtra (update currentDate isChw id db) extraMsgs

        SaveHeadCircumference personId saved nextTask_ ->
            let
                measurementId =
                    Maybe.map Tuple.first saved

                measurement =
                    getMeasurementValueFunc saved

                extraMsgs =
                    generateNutritionAssessmentMsgs nextTask_

                appMsgs =
                    model.nutritionAssessmentData.headCircumferenceForm
                        |> toHeadCircumferenceValueWithDefault measurement
                        |> Maybe.map
                            (Backend.WellChildEncounter.Model.SaveHeadCircumference personId measurementId
                                >> Backend.Model.MsgWellChildEncounter id
                                >> App.Model.MsgIndexedDb
                                >> List.singleton
                            )
                        |> Maybe.withDefault []
            in
            ( model
            , Cmd.none
            , appMsgs
            )
                |> sequenceExtra (update currentDate isChw id db) extraMsgs

        SetMuac string ->
            let
                updatedForm =
                    model.nutritionAssessmentData.muacForm
                        |> (\form ->
                                { form | muac = String.toFloat string, muacDirty = True }
                           )

                updatedData =
                    model.nutritionAssessmentData
                        |> (\data -> { data | muacForm = updatedForm })
            in
            ( { model | nutritionAssessmentData = updatedData }
            , Cmd.none
            , []
            )

        SaveMuac personId saved nextTask_ ->
            let
                measurementId =
                    Maybe.map Tuple.first saved

                measurement =
                    getMeasurementValueFunc saved

                extraMsgs =
                    generateNutritionAssessmentMsgs nextTask_

                appMsgs =
                    model.nutritionAssessmentData.muacForm
                        |> toMuacValueWithDefault measurement
                        |> Maybe.map
                            (Backend.WellChildEncounter.Model.SaveMuac personId measurementId
                                >> Backend.Model.MsgWellChildEncounter id
                                >> App.Model.MsgIndexedDb
                                >> List.singleton
                            )
                        |> Maybe.withDefault []
            in
            ( model
            , Cmd.none
            , appMsgs
            )
                |> sequenceExtra (update currentDate isChw id db) extraMsgs

        SetNutritionSign sign ->
            let
                form =
                    Dict.get id db.wellChildMeasurements
                        |> Maybe.withDefault NotAsked
                        |> RemoteData.toMaybe
                        |> Maybe.map
                            (.nutrition
                                >> getMeasurementValueFunc
                                >> nutritionFormWithDefault model.nutritionAssessmentData.nutritionForm
                            )
                        |> Maybe.withDefault model.nutritionAssessmentData.nutritionForm

                updatedForm =
                    setMultiSelectInputValue .signs
                        (\signs -> { form | signs = signs })
                        NormalChildNutrition
                        sign
                        form

                updatedData =
                    model.nutritionAssessmentData
                        |> (\data -> { data | nutritionForm = updatedForm })
            in
            ( { model | nutritionAssessmentData = updatedData }
            , Cmd.none
            , []
            )

        SaveNutrition personId saved assessment nextTask_ ->
            let
                measurementId =
                    Maybe.map Tuple.first saved

                measurement =
                    getMeasurementValueFunc saved

                extraMsgs =
                    generateNutritionAssessmentMsgs nextTask_

                appMsgs =
                    model.nutritionAssessmentData.nutritionForm
                        |> (\form -> { form | assesment = Just assessment })
                        |> toNutritionValueWithDefault measurement
                        |> Maybe.map
                            (Backend.WellChildEncounter.Model.SaveNutrition personId measurementId
                                >> Backend.Model.MsgWellChildEncounter id
                                >> App.Model.MsgIndexedDb
                                >> List.singleton
                            )
                        |> Maybe.withDefault []
            in
            ( model
            , Cmd.none
            , appMsgs
            )
                |> sequenceExtra (update currentDate isChw id db) extraMsgs

        SetWeight string ->
            let
                updatedForm =
                    model.nutritionAssessmentData.weightForm
                        |> (\form ->
                                { form | weight = String.toFloat string, weightDirty = True }
                           )

                updatedData =
                    model.nutritionAssessmentData
                        |> (\data -> { data | weightForm = updatedForm })
            in
            ( { model | nutritionAssessmentData = updatedData }
            , Cmd.none
            , []
            )

        SaveWeight personId saved nextTask_ ->
            let
                measurementId =
                    Maybe.map Tuple.first saved

                measurement =
                    getMeasurementValueFunc saved

                extraMsgs =
                    generateNutritionAssessmentMsgs nextTask_

                appMsgs =
                    model.nutritionAssessmentData.weightForm
                        |> toWeightValueWithDefault measurement
                        |> Maybe.map
                            (Backend.WellChildEncounter.Model.SaveWeight personId measurementId
                                >> Backend.Model.MsgWellChildEncounter id
                                >> App.Model.MsgIndexedDb
                                >> List.singleton
                            )
                        |> Maybe.withDefault []
            in
            ( model
            , Cmd.none
            , appMsgs
            )
                |> sequenceExtra (update currentDate isChw id db) extraMsgs

        SetActiveImmunisationTask task ->
            let
                updatedData =
                    model.immunisationData
                        |> (\data -> { data | activeTask = Just task })
            in
            ( { model | immunisationData = updatedData }
            , Cmd.none
            , []
            )

        SetVaccinationFormViewMode vaccineType mode ->
            let
                form =
                    getFormByVaccineTypeFunc vaccineType model.immunisationData

                updatedForm =
                    { form | viewMode = mode }
            in
            ( { model | immunisationData = updateVaccinationFormByVaccineType vaccineType updatedForm model.immunisationData }
            , Cmd.none
            , []
            )

        SetUpdatePreviousVaccines vaccineType dose value ->
            let
                form =
                    getFormByVaccineTypeFunc vaccineType model.immunisationData
                        |> resolveVaccinationForm vaccineType

                updatedForm =
                    if value == True then
                        { form
                            | viewMode = ViewModeVaccinationUpdate dose
                            , updatePreviousVaccines = Nothing
                            , willReceiveVaccineToday = Nothing
                            , administrationNote = Nothing
                            , administrationNoteDirty = True
                        }

                    else
                        { form | updatePreviousVaccines = Just False }
            in
            ( { model | immunisationData = updateVaccinationFormByVaccineType vaccineType updatedForm model.immunisationData }
            , Cmd.none
            , []
            )

        SetWillReceiveVaccineToday vaccineType dose willReceive ->
            let
                form =
                    getFormByVaccineTypeFunc vaccineType model.immunisationData
                        |> resolveVaccinationForm vaccineType

                updatedForm =
                    if willReceive then
                        { form
                            | willReceiveVaccineToday = Just True
                            , administeredDoses = insertIntoSet dose form.administeredDoses
                            , administeredDosesDirty = True
                            , administrationDates = insertIntoSet currentDate form.administrationDates
                            , administrationNote = Just AdministeredToday
                            , administrationNoteDirty = True
                        }

                    else
                        let
                            administeredDoses =
                                Maybe.map EverySet.toList form.administeredDoses
                                    |> Maybe.withDefault []

                            ( updatedDoses, updatedDates ) =
                                if List.member dose administeredDoses then
                                    ( Maybe.map
                                        (EverySet.toList
                                            >> List.sortBy vaccineDoseToComparable
                                            >> List.reverse
                                            >> List.drop 1
                                            >> EverySet.fromList
                                        )
                                        form.administeredDoses
                                    , Maybe.map
                                        (EverySet.toList
                                            >> List.sortWith Date.compare
                                            >> List.reverse
                                            >> List.drop 1
                                            >> EverySet.fromList
                                        )
                                        form.administrationDates
                                    )

                                else
                                    ( form.administeredDoses, form.administrationDates )
                        in
                        { form
                            | willReceiveVaccineToday = Just False
                            , administeredDoses = updatedDoses
                            , administeredDosesDirty = True
                            , administrationDates = updatedDates
                            , administrationNote = Nothing
                            , administrationNoteDirty = True
                        }
            in
            ( { model | immunisationData = updateVaccinationFormByVaccineType vaccineType updatedForm model.immunisationData }
            , Cmd.none
            , []
            )

        SetAdministrationNote vaccineType note ->
            let
                form =
                    getFormByVaccineTypeFunc vaccineType model.immunisationData
                        |> resolveVaccinationForm vaccineType

                updatedForm =
                    { form | administrationNote = Just note, administrationNoteDirty = True }
            in
            ( { model | immunisationData = updateVaccinationFormByVaccineType vaccineType updatedForm model.immunisationData }
            , Cmd.none
            , []
            )

        ToggleDateSelectorInput vaccineType ->
            let
                form =
                    getFormByVaccineTypeFunc vaccineType model.immunisationData

                updatedForm =
                    { form | dateSelectorOpen = not form.dateSelectorOpen }
            in
            ( { model | immunisationData = updateVaccinationFormByVaccineType vaccineType updatedForm model.immunisationData }
            , Cmd.none
            , []
            )

        SetVaccinationUpdateDate vaccineType date ->
            let
                form =
                    getFormByVaccineTypeFunc vaccineType model.immunisationData
                        |> resolveVaccinationForm vaccineType

                updatedForm =
                    { form | vaccinationUpdateDate = Just date }
            in
            ( { model | immunisationData = updateVaccinationFormByVaccineType vaccineType updatedForm model.immunisationData }
            , Cmd.none
            , []
            )

        SaveVaccinationUpdateDate vaccineType dose ->
            let
                form =
                    getFormByVaccineTypeFunc vaccineType model.immunisationData
                        |> resolveVaccinationForm vaccineType

                updatedModel =
                    Maybe.map
                        (\date ->
                            let
                                updatedForm =
                                    { form
                                        | administeredDoses = insertIntoSet dose form.administeredDoses
                                        , administeredDosesDirty = True
                                        , administrationDates = insertIntoSet date form.administrationDates
                                        , vaccinationUpdateDate = Nothing
                                        , viewMode = ViewModeInitial
                                    }
                            in
                            { model | immunisationData = updateVaccinationFormByVaccineType vaccineType updatedForm model.immunisationData }
                        )
                        form.vaccinationUpdateDate
                        |> Maybe.withDefault model
            in
            ( updatedModel
            , Cmd.none
            , []
            )

        DeleteVaccinationUpdateDate vaccineType doseToDelete dateToDelete ->
            let
                form =
                    getFormByVaccineTypeFunc vaccineType model.immunisationData
                        |> resolveVaccinationForm vaccineType

                updatedDoses =
                    Maybe.map
                        (EverySet.toList
                            >> List.filter ((/=) doseToDelete)
                            >> EverySet.fromList
                        )
                        form.administeredDoses

                updatedDates =
                    Maybe.map
                        (EverySet.toList
                            >> List.filter ((/=) dateToDelete)
                            >> EverySet.fromList
                        )
                        form.administrationDates

                updatedForm =
                    { form
                        | administeredDoses = updatedDoses
                        , administeredDosesDirty = True
                        , administrationDates = updatedDates
                    }
            in
            ( { model | immunisationData = updateVaccinationFormByVaccineType vaccineType updatedForm model.immunisationData }
            , Cmd.none
            , []
            )

        SaveBCGImmunisation personId saved nextTask_ ->
            let
                measurementId =
                    Maybe.map Tuple.first saved

                measurement =
                    getMeasurementValueFunc saved

                extraMsgs =
                    generateImmunisationMsgs nextTask_

                appMsgs =
                    model.immunisationData.bcgForm
                        |> toVaccinationValueWithDefault measurement
                        |> Maybe.map
                            (Backend.WellChildEncounter.Model.SaveBCGImmunisation personId measurementId
                                >> Backend.Model.MsgWellChildEncounter id
                                >> App.Model.MsgIndexedDb
                                >> List.singleton
                            )
                        |> Maybe.withDefault []
            in
            ( model
            , Cmd.none
            , appMsgs
            )
                |> sequenceExtra (update currentDate isChw id db) extraMsgs

        SaveDTPImmunisation personId saved nextTask_ ->
            let
                measurementId =
                    Maybe.map Tuple.first saved

                measurement =
                    getMeasurementValueFunc saved

                extraMsgs =
                    generateImmunisationMsgs nextTask_

                appMsgs =
                    model.immunisationData.dtpForm
                        |> toVaccinationValueWithDefault measurement
                        |> Maybe.map
                            (Backend.WellChildEncounter.Model.SaveDTPImmunisation personId measurementId
                                >> Backend.Model.MsgWellChildEncounter id
                                >> App.Model.MsgIndexedDb
                                >> List.singleton
                            )
                        |> Maybe.withDefault []
            in
            ( model
            , Cmd.none
            , appMsgs
            )
                |> sequenceExtra (update currentDate isChw id db) extraMsgs

        SaveHPVImmunisation personId saved nextTask_ ->
            let
                measurementId =
                    Maybe.map Tuple.first saved

                measurement =
                    getMeasurementValueFunc saved

                extraMsgs =
                    generateImmunisationMsgs nextTask_

                appMsgs =
                    model.immunisationData.hpvForm
                        |> toVaccinationValueWithDefault measurement
                        |> Maybe.map
                            (Backend.WellChildEncounter.Model.SaveHPVImmunisation personId measurementId
                                >> Backend.Model.MsgWellChildEncounter id
                                >> App.Model.MsgIndexedDb
                                >> List.singleton
                            )
                        |> Maybe.withDefault []
            in
            ( model
            , Cmd.none
            , appMsgs
            )
                |> sequenceExtra (update currentDate isChw id db) extraMsgs

        SaveIPVImmunisation personId saved nextTask_ ->
            let
                measurementId =
                    Maybe.map Tuple.first saved

                measurement =
                    getMeasurementValueFunc saved

                extraMsgs =
                    generateImmunisationMsgs nextTask_

                appMsgs =
                    model.immunisationData.ipvForm
                        |> toVaccinationValueWithDefault measurement
                        |> Maybe.map
                            (Backend.WellChildEncounter.Model.SaveIPVImmunisation personId measurementId
                                >> Backend.Model.MsgWellChildEncounter id
                                >> App.Model.MsgIndexedDb
                                >> List.singleton
                            )
                        |> Maybe.withDefault []
            in
            ( model
            , Cmd.none
            , appMsgs
            )
                |> sequenceExtra (update currentDate isChw id db) extraMsgs

        SaveMRImmunisation personId saved nextTask_ ->
            let
                measurementId =
                    Maybe.map Tuple.first saved

                measurement =
                    getMeasurementValueFunc saved

                extraMsgs =
                    generateImmunisationMsgs nextTask_

                appMsgs =
                    model.immunisationData.mrForm
                        |> toVaccinationValueWithDefault measurement
                        |> Maybe.map
                            (Backend.WellChildEncounter.Model.SaveMRImmunisation personId measurementId
                                >> Backend.Model.MsgWellChildEncounter id
                                >> App.Model.MsgIndexedDb
                                >> List.singleton
                            )
                        |> Maybe.withDefault []
            in
            ( model
            , Cmd.none
            , appMsgs
            )
                |> sequenceExtra (update currentDate isChw id db) extraMsgs

        SaveOPVImmunisation personId saved nextTask_ ->
            let
                measurementId =
                    Maybe.map Tuple.first saved

                measurement =
                    getMeasurementValueFunc saved

                extraMsgs =
                    generateImmunisationMsgs nextTask_

                appMsgs =
                    model.immunisationData.opvForm
                        |> toVaccinationValueWithDefault measurement
                        |> Maybe.map
                            (Backend.WellChildEncounter.Model.SaveOPVImmunisation personId measurementId
                                >> Backend.Model.MsgWellChildEncounter id
                                >> App.Model.MsgIndexedDb
                                >> List.singleton
                            )
                        |> Maybe.withDefault []
            in
            ( model
            , Cmd.none
            , appMsgs
            )
                |> sequenceExtra (update currentDate isChw id db) extraMsgs

        SavePCV13Immunisation personId saved nextTask_ ->
            let
                measurementId =
                    Maybe.map Tuple.first saved

                measurement =
                    getMeasurementValueFunc saved

                extraMsgs =
                    generateImmunisationMsgs nextTask_

                appMsgs =
                    model.immunisationData.pcv13Form
                        |> toVaccinationValueWithDefault measurement
                        |> Maybe.map
                            (Backend.WellChildEncounter.Model.SavePCV13Immunisation personId measurementId
                                >> Backend.Model.MsgWellChildEncounter id
                                >> App.Model.MsgIndexedDb
                                >> List.singleton
                            )
                        |> Maybe.withDefault []
            in
            ( model
            , Cmd.none
            , appMsgs
            )
                |> sequenceExtra (update currentDate isChw id db) extraMsgs

        SaveRotarixImmunisation personId saved nextTask_ ->
            let
                measurementId =
                    Maybe.map Tuple.first saved

                measurement =
                    getMeasurementValueFunc saved

                extraMsgs =
                    generateImmunisationMsgs nextTask_

                appMsgs =
                    model.immunisationData.rotarixForm
                        |> toVaccinationValueWithDefault measurement
                        |> Maybe.map
                            (Backend.WellChildEncounter.Model.SaveRotarixImmunisation personId measurementId
                                >> Backend.Model.MsgWellChildEncounter id
                                >> App.Model.MsgIndexedDb
                                >> List.singleton
                            )
                        |> Maybe.withDefault []
            in
            ( model
            , Cmd.none
            , appMsgs
            )
                |> sequenceExtra (update currentDate isChw id db) extraMsgs

        SetECDBoolInput formUpdateFunc value ->
            let
                updatedForm =
                    formUpdateFunc value model.ecdForm
            in
            ( { model | ecdForm = updatedForm }
            , Cmd.none
            , []
            )

        SaveECD personId saved ->
            let
                measurementId =
                    Maybe.map Tuple.first saved

                measurement =
                    getMeasurementValueFunc saved

                appMsgs =
                    model.ecdForm
                        |> toWellChildECDValueWithDefault measurement
                        |> unwrap
                            []
                            (\value ->
                                [ Backend.WellChildEncounter.Model.SaveECD personId measurementId value
                                    |> Backend.Model.MsgWellChildEncounter id
                                    |> App.Model.MsgIndexedDb
                                , App.Model.SetActivePage <| UserPage <| WellChildEncounterPage id
                                ]
                            )
            in
            ( model
            , Cmd.none
            , appMsgs
            )

        SetActiveMedicationTask task ->
            let
                updatedData =
                    model.medicationData
                        |> (\data -> { data | activeTask = Just task })
            in
            ( { model | medicationData = updatedData }
            , Cmd.none
            , []
            )

        SetAlbendazoleAdministered value ->
            let
                updatedForm =
                    model.medicationData.albendazoleForm
                        |> (\form -> { form | medicationAdministered = Just value, reasonForNonAdministration = Nothing })

                updatedData =
                    model.medicationData
                        |> (\data -> { data | albendazoleForm = updatedForm })
            in
            ( { model | medicationData = updatedData }
            , Cmd.none
            , []
            )

        SetAlbendazoleReasonForNonAdministration value ->
            let
                updatedForm =
                    model.medicationData.albendazoleForm
                        |> (\form -> { form | reasonForNonAdministration = Just value })

                updatedData =
                    model.medicationData
                        |> (\data -> { data | albendazoleForm = updatedForm })
            in
            ( { model | medicationData = updatedData }
            , Cmd.none
            , []
            )

        SaveAlbendazole personId saved nextTask_ ->
            let
                measurementId =
                    Maybe.map Tuple.first saved

                measurement =
                    getMeasurementValueFunc saved

                extraMsgs =
                    generateMedicationMsgs nextTask_

                appMsgs =
                    model.medicationData.albendazoleForm
                        |> toAdministrationNoteWithDefault measurement
                        |> Maybe.map
                            (Backend.WellChildEncounter.Model.SaveAlbendazole personId measurementId
                                >> Backend.Model.MsgWellChildEncounter id
                                >> App.Model.MsgIndexedDb
                                >> List.singleton
                            )
                        |> Maybe.withDefault []
            in
            ( model
            , Cmd.none
            , appMsgs
            )
                |> sequenceExtra (update currentDate isChw id db) extraMsgs

        SetMebendezoleAdministered value ->
            let
                updatedForm =
                    model.medicationData.mebendezoleForm
                        |> (\form -> { form | medicationAdministered = Just value, reasonForNonAdministration = Nothing })

                updatedData =
                    model.medicationData
                        |> (\data -> { data | mebendezoleForm = updatedForm })
            in
            ( { model | medicationData = updatedData }
            , Cmd.none
            , []
            )

        SetMebendezoleReasonForNonAdministration value ->
            let
                updatedForm =
                    model.medicationData.mebendezoleForm
                        |> (\form -> { form | reasonForNonAdministration = Just value })

                updatedData =
                    model.medicationData
                        |> (\data -> { data | mebendezoleForm = updatedForm })
            in
            ( { model | medicationData = updatedData }
            , Cmd.none
            , []
            )

        SaveMebendezole personId saved nextTask_ ->
            let
                measurementId =
                    Maybe.map Tuple.first saved

                measurement =
                    getMeasurementValueFunc saved

                extraMsgs =
                    generateMedicationMsgs nextTask_

                appMsgs =
                    model.medicationData.mebendezoleForm
                        |> toAdministrationNoteWithDefault measurement
                        |> Maybe.map
                            (Backend.WellChildEncounter.Model.SaveMebendezole personId measurementId
                                >> Backend.Model.MsgWellChildEncounter id
                                >> App.Model.MsgIndexedDb
                                >> List.singleton
                            )
                        |> Maybe.withDefault []
            in
            ( model
            , Cmd.none
            , appMsgs
            )
                |> sequenceExtra (update currentDate isChw id db) extraMsgs

        SetVitaminAAdministered value ->
            let
                updatedForm =
                    model.medicationData.vitaminAForm
                        |> (\form -> { form | medicationAdministered = Just value, reasonForNonAdministration = Nothing })

                updatedData =
                    model.medicationData
                        |> (\data -> { data | vitaminAForm = updatedForm })
            in
            ( { model | medicationData = updatedData }
            , Cmd.none
            , []
            )

        SetVitaminAReasonForNonAdministration value ->
            let
                updatedForm =
                    model.medicationData.vitaminAForm
                        |> (\form -> { form | reasonForNonAdministration = Just value })

                updatedData =
                    model.medicationData
                        |> (\data -> { data | vitaminAForm = updatedForm })
            in
            ( { model | medicationData = updatedData }
            , Cmd.none
            , []
            )

        SaveVitaminA personId saved nextTask_ ->
            let
                measurementId =
                    Maybe.map Tuple.first saved

                measurement =
                    getMeasurementValueFunc saved

                extraMsgs =
                    generateMedicationMsgs nextTask_

                appMsgs =
                    model.medicationData.vitaminAForm
                        |> toAdministrationNoteWithDefault measurement
                        |> Maybe.map
                            (Backend.WellChildEncounter.Model.SaveVitaminA personId measurementId
                                >> Backend.Model.MsgWellChildEncounter id
                                >> App.Model.MsgIndexedDb
                                >> List.singleton
                            )
                        |> Maybe.withDefault []
            in
            ( model
            , Cmd.none
            , appMsgs
            )
                |> sequenceExtra (update currentDate isChw id db) extraMsgs

        SetActiveNextStepsTask task ->
            let
                updatedData =
                    model.nextStepsData
                        |> (\data -> { data | activeTask = Just task })
            in
            ( { model | nextStepsData = updatedData }
            , Cmd.none
            , []
            )

        SetReferToHealthCenter value ->
            let
                form =
                    model.nextStepsData.sendToHCForm

                updatedForm =
                    { form | referToHealthCenter = Just value, reasonForNotSendingToHC = Nothing }

                updatedData =
                    model.nextStepsData
                        |> (\data -> { data | sendToHCForm = updatedForm })
            in
            ( { model | nextStepsData = updatedData }
            , Cmd.none
            , []
            )

        SetHandReferralForm value ->
            let
                form =
                    model.nextStepsData.sendToHCForm

                updatedForm =
                    { form | handReferralForm = Just value }

                updatedData =
                    model.nextStepsData
                        |> (\data -> { data | sendToHCForm = updatedForm })
            in
            ( { model | nextStepsData = updatedData }
            , Cmd.none
            , []
            )

        SetEnrollToNutritionProgram value ->
            let
                form =
                    model.nextStepsData.sendToHCForm

                updatedForm =
                    { form | enrollToNutritionProgram = Just value }

                updatedData =
                    model.nextStepsData
                        |> (\data -> { data | sendToHCForm = updatedForm })
            in
            ( { model | nextStepsData = updatedData }
            , Cmd.none
            , []
            )

        SetReferToNutritionProgram value ->
            let
                form =
                    model.nextStepsData.sendToHCForm

                updatedForm =
                    { form | referToNutritionProgram = Just value }

                updatedData =
                    model.nextStepsData
                        |> (\data -> { data | sendToHCForm = updatedForm })
            in
            ( { model | nextStepsData = updatedData }
            , Cmd.none
            , []
            )

        SetReasonForNotSendingToHC value ->
            let
                form =
                    model.nextStepsData.sendToHCForm

                updatedForm =
                    { form | reasonForNotSendingToHC = Just value }

                updatedData =
                    model.nextStepsData
                        |> (\data -> { data | sendToHCForm = updatedForm })
            in
            ( { model | nextStepsData = updatedData }
            , Cmd.none
            , []
            )

        SaveSendToHC personId saved nextTask_ ->
            let
                measurementId =
                    Maybe.map Tuple.first saved

                measurement =
                    getMeasurementValueFunc saved

                extraMsgs =
                    generateNextStepsMsgs nextTask_

                appMsgs =
                    model.nextStepsData.sendToHCForm
                        |> toSendToHCValueWithDefault measurement
                        |> Maybe.map
                            (Backend.WellChildEncounter.Model.SaveSendToHC personId measurementId
                                >> Backend.Model.MsgWellChildEncounter id
                                >> App.Model.MsgIndexedDb
                                >> List.singleton
                            )
                        |> Maybe.withDefault []
            in
            ( model
            , Cmd.none
            , appMsgs
            )
                |> sequenceExtra (update currentDate isChw id db) extraMsgs

        SetProvidedEducationForDiagnosis value ->
            let
                form =
                    model.nextStepsData.healthEducationForm

                updatedForm =
                    { form | educationForDiagnosis = Just value, reasonForNotProvidingHealthEducation = Nothing }

                updatedData =
                    model.nextStepsData
                        |> (\data -> { data | healthEducationForm = updatedForm })
            in
            ( { model | nextStepsData = updatedData }
            , Cmd.none
            , []
            )

        SetReasonForNotProvidingHealthEducation value ->
            let
                form =
                    model.nextStepsData.healthEducationForm

                updatedForm =
                    { form | reasonForNotProvidingHealthEducation = Just value }

                updatedData =
                    model.nextStepsData
                        |> (\data -> { data | healthEducationForm = updatedForm })
            in
            ( { model | nextStepsData = updatedData }
            , Cmd.none
            , []
            )

        SaveHealthEducation personId saved nextTask_ ->
            let
                measurementId =
                    Maybe.map Tuple.first saved

                measurement =
                    getMeasurementValueFunc saved

                extraMsgs =
                    generateNextStepsMsgs nextTask_

                appMsgs =
                    model.nextStepsData.healthEducationForm
                        |> toHealthEducationValueWithDefault measurement
                        |> Maybe.map
                            (Backend.WellChildEncounter.Model.SaveHealthEducation personId measurementId
                                >> Backend.Model.MsgWellChildEncounter id
                                >> App.Model.MsgIndexedDb
                                >> List.singleton
                            )
                        |> Maybe.withDefault []
            in
            ( model
            , Cmd.none
            , appMsgs
            )
                |> sequenceExtra (update currentDate isChw id db) extraMsgs

        SetContributingFactorsSign sign ->
            let
                form =
                    Dict.get id db.wellChildMeasurements
                        |> Maybe.withDefault NotAsked
                        |> RemoteData.toMaybe
                        |> Maybe.map
                            (.contributingFactors
                                >> getMeasurementValueFunc
                                >> contributingFactorsFormWithDefault model.nextStepsData.contributingFactorsForm
                            )
                        |> Maybe.withDefault model.nextStepsData.contributingFactorsForm

                updatedForm =
                    setMultiSelectInputValue .signs
                        (\signs -> { form | signs = signs })
                        NoContributingFactorsSign
                        sign
                        form

                updatedData =
                    model.nextStepsData
                        |> (\data -> { data | contributingFactorsForm = updatedForm })
            in
            ( { model | nextStepsData = updatedData }
            , Cmd.none
            , []
            )

        SaveContributingFactors personId saved nextTask_ ->
            let
                measurementId =
                    Maybe.map Tuple.first saved

                measurement =
                    getMeasurementValueFunc saved

                extraMsgs =
                    generateNextStepsMsgs nextTask_

                appMsgs =
                    model.nextStepsData.contributingFactorsForm
                        |> toContributingFactorsValueWithDefault measurement
                        |> Maybe.map
                            (Backend.WellChildEncounter.Model.SaveContributingFactors personId measurementId
                                >> Backend.Model.MsgWellChildEncounter id
                                >> App.Model.MsgIndexedDb
                                >> List.singleton
                            )
                        |> Maybe.withDefault []
            in
            ( model
            , Cmd.none
            , appMsgs
            )
                |> sequenceExtra (update currentDate isChw id db) extraMsgs

        SetFollowUpOption option ->
            let
                form =
                    model.nextStepsData.followUpForm

                updatedForm =
                    { form | option = Just option }

                updatedData =
                    model.nextStepsData
                        |> (\data -> { data | followUpForm = updatedForm })
            in
            ( { model | nextStepsData = updatedData }
            , Cmd.none
            , []
            )

        SaveFollowUp personId saved assesment nextTask_ ->
            let
                measurementId =
                    Maybe.map Tuple.first saved

                measurement =
                    getMeasurementValueFunc saved

                extraMsgs =
                    generateNextStepsMsgs nextTask_

                appMsgs =
                    model.nextStepsData.followUpForm
                        |> (\form -> { form | assesment = Just assesment })
                        |> toFollowUpValueWithDefault measurement
                        |> Maybe.map
                            (Backend.WellChildEncounter.Model.SaveFollowUp personId measurementId
                                >> Backend.Model.MsgWellChildEncounter id
                                >> App.Model.MsgIndexedDb
                                >> List.singleton
                            )
                        |> Maybe.withDefault []
            in
            ( model
            , Cmd.none
            , appMsgs
            )
                |> sequenceExtra (update currentDate isChw id db) extraMsgs

        SaveNextVisit personId saved nextDateForImmunisationVisit nextDateForPediatricVisit nextTask_ ->
            let
                measurementId =
                    Maybe.map Tuple.first saved

                measurement =
                    getMeasurementValueFunc saved

                extraMsgs =
                    generateNextStepsMsgs nextTask_

                appMsgs =
                    model.nextStepsData.nextVisitForm
                        |> (\form -> { form | immunisationDate = nextDateForImmunisationVisit, pediatricVisitDate = nextDateForPediatricVisit })
                        |> toNextVisitValueWithDefault measurement
                        |> Maybe.map
                            (Backend.WellChildEncounter.Model.SaveNextVisit personId measurementId
                                >> Backend.Model.MsgWellChildEncounter id
                                >> App.Model.MsgIndexedDb
                                >> List.singleton
                            )
                        |> Maybe.withDefault []
            in
            ( model
            , Cmd.none
            , appMsgs
            )
                |> sequenceExtra (update currentDate isChw id db) extraMsgs

        DropZoneComplete result ->
            let
                updatedForm =
                    model.photoForm
                        |> (\form -> { form | url = Just (PhotoUrl result.url) })
            in
            ( { model | photoForm = updatedForm }
            , Cmd.none
            , []
            )

        SavePhoto personId maybePhotoId url ->
            let
                appMsgs =
                    [ Backend.WellChildEncounter.Model.SavePhoto personId maybePhotoId url
                        |> Backend.Model.MsgWellChildEncounter id
                        |> App.Model.MsgIndexedDb
                    , App.Model.SetActivePage <| UserPage <| WellChildEncounterPage id
                    ]
            in
            ( { model | photoForm = emptyPhotoForm }
            , Cmd.none
            , appMsgs
            )


insertIntoSet : a -> Maybe (EverySet a) -> Maybe (EverySet a)
insertIntoSet value set =
    Maybe.map (EverySet.insert value) set
        |> Maybe.withDefault (EverySet.singleton value)
        |> Just
