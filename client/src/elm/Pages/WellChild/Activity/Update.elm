module Pages.WellChild.Activity.Update exposing (update)

import App.Model
import App.Utils exposing (focusOnCalendarMsg)
import AssocList as Dict
import Backend.Entities exposing (..)
import Backend.Measurement.Model exposing (..)
import Backend.Measurement.Utils exposing (getMeasurementValueFunc)
import Backend.Model exposing (ModelIndexedDb)
import Backend.WellChildEncounter.Model exposing (EncounterWarning(..))
import Date
import EverySet
import Gizra.NominalDate exposing (NominalDate)
import Gizra.Update exposing (sequenceExtra)
import Maybe.Extra exposing (unwrap)
import Measurement.Model
    exposing
        ( AnthropometricMeasurement(..)
        , ImmunisationTask(..)
        , VaccinationFormViewMode(..)
        , emptyPhotoForm
        )
import Measurement.Utils exposing (birthWeightOutsideConstraints, contributingFactorsFormWithDefault, heightFormWithDefault, heightOutOfRange, muacFormWithDefault, muacOutOfRange, ncdaFormWithDefault, nutritionFormWithDefault, toAdministrationNoteWithDefault, toContributingFactorsValueWithDefault, toHealthEducationValueWithDefault, toHeightValueWithDefault, toMuacValueWithDefault, toNCDAValueWithDefault, toNutritionCaringValueWithDefault, toNutritionFeedingValueWithDefault, toNutritionFollowUpValueWithDefault, toNutritionFoodSecurityValueWithDefault, toNutritionHygieneValueWithDefault, toNutritionValueWithDefault, toSendToHCValueWithDefault, toVaccinationValueWithDefault, toVitalsValueWithDefault, toWeightValueWithDefault, vaccinationFormWithDefault, vaccineDoseToComparable, weightFormWithDefault, weightOutOfRange)
import Pages.Page exposing (Page(..), UserPage(..))
import Pages.Utils exposing (insertIntoSet, saveMeasurementMsgs, setMuacValueForSite, setMultiSelectInputValue)
import Pages.WellChild.Activity.Model exposing (Model, Msg(..), WarningPopupType(..))
import Pages.WellChild.Activity.Utils exposing (birthLengthOutsideConstraints, getFormByVaccineTypeFunc, getMeasurementByVaccineTypeFunc, pregnancySummaryFormWithDefault, symptomsReviewFormWithDefault, toHeadCircumferenceValueWithDefault, toNextVisitValueWithDefault, toPregnancySummaryValueWithDefault, toSymptomsReviewValueWithDefault, toWellChildECDValueWithDefault, updateVaccinationFormByVaccineType)
import RemoteData exposing (RemoteData(..))
import SyncManager.Model exposing (Site)


update : NominalDate -> Site -> WellChildEncounterId -> ModelIndexedDb -> Msg -> Model -> ( Model, Cmd Msg, List App.Model.Msg )
update currentDate site id db msg model =
    let
        pregnancySummaryForm =
            resolveFormWithDefaults .pregnancySummary pregnancySummaryFormWithDefault model.pregnancySummaryForm

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

        ncdaForm =
            Dict.get id db.wellChildMeasurements
                |> Maybe.andThen RemoteData.toMaybe
                |> Maybe.map
                    (.ncda
                        >> getMeasurementValueFunc
                        >> ncdaFormWithDefault model.ncdaData.form
                    )
                |> Maybe.withDefault model.ncdaData.form

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
            Maybe.map (\task -> [ SetActiveImmunisationTask task ]) nextTask
                |> Maybe.withDefault [ SetActiveImmunisationTask TaskOverview ]

        generateHomeVisitMsgs nextTask =
            Maybe.map (\task -> [ SetActiveHomeVisitTask task ]) nextTask
                |> Maybe.withDefault [ SetActivePage <| UserPage <| WellChildEncounterPage id ]

        toIndexedDbMsg =
            Backend.Model.MsgWellChildEncounter id >> App.Model.MsgIndexedDb
    in
    case msg of
        NoOp ->
            ( model
            , Cmd.none
            , []
            )

        SetActivePage page ->
            ( model
            , Cmd.none
            , [ App.Model.SetActivePage page ]
            )

        SetWarningPopupState state ->
            ( { model | warningPopupState = state }, Cmd.none, [] )

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

        SetExpectedDateConcludedSelectorState state ->
            let
                updatedForm =
                    model.pregnancySummaryForm
                        |> (\form -> { form | dateSelectorPopupState = state })
            in
            ( { model | pregnancySummaryForm = updatedForm }
            , Cmd.none
            , [ focusOnCalendarMsg ]
            )

        SetPregnancySummaryBoolInput updateFunc value ->
            let
                updatedForm =
                    updateFunc value model.pregnancySummaryForm
            in
            ( { model | pregnancySummaryForm = updatedForm }
            , Cmd.none
            , []
            )

        SetPregnancySummaryNumberInput updateFunc value ->
            let
                updatedForm =
                    updateFunc value model.pregnancySummaryForm
            in
            ( { model | pregnancySummaryForm = updatedForm }
            , Cmd.none
            , []
            )

        SetDeliveryComplication complication ->
            let
                updatedForm =
                    setMultiSelectInputValue .deliveryComplications
                        (\complications -> { pregnancySummaryForm | deliveryComplications = complications })
                        NoDeliveryComplications
                        complication
                        pregnancySummaryForm
            in
            ( { model | pregnancySummaryForm = updatedForm }
            , Cmd.none
            , []
            )

        SetBirthDefect defect ->
            let
                updatedForm =
                    setMultiSelectInputValue .birthDefects
                        (\defects -> { pregnancySummaryForm | birthDefects = defects })
                        NoBirthDefects
                        defect
                        pregnancySummaryForm
            in
            ( { model | pregnancySummaryForm = updatedForm }
            , Cmd.none
            , []
            )

        PreSaveHeight skippedForms personId saved nextTask ->
            (getMeasurementValueFunc saved
                |> heightFormWithDefault skippedForms model.nutritionAssessmentData.heightForm
                |> heightOutOfRange site
            )
                |> preSaveNutritionAssessment currentDate site id db model (SaveHeight skippedForms personId saved nextTask)

        PreSaveMuac personId saved nextTask ->
            (getMeasurementValueFunc saved
                |> muacFormWithDefault model.nutritionAssessmentData.muacForm
                |> muacOutOfRange site
            )
                |> preSaveNutritionAssessment currentDate site id db model (SaveMuac personId saved nextTask)

        PreSaveWeight skippedForms personId saved nextTask ->
            (getMeasurementValueFunc saved
                |> weightFormWithDefault skippedForms model.nutritionAssessmentData.weightForm
                |> weightOutOfRange site
            )
                |> preSaveNutritionAssessment currentDate site id db model (SaveWeight skippedForms personId saved nextTask)

        PreSavePregnancySummary personId saved ->
            let
                outOfRange =
                    -- Both are asked for behind the one button, so the nurse is
                    -- told about each of them that is wrong rather than being
                    -- sent back a second time for the other.
                    List.filterMap identity
                        [ if birthWeightOutsideConstraints site pregnancySummaryForm.birthWeight then
                            Just MeasurementBirthWeight

                          else
                            Nothing
                        , if birthLengthOutsideConstraints site pregnancySummaryForm then
                            Just MeasurementBirthLength

                          else
                            Nothing
                        ]

                extraMsgs =
                    if List.isEmpty outOfRange then
                        [ SavePregnancySummary personId saved ]

                    else
                        -- Tell the nurse what is wrong and leave the form as it
                        -- is, so the measurements can be entered again. Nothing
                        -- is saved until they are within range.
                        [ SetWarningPopupState <| Just <| PopupMeasurementOutOfRange outOfRange ]
            in
            ( model
            , Cmd.none
            , []
            )
                |> sequenceExtra (update currentDate site id db) extraMsgs

        SavePregnancySummary personId saved ->
            let
                measurement =
                    getMeasurementValueFunc saved

                appMsgs =
                    model.pregnancySummaryForm
                        |> toPregnancySummaryValueWithDefault measurement
                        |> Maybe.map
                            (\value ->
                                let
                                    measurementId =
                                        Maybe.map Tuple.first saved
                                in
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

        SaveSymptomsReview personId saved nextTask ->
            ( model
            , Cmd.none
            , saveMeasurementMsgs toSymptomsReviewValueWithDefault
                model.dangerSignsData.symptomsReviewForm
                saved
                (Backend.WellChildEncounter.Model.SaveSymptomsReview personId)
                toIndexedDbMsg
            )
                |> sequenceExtra (update currentDate site id db) (generateDangerSignsMsgs nextTask)

        SetVitalsIntInput formUpdateFunc value ->
            let
                updatedData =
                    let
                        form =
                            model.dangerSignsData.vitalsForm

                        updatedForm =
                            formUpdateFunc (String.toInt value) form
                    in
                    model.dangerSignsData
                        |> (\data -> { data | vitalsForm = updatedForm })
            in
            ( { model | dangerSignsData = updatedData }
            , Cmd.none
            , []
            )

        SetVitalsFloatInput formUpdateFunc value ->
            let
                updatedData =
                    let
                        form =
                            model.dangerSignsData.vitalsForm

                        updatedForm =
                            formUpdateFunc (String.toFloat value) form
                    in
                    model.dangerSignsData
                        |> (\data -> { data | vitalsForm = updatedForm })
            in
            ( { model | dangerSignsData = updatedData }
            , Cmd.none
            , []
            )

        SetBodyTemperatureNotTaken value ->
            let
                updatedData =
                    let
                        updatedForm =
                            model.dangerSignsData.vitalsForm
                                |> (\form ->
                                        { form
                                            | bodyTemperature = Nothing
                                            , bodyTemperatureDirty = True
                                            , bodyTemperatureNotTaken = Just value
                                        }
                                   )
                    in
                    model.dangerSignsData
                        |> (\data -> { data | vitalsForm = updatedForm })
            in
            ( { model | dangerSignsData = updatedData }
            , Cmd.none
            , []
            )

        SetRespiratoryRateNotTaken value ->
            let
                updatedData =
                    let
                        updatedForm =
                            model.dangerSignsData.vitalsForm
                                |> (\form ->
                                        { form
                                            | respiratoryRate = Nothing
                                            , respiratoryRateDirty = True
                                            , respiratoryRateNotTaken = Just value
                                        }
                                   )
                    in
                    model.dangerSignsData
                        |> (\data -> { data | vitalsForm = updatedForm })
            in
            ( { model | dangerSignsData = updatedData }
            , Cmd.none
            , []
            )

        SaveVitals personId saved nextTask ->
            ( model
            , Cmd.none
            , saveMeasurementMsgs toVitalsValueWithDefault
                model.dangerSignsData.vitalsForm
                saved
                (Backend.WellChildEncounter.Model.SaveVitals personId)
                toIndexedDbMsg
            )
                |> sequenceExtra (update currentDate site id db) (generateDangerSignsMsgs nextTask)

        SetActiveNutritionAssessmentTask task ->
            let
                updatedData =
                    model.nutritionAssessmentData
                        |> (\data -> { data | activeTask = Just task })

                -- Moving to another task forgets what the one before it was
                -- complaining about: the warning names a measurement, and the
                -- tasks of this activity share one place to put it. Only that
                -- warning is dropped, so a head circumference one still stands.
                warningPopupState =
                    case model.warningPopupState of
                        Just (PopupMeasurementOutOfRange _) ->
                            Nothing

                        other ->
                            other
            in
            ( { model | nutritionAssessmentData = updatedData, warningPopupState = warningPopupState }
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

        SetHeightNotTaken value ->
            let
                updatedForm =
                    model.nutritionAssessmentData.heightForm
                        |> (\form ->
                                { form | height = Nothing, heightDirty = True, measurementNotTaken = Just value }
                           )

                updatedData =
                    model.nutritionAssessmentData
                        |> (\data -> { data | heightForm = updatedForm })
            in
            ( { model | nutritionAssessmentData = updatedData }
            , Cmd.none
            , []
            )

        SaveHeight skippedForms personId saved nextTask ->
            let
                extraMsgs =
                    generateNutritionAssessmentMsgs nextTask

                form_ =
                    model.nutritionAssessmentData.heightForm

                appMsgs =
                    if form_.measurementNotTaken == Just True then
                        if EverySet.member SkippedHeight skippedForms then
                            []

                        else
                            [ Backend.WellChildEncounter.Model.AddSkippedForm SkippedHeight
                                |> Backend.Model.MsgWellChildEncounter id
                                |> App.Model.MsgIndexedDb
                            ]

                    else
                        let
                            measurementId =
                                Maybe.map Tuple.first saved

                            measurement =
                                getMeasurementValueFunc saved
                        in
                        toHeightValueWithDefault skippedForms measurement form_
                            |> Maybe.map
                                (Backend.WellChildEncounter.Model.SaveHeight personId measurementId
                                    >> Backend.Model.MsgWellChildEncounter id
                                    >> App.Model.MsgIndexedDb
                                    >> List.singleton
                                    >> List.append
                                        [ Backend.WellChildEncounter.Model.RemoveSkippedForm SkippedHeight
                                            |> Backend.Model.MsgWellChildEncounter id
                                            |> App.Model.MsgIndexedDb
                                        ]
                                )
                            |> Maybe.withDefault []
            in
            ( model
            , Cmd.none
            , appMsgs
            )
                |> sequenceExtra (update currentDate site id db) extraMsgs

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
            , [ focusOnCalendarMsg ]
            )

        CloseHeadCircumferencePopup personId saved nextTask ->
            let
                extraMsgs =
                    [ SetWarningPopupState Nothing
                    , SaveHeadCircumference personId saved nextTask
                    ]
            in
            ( model
            , Cmd.none
            , []
            )
                |> sequenceExtra (update currentDate site id db) extraMsgs

        PreSaveHeadCircumference personId maybeZscore saved nextTask ->
            let
                ( warning, warningMessage ) =
                    let
                        setEncounterWarningMsg encounterWarning =
                            Backend.WellChildEncounter.Model.SetWellChildEncounterWarning encounterWarning
                                |> Backend.Model.MsgWellChildEncounter id
                                |> App.Model.MsgIndexedDb
                    in
                    Maybe.map
                        (\zscore ->
                            if zscore > 3 then
                                ( WarningHeadCircumferenceMacrocephaly
                                , [ setEncounterWarningMsg WarningHeadCircumferenceMacrocephaly ]
                                )

                            else if zscore < -3 then
                                ( WarningHeadCircumferenceMicrocephaly
                                , [ setEncounterWarningMsg WarningHeadCircumferenceMicrocephaly ]
                                )

                            else
                                -- Z-score value is within range, so no warning is
                                -- shown / recorded on encounter.
                                ( NoHeadCircumferenceWarning, [] )
                        )
                        maybeZscore
                        |> Maybe.withDefault
                            ( NoHeadCircumferenceWarning
                            , -- There's no z-score, meaning that measurement was not taken.
                              -- Therefore, we set appropriate encounter warning.
                              [ setEncounterWarningMsg NoHeadCircumferenceWarning ]
                            )

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
            , warningMessage
            )
                |> sequenceExtra (update currentDate site id db) extraMsgs

        SaveHeadCircumference personId saved nextTask ->
            ( model
            , Cmd.none
            , saveMeasurementMsgs toHeadCircumferenceValueWithDefault
                model.nutritionAssessmentData.headCircumferenceForm
                saved
                (Backend.WellChildEncounter.Model.SaveHeadCircumference personId)
                toIndexedDbMsg
            )
                |> sequenceExtra (update currentDate site id db) (generateNutritionAssessmentMsgs nextTask)

        SetMuac string ->
            let
                updatedForm =
                    model.nutritionAssessmentData.muacForm
                        |> (\form ->
                                { form | muac = setMuacValueForSite site string, muacDirty = True }
                           )

                updatedData =
                    model.nutritionAssessmentData
                        |> (\data -> { data | muacForm = updatedForm })
            in
            ( { model | nutritionAssessmentData = updatedData }
            , Cmd.none
            , []
            )

        SaveMuac personId saved nextTask ->
            ( model
            , Cmd.none
            , saveMeasurementMsgs toMuacValueWithDefault
                model.nutritionAssessmentData.muacForm
                saved
                (Backend.WellChildEncounter.Model.SaveMuac personId)
                toIndexedDbMsg
            )
                |> sequenceExtra (update currentDate site id db) (generateNutritionAssessmentMsgs nextTask)

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

        SaveNutrition personId saved assessment nextTask ->
            ( model
            , Cmd.none
            , saveMeasurementMsgs toNutritionValueWithDefault
                (model.nutritionAssessmentData.nutritionForm
                    |> (\form -> { form | assesment = Just assessment })
                )
                saved
                (Backend.WellChildEncounter.Model.SaveNutrition personId)
                toIndexedDbMsg
            )
                |> sequenceExtra (update currentDate site id db) (generateNutritionAssessmentMsgs nextTask)

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

        SetWeightNotTaken value ->
            let
                updatedForm =
                    model.nutritionAssessmentData.weightForm
                        |> (\form ->
                                { form | weight = Nothing, weightDirty = True, measurementNotTaken = Just value }
                           )

                updatedData =
                    model.nutritionAssessmentData
                        |> (\data -> { data | weightForm = updatedForm })
            in
            ( { model | nutritionAssessmentData = updatedData }
            , Cmd.none
            , []
            )

        SaveWeight skippedForms personId saved nextTask ->
            let
                extraMsgs =
                    generateNutritionAssessmentMsgs nextTask

                form_ =
                    model.nutritionAssessmentData.weightForm

                appMsgs =
                    if form_.measurementNotTaken == Just True then
                        if EverySet.member SkippedWeight skippedForms then
                            []

                        else
                            [ Backend.WellChildEncounter.Model.AddSkippedForm SkippedWeight
                                |> Backend.Model.MsgWellChildEncounter id
                                |> App.Model.MsgIndexedDb
                            ]

                    else
                        let
                            measurementId =
                                Maybe.map Tuple.first saved

                            measurement =
                                getMeasurementValueFunc saved
                        in
                        toWeightValueWithDefault skippedForms measurement form_
                            |> Maybe.map
                                (Backend.WellChildEncounter.Model.SaveWeight personId measurementId
                                    >> Backend.Model.MsgWellChildEncounter id
                                    >> App.Model.MsgIndexedDb
                                    >> List.singleton
                                    >> List.append
                                        [ Backend.WellChildEncounter.Model.RemoveSkippedForm SkippedWeight
                                            |> Backend.Model.MsgWellChildEncounter id
                                            |> App.Model.MsgIndexedDb
                                        ]
                                )
                            |> Maybe.withDefault []
            in
            ( model
            , Cmd.none
            , appMsgs
            )
                |> sequenceExtra (update currentDate site id db) extraMsgs

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
                    if value then
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

        SetVaccinationUpdateDateSelectorState vaccineType state ->
            let
                form =
                    getFormByVaccineTypeFunc vaccineType model.immunisationData

                defaultSelection =
                    Maybe.Extra.or form.vaccinationUpdateDate (Maybe.andThen .dateDefault state)

                updatedForm =
                    { form | dateSelectorPopupState = state, vaccinationUpdateDate = defaultSelection }
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

        SaveBCGImmunisation personId saved nextTask ->
            ( model
            , Cmd.none
            , saveMeasurementMsgs toVaccinationValueWithDefault
                model.immunisationData.bcgForm
                saved
                (Backend.WellChildEncounter.Model.SaveBCGImmunisation personId)
                toIndexedDbMsg
            )
                |> sequenceExtra (update currentDate site id db) (generateImmunisationMsgs nextTask)

        SaveDTPImmunisation personId saved nextTask ->
            ( model
            , Cmd.none
            , saveMeasurementMsgs toVaccinationValueWithDefault
                model.immunisationData.dtpForm
                saved
                (Backend.WellChildEncounter.Model.SaveDTPImmunisation personId)
                toIndexedDbMsg
            )
                |> sequenceExtra (update currentDate site id db) (generateImmunisationMsgs nextTask)

        SaveDTPStandaloneImmunisation personId saved nextTask ->
            ( model
            , Cmd.none
            , saveMeasurementMsgs toVaccinationValueWithDefault
                model.immunisationData.dtpForm
                saved
                (Backend.WellChildEncounter.Model.SaveDTPStandaloneImmunisation personId)
                toIndexedDbMsg
            )
                |> sequenceExtra (update currentDate site id db) (generateImmunisationMsgs nextTask)

        SaveHPVImmunisation personId saved nextTask ->
            ( model
            , Cmd.none
            , saveMeasurementMsgs toVaccinationValueWithDefault
                model.immunisationData.hpvForm
                saved
                (Backend.WellChildEncounter.Model.SaveHPVImmunisation personId)
                toIndexedDbMsg
            )
                |> sequenceExtra (update currentDate site id db) (generateImmunisationMsgs nextTask)

        SaveIPVImmunisation personId saved nextTask ->
            ( model
            , Cmd.none
            , saveMeasurementMsgs toVaccinationValueWithDefault
                model.immunisationData.ipvForm
                saved
                (Backend.WellChildEncounter.Model.SaveIPVImmunisation personId)
                toIndexedDbMsg
            )
                |> sequenceExtra (update currentDate site id db) (generateImmunisationMsgs nextTask)

        SaveMRImmunisation personId saved nextTask ->
            ( model
            , Cmd.none
            , saveMeasurementMsgs toVaccinationValueWithDefault
                model.immunisationData.mrForm
                saved
                (Backend.WellChildEncounter.Model.SaveMRImmunisation personId)
                toIndexedDbMsg
            )
                |> sequenceExtra (update currentDate site id db) (generateImmunisationMsgs nextTask)

        SaveOPVImmunisation personId saved nextTask ->
            ( model
            , Cmd.none
            , saveMeasurementMsgs toVaccinationValueWithDefault
                model.immunisationData.opvForm
                saved
                (Backend.WellChildEncounter.Model.SaveOPVImmunisation personId)
                toIndexedDbMsg
            )
                |> sequenceExtra (update currentDate site id db) (generateImmunisationMsgs nextTask)

        SavePCV13Immunisation personId saved nextTask ->
            ( model
            , Cmd.none
            , saveMeasurementMsgs toVaccinationValueWithDefault
                model.immunisationData.pcv13Form
                saved
                (Backend.WellChildEncounter.Model.SavePCV13Immunisation personId)
                toIndexedDbMsg
            )
                |> sequenceExtra (update currentDate site id db) (generateImmunisationMsgs nextTask)

        SaveRotarixImmunisation personId saved nextTask ->
            ( model
            , Cmd.none
            , saveMeasurementMsgs toVaccinationValueWithDefault
                model.immunisationData.rotarixForm
                saved
                (Backend.WellChildEncounter.Model.SaveRotarixImmunisation personId)
                toIndexedDbMsg
            )
                |> sequenceExtra (update currentDate site id db) (generateImmunisationMsgs nextTask)

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

        SaveAlbendazole personId saved nextTask ->
            ( model
            , Cmd.none
            , saveMeasurementMsgs toAdministrationNoteWithDefault
                model.medicationData.albendazoleForm
                saved
                (Backend.WellChildEncounter.Model.SaveAlbendazole personId)
                toIndexedDbMsg
            )
                |> sequenceExtra (update currentDate site id db) (generateMedicationMsgs nextTask)

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

        SaveMebendezole personId saved nextTask ->
            ( model
            , Cmd.none
            , saveMeasurementMsgs toAdministrationNoteWithDefault
                model.medicationData.mebendezoleForm
                saved
                (Backend.WellChildEncounter.Model.SaveMebendezole personId)
                toIndexedDbMsg
            )
                |> sequenceExtra (update currentDate site id db) (generateMedicationMsgs nextTask)

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

        SaveVitaminA personId saved nextTask ->
            ( model
            , Cmd.none
            , saveMeasurementMsgs toAdministrationNoteWithDefault
                model.medicationData.vitaminAForm
                saved
                (Backend.WellChildEncounter.Model.SaveVitaminA personId)
                toIndexedDbMsg
            )
                |> sequenceExtra (update currentDate site id db) (generateMedicationMsgs nextTask)

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

        SetReasonForNonReferral value ->
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

        SaveSendToHC personId saved nextTask ->
            ( model
            , Cmd.none
            , saveMeasurementMsgs toSendToHCValueWithDefault
                model.nextStepsData.sendToHCForm
                saved
                (Backend.WellChildEncounter.Model.SaveSendToHC personId)
                toIndexedDbMsg
            )
                |> sequenceExtra (update currentDate site id db) (generateNextStepsMsgs nextTask)

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

        SaveHealthEducation personId saved nextTask ->
            ( model
            , Cmd.none
            , saveMeasurementMsgs toHealthEducationValueWithDefault
                model.nextStepsData.healthEducationForm
                saved
                (Backend.WellChildEncounter.Model.SaveHealthEducation personId)
                toIndexedDbMsg
            )
                |> sequenceExtra (update currentDate site id db) (generateNextStepsMsgs nextTask)

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

        SaveContributingFactors personId saved nextTask ->
            ( model
            , Cmd.none
            , saveMeasurementMsgs toContributingFactorsValueWithDefault
                model.nextStepsData.contributingFactorsForm
                saved
                (Backend.WellChildEncounter.Model.SaveContributingFactors personId)
                toIndexedDbMsg
            )
                |> sequenceExtra (update currentDate site id db) (generateNextStepsMsgs nextTask)

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

        SaveFollowUp personId saved assesment nextTask ->
            ( model
            , Cmd.none
            , saveMeasurementMsgs toNutritionFollowUpValueWithDefault
                (model.nextStepsData.followUpForm
                    |> (\form -> { form | assesment = Just assesment })
                )
                saved
                (Backend.WellChildEncounter.Model.SaveFollowUp personId)
                toIndexedDbMsg
            )
                |> sequenceExtra (update currentDate site id db) (generateNextStepsMsgs nextTask)

        SaveNextVisit personId saved nextDateForImmunisationVisit nextDateForPediatricVisit asapImmunisationDate nextTask ->
            ( model
            , Cmd.none
            , saveMeasurementMsgs toNextVisitValueWithDefault
                (model.nextStepsData.nextVisitForm
                    |> (\form ->
                            { form
                                | immunisationDate = nextDateForImmunisationVisit
                                , asapImmunisationDate = asapImmunisationDate
                                , pediatricVisitDate = nextDateForPediatricVisit
                            }
                       )
                )
                saved
                (Backend.WellChildEncounter.Model.SaveNextVisit personId)
                toIndexedDbMsg
            )
                |> sequenceExtra (update currentDate site id db) (generateNextStepsMsgs nextTask)

        DropZoneComplete result ->
            let
                updatedForm =
                    model.photoForm
                        |> (\form -> { form | url = Just (ImageUrl result.url) })
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

        SetUpdateANCVisits value ->
            let
                updatedForm =
                    { ncdaForm | updateANCVisits = Just value, ancVisitsDates = Just EverySet.empty }

                updatedData =
                    model.ncdaData
                        |> (\data -> { data | form = updatedForm })
            in
            ( { model | ncdaData = updatedData }
            , Cmd.none
            , []
            )

        ToggleANCVisitDate date ->
            let
                updatedANCVisitsDates =
                    Maybe.map
                        (\set ->
                            if EverySet.member date set then
                                EverySet.remove date set

                            else
                                EverySet.insert date set
                        )
                        ncdaForm.ancVisitsDates
                        |> Maybe.withDefault (EverySet.singleton date)

                updateANCVisits =
                    if EverySet.isEmpty updatedANCVisitsDates then
                        Just False

                    else
                        ncdaForm.updateANCVisits

                updatedForm =
                    { ncdaForm | ancVisitsDates = Just updatedANCVisitsDates, updateANCVisits = updateANCVisits }

                updatedData =
                    model.ncdaData
                        |> (\data -> { data | form = updatedForm })
            in
            ( { model | ncdaData = updatedData }
            , Cmd.none
            , []
            )

        SetNCDABoolInput formUpdateFunc value ->
            let
                updatedForm =
                    formUpdateFunc value model.ncdaData.form

                updatedData =
                    model.ncdaData
                        |> (\data -> { data | form = updatedForm })
            in
            ( { model | ncdaData = updatedData }
            , Cmd.none
            , []
            )

        SetBirthWeight string ->
            let
                updatedForm =
                    model.ncdaData.form
                        |> (\form ->
                                { form
                                    | birthWeight = String.toFloat string |> Maybe.map WeightInGrm
                                }
                           )

                updatedData =
                    model.ncdaData
                        |> (\data -> { data | form = updatedForm })
            in
            ( { model | ncdaData = updatedData }
            , Cmd.none
            , []
            )

        SetChildReceivesVitaminA value ->
            let
                updatedForm =
                    model.ncdaData.form
                        |> (\form -> { form | childReceivesVitaminA = Just value })

                updatedData =
                    model.ncdaData
                        |> (\data -> { data | form = updatedForm })
            in
            ( { model | ncdaData = updatedData }
            , Cmd.none
            , []
            )

        SetStuntingLevel value ->
            let
                updatedForm =
                    model.ncdaData.form
                        |> (\form -> { form | stuntingLevel = Just value })

                updatedData =
                    model.ncdaData
                        |> (\data -> { data | form = updatedForm })
            in
            ( { model | ncdaData = updatedData }
            , Cmd.none
            , []
            )

        SetWeightForNCDA string ->
            let
                updatedForm =
                    model.ncdaData.form
                        |> (\form ->
                                { form
                                    | weight = String.toFloat string |> Maybe.map WeightInKg
                                }
                           )

                updatedData =
                    model.ncdaData
                        |> (\data -> { data | form = updatedForm })
            in
            ( { model | ncdaData = updatedData }
            , Cmd.none
            , []
            )

        SetMuacForNCDA string ->
            let
                updatedForm =
                    model.ncdaData.form
                        |> (\form ->
                                { form
                                    | muac = setMuacValueForSite site string |> Maybe.map MuacInCm
                                }
                           )

                updatedData =
                    model.ncdaData
                        |> (\data -> { data | form = updatedForm })
            in
            ( { model | ncdaData = updatedData }
            , Cmd.none
            , []
            )

        SetNCDAFormStep step ->
            let
                updatedForm =
                    model.ncdaData.form
                        |> (\form -> { form | step = Just step })

                updatedData =
                    model.ncdaData
                        |> (\data -> { data | form = updatedForm })
            in
            ( { model | ncdaData = updatedData }
            , Cmd.none
            , []
            )

        SetBirthWeightOutOfRangePopup isOpen ->
            let
                updatedData =
                    model.ncdaData
                        |> (\data -> { data | showBirthWeightOutOfRangePopup = isOpen })
            in
            ( { model | ncdaData = updatedData }
            , Cmd.none
            , []
            )

        SetNCDAHelperState state ->
            let
                updatedData =
                    model.ncdaData
                        |> (\data -> { data | helperState = state })
            in
            ( { model | ncdaData = updatedData }
            , Cmd.none
            , []
            )

        SaveNCDA personId saved ->
            let
                measurementId =
                    Maybe.map Tuple.first saved

                measurement =
                    getMeasurementValueFunc saved

                appMsgs =
                    model.ncdaData.form
                        |> toNCDAValueWithDefault measurement
                        |> unwrap
                            []
                            (\value ->
                                [ Backend.WellChildEncounter.Model.SaveNCDA personId measurementId value
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

        SetActiveHomeVisitTask task ->
            let
                updatedData =
                    model.homeVisitData
                        |> (\data -> { data | activeTask = Just task })
            in
            ( { model | homeVisitData = updatedData }
            , Cmd.none
            , []
            )

        SetFeedingBoolInput formUpdateFunc value ->
            let
                updatedForm =
                    formUpdateFunc value model.homeVisitData.feedingForm

                updatedData =
                    model.homeVisitData
                        |> (\data -> { data | feedingForm = updatedForm })
            in
            ( { model | homeVisitData = updatedData }
            , Cmd.none
            , []
            )

        SetNutritionSupplementType value ->
            let
                form =
                    model.homeVisitData.feedingForm

                updatedForm =
                    { form | supplementType = Just value, sachetsPerDay = Nothing, eatenWithWater = Nothing }

                updatedData =
                    model.homeVisitData
                        |> (\data -> { data | feedingForm = updatedForm })
            in
            ( { model | homeVisitData = updatedData }
            , Cmd.none
            , []
            )

        SetSachetsPerDay value ->
            let
                form =
                    model.homeVisitData.feedingForm

                updatedForm =
                    { form | sachetsPerDay = String.toFloat value }

                updatedData =
                    model.homeVisitData
                        |> (\data -> { data | feedingForm = updatedForm })
            in
            ( { model | homeVisitData = updatedData }
            , Cmd.none
            , []
            )

        SaveFeeding personId saved nextTask ->
            ( model
            , Cmd.none
            , saveMeasurementMsgs toNutritionFeedingValueWithDefault
                model.homeVisitData.feedingForm
                saved
                (Backend.WellChildEncounter.Model.SaveFeeding personId)
                toIndexedDbMsg
            )
                |> sequenceExtra (update currentDate site id db) (generateHomeVisitMsgs nextTask)

        SetHygieneBoolInput formUpdateFunc value ->
            let
                updatedForm =
                    formUpdateFunc value model.homeVisitData.hygieneForm

                updatedData =
                    model.homeVisitData
                        |> (\data -> { data | hygieneForm = updatedForm })
            in
            ( { model | homeVisitData = updatedData }
            , Cmd.none
            , []
            )

        SetMainWaterSource value ->
            let
                form =
                    model.homeVisitData.hygieneForm

                updatedForm =
                    { form | mainWaterSource = Just value }

                updatedData =
                    model.homeVisitData
                        |> (\data -> { data | hygieneForm = updatedForm })
            in
            ( { model | homeVisitData = updatedData }
            , Cmd.none
            , []
            )

        SetWaterPreparationOption value ->
            let
                form =
                    model.homeVisitData.hygieneForm

                updatedForm =
                    { form | waterPreparationOption = Just value }

                updatedData =
                    model.homeVisitData
                        |> (\data -> { data | hygieneForm = updatedForm })
            in
            ( { model | homeVisitData = updatedData }
            , Cmd.none
            , []
            )

        SaveHygiene personId saved nextTask ->
            ( model
            , Cmd.none
            , saveMeasurementMsgs toNutritionHygieneValueWithDefault
                model.homeVisitData.hygieneForm
                saved
                (Backend.WellChildEncounter.Model.SaveHygiene personId)
                toIndexedDbMsg
            )
                |> sequenceExtra (update currentDate site id db) (generateHomeVisitMsgs nextTask)

        SetFoodSecurityBoolInput formUpdateFunc value ->
            let
                updatedForm =
                    formUpdateFunc value model.homeVisitData.foodSecurityForm

                updatedData =
                    model.homeVisitData
                        |> (\data -> { data | foodSecurityForm = updatedForm })
            in
            ( { model | homeVisitData = updatedData }
            , Cmd.none
            , []
            )

        SetMainIncomeSource value ->
            let
                form =
                    model.homeVisitData.foodSecurityForm

                updatedForm =
                    { form | mainIncomeSource = Just value }

                updatedData =
                    model.homeVisitData
                        |> (\data -> { data | foodSecurityForm = updatedForm })
            in
            ( { model | homeVisitData = updatedData }
            , Cmd.none
            , []
            )

        SaveFoodSecurity personId saved nextTask ->
            ( model
            , Cmd.none
            , saveMeasurementMsgs toNutritionFoodSecurityValueWithDefault
                model.homeVisitData.foodSecurityForm
                saved
                (Backend.WellChildEncounter.Model.SaveFoodSecurity personId)
                toIndexedDbMsg
            )
                |> sequenceExtra (update currentDate site id db) (generateHomeVisitMsgs nextTask)

        SetParentsAliveAndHealthy value ->
            let
                form =
                    model.homeVisitData.caringForm

                updatedForm =
                    { form | parentHealth = Just value }

                updatedData =
                    model.homeVisitData
                        |> (\data -> { data | caringForm = updatedForm })
            in
            ( { model | homeVisitData = updatedData }
            , Cmd.none
            , []
            )

        SetChildClean value ->
            let
                form =
                    model.homeVisitData.caringForm

                updatedForm =
                    { form | childClean = Just value }

                updatedData =
                    model.homeVisitData
                        |> (\data -> { data | caringForm = updatedForm })
            in
            ( { model | homeVisitData = updatedData }
            , Cmd.none
            , []
            )

        SetNutritionCaringOption option ->
            let
                form =
                    model.homeVisitData.caringForm

                updatedForm =
                    { form | caringOption = Just option }

                updatedData =
                    model.homeVisitData
                        |> (\data -> { data | caringForm = updatedForm })
            in
            ( { model | homeVisitData = updatedData }
            , Cmd.none
            , []
            )

        SaveNutritionCaring personId saved nextTask ->
            ( model
            , Cmd.none
            , saveMeasurementMsgs toNutritionCaringValueWithDefault
                model.homeVisitData.caringForm
                saved
                (Backend.WellChildEncounter.Model.SaveCaring personId)
                toIndexedDbMsg
            )
                |> sequenceExtra (update currentDate site id db) (generateHomeVisitMsgs nextTask)


{-| Saves a Nutrition Assessment task, unless a measurement on it was entered
outside the range it can take.

The nurse is told which measurement is wrong and the form is left as it is, so
it can be entered again. Nothing is saved until it is within range.

The measurements are read from the form the nurse is looking at, which is the
one the saved measurement and the forms skipped on the encounter have been
merged into - not the form as it was last typed into.

-}
preSaveNutritionAssessment :
    NominalDate
    -> Site
    -> WellChildEncounterId
    -> ModelIndexedDb
    -> Model
    -> Msg
    -> List AnthropometricMeasurement
    -> ( Model, Cmd Msg, List App.Model.Msg )
preSaveNutritionAssessment currentDate site id db model saveMsg outOfRange =
    let
        extraMsgs =
            if List.isEmpty outOfRange then
                [ saveMsg ]

            else
                [ SetWarningPopupState <| Just <| PopupMeasurementOutOfRange outOfRange ]
    in
    ( model
    , Cmd.none
    , []
    )
        |> sequenceExtra (update currentDate site id db) extraMsgs
