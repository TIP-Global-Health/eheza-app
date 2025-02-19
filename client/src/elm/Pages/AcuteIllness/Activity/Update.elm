module Pages.AcuteIllness.Activity.Update exposing (update)

import App.Model
import App.Utils exposing (focusOnCalendarMsg)
import AssocList as Dict
import Backend.AcuteIllnessEncounter.Model
import Backend.Entities exposing (..)
import Backend.Measurement.Decoder exposing (malariaRapidTestResultFromString)
import Backend.Measurement.Model
    exposing
        ( AcuteFindingsGeneralSign(..)
        , AcuteFindingsRespiratorySign(..)
        , AcuteIllnessDangerSign(..)
        , AdverseEvent(..)
        , ChildNutritionSign(..)
        , LungsCPESign(..)
        , ReasonForNotIsolating(..)
        , SymptomsGISign(..)
        , SymptomsGeneralSign(..)
        , SymptomsRespiratorySign(..)
        )
import Backend.Measurement.Utils exposing (getMeasurementValueFunc)
import Backend.Model exposing (ModelIndexedDb)
import Backend.Person.Form
import Backend.Person.Model
import Backend.Village.Utils exposing (getVillageHealthCenterId, getVillageIdByGeoFields)
import Components.PatientsSearchForm.Update
import EverySet
import Form
import Gizra.NominalDate exposing (NominalDate)
import Gizra.Update exposing (sequenceExtra)
import Maybe.Extra exposing (isJust, isNothing, unwrap)
import Measurement.Utils
    exposing
        ( ongoingTreatmentReviewFormWithDefault
        , toHealthEducationValueWithDefault
        , toMuacValueWithDefault
        , toOngoingTreatmentReviewValueWithDefault
        , toSendToHCValueWithDefault
        , toVitalsValueWithDefault
        )
import Pages.AcuteIllness.Activity.Model exposing (..)
import Pages.AcuteIllness.Activity.Types exposing (..)
import Pages.AcuteIllness.Activity.Utils exposing (..)
import Pages.Page exposing (Page(..), UserPage(..))
import Pages.Utils exposing (nonAdministrationReasonToSign, setMuacValueForSite, setMultiSelectInputValue)
import RemoteData exposing (RemoteData(..))
import SyncManager.Model exposing (Site)


update : NominalDate -> Site -> Maybe HealthCenterId -> AcuteIllnessEncounterId -> ModelIndexedDb -> Msg -> Model -> ( Model, Cmd Msg, List App.Model.Msg )
update currentDate site selectedHealthCenter id db msg model =
    let
        noChange =
            ( model, Cmd.none, [] )

        resolveFormWithDefaults getMeasurementFunc formWithDefaultsFunc form =
            Dict.get id db.acuteIllnessMeasurements
                |> Maybe.withDefault NotAsked
                |> RemoteData.toMaybe
                |> Maybe.map
                    (getMeasurementFunc
                        >> getMeasurementValueFunc
                        >> formWithDefaultsFunc form
                    )
                |> Maybe.withDefault form

        symptomsGeneralForm =
            resolveFormWithDefaults .symptomsGeneral symptomsGeneralFormWithDefault model.symptomsData.symptomsGeneralForm

        symptomsRespiratoryForm =
            resolveFormWithDefaults .symptomsRespiratory symptomsRespiratoryFormWithDefault model.symptomsData.symptomsRespiratoryForm

        symptomsGIForm =
            resolveFormWithDefaults .symptomsGI symptomsGIFormWithDefault model.symptomsData.symptomsGIForm

        coreExamForm =
            resolveFormWithDefaults .coreExam coreExamFormWithDefault model.physicalExamData.coreExamForm

        acuteFindingsForm =
            resolveFormWithDefaults .acuteFindings acuteFindingsFormWithDefault model.physicalExamData.acuteFindingsForm

        treatmentReviewForm =
            resolveFormWithDefaults .treatmentOngoing ongoingTreatmentReviewFormWithDefault model.ongoingTreatmentData.treatmentReviewForm

        contactsTracingForm =
            resolveFormWithDefaults .contactsTracing Pages.AcuteIllness.Activity.Utils.contactsTracingFormWithDefault model.nextStepsData.contactsTracingForm

        generateSymptomsReviewMsgs nextTask =
            Maybe.map (\task -> [ SetActiveSymptomsTask task ]) nextTask
                |> Maybe.withDefault [ SetActivePage <| UserPage <| AcuteIllnessEncounterPage id ]

        generatePhysicalExamMsgs nextTask =
            Maybe.map (\task -> [ SetActivePhysicalExamTask task ]) nextTask
                |> Maybe.withDefault [ SetActivePage <| UserPage <| AcuteIllnessEncounterPage id ]

        generateLaboratoryMsgs nextTask =
            Maybe.map (\task -> [ SetActiveLaboratoryTask task ]) nextTask
                |> Maybe.withDefault [ SetActivePage <| UserPage <| AcuteIllnessEncounterPage id ]

        generateNextStepsMsgs nextTask =
            Maybe.map (\task -> [ SetActiveNextStepsTask task ]) nextTask
                |> Maybe.withDefault [ SetActivePage <| UserPage <| AcuteIllnessEncounterPage id ]

        generateExposureMsgs nextTask =
            Maybe.map (\task -> [ SetActiveExposureTask task ]) nextTask
                |> Maybe.withDefault [ SetActivePage <| UserPage <| AcuteIllnessEncounterPage id ]
    in
    case msg of
        SetActivePage page ->
            ( model
            , Cmd.none
            , [ App.Model.SetActivePage page ]
            )

        SetAlertsDialogState isOpen ->
            ( { model | showAlertsDialog = isOpen }, Cmd.none, [] )

        SetWarningPopupState diagnosis ->
            ( { model | warningPopupState = diagnosis }, Cmd.none, [] )

        SetPertinentSymptomsPopupState isOpen ->
            ( { model | showPertinentSymptomsPopup = isOpen }, Cmd.none, [] )

        SetActiveSymptomsTask task ->
            let
                updatedData =
                    model.symptomsData
                        |> (\data -> { data | activeTask = Just task })
            in
            ( { model | symptomsData = updatedData }
            , Cmd.none
            , []
            )

        ToggleSymptomsGeneralSign sign ->
            let
                updatedSigns =
                    toggleSymptomsSign SymptomsGeneral sign NoSymptomsGeneral symptomsGeneralForm.signs

                updatedForm =
                    { symptomsGeneralForm | signs = updatedSigns, signsDirty = True }

                updatedData =
                    model.symptomsData
                        |> (\data -> { data | symptomsGeneralForm = updatedForm })
            in
            ( { model | symptomsData = updatedData }
            , Cmd.none
            , []
            )

        ToggleSymptomsGISign sign ->
            let
                updatedSigns =
                    toggleSymptomsSign SymptomsGI sign NoSymptomsGI symptomsGIForm.signs

                updatedForm =
                    { symptomsGIForm | signs = updatedSigns, signsDirty = True }

                updatedData =
                    model.symptomsData
                        |> (\data -> { data | symptomsGIForm = updatedForm })
            in
            ( { model | symptomsData = updatedData }
            , Cmd.none
            , []
            )

        ToggleSymptomsRespiratorySign sign ->
            let
                updatedSigns =
                    toggleSymptomsSign SymptomsRespiratory sign NoSymptomsRespiratory symptomsRespiratoryForm.signs

                updatedForm =
                    { symptomsRespiratoryForm | signs = updatedSigns, signsDirty = True }

                updatedData =
                    model.symptomsData
                        |> (\data -> { data | symptomsRespiratoryForm = updatedForm })
            in
            ( { model | symptomsData = updatedData }
            , Cmd.none
            , []
            )

        SetSymptomsGeneralSignValue sign string ->
            String.toInt string
                |> Maybe.map
                    (\value ->
                        let
                            updatedForm =
                                { symptomsGeneralForm | signs = Dict.insert sign value symptomsGeneralForm.signs }

                            updatedData =
                                model.symptomsData
                                    |> (\data -> { data | symptomsGeneralForm = updatedForm })
                        in
                        ( { model | symptomsData = updatedData }
                        , Cmd.none
                        , []
                        )
                    )
                |> Maybe.withDefault noChange

        SetSymptomsGISignValue sign string ->
            String.toInt string
                |> Maybe.map
                    (\value ->
                        let
                            updatedForm =
                                { symptomsGIForm | signs = Dict.insert sign value symptomsGIForm.signs }

                            updatedData =
                                model.symptomsData
                                    |> (\data -> { data | symptomsGIForm = updatedForm })
                        in
                        ( { model | symptomsData = updatedData }
                        , Cmd.none
                        , []
                        )
                    )
                |> Maybe.withDefault noChange

        SetSymptomsRespiratorySignValue sign string ->
            String.toInt string
                |> Maybe.map
                    (\value ->
                        let
                            updatedForm =
                                { symptomsRespiratoryForm | signs = Dict.insert sign value symptomsRespiratoryForm.signs }

                            updatedData =
                                model.symptomsData
                                    |> (\data -> { data | symptomsRespiratoryForm = updatedForm })
                        in
                        ( { model | symptomsData = updatedData }
                        , Cmd.none
                        , []
                        )
                    )
                |> Maybe.withDefault noChange

        SetSymptomsRespiratoryCough moreThan2Weeks ->
            let
                valueToSet =
                    if moreThan2Weeks then
                        symptomMaxDuration

                    else
                        coughLessThan2WeeksConstant

                extraMsgs =
                    [ SetSymptomsRespiratorySignValue Cough (String.fromInt valueToSet) ]
            in
            sequenceExtra (update currentDate site selectedHealthCenter id db) extraMsgs noChange

        SetSymptomsGIIntractableVomiting value ->
            let
                updatedForm =
                    { symptomsGIForm | intractableVomiting = Just value, intractableVomitingDirty = True }

                updatedData =
                    model.symptomsData
                        |> (\data -> { data | symptomsGIForm = updatedForm })
            in
            ( { model | symptomsData = updatedData }
            , Cmd.none
            , []
            )

        SaveSymptomsGeneral personId saved nextTask ->
            let
                measurementId =
                    Maybe.map Tuple.first saved

                measurement =
                    getMeasurementValueFunc saved

                form =
                    model.symptomsData.symptomsGeneralForm

                value =
                    toSymptomsGeneralValueWithDefault measurement form

                extraMsgs =
                    generateSymptomsReviewMsgs nextTask

                appMsgs =
                    Backend.AcuteIllnessEncounter.Model.SaveSymptomsGeneral personId measurementId value
                        |> Backend.Model.MsgAcuteIllnessEncounter id
                        |> App.Model.MsgIndexedDb
                        |> List.singleton
            in
            ( model
            , Cmd.none
            , appMsgs
            )
                |> sequenceExtra (update currentDate site selectedHealthCenter id db) extraMsgs

        SaveSymptomsRespiratory personId saved nextTask ->
            let
                measurementId =
                    Maybe.map Tuple.first saved

                measurement =
                    getMeasurementValueFunc saved

                form =
                    model.symptomsData.symptomsRespiratoryForm

                value =
                    toSymptomsRespiratoryValueWithDefault measurement form

                extraMsgs =
                    generateSymptomsReviewMsgs nextTask

                appMsgs =
                    Backend.AcuteIllnessEncounter.Model.SaveSymptomsRespiratory personId measurementId value
                        |> Backend.Model.MsgAcuteIllnessEncounter id
                        |> App.Model.MsgIndexedDb
                        |> List.singleton
            in
            ( model
            , Cmd.none
            , appMsgs
            )
                |> sequenceExtra (update currentDate site selectedHealthCenter id db) extraMsgs

        SaveSymptomsGI personId saved nextTask ->
            let
                measurementId =
                    Maybe.map Tuple.first saved

                measurement =
                    getMeasurementValueFunc saved

                form =
                    model.symptomsData.symptomsGIForm

                value =
                    toSymptomsGIValueWithDefault measurement form

                extraMsgs =
                    generateSymptomsReviewMsgs nextTask

                appMsgs =
                    Backend.AcuteIllnessEncounter.Model.SaveSymptomsGI personId measurementId value
                        |> Backend.Model.MsgAcuteIllnessEncounter id
                        |> App.Model.MsgIndexedDb
                        |> List.singleton
            in
            ( model
            , Cmd.none
            , appMsgs
            )
                |> sequenceExtra (update currentDate site selectedHealthCenter id db) extraMsgs

        SetActivePhysicalExamTask task ->
            let
                updatedData =
                    model.physicalExamData
                        |> (\data -> { data | activeTask = Just task })
            in
            ( { model | physicalExamData = updatedData }
            , Cmd.none
            , []
            )

        SetVitalsIntInput formUpdateFunc value ->
            let
                updatedData =
                    let
                        form =
                            model.physicalExamData.vitalsForm

                        updatedForm =
                            formUpdateFunc (String.toInt value) form
                    in
                    model.physicalExamData
                        |> (\data -> { data | vitalsForm = updatedForm })
            in
            ( { model | physicalExamData = updatedData }
            , Cmd.none
            , []
            )

        SetVitalsFloatInput formUpdateFunc value ->
            let
                updatedData =
                    let
                        form =
                            model.physicalExamData.vitalsForm

                        updatedForm =
                            formUpdateFunc (String.toFloat value) form
                    in
                    model.physicalExamData
                        |> (\data -> { data | vitalsForm = updatedForm })
            in
            ( { model | physicalExamData = updatedData }
            , Cmd.none
            , []
            )

        SetAcuteFindingsGeneralSign sign ->
            let
                form =
                    acuteFindingsForm

                updatedForm =
                    setMultiSelectInputValue .signsGeneral
                        (\signs -> { form | signsGeneral = signs })
                        NoAcuteFindingsGeneralSigns
                        sign
                        form

                updatedData =
                    model.physicalExamData
                        |> (\data -> { data | acuteFindingsForm = updatedForm })
            in
            ( { model | physicalExamData = updatedData }
            , Cmd.none
            , []
            )

        SetAcuteFindingsRespiratorySign sign ->
            let
                form =
                    acuteFindingsForm

                updatedForm =
                    setMultiSelectInputValue .signsRespiratory
                        (\signs -> { form | signsRespiratory = signs })
                        NoAcuteFindingsRespiratorySigns
                        sign
                        form

                updatedData =
                    model.physicalExamData
                        |> (\data -> { data | acuteFindingsForm = updatedForm })
            in
            ( { model | physicalExamData = updatedData }
            , Cmd.none
            , []
            )

        SaveVitals personId saved nextTask ->
            let
                measurementId =
                    Maybe.map Tuple.first saved

                measurement =
                    getMeasurementValueFunc saved

                extraMsgs =
                    generatePhysicalExamMsgs nextTask

                appMsgs =
                    toVitalsValueWithDefault measurement model.physicalExamData.vitalsForm
                        |> Maybe.map
                            (Backend.AcuteIllnessEncounter.Model.SaveVitals personId measurementId
                                >> Backend.Model.MsgAcuteIllnessEncounter id
                                >> App.Model.MsgIndexedDb
                                >> List.singleton
                            )
                        |> Maybe.withDefault []
            in
            ( model
            , Cmd.none
            , appMsgs
            )
                |> sequenceExtra (update currentDate site selectedHealthCenter id db) extraMsgs

        SaveAcuteFindings personId saved nextTask ->
            let
                measurementId =
                    Maybe.map Tuple.first saved

                measurement =
                    getMeasurementValueFunc saved

                extraMsgs =
                    generatePhysicalExamMsgs nextTask

                appMsgs =
                    toAcuteFindingsValueWithDefault measurement model.physicalExamData.acuteFindingsForm
                        |> Maybe.map
                            (Backend.AcuteIllnessEncounter.Model.SaveAcuteFindings personId measurementId
                                >> Backend.Model.MsgAcuteIllnessEncounter id
                                >> App.Model.MsgIndexedDb
                                >> List.singleton
                            )
                        |> Maybe.withDefault []
            in
            ( model
            , Cmd.none
            , appMsgs
            )
                |> sequenceExtra (update currentDate site selectedHealthCenter id db) extraMsgs

        SetMuac string ->
            let
                form =
                    model.physicalExamData.muacForm

                updatedForm =
                    { form | muac = setMuacValueForSite site string, muacDirty = True }

                updatedData =
                    model.physicalExamData
                        |> (\data -> { data | muacForm = updatedForm })
            in
            ( { model | physicalExamData = updatedData }
            , Cmd.none
            , []
            )

        SaveMuac personId saved nextTask ->
            let
                measurementId =
                    Maybe.map Tuple.first saved

                measurement =
                    getMeasurementValueFunc saved

                extraMsgs =
                    generatePhysicalExamMsgs nextTask

                appMsgs =
                    toMuacValueWithDefault measurement model.physicalExamData.muacForm
                        |> Maybe.map
                            (Backend.AcuteIllnessEncounter.Model.SaveMuac personId measurementId
                                >> Backend.Model.MsgAcuteIllnessEncounter id
                                >> App.Model.MsgIndexedDb
                                >> List.singleton
                            )
                        |> Maybe.withDefault []
            in
            ( model
            , Cmd.none
            , appMsgs
            )
                |> sequenceExtra (update currentDate site selectedHealthCenter id db) extraMsgs

        SetNutritionSign sign ->
            let
                form =
                    resolveFormWithDefaults .nutrition Pages.AcuteIllness.Activity.Utils.nutritionFormWithDefault model.physicalExamData.nutritionForm

                updatedForm =
                    setMultiSelectInputValue .signs
                        (\signs -> { form | signs = signs })
                        NormalChildNutrition
                        sign
                        form

                updatedData =
                    model.physicalExamData
                        |> (\data -> { data | nutritionForm = updatedForm })
            in
            ( { model | physicalExamData = updatedData }
            , Cmd.none
            , []
            )

        SaveNutrition personId saved nextTask ->
            let
                measurementId =
                    Maybe.map Tuple.first saved

                measurement =
                    getMeasurementValueFunc saved

                extraMsgs =
                    generatePhysicalExamMsgs nextTask

                appMsgs =
                    Pages.AcuteIllness.Activity.Utils.toNutritionValueWithDefault measurement model.physicalExamData.nutritionForm
                        |> Maybe.map
                            (Backend.AcuteIllnessEncounter.Model.SaveNutrition personId measurementId
                                >> Backend.Model.MsgAcuteIllnessEncounter id
                                >> App.Model.MsgIndexedDb
                                >> List.singleton
                            )
                        |> Maybe.withDefault []
            in
            ( model
            , Cmd.none
            , appMsgs
            )
                |> sequenceExtra (update currentDate site selectedHealthCenter id db) extraMsgs

        SetCoreExamHeart value ->
            let
                updatedForm =
                    { coreExamForm | heart = Just value }

                updatedData =
                    model.physicalExamData
                        |> (\data -> { data | coreExamForm = updatedForm })
            in
            ( { model | physicalExamData = updatedData }
            , Cmd.none
            , []
            )

        SetCoreExamLungs sign ->
            let
                form =
                    coreExamForm

                updatedForm =
                    setMultiSelectInputValue .lungs
                        (\signs -> { form | lungs = signs })
                        NormalLungs
                        sign
                        form

                updatedData =
                    model.physicalExamData
                        |> (\data -> { data | coreExamForm = updatedForm })
            in
            ( { model | physicalExamData = updatedData }
            , Cmd.none
            , []
            )

        SaveCoreExam personId saved nextTask ->
            let
                measurementId =
                    Maybe.map Tuple.first saved

                measurement =
                    getMeasurementValueFunc saved

                extraMsgs =
                    generatePhysicalExamMsgs nextTask

                appMsgs =
                    toCoreExamValueWithDefault measurement model.physicalExamData.coreExamForm
                        |> Maybe.map
                            (Backend.AcuteIllnessEncounter.Model.SaveCoreExam personId measurementId
                                >> Backend.Model.MsgAcuteIllnessEncounter id
                                >> App.Model.MsgIndexedDb
                                >> List.singleton
                            )
                        |> Maybe.withDefault []
            in
            ( model
            , Cmd.none
            , appMsgs
            )
                |> sequenceExtra (update currentDate site selectedHealthCenter id db) extraMsgs

        SetActiveLaboratoryTask task ->
            let
                updatedData =
                    model.laboratoryData
                        |> (\data -> { data | activeTask = Just task })
            in
            ( { model | laboratoryData = updatedData }
            , Cmd.none
            , []
            )

        SetRapidTestResult value ->
            let
                form =
                    model.laboratoryData.malariaTestingForm

                updatedForm =
                    { form | rapidTestResult = malariaRapidTestResultFromString value, isPregnant = Nothing }

                updatedData =
                    model.laboratoryData
                        |> (\data -> { data | malariaTestingForm = updatedForm })
            in
            ( { model | laboratoryData = updatedData }
            , Cmd.none
            , []
            )

        SetIsPregnant value ->
            let
                form =
                    model.laboratoryData.malariaTestingForm

                updatedForm =
                    { form | isPregnant = Just value }

                updatedData =
                    model.laboratoryData
                        |> (\data -> { data | malariaTestingForm = updatedForm })
            in
            ( { model | laboratoryData = updatedData }
            , Cmd.none
            , []
            )

        SaveMalariaTesting personId saved nextTask ->
            let
                measurementId =
                    Maybe.map Tuple.first saved

                measurement =
                    getMeasurementValueFunc saved

                extraMsgs =
                    generateLaboratoryMsgs nextTask

                appMsgs =
                    toMalariaTestingValueWithDefault measurement model.laboratoryData.malariaTestingForm
                        |> Maybe.map
                            (Backend.AcuteIllnessEncounter.Model.SaveMalariaTesting personId measurementId
                                >> Backend.Model.MsgAcuteIllnessEncounter id
                                >> App.Model.MsgIndexedDb
                                >> List.singleton
                            )
                        |> Maybe.withDefault []
            in
            ( model
            , Cmd.none
            , appMsgs
            )
                |> sequenceExtra (update currentDate site selectedHealthCenter id db) extraMsgs

        SetCovidTestingBoolInput formUpdateFunc value ->
            let
                form =
                    model.laboratoryData.covidTestingForm

                updatedForm =
                    formUpdateFunc value form

                updatedData =
                    model.laboratoryData
                        |> (\data -> { data | covidTestingForm = updatedForm })
            in
            ( { model | laboratoryData = updatedData }
            , Cmd.none
            , []
            )

        SetCovidTestingAdministrationNote note ->
            let
                form =
                    model.laboratoryData.covidTestingForm

                updatedForm =
                    { form | administrationNote = Just note }

                updatedData =
                    model.laboratoryData
                        |> (\data -> { data | covidTestingForm = updatedForm })
            in
            ( { model | laboratoryData = updatedData }
            , Cmd.none
            , []
            )

        SaveCovidTesting personId saved nextTask ->
            let
                measurementId =
                    Maybe.map Tuple.first saved

                measurement =
                    getMeasurementValueFunc saved

                extraMsgs =
                    generateLaboratoryMsgs nextTask

                appMsgs =
                    toCovidTestingValueWithDefault measurement model.laboratoryData.covidTestingForm
                        |> Maybe.map
                            (Backend.AcuteIllnessEncounter.Model.SaveCovidTesting personId measurementId
                                >> Backend.Model.MsgAcuteIllnessEncounter id
                                >> App.Model.MsgIndexedDb
                                >> List.singleton
                            )
                        |> Maybe.withDefault []
            in
            ( model
            , Cmd.none
            , appMsgs
            )
                |> sequenceExtra (update currentDate site selectedHealthCenter id db) extraMsgs

        SetActiveExposureTask task ->
            let
                updatedData =
                    model.exposureData
                        |> (\data -> { data | activeTask = Just task })
            in
            ( { model | exposureData = updatedData }
            , Cmd.none
            , []
            )

        SetCovid19Country value ->
            let
                form =
                    model.exposureData.travelHistoryForm

                updatedForm =
                    { form | covid19Country = Just value }

                updatedData =
                    model.exposureData
                        |> (\data -> { data | travelHistoryForm = updatedForm })
            in
            ( { model | exposureData = updatedData }
            , Cmd.none
            , []
            )

        SaveTravelHistory personId saved nextTask ->
            let
                measurementId =
                    Maybe.map Tuple.first saved

                measurement =
                    getMeasurementValueFunc saved

                extraMsgs =
                    generateExposureMsgs nextTask

                appMsgs =
                    toTravelHistoryValueWithDefault measurement model.exposureData.travelHistoryForm
                        |> Maybe.map
                            (Backend.AcuteIllnessEncounter.Model.SaveTravelHistory personId measurementId
                                >> Backend.Model.MsgAcuteIllnessEncounter id
                                >> App.Model.MsgIndexedDb
                                >> List.singleton
                            )
                        |> Maybe.withDefault []
            in
            ( model
            , Cmd.none
            , appMsgs
            )
                |> sequenceExtra (update currentDate site selectedHealthCenter id db) extraMsgs

        SetCovid19Symptoms value ->
            let
                form =
                    model.exposureData.exposureForm

                updatedForm =
                    { form | covid19Symptoms = Just value }

                updatedData =
                    model.exposureData
                        |> (\data -> { data | exposureForm = updatedForm })
            in
            ( { model | exposureData = updatedData }
            , Cmd.none
            , []
            )

        SaveExposure personId saved nextTask ->
            let
                measurementId =
                    Maybe.map Tuple.first saved

                measurement =
                    getMeasurementValueFunc saved

                extraMsgs =
                    generateExposureMsgs nextTask

                appMsgs =
                    toExposureValueWithDefault measurement model.exposureData.exposureForm
                        |> Maybe.map
                            (Backend.AcuteIllnessEncounter.Model.SaveExposure personId measurementId
                                >> Backend.Model.MsgAcuteIllnessEncounter id
                                >> App.Model.MsgIndexedDb
                                >> List.singleton
                            )
                        |> Maybe.withDefault []
            in
            ( model
            , Cmd.none
            , appMsgs
            )
                |> sequenceExtra (update currentDate site selectedHealthCenter id db) extraMsgs

        SetActivePriorTreatmentTask task ->
            let
                updatedData =
                    model.priorTreatmentData
                        |> (\data -> { data | activeTask = Just task })
            in
            ( { model | priorTreatmentData = updatedData }
            , Cmd.none
            , []
            )

        SetTreatmentReviewBoolInput formUpdateFunc value ->
            let
                updatedData =
                    let
                        updatedForm =
                            formUpdateFunc value model.priorTreatmentData.treatmentReviewForm
                    in
                    model.priorTreatmentData
                        |> (\data -> { data | treatmentReviewForm = updatedForm })
            in
            ( { model | priorTreatmentData = updatedData }
            , Cmd.none
            , []
            )

        SaveTreatmentReview personId saved ->
            let
                measurementId =
                    Maybe.map Tuple.first saved

                measurement =
                    getMeasurementValueFunc saved

                appMsgs =
                    toTreatmentReviewValueWithDefault measurement model.priorTreatmentData.treatmentReviewForm
                        |> unwrap
                            []
                            (\value ->
                                [ Backend.AcuteIllnessEncounter.Model.SaveTreatmentReview personId measurementId value
                                    |> Backend.Model.MsgAcuteIllnessEncounter id
                                    |> App.Model.MsgIndexedDb
                                , App.Model.SetActivePage <| UserPage <| AcuteIllnessEncounterPage id
                                ]
                            )
            in
            ( model
            , Cmd.none
            , appMsgs
            )

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

        SetPatientIsolated value ->
            let
                form =
                    model.nextStepsData.isolationForm

                updatedForm =
                    { form | patientIsolated = Just value, signOnDoor = Nothing }

                updatedData =
                    model.nextStepsData
                        |> (\data -> { data | isolationForm = updatedForm })
            in
            ( { model | nextStepsData = updatedData }
            , Cmd.none
            , []
            )

        SetHealthEducation value ->
            let
                form =
                    model.nextStepsData.isolationForm

                updatedForm =
                    { form | healthEducation = Just value }

                updatedData =
                    model.nextStepsData
                        |> (\data -> { data | isolationForm = updatedForm })
            in
            ( { model | nextStepsData = updatedData }
            , Cmd.none
            , []
            )

        SetSignOnDoor value ->
            let
                form =
                    model.nextStepsData.isolationForm

                updatedForm =
                    { form | signOnDoor = Just value }

                updatedData =
                    model.nextStepsData
                        |> (\data -> { data | isolationForm = updatedForm })
            in
            ( { model | nextStepsData = updatedData }
            , Cmd.none
            , []
            )

        SetReasonForNotIsolating reason ->
            let
                form =
                    Dict.get id db.acuteIllnessMeasurements
                        |> Maybe.withDefault NotAsked
                        |> RemoteData.toMaybe
                        |> Maybe.map
                            (.isolation
                                >> getMeasurementValueFunc
                                >> isolationFormWithDefault model.nextStepsData.isolationForm
                            )
                        |> Maybe.withDefault model.nextStepsData.isolationForm

                updatedForm =
                    setMultiSelectInputValue .reasonsForNotIsolating
                        (\reasons -> { form | reasonsForNotIsolating = reasons })
                        IsolationReasonNotApplicable
                        reason
                        form

                updatedData =
                    model.nextStepsData
                        |> (\data -> { data | isolationForm = updatedForm })
            in
            ( { model | nextStepsData = updatedData }
            , Cmd.none
            , []
            )

        SaveIsolation personId saved nextTask ->
            let
                measurementId =
                    Maybe.map Tuple.first saved

                measurement =
                    getMeasurementValueFunc saved

                extraMsgs =
                    generateNextStepsMsgs nextTask

                appMsgs =
                    toIsolationValueWithDefault measurement model.nextStepsData.isolationForm
                        |> Maybe.map
                            (Backend.AcuteIllnessEncounter.Model.SaveIsolation personId measurementId
                                >> Backend.Model.MsgAcuteIllnessEncounter id
                                >> App.Model.MsgIndexedDb
                                >> List.singleton
                            )
                        |> Maybe.withDefault []
            in
            ( model
            , Cmd.none
            , appMsgs
            )
                |> sequenceExtra (update currentDate site selectedHealthCenter id db) extraMsgs

        SetContactedHC value ->
            let
                form =
                    model.nextStepsData.hcContactForm

                updatedForm =
                    { form | contactedHC = Just value }

                updatedData =
                    model.nextStepsData
                        |> (\data -> { data | hcContactForm = updatedForm })
            in
            ( { model | nextStepsData = updatedData }
            , Cmd.none
            , []
            )

        SetHCRecommendation value ->
            let
                form =
                    model.nextStepsData.hcContactForm

                updatedForm =
                    case form.recommendations of
                        Just period ->
                            if period == value then
                                { form | recommendations = Nothing }

                            else
                                { form | recommendations = Just value }

                        Nothing ->
                            { form | recommendations = Just value }

                updatedData =
                    model.nextStepsData
                        |> (\data -> { data | hcContactForm = updatedForm })
            in
            ( { model | nextStepsData = updatedData }
            , Cmd.none
            , []
            )

        SetResponsePeriod value ->
            let
                form =
                    model.nextStepsData.hcContactForm

                updatedForm =
                    case form.responsePeriod of
                        Just period ->
                            if period == value then
                                { form | responsePeriod = Nothing }

                            else
                                { form | responsePeriod = Just value }

                        Nothing ->
                            { form | responsePeriod = Just value }

                updatedData =
                    model.nextStepsData
                        |> (\data -> { data | hcContactForm = updatedForm })
            in
            ( { model | nextStepsData = updatedData }
            , Cmd.none
            , []
            )

        SetAmbulanceArrivalPeriod value ->
            let
                form =
                    model.nextStepsData.hcContactForm

                updatedForm =
                    case form.ambulanceArrivalPeriod of
                        Just period ->
                            if period == value then
                                { form | ambulanceArrivalPeriod = Nothing }

                            else
                                { form | ambulanceArrivalPeriod = Just value }

                        Nothing ->
                            { form | ambulanceArrivalPeriod = Just value }

                updatedData =
                    model.nextStepsData
                        |> (\data -> { data | hcContactForm = updatedForm })
            in
            ( { model | nextStepsData = updatedData }
            , Cmd.none
            , []
            )

        SaveHCContact personId saved nextTask ->
            let
                measurementId =
                    Maybe.map Tuple.first saved

                measurement =
                    getMeasurementValueFunc saved

                extraMsgs =
                    generateNextStepsMsgs nextTask

                appMsgs =
                    toHCContactValueWithDefault measurement model.nextStepsData.hcContactForm
                        |> Maybe.map
                            (Backend.AcuteIllnessEncounter.Model.SaveHCContact personId measurementId
                                >> Backend.Model.MsgAcuteIllnessEncounter id
                                >> App.Model.MsgIndexedDb
                                >> List.singleton
                            )
                        |> Maybe.withDefault []
            in
            ( model
            , Cmd.none
            , appMsgs
            )
                |> sequenceExtra (update currentDate site selectedHealthCenter id db) extraMsgs

        SetCalled114 value ->
            let
                form =
                    model.nextStepsData.call114Form

                updatedForm =
                    { form
                        | called114 = Just value
                        , recommendation114 = Nothing
                        , recommendation114Dirty = True
                        , contactedSite = Nothing
                        , contactedSiteDirty = True
                        , recommendationSite = Nothing
                        , recommendationSiteDirty = True
                    }

                updatedData =
                    model.nextStepsData
                        |> (\data -> { data | call114Form = updatedForm })
            in
            ( { model | nextStepsData = updatedData }
            , Cmd.none
            , []
            )

        SetRecommendation114 value ->
            let
                form =
                    model.nextStepsData.call114Form

                updatedForm =
                    case form.recommendation114 of
                        Just period ->
                            if period == value then
                                { form
                                    | recommendation114 = Nothing
                                    , recommendation114Dirty = True
                                    , contactedSite = Nothing
                                    , contactedSiteDirty = True
                                    , recommendationSite = Nothing
                                    , recommendationSiteDirty = True
                                }

                            else
                                { form
                                    | recommendation114 = Just value
                                    , recommendation114Dirty = True
                                    , contactedSite = Nothing
                                    , contactedSiteDirty = True
                                    , recommendationSite = Nothing
                                    , recommendationSiteDirty = True
                                }

                        Nothing ->
                            { form
                                | recommendation114 = Just value
                                , recommendation114Dirty = True
                                , contactedSite = Nothing
                                , contactedSiteDirty = True
                                , recommendationSite = Nothing
                                , recommendationSiteDirty = True
                            }

                updatedData =
                    model.nextStepsData
                        |> (\data -> { data | call114Form = updatedForm })
            in
            ( { model | nextStepsData = updatedData }
            , Cmd.none
            , []
            )

        SetContactedSite value ->
            let
                form =
                    model.nextStepsData.call114Form

                updatedForm =
                    { form
                        | contactedSite = Just value
                        , contactedSiteDirty = True
                        , recommendationSite = Nothing
                        , recommendationSiteDirty = True
                    }

                updatedData =
                    model.nextStepsData
                        |> (\data -> { data | call114Form = updatedForm })
            in
            ( { model | nextStepsData = updatedData }
            , Cmd.none
            , []
            )

        SetRecommendationSite value ->
            let
                form =
                    model.nextStepsData.call114Form

                updatedForm =
                    case form.recommendationSite of
                        Just period ->
                            if period == value then
                                { form | recommendationSite = Nothing, recommendationSiteDirty = True }

                            else
                                { form | recommendationSite = Just value, recommendationSiteDirty = True }

                        Nothing ->
                            { form | recommendationSite = Just value, recommendationSiteDirty = True }

                updatedData =
                    model.nextStepsData
                        |> (\data -> { data | call114Form = updatedForm })
            in
            ( { model | nextStepsData = updatedData }
            , Cmd.none
            , []
            )

        SaveCall114 personId saved nextTask ->
            let
                measurementId =
                    Maybe.map Tuple.first saved

                measurement =
                    getMeasurementValueFunc saved

                extraMsgs =
                    generateNextStepsMsgs nextTask

                appMsgs =
                    toCall114ValueWithDefault measurement model.nextStepsData.call114Form
                        |> Maybe.map
                            (Backend.AcuteIllnessEncounter.Model.SaveCall114 personId measurementId
                                >> Backend.Model.MsgAcuteIllnessEncounter id
                                >> App.Model.MsgIndexedDb
                                >> List.singleton
                            )
                        |> Maybe.withDefault []
            in
            ( model
            , Cmd.none
            , appMsgs
            )
                |> sequenceExtra (update currentDate site selectedHealthCenter id db) extraMsgs

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
            let
                measurementId =
                    Maybe.map Tuple.first saved

                measurement =
                    getMeasurementValueFunc saved

                extraMsgs =
                    generateNextStepsMsgs nextTask

                appMsgs =
                    toSendToHCValueWithDefault measurement model.nextStepsData.sendToHCForm
                        |> Maybe.map
                            (Backend.AcuteIllnessEncounter.Model.SaveSendToHC personId measurementId
                                >> Backend.Model.MsgAcuteIllnessEncounter id
                                >> App.Model.MsgIndexedDb
                                >> List.singleton
                            )
                        |> Maybe.withDefault []
            in
            ( model
            , Cmd.none
            , appMsgs
            )
                |> sequenceExtra (update currentDate site selectedHealthCenter id db) extraMsgs

        SetMedicationDistributionBoolInput formUpdateFunc value ->
            let
                updatedData =
                    let
                        updatedForm =
                            formUpdateFunc value model.nextStepsData.medicationDistributionForm
                    in
                    model.nextStepsData
                        |> (\data -> { data | medicationDistributionForm = updatedForm })
            in
            ( { model | nextStepsData = updatedData }
            , Cmd.none
            , []
            )

        SetMedicationDistributionAdministrationNote currentValue medication reason ->
            let
                updatedData =
                    let
                        form =
                            model.nextStepsData.medicationDistributionForm

                        updatedValue =
                            nonAdministrationReasonToSign medication reason

                        updatedNonAdministrationSigns =
                            form.nonAdministrationSigns
                                |> Maybe.map
                                    (\nonAdministrationSigns ->
                                        case currentValue of
                                            Just value ->
                                                EverySet.remove (nonAdministrationReasonToSign medication value) nonAdministrationSigns
                                                    |> EverySet.insert updatedValue

                                            Nothing ->
                                                EverySet.insert updatedValue nonAdministrationSigns
                                    )
                                |> Maybe.withDefault (EverySet.singleton updatedValue)

                        updatedForm =
                            { form | nonAdministrationSigns = Just updatedNonAdministrationSigns }
                    in
                    model.nextStepsData
                        |> (\data -> { data | medicationDistributionForm = updatedForm })
            in
            ( { model | nextStepsData = updatedData }
            , Cmd.none
            , []
            )

        SaveMedicationDistribution personId saved nextTask ->
            let
                measurementId =
                    Maybe.map Tuple.first saved

                measurement =
                    getMeasurementValueFunc saved

                extraMsgs =
                    generateNextStepsMsgs nextTask

                appMsgs =
                    toMedicationDistributionValueWithDefault measurement model.nextStepsData.medicationDistributionForm
                        |> Maybe.map
                            (Backend.AcuteIllnessEncounter.Model.SaveMedicationDistribution personId measurementId
                                >> Backend.Model.MsgAcuteIllnessEncounter id
                                >> App.Model.MsgIndexedDb
                                >> List.singleton
                            )
                        |> Maybe.withDefault []
            in
            ( model
            , Cmd.none
            , appMsgs
            )
                |> sequenceExtra (update currentDate site selectedHealthCenter id db) extraMsgs

        SetActiveOngoingTreatmentTask task ->
            let
                updatedData =
                    model.ongoingTreatmentData
                        |> (\data -> { data | activeTask = Just task })
            in
            ( { model | ongoingTreatmentData = updatedData }
            , Cmd.none
            , []
            )

        SetOngoingTreatmentReviewBoolInput formUpdateFunc value ->
            let
                updatedData =
                    let
                        updatedForm =
                            formUpdateFunc value treatmentReviewForm
                    in
                    model.ongoingTreatmentData
                        |> (\data -> { data | treatmentReviewForm = updatedForm })
            in
            ( { model | ongoingTreatmentData = updatedData }
            , Cmd.none
            , []
            )

        SetReasonForNotTaking value ->
            let
                form =
                    treatmentReviewForm

                updatedForm =
                    { form | reasonForNotTaking = Just value, reasonForNotTakingDirty = True }

                updatedData =
                    model.ongoingTreatmentData
                        |> (\data -> { data | treatmentReviewForm = updatedForm })
            in
            ( { model | ongoingTreatmentData = updatedData }
            , Cmd.none
            , []
            )

        SetTotalMissedDoses value ->
            let
                form =
                    treatmentReviewForm

                updatedForm =
                    { form | totalMissedDoses = String.toInt value, totalMissedDosesDirty = True }

                updatedData =
                    model.ongoingTreatmentData
                        |> (\data -> { data | treatmentReviewForm = updatedForm })
            in
            ( { model | ongoingTreatmentData = updatedData }
            , Cmd.none
            , []
            )

        SetAdverseEvent event ->
            let
                form =
                    treatmentReviewForm

                updatedForm =
                    setMultiSelectInputValue .adverseEvents
                        (\events -> { form | adverseEvents = events, adverseEventsDirty = True })
                        NoAdverseEvent
                        event
                        form

                updatedData =
                    model.ongoingTreatmentData
                        |> (\data -> { data | treatmentReviewForm = updatedForm })
            in
            ( { model | ongoingTreatmentData = updatedData }
            , Cmd.none
            , []
            )

        SaveOngoingTreatmentReview personId saved ->
            let
                measurementId =
                    Maybe.map Tuple.first saved

                measurement =
                    getMeasurementValueFunc saved

                appMsgs =
                    toOngoingTreatmentReviewValueWithDefault measurement model.ongoingTreatmentData.treatmentReviewForm
                        |> unwrap
                            []
                            (\value ->
                                [ Backend.AcuteIllnessEncounter.Model.SaveTreatmentOngoing personId measurementId value
                                    |> Backend.Model.MsgAcuteIllnessEncounter id
                                    |> App.Model.MsgIndexedDb
                                , App.Model.SetActivePage <| UserPage <| AcuteIllnessEncounterPage id
                                ]
                            )
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

        SetConditionImproving value ->
            let
                form =
                    model.dangerSignsData.reviewDangerSignsForm

                updatedForm =
                    { form | conditionImproving = Just value }

                updatedData =
                    model.dangerSignsData
                        |> (\data -> { data | reviewDangerSignsForm = updatedForm })
            in
            ( { model | dangerSignsData = updatedData }
            , Cmd.none
            , []
            )

        SetDangerSign sign ->
            let
                form =
                    resolveFormWithDefaults .dangerSigns reviewDangerSignsFormWithDefault model.dangerSignsData.reviewDangerSignsForm

                updatedForm =
                    setMultiSelectInputValue .symptoms
                        (\signs -> { form | symptoms = signs })
                        NoAcuteIllnessDangerSign
                        sign
                        form

                updatedData =
                    model.dangerSignsData
                        |> (\data -> { data | reviewDangerSignsForm = updatedForm })
            in
            ( { model | dangerSignsData = updatedData }
            , Cmd.none
            , []
            )

        SaveReviewDangerSigns personId saved ->
            let
                measurementId =
                    Maybe.map Tuple.first saved

                measurement =
                    getMeasurementValueFunc saved

                appMsgs =
                    toReviewDangerSignsValueWithDefault measurement model.dangerSignsData.reviewDangerSignsForm
                        |> unwrap
                            []
                            (\value ->
                                [ Backend.AcuteIllnessEncounter.Model.SaveDangerSigns personId measurementId value
                                    |> Backend.Model.MsgAcuteIllnessEncounter id
                                    |> App.Model.MsgIndexedDb
                                , App.Model.SetActivePage <| UserPage <| AcuteIllnessEncounterPage id
                                ]
                            )
            in
            ( model
            , Cmd.none
            , appMsgs
            )

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
            let
                measurementId =
                    Maybe.map Tuple.first saved

                measurement =
                    getMeasurementValueFunc saved

                extraMsgs =
                    generateNextStepsMsgs nextTask

                appMsgs =
                    toHealthEducationValueWithDefault measurement model.nextStepsData.healthEducationForm
                        |> Maybe.map
                            (Backend.AcuteIllnessEncounter.Model.SaveHealthEducation personId measurementId
                                >> Backend.Model.MsgAcuteIllnessEncounter id
                                >> App.Model.MsgIndexedDb
                                >> List.singleton
                            )
                        |> Maybe.withDefault []
            in
            ( model
            , Cmd.none
            , appMsgs
            )
                |> sequenceExtra (update currentDate site selectedHealthCenter id db) extraMsgs

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

        SaveFollowUp personId diagnosis saved nextTask ->
            let
                measurementId =
                    Maybe.map Tuple.first saved

                measurement =
                    getMeasurementValueFunc saved

                extraMsgs =
                    generateNextStepsMsgs nextTask

                followUpForm =
                    model.nextStepsData.followUpForm

                appMsgs =
                    toFollowUpValueWithDefault measurement { followUpForm | diagnosis = diagnosis }
                        |> Maybe.map
                            (Backend.AcuteIllnessEncounter.Model.SaveFollowUp personId measurementId
                                >> Backend.Model.MsgAcuteIllnessEncounter id
                                >> App.Model.MsgIndexedDb
                                >> List.singleton
                            )
                        |> Maybe.withDefault []
            in
            ( model
            , Cmd.none
            , appMsgs
            )
                |> sequenceExtra (update currentDate site selectedHealthCenter id db) extraMsgs

        SetContactsTracingFormState newState ->
            let
                form =
                    model.nextStepsData.contactsTracingForm

                updatedForm =
                    { form | state = newState }

                updatedData =
                    model.nextStepsData
                        |> (\data -> { data | contactsTracingForm = updatedForm })

                fetchPersonMsg =
                    case updatedForm.state of
                        ContactsTracingFormRecordContactDetails personId _ ->
                            [ Backend.Model.FetchPerson personId |> App.Model.MsgIndexedDb ]

                        _ ->
                            []
            in
            ( { model | nextStepsData = updatedData }
            , Cmd.none
            , fetchPersonMsg
            )

        MsgPatientsSearchForm subMsg ->
            let
                form =
                    model.nextStepsData.contactsTracingForm
            in
            case form.state of
                ContactsTracingFormSearchParticipants searchData ->
                    let
                        ( updatedSearchData, cmd ) =
                            Components.PatientsSearchForm.Update.update subMsg searchData

                        updatedForm =
                            { form | state = ContactsTracingFormSearchParticipants updatedSearchData }

                        updatedData =
                            model.nextStepsData
                                |> (\data -> { data | contactsTracingForm = updatedForm })
                    in
                    ( { model | nextStepsData = updatedData }
                    , Cmd.map MsgPatientsSearchForm cmd
                    , []
                    )

                _ ->
                    noChange

        SetContactsTracingDate date ->
            let
                form =
                    model.nextStepsData.contactsTracingForm
            in
            case form.state of
                ContactsTracingFormRecordContactDetails personId recordData ->
                    let
                        updatedRecordData =
                            { recordData | contactDate = Just date }

                        updatedForm =
                            { form | state = ContactsTracingFormRecordContactDetails personId updatedRecordData }

                        updatedData =
                            model.nextStepsData
                                |> (\data -> { data | contactsTracingForm = updatedForm })
                    in
                    ( { model | nextStepsData = updatedData }
                    , Cmd.none
                    , [ focusOnCalendarMsg ]
                    )

                _ ->
                    noChange

        SetContactsTracingDateSelectorState state ->
            let
                form =
                    model.nextStepsData.contactsTracingForm
            in
            case form.state of
                ContactsTracingFormRecordContactDetails personId recordData ->
                    let
                        updatedRecordData =
                            { recordData | dateSelectorPopupState = state }

                        updatedForm =
                            { form | state = ContactsTracingFormRecordContactDetails personId updatedRecordData }

                        updatedData =
                            model.nextStepsData
                                |> (\data -> { data | contactsTracingForm = updatedForm })
                    in
                    ( { model | nextStepsData = updatedData }
                    , Cmd.none
                    , []
                    )

                _ ->
                    noChange

        SetContactsTracingPhoneNumber value ->
            let
                form =
                    model.nextStepsData.contactsTracingForm
            in
            case form.state of
                ContactsTracingFormRecordContactDetails personId recordData ->
                    let
                        updatedPhoneNumber =
                            -- Allow empty value, or digits only.
                            if String.isEmpty value || isJust (String.toInt value) then
                                Just value

                            else
                                recordData.phoneNumber

                        updatedRecordData =
                            { recordData | phoneNumber = updatedPhoneNumber }

                        updatedForm =
                            { form | state = ContactsTracingFormRecordContactDetails personId updatedRecordData }

                        updatedData =
                            model.nextStepsData
                                |> (\data -> { data | contactsTracingForm = updatedForm })
                    in
                    ( { model | nextStepsData = updatedData }
                    , Cmd.none
                    , []
                    )

                _ ->
                    noChange

        SetContactsTracingFinished ->
            let
                form =
                    model.nextStepsData.contactsTracingForm

                contacts =
                    if isNothing form.contacts then
                        -- When there're no contacts, we
                        -- send an empty distionary.
                        Just Dict.empty

                    else
                        form.contacts

                updatedForm =
                    { form | contacts = contacts, finished = True }

                updatedData =
                    model.nextStepsData
                        |> (\data -> { data | contactsTracingForm = updatedForm })
            in
            ( { model | nextStepsData = updatedData }
            , Cmd.none
            , []
            )

        SaveTracedContact item ->
            let
                updatedContacts =
                    Maybe.map (Dict.insert item.personId item) contactsTracingForm.contacts
                        |> Maybe.withDefault (Dict.singleton item.personId item)

                updatedForm =
                    { contactsTracingForm | contacts = Just updatedContacts, state = ContactsTracingFormSummary }

                updatedData =
                    model.nextStepsData
                        |> (\data -> { data | contactsTracingForm = updatedForm })
            in
            ( { model | nextStepsData = updatedData }
            , Cmd.none
            , []
            )

        DeleteTracedContact personId ->
            let
                updatedContacts =
                    Maybe.map (Dict.remove personId) contactsTracingForm.contacts

                updatedForm =
                    { contactsTracingForm | contacts = updatedContacts }

                updatedData =
                    model.nextStepsData
                        |> (\data -> { data | contactsTracingForm = updatedForm })
            in
            ( { model | nextStepsData = updatedData }
            , Cmd.none
            , []
            )

        RegisterContactMsgForm subMsg ->
            let
                form =
                    model.nextStepsData.contactsTracingForm
            in
            case form.state of
                ContactsTracingFormRegisterContact registrationData ->
                    let
                        updatedRegistrationData =
                            Form.update (Backend.Person.Form.validateContact site) subMsg registrationData

                        appMsgs =
                            case subMsg of
                                Form.Submit ->
                                    let
                                        initiator =
                                            Backend.Person.Model.AcuteIllnessContactsTracingActivityOrigin id
                                    in
                                    Form.getOutput registrationData
                                        |> Maybe.map
                                            (\person ->
                                                let
                                                    personForCreate =
                                                        { person | healthCenterId = resolvedHealthCenterId, shard = selectedHealthCenter }

                                                    resolvedHealthCenterId =
                                                        Maybe.Extra.or healthCenterIdByGeoFields selectedHealthCenter

                                                    healthCenterIdByGeoFields =
                                                        Maybe.map5 (getVillageIdByGeoFields db)
                                                            person.province
                                                            person.district
                                                            person.sector
                                                            person.cell
                                                            person.village
                                                            |> Maybe.Extra.join
                                                            |> Maybe.andThen (\villageId -> getVillageHealthCenterId villageId db)
                                                in
                                                [ Backend.Model.PostPerson Nothing initiator personForCreate
                                                    |> App.Model.MsgIndexedDb
                                                ]
                                            )
                                        -- If we submit, but can't actually submit,
                                        -- then change the request status to
                                        -- `NotAsked` (to reset network errors
                                        -- etc.)
                                        |> Maybe.withDefault
                                            [ Backend.Model.HandlePostedPerson Nothing initiator NotAsked
                                                |> App.Model.MsgIndexedDb
                                            ]

                                _ ->
                                    []

                        updatedForm =
                            { form | state = ContactsTracingFormRegisterContact updatedRegistrationData }

                        updatedData =
                            model.nextStepsData
                                |> (\data -> { data | contactsTracingForm = updatedForm })
                    in
                    ( { model | nextStepsData = updatedData }
                    , Cmd.none
                    , appMsgs
                    )

                _ ->
                    noChange

        SaveContactsTracing personId saved nextTask ->
            let
                measurementId =
                    Maybe.map Tuple.first saved

                measurement =
                    getMeasurementValueFunc saved

                extraMsgs =
                    generateNextStepsMsgs nextTask

                appMsgs =
                    toContactsTracingValueWithDefault measurement model.nextStepsData.contactsTracingForm
                        |> Maybe.map
                            (Backend.AcuteIllnessEncounter.Model.SaveContactsTracing personId measurementId
                                >> Backend.Model.MsgAcuteIllnessEncounter id
                                >> App.Model.MsgIndexedDb
                                >> List.singleton
                            )
                        |> Maybe.withDefault []
            in
            ( model
            , Cmd.none
            , appMsgs
            )
                |> sequenceExtra (update currentDate site selectedHealthCenter id db) extraMsgs
