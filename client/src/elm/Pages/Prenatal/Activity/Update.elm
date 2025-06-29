module Pages.Prenatal.Activity.Update exposing (update)

import App.Model
import AssocList as Dict
import Backend.Entities exposing (PrenatalEncounterId)
import Backend.IndividualEncounterParticipant.Model
import Backend.Measurement.Model
    exposing
        ( AbdomenCPESign(..)
        , AdministrationNote(..)
        , BreastExamSign(..)
        , DangerSign(..)
        , FamilyPlanningSign(..)
        , HandsCPESign(..)
        , IllnessSymptom(..)
        , ImageUrl(..)
        , LegsCPESign(..)
        , LungsCPESign(..)
        , MedicalHistoryInfectiousDisease(..)
        , MedicalHistoryMentalHealthIssue(..)
        , MedicalHistoryPhysicalCondition(..)
        , MedicalHistorySign(..)
        , NeckCPESign(..)
        , ObstetricHistoryStep2Sign(..)
        , OutsideCareMedication(..)
        , PhaseRecorded(..)
        , PostpartumChildDangerSign(..)
        , PostpartumHealingProblem(..)
        , PostpartumMotherDangerSign(..)
        , PrenatalSymptom(..)
        , VaginalExamSign(..)
        , ViralLoadStatus(..)
        )
import Backend.Measurement.Utils exposing (..)
import Backend.Model exposing (ModelIndexedDb)
import Backend.PrenatalEncounter.Model
import Backend.PrenatalEncounter.Types exposing (PrenatalDiagnosis(..))
import Backend.PrenatalEncounter.Utils exposing (lmpToEDDDate)
import Date
import EverySet
import Gizra.NominalDate exposing (NominalDate)
import Gizra.Update exposing (sequenceExtra)
import Maybe.Extra exposing (unwrap)
import Measurement.Model exposing (VaccinationFormViewMode(..))
import Measurement.Utils
    exposing
        ( corePhysicalExamFormWithDefault
        , familyPlanningFormWithDefault
        , outsideCareFormWithDefault
        , syphilisTestFormWithDefault
        , toAdministrationNoteWithDefault
        , toBloodGpRsTestValueWithDefault
        , toCorePhysicalExamValueWithDefault
        , toFamilyPlanningValueWithDefault
        , toHIVPCRTestValueWithDefault
        , toHIVTestValueUniversalWithDefault
        , toHemoglobinTestValueWithDefault
        , toHepatitisBTestValueWithDefault
        , toMalariaTestValueWithDefault
        , toOutsideCareValueWithDefault
        , toPartnerHIVTestValueWithDefault
        , toRandomBloodSugarTestValueUniversalWithDefault
        , toSyphilisTestValueWithDefault
        , toUrineDipstickTestValueUniversalWithDefault
        , toVaccinationValueWithDefault
        , toVitalsValueWithDefault
        , vaccinationFormWithDefault
        , vaccineDoseToComparable
        )
import Pages.Page exposing (Page(..), UserPage(..))
import Pages.Prenatal.Activity.Model exposing (..)
import Pages.Prenatal.Activity.Types exposing (..)
import Pages.Prenatal.Activity.Utils exposing (..)
import Pages.Prenatal.Utils exposing (..)
import Pages.Utils exposing (insertIntoSet, nonAdministrationReasonToSign, setMultiSelectInputValue, tasksBarId)
import RemoteData exposing (RemoteData(..))
import Translate exposing (Language)


update : Language -> NominalDate -> PrenatalEncounterId -> Bool -> ModelIndexedDb -> Msg -> Model -> ( Model, Cmd Msg, List App.Model.Msg )
update language currentDate id isLabTech db msg model =
    let
        obstetricFormSecondStep =
            Dict.get id db.prenatalMeasurements
                |> Maybe.andThen RemoteData.toMaybe
                |> Maybe.map
                    (.obstetricHistoryStep2
                        >> getMeasurementValueFunc
                        >> obstetricHistoryStep2FormWithDefault model.historyData.obstetricFormSecondStep
                    )
                |> Maybe.withDefault model.historyData.obstetricFormSecondStep

        medicalHistoryForm =
            Dict.get id db.prenatalMeasurements
                |> Maybe.andThen RemoteData.toMaybe
                |> Maybe.map
                    (.medicalHistory
                        >> getMeasurementValueFunc
                        >> medicalHistoryFormWithDefault model.historyData.medicalForm
                    )
                |> Maybe.withDefault model.historyData.medicalForm

        corePhysicalExamForm =
            Dict.get id db.prenatalMeasurements
                |> Maybe.andThen RemoteData.toMaybe
                |> Maybe.map
                    (.corePhysicalExam
                        >> getMeasurementValueFunc
                        >> corePhysicalExamFormWithDefault model.examinationData.corePhysicalExamForm
                    )
                |> Maybe.withDefault model.examinationData.corePhysicalExamForm

        medicationDistributionForm =
            Dict.get id db.prenatalMeasurements
                |> Maybe.andThen RemoteData.toMaybe
                |> Maybe.map
                    (.medicationDistribution
                        >> getMeasurementValueFunc
                        >> medicationDistributionFormWithDefaultInitialPhase model.nextStepsData.medicationDistributionForm
                    )
                |> Maybe.withDefault model.nextStepsData.medicationDistributionForm

        outsideCareForm =
            Dict.get id db.prenatalMeasurements
                |> Maybe.andThen RemoteData.toMaybe
                |> Maybe.map
                    (.outsideCare
                        >> getMeasurementValueFunc
                        >> outsideCareFormWithDefault model.historyData.outsideCareForm
                    )
                |> Maybe.withDefault model.historyData.outsideCareForm

        resolveVaccinationForm vaccineType form =
            Dict.get id db.prenatalMeasurements
                |> Maybe.andThen RemoteData.toMaybe
                |> Maybe.map
                    (getMeasurementByVaccineTypeFunc vaccineType
                        >> vaccinationFormWithDefault form
                    )
                |> Maybe.withDefault form

        guExamForm =
            Dict.get id db.prenatalMeasurements
                |> Maybe.andThen RemoteData.toMaybe
                |> Maybe.map
                    (.guExam
                        >> getMeasurementValueFunc
                        >> guExamFormWithDefault model.examinationData.guExamForm
                    )
                |> Maybe.withDefault model.examinationData.guExamForm

        generateLaboratoryMsgs nextTask =
            Maybe.map (\task -> [ SetActiveLaboratoryTask task ]) nextTask
                |> Maybe.withDefault [ SetActivePage <| UserPage <| PrenatalEncounterPage id ]

        generateNextStepsMsgs secondPhaseRequired nextTask =
            let
                destinationPage =
                    if secondPhaseRequired then
                        UserPage <| PrenatalEncounterPage id

                    else
                        UserPage <| ClinicalProgressReportPage (Backend.PrenatalEncounter.Model.InitiatorEncounterPage id) id
            in
            Maybe.map (\task -> [ SetActiveNextStepsTask task ]) nextTask
                |> Maybe.withDefault [ SetActivePage destinationPage ]

        generateMedicationSubActivityMsgs nextTask =
            SetWarningPopupState Nothing
                :: (Maybe.map (\task -> [ SetActiveTreatmentReviewTask task ]) nextTask
                        |> Maybe.withDefault [ SetActivePage <| UserPage <| PrenatalEncounterPage id ]
                   )

        generateHistoryMsgs nextTask =
            Maybe.map (\task -> [ SetActiveHistoryTask task ]) nextTask
                |> Maybe.withDefault [ SetActivePage <| UserPage <| PrenatalEncounterPage id ]

        generateExaminationMsgs nextTask =
            Maybe.map (\task -> [ SetActiveExaminationTask task ]) nextTask
                |> Maybe.withDefault [ SetActivePage <| UserPage <| PrenatalEncounterPage id ]

        generateMedicationMsgs nextTask =
            Maybe.map (\task -> [ SetActiveMedicationTask task ]) nextTask
                |> Maybe.withDefault [ SetActivePage <| UserPage <| PrenatalEncounterPage id ]
    in
    case msg of
        NoOp ->
            ( model, Cmd.none, [] )

        DropZoneComplete result ->
            let
                updatedData =
                    model.prenatalPhotoData
                        |> (\data -> { data | url = Just (ImageUrl result.url) })
            in
            ( { model | prenatalPhotoData = updatedData }
            , Cmd.none
            , []
            )

        SetActivePage page ->
            ( model
            , Cmd.none
            , [ App.Model.SetActivePage page ]
            )

        SetAlertsDialogState isOpen ->
            ( { model | showAlertsDialog = isOpen }, Cmd.none, [] )

        SetWarningPopupState state ->
            ( { model | warningPopupState = state }, Cmd.none, [] )

        SetLmpDateSelectorState state ->
            let
                form =
                    model.pregnancyDatingData.form

                defaultSelection =
                    Maybe.Extra.or form.lmpDate (Maybe.andThen .dateDefault state)

                updatedForm =
                    { form | dateSelectorPopupState = state, lmpDate = defaultSelection }

                updatedData =
                    model.pregnancyDatingData
                        |> (\data -> { data | form = updatedForm })
            in
            ( { model | pregnancyDatingData = updatedData }
            , Cmd.none
            , []
            )

        SetConfirmLmpDate originalValue confirmed ->
            let
                updatedForm =
                    if confirmed then
                        model.pregnancyDatingData.form
                            |> (\form ->
                                    { form
                                        | chwLmpConfirmation = Just True
                                        , lmpDate = Just originalValue.date
                                        , lmpDateConfident = Just originalValue.confident
                                        , lmpDateNotConfidentReason = originalValue.notConfidentReason
                                    }
                               )

                    else
                        { emptyPregnancyDatingForm | chwLmpConfirmation = Just False }

                updatedData =
                    model.pregnancyDatingData
                        |> (\data -> { data | form = updatedForm })
            in
            ( { model | pregnancyDatingData = updatedData }
            , Cmd.none
            , []
            )

        SetLmpDate value ->
            let
                updatedForm =
                    model.pregnancyDatingData.form
                        |> (\form -> { form | lmpDate = Just value })

                updatedData =
                    model.pregnancyDatingData
                        |> (\data -> { data | form = updatedForm })
            in
            ( { model | pregnancyDatingData = updatedData }
            , Cmd.none
            , []
            )

        SetLmpDateConfident value ->
            let
                updatedForm =
                    model.pregnancyDatingData.form
                        |> (\form ->
                                { form
                                    | lmpDateConfident = Just value
                                    , lmpDateNotConfidentReason = Nothing
                                }
                           )

                updatedData =
                    model.pregnancyDatingData
                        |> (\data -> { data | form = updatedForm })
            in
            ( { model | pregnancyDatingData = updatedData }
            , Cmd.none
            , []
            )

        SetLmpDateNotConfidentReason value ->
            let
                updatedForm =
                    model.pregnancyDatingData.form
                        |> (\form -> { form | lmpDateNotConfidentReason = Just value })

                updatedData =
                    model.pregnancyDatingData
                        |> (\data -> { data | form = updatedForm })
            in
            ( { model | pregnancyDatingData = updatedData }
            , Cmd.none
            , []
            )

        SetPrePregnancyWeight value ->
            let
                updatedForm =
                    model.pregnancyDatingData.form
                        |> (\form -> { form | prePregnancyWeight = String.toFloat value, prePregnancyWeightDirty = True })

                updatedData =
                    model.pregnancyDatingData
                        |> (\data -> { data | form = updatedForm })
            in
            ( { model | pregnancyDatingData = updatedData }
            , Cmd.none
            , []
            )

        SavePregnancyDating prenatalParticipantId personId saved ->
            let
                measurementId =
                    Maybe.map Tuple.first saved

                measurement =
                    getMeasurementValueFunc saved

                appMsgs =
                    model.pregnancyDatingData.form
                        |> toLastMenstrualPeriodValueWithDefault measurement
                        |> unwrap
                            []
                            (\lastMenstrualPeriodValue ->
                                [ Backend.PrenatalEncounter.Model.SaveLastMenstrualPeriod personId measurementId lastMenstrualPeriodValue
                                    |> Backend.Model.MsgPrenatalEncounter id
                                    |> App.Model.MsgIndexedDb
                                , -- We store EDD date on pregnancy, to be able
                                  -- to decide that pregnancy has ended even if end date was
                                  -- not set - that is when we're 3 month past EDD date.
                                  lmpToEDDDate lastMenstrualPeriodValue.date
                                    |> Backend.IndividualEncounterParticipant.Model.SetEddDate
                                    |> Backend.Model.MsgIndividualEncounterParticipant prenatalParticipantId
                                    |> App.Model.MsgIndexedDb
                                , PrenatalEncounterPage id |> UserPage |> App.Model.SetActivePage
                                ]
                            )
            in
            ( model
            , Cmd.none
            , appMsgs
            )

        SetActiveHistoryTask task ->
            let
                updatedData =
                    model.historyData
                        |> (\data -> { data | activeTask = Just task })
            in
            ( { model | historyData = updatedData }
            , Cmd.none
            , []
            )

        SetCurrentlyPregnant value ->
            let
                form =
                    model.historyData.obstetricFormFirstStep

                updatedForm =
                    { form | currentlyPregnant = Just value }

                updatedData =
                    model.historyData
                        |> (\data -> { data | obstetricFormFirstStep = updatedForm })
            in
            ( { model | historyData = updatedData }
            , Cmd.none
            , []
            )

        SetOBIntInput formUpdateFunc value ->
            let
                form =
                    model.historyData.obstetricFormFirstStep

                updatedForm =
                    formUpdateFunc (String.toInt value) form

                updatedData =
                    model.historyData
                        |> (\data -> { data | obstetricFormFirstStep = updatedForm })
            in
            ( { model | historyData = updatedData }
            , Cmd.none
            , []
            )

        SaveOBHistoryStep1 skipSecondStep personId saved nextTask ->
            let
                measurementId =
                    Maybe.map Tuple.first saved

                measurement =
                    getMeasurementValueFunc saved

                ( extraMsgs, updatedData ) =
                    if skipSecondStep then
                        ( generateHistoryMsgs nextTask
                        , model.historyData
                        )

                    else
                        ( []
                        , model.historyData
                            |> (\data -> { data | obstetricHistoryStep = ObstetricHistorySecondStep })
                        )

                appMsgs =
                    model.historyData.obstetricFormFirstStep
                        |> toObstetricHistoryValueWithDefault measurement
                        |> Maybe.map
                            (Backend.PrenatalEncounter.Model.SaveObstetricHistory personId measurementId
                                >> Backend.Model.MsgPrenatalEncounter id
                                >> App.Model.MsgIndexedDb
                                >> List.singleton
                            )
                        |> Maybe.withDefault []
            in
            ( { model | historyData = updatedData }
            , Cmd.none
            , appMsgs
            )
                |> sequenceExtra (update language currentDate id isLabTech db) extraMsgs

        SetCSectionReason reason ->
            let
                form =
                    model.historyData.obstetricFormSecondStep

                updatedReason =
                    if form.cSectionReason == Just reason then
                        Nothing

                    else
                        Just reason

                updatedForm =
                    { form | cSectionReason = updatedReason, cSectionReasonDirty = True }

                updatedData =
                    model.historyData
                        |> (\data -> { data | obstetricFormSecondStep = updatedForm })
            in
            ( { model | historyData = updatedData }
            , Cmd.none
            , []
            )

        SetOBBoolInput formUpdateFunc value ->
            let
                updatedForm =
                    formUpdateFunc value model.historyData.obstetricFormSecondStep

                updatedData =
                    model.historyData
                        |> (\data -> { data | obstetricFormSecondStep = updatedForm })
            in
            ( { model | historyData = updatedData }
            , Cmd.none
            , []
            )

        SetPreviousDeliveryPeriod period ->
            let
                form =
                    model.historyData.obstetricFormSecondStep

                updatedPeriod =
                    if form.previousDeliveryPeriod == Just period then
                        Nothing

                    else
                        Just period

                updatedForm =
                    { form | previousDeliveryPeriod = updatedPeriod }

                updatedData =
                    model.historyData
                        |> (\data -> { data | obstetricFormSecondStep = updatedForm })
            in
            ( { model | historyData = updatedData }
            , Cmd.none
            , []
            )

        SetObstetricFormSecondStepSign sign ->
            let
                form =
                    obstetricFormSecondStep

                updatedForm =
                    setMultiSelectInputValue .signs
                        (\signs -> { form | signs = signs })
                        NoObstetricHistoryStep2Sign
                        sign
                        form

                updatedData =
                    model.historyData
                        |> (\data -> { data | obstetricFormSecondStep = updatedForm })
            in
            ( { model | historyData = updatedData }
            , Cmd.none
            , []
            )

        BackToOBHistoryStep1 ->
            let
                updatedData =
                    model.historyData
                        |> (\data -> { data | obstetricHistoryStep = ObstetricHistoryFirstStep })
            in
            ( { model | historyData = updatedData }
            , Cmd.none
            , []
            )

        SaveOBHistoryStep2 personId saved nextTask ->
            let
                measurementId =
                    Maybe.map Tuple.first saved

                measurement =
                    getMeasurementValueFunc saved

                extraMsgs =
                    generateHistoryMsgs nextTask

                ( appMsgs, updatedData ) =
                    ( model.historyData.obstetricFormSecondStep
                        |> toObstetricHistoryStep2ValueWithDefault measurement
                        |> Maybe.map
                            (Backend.PrenatalEncounter.Model.SaveObstetricHistoryStep2 personId measurementId
                                >> Backend.Model.MsgPrenatalEncounter id
                                >> App.Model.MsgIndexedDb
                                >> List.singleton
                            )
                        |> Maybe.withDefault []
                    , model.historyData
                        |> (\data -> { data | obstetricHistoryStep = ObstetricHistoryFirstStep })
                    )
            in
            ( { model | historyData = updatedData }
            , Cmd.none
            , App.Model.ScrollToElement tasksBarId :: appMsgs
            )
                |> sequenceExtra (update language currentDate id isLabTech db) extraMsgs

        SetMedicalHistorySigns value ->
            let
                form =
                    medicalHistoryForm

                updatedForm =
                    setMultiSelectInputValue .signs
                        (\signs -> { form | signs = signs })
                        NoMedicalHistorySigns
                        value
                        form

                updatedData =
                    model.historyData
                        |> (\data -> { data | medicalForm = updatedForm })
            in
            ( { model | historyData = updatedData }
            , Cmd.none
            , []
            )

        SetMedicalHistoryPhysicalCondition value ->
            let
                form =
                    medicalHistoryForm

                updatedForm =
                    setMultiSelectInputValue .physicalConditions
                        (\physicalConditions -> { form | physicalConditions = physicalConditions })
                        NoMedicalHistoryPhysicalCondition
                        value
                        form

                updatedData =
                    model.historyData
                        |> (\data -> { data | medicalForm = updatedForm })
            in
            ( { model | historyData = updatedData }
            , Cmd.none
            , []
            )

        SetMedicalHistoryInfectiousDisease value ->
            let
                form =
                    medicalHistoryForm

                updatedForm =
                    setMultiSelectInputValue .infectiousDiseases
                        (\infectiousDiseases -> { form | infectiousDiseases = infectiousDiseases })
                        NoMedicalHistoryInfectiousDisease
                        value
                        form

                updatedData =
                    model.historyData
                        |> (\data -> { data | medicalForm = updatedForm })
            in
            ( { model | historyData = updatedData }
            , Cmd.none
            , []
            )

        SetMedicalHistoryMentalHealthIssue value ->
            let
                form =
                    medicalHistoryForm

                updatedForm =
                    setMultiSelectInputValue .mentalHealthIssues
                        (\mentalHealthIssues -> { form | mentalHealthIssues = mentalHealthIssues })
                        NoMedicalHistoryMentalHealthIssue
                        value
                        form

                updatedData =
                    model.historyData
                        |> (\data -> { data | medicalForm = updatedForm })
            in
            ( { model | historyData = updatedData }
            , Cmd.none
            , []
            )

        SaveMedicalHistory personId saved nextTask ->
            let
                measurementId =
                    Maybe.map Tuple.first saved

                measurement =
                    getMeasurementValueFunc saved

                extraMsgs =
                    generateHistoryMsgs nextTask

                appMsgs =
                    model.historyData.medicalForm
                        |> toMedicalHistoryValueWithDefault measurement
                        |> Maybe.map
                            (Backend.PrenatalEncounter.Model.SaveMedicalHistory personId measurementId
                                >> Backend.Model.MsgPrenatalEncounter id
                                >> App.Model.MsgIndexedDb
                                >> List.singleton
                            )
                        |> Maybe.withDefault []
            in
            ( model
            , Cmd.none
            , appMsgs
            )
                |> sequenceExtra (update language currentDate id isLabTech db) extraMsgs

        SetSocialBoolInput formUpdateFunc value ->
            let
                updatedData =
                    let
                        updatedForm =
                            formUpdateFunc value model.historyData.socialForm
                    in
                    model.historyData
                        |> (\data -> { data | socialForm = updatedForm })
            in
            ( { model | historyData = updatedData }
            , Cmd.none
            , []
            )

        SaveSocialHistory personId saved nextTask ->
            let
                measurementId =
                    Maybe.map Tuple.first saved

                measurement =
                    getMeasurementValueFunc saved

                extraMsgs =
                    generateHistoryMsgs nextTask

                appMsgs =
                    toSocialHistoryValueWithDefault measurement model.historyData.socialForm
                        |> Maybe.map
                            (Backend.PrenatalEncounter.Model.SaveSocialHistory personId measurementId
                                >> Backend.Model.MsgPrenatalEncounter id
                                >> App.Model.MsgIndexedDb
                                >> List.singleton
                            )
                        |> Maybe.withDefault []
            in
            ( model
            , Cmd.none
            , appMsgs
            )
                |> sequenceExtra (update language currentDate id isLabTech db) extraMsgs

        SetOutsideCareStep step ->
            let
                updatedForm =
                    model.historyData.outsideCareForm
                        |> (\form -> { form | step = step })

                updatedData =
                    model.historyData
                        |> (\data -> { data | outsideCareForm = updatedForm })
            in
            ( { model | historyData = updatedData }
            , Cmd.none
            , []
            )

        SetOutsideCareSignBoolInput formUpdateFunc value ->
            let
                updatedForm =
                    formUpdateFunc value model.historyData.outsideCareForm

                updatedData =
                    model.historyData
                        |> (\data -> { data | outsideCareForm = updatedForm })
            in
            ( { model | historyData = updatedData }
            , Cmd.none
            , []
            )

        SetOutsideCareDiagnosis diagnosis ->
            let
                updatedForm =
                    setMultiSelectInputValue .diagnoses
                        (\value -> { outsideCareForm | diagnoses = value })
                        DiagnosisOther
                        diagnosis
                        outsideCareForm

                updatedData =
                    model.historyData
                        |> (\data -> { data | outsideCareForm = updatedForm })
            in
            ( { model | historyData = updatedData }
            , Cmd.none
            , []
            )

        SetOutsideCareMalariaMedication value ->
            let
                updatedForm =
                    setMultiSelectInputValue .malariaMedications
                        (\medication -> { outsideCareForm | malariaMedications = medication })
                        NoOutsideCareMedicationForMalaria
                        value
                        outsideCareForm

                updatedData =
                    model.historyData
                        |> (\data -> { data | outsideCareForm = updatedForm })
            in
            ( { model | historyData = updatedData }
            , Cmd.none
            , []
            )

        SetOutsideCareHypertensionMedication value ->
            let
                updatedForm =
                    setMultiSelectInputValue .hypertensionMedications
                        (\medication -> { outsideCareForm | hypertensionMedications = medication })
                        NoOutsideCareMedicationForHypertension
                        value
                        outsideCareForm

                updatedData =
                    model.historyData
                        |> (\data -> { data | outsideCareForm = updatedForm })
            in
            ( { model | historyData = updatedData }
            , Cmd.none
            , []
            )

        SetOutsideCareSyphilisMedication value ->
            let
                updatedForm =
                    setMultiSelectInputValue .syphilisMedications
                        (\medication -> { outsideCareForm | syphilisMedications = medication })
                        NoOutsideCareMedicationForSyphilis
                        value
                        outsideCareForm

                updatedData =
                    model.historyData
                        |> (\data -> { data | outsideCareForm = updatedForm })
            in
            ( { model | historyData = updatedData }
            , Cmd.none
            , []
            )

        SetOutsideCareAnemiaMedication value ->
            let
                updatedForm =
                    setMultiSelectInputValue .anemiaMedications
                        (\medication -> { outsideCareForm | anemiaMedications = medication })
                        NoOutsideCareMedicationForAnemia
                        value
                        outsideCareForm

                updatedData =
                    model.historyData
                        |> (\data -> { data | outsideCareForm = updatedForm })
            in
            ( { model | historyData = updatedData }
            , Cmd.none
            , []
            )

        SetOutsideCareHIVMedication value ->
            let
                updatedForm =
                    setMultiSelectInputValue .hivMedications
                        (\medication -> { outsideCareForm | hivMedications = medication })
                        NoOutsideCareMedicationForHIV
                        value
                        outsideCareForm

                updatedData =
                    model.historyData
                        |> (\data -> { data | outsideCareForm = updatedForm })
            in
            ( { model | historyData = updatedData }
            , Cmd.none
            , []
            )

        SaveOutsideCare personId saved nextTask ->
            let
                measurementId =
                    Maybe.map Tuple.first saved

                measurement =
                    getMeasurementValueFunc saved

                extraMsgs =
                    generateHistoryMsgs nextTask

                appMsgs =
                    toOutsideCareValueWithDefault NoPrenatalDiagnosis measurement model.historyData.outsideCareForm
                        |> Maybe.map
                            (Backend.PrenatalEncounter.Model.SaveOutsideCare personId measurementId
                                >> Backend.Model.MsgPrenatalEncounter id
                                >> App.Model.MsgIndexedDb
                                >> List.singleton
                            )
                        |> Maybe.withDefault []
            in
            ( model
            , Cmd.none
            , appMsgs
            )
                |> sequenceExtra (update language currentDate id isLabTech db) extraMsgs

        SetActiveExaminationTask task ->
            let
                updatedData =
                    model.examinationData
                        |> (\data -> { data | activeTask = Just task })
            in
            ( { model | examinationData = updatedData }
            , Cmd.none
            , []
            )

        SetVitalsIntInput formUpdateFunc value ->
            let
                updatedData =
                    let
                        updatedForm =
                            formUpdateFunc (String.toInt value) model.examinationData.vitalsForm
                    in
                    model.examinationData
                        |> (\data -> { data | vitalsForm = updatedForm })
            in
            ( { model | examinationData = updatedData }
            , Cmd.none
            , []
            )

        SetVitalsFloatInput formUpdateFunc value ->
            let
                updatedData =
                    let
                        updatedForm =
                            formUpdateFunc (String.toFloat value) model.examinationData.vitalsForm
                    in
                    model.examinationData
                        |> (\data -> { data | vitalsForm = updatedForm })
            in
            ( { model | examinationData = updatedData }
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
                    generateExaminationMsgs nextTask

                appMsgs =
                    toVitalsValueWithDefault measurement model.examinationData.vitalsForm
                        |> Maybe.map
                            (Backend.PrenatalEncounter.Model.SaveVitals personId measurementId
                                >> Backend.Model.MsgPrenatalEncounter id
                                >> App.Model.MsgIndexedDb
                                >> List.singleton
                            )
                        |> Maybe.withDefault []
            in
            ( model
            , Cmd.none
            , appMsgs
            )
                |> sequenceExtra (update language currentDate id isLabTech db) extraMsgs

        SetNutritionAssessmentMeasurement formUpdateFunc value ->
            let
                updatedData =
                    let
                        updatedForm =
                            formUpdateFunc (String.toFloat value) model.examinationData.nutritionAssessmentForm
                    in
                    model.examinationData
                        |> (\data -> { data | nutritionAssessmentForm = updatedForm })
            in
            ( { model | examinationData = updatedData }
            , Cmd.none
            , []
            )

        SaveNutritionAssessment personId saved maybeHeight nextTask ->
            let
                measurementId =
                    Maybe.map Tuple.first saved

                measurement =
                    getMeasurementValueFunc saved

                extraMsgs =
                    generateExaminationMsgs nextTask

                form_ =
                    model.examinationData.nutritionAssessmentForm

                form =
                    Maybe.map (\height -> { form_ | height = Just height }) maybeHeight
                        |> Maybe.withDefault form_

                appMsgs =
                    toPrenatalNutritionValueWithDefault measurement form
                        |> Maybe.map
                            (Backend.PrenatalEncounter.Model.SaveNutrition personId measurementId
                                >> Backend.Model.MsgPrenatalEncounter id
                                >> App.Model.MsgIndexedDb
                                >> List.singleton
                            )
                        |> Maybe.withDefault []
            in
            ( model
            , Cmd.none
            , appMsgs
            )
                |> sequenceExtra (update language currentDate id isLabTech db) extraMsgs

        SetCorePhysicalExamBoolInput formUpdateFunc value ->
            let
                updatedData =
                    let
                        updatedForm =
                            formUpdateFunc value model.examinationData.corePhysicalExamForm
                    in
                    model.examinationData
                        |> (\data -> { data | corePhysicalExamForm = updatedForm })
            in
            ( { model | examinationData = updatedData }
            , Cmd.none
            , []
            )

        SetCorePhysicalExamHeart value ->
            let
                updatedData =
                    let
                        updatedForm =
                            { corePhysicalExamForm | heart = Just value }
                    in
                    model.examinationData
                        |> (\data -> { data | corePhysicalExamForm = updatedForm })
            in
            ( { model | examinationData = updatedData }
            , Cmd.none
            , []
            )

        SetCorePhysicalExamNeck sign ->
            let
                form =
                    corePhysicalExamForm

                updatedForm =
                    setMultiSelectInputValue .neck
                        (\signs -> { form | neck = signs })
                        NormalNeck
                        sign
                        form

                updatedData =
                    model.examinationData
                        |> (\data -> { data | corePhysicalExamForm = updatedForm })
            in
            ( { model | examinationData = updatedData }
            , Cmd.none
            , []
            )

        SetCorePhysicalExamLungs sign ->
            let
                form =
                    corePhysicalExamForm

                updatedForm =
                    setMultiSelectInputValue .lungs
                        (\signs -> { form | lungs = signs })
                        NormalLungs
                        sign
                        form

                updatedData =
                    model.examinationData
                        |> (\data -> { data | corePhysicalExamForm = updatedForm })
            in
            ( { model | examinationData = updatedData }
            , Cmd.none
            , []
            )

        SetCorePhysicalExamAbdomen sign ->
            let
                form =
                    corePhysicalExamForm

                updatedForm =
                    setMultiSelectInputValue .abdomen
                        (\signs -> { form | abdomen = signs })
                        NormalAbdomen
                        sign
                        form

                updatedData =
                    model.examinationData
                        |> (\data -> { data | corePhysicalExamForm = updatedForm })
            in
            ( { model | examinationData = updatedData }
            , Cmd.none
            , []
            )

        SetCorePhysicalExamHands sign ->
            let
                form =
                    corePhysicalExamForm

                updatedForm =
                    setMultiSelectInputValue .hands
                        (\signs -> { form | hands = signs })
                        NormalHands
                        sign
                        form

                updatedData =
                    model.examinationData
                        |> (\data -> { data | corePhysicalExamForm = updatedForm })
            in
            ( { model | examinationData = updatedData }
            , Cmd.none
            , []
            )

        SetCorePhysicalExamLegs sign ->
            let
                form =
                    corePhysicalExamForm

                updatedForm =
                    setMultiSelectInputValue .legs
                        (\signs -> { form | legs = signs })
                        NormalLegs
                        sign
                        form

                updatedData =
                    model.examinationData
                        |> (\data -> { data | corePhysicalExamForm = updatedForm })
            in
            ( { model | examinationData = updatedData }
            , Cmd.none
            , []
            )

        SaveCorePhysicalExam personId saved nextTask ->
            let
                measurementId =
                    Maybe.map Tuple.first saved

                measurement =
                    getMeasurementValueFunc saved

                extraMsgs =
                    generateExaminationMsgs nextTask

                appMsgs =
                    toCorePhysicalExamValueWithDefault measurement model.examinationData.corePhysicalExamForm
                        |> Maybe.map
                            (Backend.PrenatalEncounter.Model.SaveCorePhysicalExam personId measurementId
                                >> Backend.Model.MsgPrenatalEncounter id
                                >> App.Model.MsgIndexedDb
                                >> List.singleton
                            )
                        |> Maybe.withDefault []
            in
            ( model
            , Cmd.none
            , appMsgs
            )
                |> sequenceExtra (update language currentDate id isLabTech db) extraMsgs

        SetObstetricalExamBoolInput formUpdateFunc value ->
            let
                updatedData =
                    let
                        updatedForm =
                            formUpdateFunc value model.examinationData.obstetricalExamForm
                    in
                    model.examinationData
                        |> (\data -> { data | obstetricalExamForm = updatedForm })
            in
            ( { model | examinationData = updatedData }
            , Cmd.none
            , []
            )

        SetObstetricalExamIntMeasurement formUpdateFunc value ->
            let
                updatedData =
                    let
                        valueAsInt =
                            String.toInt value
                                |> Maybe.andThen
                                    (\number ->
                                        if number == 0 then
                                            Nothing

                                        else
                                            Just number
                                    )

                        updatedForm =
                            formUpdateFunc valueAsInt model.examinationData.obstetricalExamForm
                    in
                    model.examinationData
                        |> (\data -> { data | obstetricalExamForm = updatedForm })
            in
            ( { model | examinationData = updatedData }
            , Cmd.none
            , []
            )

        SetObstetricalExamFloatMeasurement formUpdateFunc value ->
            let
                updatedData =
                    let
                        updatedForm =
                            formUpdateFunc (String.toFloat value) model.examinationData.obstetricalExamForm
                    in
                    model.examinationData
                        |> (\data -> { data | obstetricalExamForm = updatedForm })
            in
            ( { model | examinationData = updatedData }
            , Cmd.none
            , []
            )

        SetObstetricalExamFetalPresentation value ->
            let
                updatedData =
                    let
                        updatedForm =
                            model.examinationData.obstetricalExamForm
                                |> (\form -> { form | fetalPresentation = Just value })
                    in
                    model.examinationData
                        |> (\data -> { data | obstetricalExamForm = updatedForm })
            in
            ( { model | examinationData = updatedData }
            , Cmd.none
            , []
            )

        SetObstetricalExamCSectionScar value ->
            let
                updatedData =
                    let
                        updatedForm =
                            model.examinationData.obstetricalExamForm
                                |> (\form -> { form | cSectionScar = Just value })
                    in
                    model.examinationData
                        |> (\data -> { data | obstetricalExamForm = updatedForm })
            in
            ( { model | examinationData = updatedData }
            , Cmd.none
            , []
            )

        HideFundalPalpablePopup ->
            let
                updatedForm =
                    model.examinationData.obstetricalExamForm
                        |> (\form -> { form | displayFundalPalpablePopup = False })

                updatedData =
                    model.examinationData
                        |> (\data -> { data | obstetricalExamForm = updatedForm })
            in
            ( { model | examinationData = updatedData }
            , Cmd.none
            , []
            )

        ToggleFetalHeartRateNotAudible ->
            let
                form =
                    model.examinationData.obstetricalExamForm

                notAudible =
                    Maybe.map not form.fetalHeartRateNotAudible
                        |> Maybe.withDefault True

                fetalHeartRate =
                    if notAudible then
                        Just 0

                    else
                        Nothing

                updatedForm =
                    { form
                        | fetalHeartRate = fetalHeartRate
                        , fetalHeartRateDirty = True
                        , fetalHeartRateNotAudible = Just notAudible
                    }

                updatedData =
                    model.examinationData
                        |> (\data -> { data | obstetricalExamForm = updatedForm })
            in
            ( { model | examinationData = updatedData }
            , Cmd.none
            , []
            )

        SaveObstetricalExam personId saved nextTask ->
            let
                measurementId =
                    Maybe.map Tuple.first saved

                measurement =
                    getMeasurementValueFunc saved

                extraMsgs =
                    generateExaminationMsgs nextTask

                appMsgs =
                    toObstetricalExamValueWithDefault measurement model.examinationData.obstetricalExamForm
                        |> Maybe.map
                            (Backend.PrenatalEncounter.Model.SaveObstetricalExam personId measurementId
                                >> Backend.Model.MsgPrenatalEncounter id
                                >> App.Model.MsgIndexedDb
                                >> List.singleton
                            )
                        |> Maybe.withDefault []
            in
            ( model
            , Cmd.none
            , appMsgs
            )
                |> sequenceExtra (update language currentDate id isLabTech db) extraMsgs

        SetBreastExamBoolInput formUpdateFunc value ->
            let
                updatedForm =
                    formUpdateFunc value model.examinationData.breastExamForm

                updatedData =
                    model.examinationData
                        |> (\data -> { data | breastExamForm = updatedForm })
            in
            ( { model | examinationData = updatedData }
            , Cmd.none
            , []
            )

        SetBreastExamBreast sign ->
            let
                form =
                    Dict.get id db.prenatalMeasurements
                        |> Maybe.andThen RemoteData.toMaybe
                        |> Maybe.map
                            (.breastExam
                                >> getMeasurementValueFunc
                                >> breastExamFormWithDefault model.examinationData.breastExamForm
                            )
                        |> Maybe.withDefault model.examinationData.breastExamForm

                updatedForm =
                    setMultiSelectInputValue .breast
                        (\signs -> { form | breast = signs })
                        NormalBreast
                        sign
                        form

                updatedFormConsideringDischarge =
                    if sign == Discharge then
                        { updatedForm | dischargeType = Nothing, dischargeTypeDirty = True }

                    else
                        updatedForm

                updatedData =
                    model.examinationData
                        |> (\data -> { data | breastExamForm = updatedFormConsideringDischarge })
            in
            ( { model | examinationData = updatedData }
            , Cmd.none
            , []
            )

        SetDischargeType value ->
            let
                updatedForm =
                    model.examinationData.breastExamForm
                        |> (\form -> { form | dischargeType = Just value, dischargeTypeDirty = True })

                updatedData =
                    model.examinationData
                        |> (\data -> { data | breastExamForm = updatedForm })
            in
            ( { model | examinationData = updatedData }
            , Cmd.none
            , []
            )

        SaveBreastExam personId saved nextTask ->
            let
                measurementId =
                    Maybe.map Tuple.first saved

                measurement =
                    getMeasurementValueFunc saved

                extraMsgs =
                    generateExaminationMsgs nextTask

                appMsgs =
                    toBreastExamValueWithDefault measurement model.examinationData.breastExamForm
                        |> Maybe.map
                            (Backend.PrenatalEncounter.Model.SaveBreastExam personId measurementId
                                >> Backend.Model.MsgPrenatalEncounter id
                                >> App.Model.MsgIndexedDb
                                >> List.singleton
                            )
                        |> Maybe.withDefault []
            in
            ( model
            , Cmd.none
            , appMsgs
            )
                |> sequenceExtra (update language currentDate id isLabTech db) extraMsgs

        SetGUExamBoolInput formUpdateFunc value ->
            let
                updatedForm =
                    formUpdateFunc value model.examinationData.guExamForm

                updatedData =
                    model.examinationData
                        |> (\data -> { data | guExamForm = updatedForm })
            in
            ( { model | examinationData = updatedData }
            , Cmd.none
            , []
            )

        SetVaginalExamSign sign ->
            let
                updatedForm =
                    setMultiSelectInputValue .vaginalExamSigns
                        (\signs -> { guExamForm | vaginalExamSigns = signs })
                        NormalVaginalExam
                        sign
                        guExamForm

                updatedData =
                    model.examinationData
                        |> (\data -> { data | guExamForm = updatedForm })
            in
            ( { model | examinationData = updatedData }
            , Cmd.none
            , []
            )

        SetPostpartumHealingProblem problem ->
            let
                updatedForm =
                    setMultiSelectInputValue .postpartumHealingProblems
                        (\problems -> { guExamForm | postpartumHealingProblems = problems })
                        NormalPostpartumHealing
                        problem
                        guExamForm

                updatedData =
                    model.examinationData
                        |> (\data -> { data | guExamForm = updatedForm })
            in
            ( { model | examinationData = updatedData }
            , Cmd.none
            , []
            )

        SaveGUExam personId saved nextTask ->
            let
                measurementId =
                    Maybe.map Tuple.first saved

                measurement =
                    getMeasurementValueFunc saved

                extraMsgs =
                    generateExaminationMsgs nextTask

                appMsgs =
                    toGUExamValueWithDefault measurement model.examinationData.guExamForm
                        |> Maybe.map
                            (Backend.PrenatalEncounter.Model.SaveGUExam personId measurementId
                                >> Backend.Model.MsgPrenatalEncounter id
                                >> App.Model.MsgIndexedDb
                                >> List.singleton
                            )
                        |> Maybe.withDefault []
            in
            ( model
            , Cmd.none
            , appMsgs
            )
                |> sequenceExtra (update language currentDate id isLabTech db) extraMsgs

        SetFamilyPlanningSign sign ->
            let
                form =
                    Dict.get id db.prenatalMeasurements
                        |> Maybe.withDefault NotAsked
                        |> RemoteData.toMaybe
                        |> Maybe.map
                            (.familyPlanning
                                >> getMeasurementValueFunc
                                >> familyPlanningFormWithDefault model.familyPlanningData.form
                            )
                        |> Maybe.withDefault model.familyPlanningData.form

                updatedForm =
                    setMultiSelectInputValue .signs
                        (\signs -> { form | signs = signs })
                        NoFamilyPlanning
                        sign
                        form

                updatedData =
                    model.familyPlanningData
                        |> (\data -> { data | form = updatedForm })
            in
            ( { model | familyPlanningData = updatedData }
            , Cmd.none
            , []
            )

        SaveFamilyPlanning personId saved ->
            let
                measurementId =
                    Maybe.map Tuple.first saved

                measurement =
                    getMeasurementValueFunc saved

                appMsgs =
                    model.familyPlanningData.form
                        |> toFamilyPlanningValueWithDefault measurement
                        |> unwrap
                            []
                            (\value ->
                                [ Backend.PrenatalEncounter.Model.SaveFamilyPlanning personId measurementId value
                                    |> Backend.Model.MsgPrenatalEncounter id
                                    |> App.Model.MsgIndexedDb
                                , App.Model.SetActivePage <| UserPage <| PrenatalEncounterPage id
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

        SetAspirinAdministered value ->
            let
                updatedForm =
                    model.medicationData.aspirinForm
                        |> (\form -> { form | medicationAdministered = Just value, reasonForNonAdministration = Nothing })

                updatedData =
                    model.medicationData
                        |> (\data -> { data | aspirinForm = updatedForm })
            in
            ( { model | medicationData = updatedData }
            , Cmd.none
            , []
            )

        SetAspirinReasonForNonAdministration value ->
            let
                updatedForm =
                    model.medicationData.aspirinForm
                        |> (\form -> { form | reasonForNonAdministration = Just value })

                updatedData =
                    model.medicationData
                        |> (\data -> { data | aspirinForm = updatedForm })
            in
            ( { model | medicationData = updatedData }
            , Cmd.none
            , []
            )

        SaveAspirin personId saved nextTask ->
            let
                measurementId =
                    Maybe.map Tuple.first saved

                measurement =
                    getMeasurementValueFunc saved

                extraMsgs =
                    generateMedicationMsgs nextTask

                appMsgs =
                    model.medicationData.aspirinForm
                        |> toAdministrationNoteWithDefault measurement
                        |> Maybe.map
                            (Backend.PrenatalEncounter.Model.SaveAspirin personId measurementId
                                >> Backend.Model.MsgPrenatalEncounter id
                                >> App.Model.MsgIndexedDb
                                >> List.singleton
                            )
                        |> Maybe.withDefault []
            in
            ( model
            , Cmd.none
            , appMsgs
            )
                |> sequenceExtra (update language currentDate id isLabTech db) extraMsgs

        SetCalciumAdministered value ->
            let
                updatedForm =
                    model.medicationData.calciumForm
                        |> (\form -> { form | medicationAdministered = Just value, reasonForNonAdministration = Nothing })

                updatedData =
                    model.medicationData
                        |> (\data -> { data | calciumForm = updatedForm })
            in
            ( { model | medicationData = updatedData }
            , Cmd.none
            , []
            )

        SetCalciumReasonForNonAdministration value ->
            let
                updatedForm =
                    model.medicationData.calciumForm
                        |> (\form -> { form | reasonForNonAdministration = Just value })

                updatedData =
                    model.medicationData
                        |> (\data -> { data | calciumForm = updatedForm })
            in
            ( { model | medicationData = updatedData }
            , Cmd.none
            , []
            )

        SaveCalcium personId saved nextTask ->
            let
                measurementId =
                    Maybe.map Tuple.first saved

                measurement =
                    getMeasurementValueFunc saved

                extraMsgs =
                    generateMedicationMsgs nextTask

                appMsgs =
                    model.medicationData.calciumForm
                        |> toAdministrationNoteWithDefault measurement
                        |> Maybe.map
                            (Backend.PrenatalEncounter.Model.SaveCalcium personId measurementId
                                >> Backend.Model.MsgPrenatalEncounter id
                                >> App.Model.MsgIndexedDb
                                >> List.singleton
                            )
                        |> Maybe.withDefault []
            in
            ( model
            , Cmd.none
            , appMsgs
            )
                |> sequenceExtra (update language currentDate id isLabTech db) extraMsgs

        SetFefolAdministered value ->
            let
                updatedForm =
                    model.medicationData.fefolForm
                        |> (\form -> { form | medicationAdministered = Just value, reasonForNonAdministration = Nothing })

                updatedData =
                    model.medicationData
                        |> (\data -> { data | fefolForm = updatedForm })
            in
            ( { model | medicationData = updatedData }
            , Cmd.none
            , []
            )

        SetFefolReasonForNonAdministration value ->
            let
                updatedForm =
                    model.medicationData.fefolForm
                        |> (\form -> { form | reasonForNonAdministration = Just value })

                updatedData =
                    model.medicationData
                        |> (\data -> { data | fefolForm = updatedForm })
            in
            ( { model | medicationData = updatedData }
            , Cmd.none
            , []
            )

        SaveFefol personId saved nextTask ->
            let
                measurementId =
                    Maybe.map Tuple.first saved

                measurement =
                    getMeasurementValueFunc saved

                extraMsgs =
                    generateMedicationMsgs nextTask

                appMsgs =
                    model.medicationData.fefolForm
                        |> toAdministrationNoteWithDefault measurement
                        |> Maybe.map
                            (Backend.PrenatalEncounter.Model.SaveFefol personId measurementId
                                >> Backend.Model.MsgPrenatalEncounter id
                                >> App.Model.MsgIndexedDb
                                >> List.singleton
                            )
                        |> Maybe.withDefault []
            in
            ( model
            , Cmd.none
            , appMsgs
            )
                |> sequenceExtra (update language currentDate id isLabTech db) extraMsgs

        SetFolateAdministered value ->
            let
                updatedForm =
                    model.medicationData.folateForm
                        |> (\form -> { form | medicationAdministered = Just value, reasonForNonAdministration = Nothing })

                updatedData =
                    model.medicationData
                        |> (\data -> { data | folateForm = updatedForm })
            in
            ( { model | medicationData = updatedData }
            , Cmd.none
            , []
            )

        SetFolateReasonForNonAdministration value ->
            let
                updatedForm =
                    model.medicationData.folateForm
                        |> (\form -> { form | reasonForNonAdministration = Just value })

                updatedData =
                    model.medicationData
                        |> (\data -> { data | folateForm = updatedForm })
            in
            ( { model | medicationData = updatedData }
            , Cmd.none
            , []
            )

        SaveFolate personId saved nextTask ->
            let
                measurementId =
                    Maybe.map Tuple.first saved

                measurement =
                    getMeasurementValueFunc saved

                extraMsgs =
                    generateMedicationMsgs nextTask

                appMsgs =
                    model.medicationData.folateForm
                        |> toAdministrationNoteWithDefault measurement
                        |> Maybe.map
                            (Backend.PrenatalEncounter.Model.SaveFolate personId measurementId
                                >> Backend.Model.MsgPrenatalEncounter id
                                >> App.Model.MsgIndexedDb
                                >> List.singleton
                            )
                        |> Maybe.withDefault []
            in
            ( model
            , Cmd.none
            , appMsgs
            )
                |> sequenceExtra (update language currentDate id isLabTech db) extraMsgs

        SetIronAdministered value ->
            let
                updatedForm =
                    model.medicationData.ironForm
                        |> (\form -> { form | medicationAdministered = Just value, reasonForNonAdministration = Nothing })

                updatedData =
                    model.medicationData
                        |> (\data -> { data | ironForm = updatedForm })
            in
            ( { model | medicationData = updatedData }
            , Cmd.none
            , []
            )

        SetIronReasonForNonAdministration value ->
            let
                updatedForm =
                    model.medicationData.ironForm
                        |> (\form -> { form | reasonForNonAdministration = Just value })

                updatedData =
                    model.medicationData
                        |> (\data -> { data | ironForm = updatedForm })
            in
            ( { model | medicationData = updatedData }
            , Cmd.none
            , []
            )

        SaveIron personId saved nextTask ->
            let
                measurementId =
                    Maybe.map Tuple.first saved

                measurement =
                    getMeasurementValueFunc saved

                extraMsgs =
                    generateMedicationMsgs nextTask

                appMsgs =
                    model.medicationData.ironForm
                        |> toAdministrationNoteWithDefault measurement
                        |> Maybe.map
                            (Backend.PrenatalEncounter.Model.SaveIron personId measurementId
                                >> Backend.Model.MsgPrenatalEncounter id
                                >> App.Model.MsgIndexedDb
                                >> List.singleton
                            )
                        |> Maybe.withDefault []
            in
            ( model
            , Cmd.none
            , appMsgs
            )
                |> sequenceExtra (update language currentDate id isLabTech db) extraMsgs

        SetMMSAdministered value ->
            let
                updatedForm =
                    model.medicationData.mmsForm
                        |> (\form -> { form | medicationAdministered = Just value, reasonForNonAdministration = Nothing })

                updatedData =
                    model.medicationData
                        |> (\data -> { data | mmsForm = updatedForm })
            in
            ( { model | medicationData = updatedData }
            , Cmd.none
            , []
            )

        SetMMSReasonForNonAdministration value ->
            let
                updatedForm =
                    model.medicationData.mmsForm
                        |> (\form -> { form | reasonForNonAdministration = Just value })

                updatedData =
                    model.medicationData
                        |> (\data -> { data | mmsForm = updatedForm })
            in
            ( { model | medicationData = updatedData }
            , Cmd.none
            , []
            )

        SaveMMS personId saved nextTask ->
            let
                measurementId =
                    Maybe.map Tuple.first saved

                measurement =
                    getMeasurementValueFunc saved

                extraMsgs =
                    generateMedicationMsgs nextTask

                appMsgs =
                    model.medicationData.mmsForm
                        |> toAdministrationNoteWithDefault measurement
                        |> Maybe.map
                            (Backend.PrenatalEncounter.Model.SaveMMS personId measurementId
                                >> Backend.Model.MsgPrenatalEncounter id
                                >> App.Model.MsgIndexedDb
                                >> List.singleton
                            )
                        |> Maybe.withDefault []
            in
            ( model
            , Cmd.none
            , appMsgs
            )
                |> sequenceExtra (update language currentDate id isLabTech db) extraMsgs

        SetMebendazoleAdministered value ->
            let
                updatedForm =
                    model.medicationData.mebendazoleForm
                        |> (\form -> { form | medicationAdministered = Just value, reasonForNonAdministration = Nothing })

                updatedData =
                    model.medicationData
                        |> (\data -> { data | mebendazoleForm = updatedForm })
            in
            ( { model | medicationData = updatedData }
            , Cmd.none
            , []
            )

        SetMebendazoleReasonForNonAdministration value ->
            let
                updatedForm =
                    model.medicationData.mebendazoleForm
                        |> (\form -> { form | reasonForNonAdministration = Just value })

                updatedData =
                    model.medicationData
                        |> (\data -> { data | mebendazoleForm = updatedForm })
            in
            ( { model | medicationData = updatedData }
            , Cmd.none
            , []
            )

        SaveMebendazole personId saved nextTask ->
            let
                measurementId =
                    Maybe.map Tuple.first saved

                measurement =
                    getMeasurementValueFunc saved

                extraMsgs =
                    generateMedicationMsgs nextTask

                appMsgs =
                    model.medicationData.mebendazoleForm
                        |> toAdministrationNoteWithDefault measurement
                        |> Maybe.map
                            (Backend.PrenatalEncounter.Model.SaveMebendazole personId measurementId
                                >> Backend.Model.MsgPrenatalEncounter id
                                >> App.Model.MsgIndexedDb
                                >> List.singleton
                            )
                        |> Maybe.withDefault []
            in
            ( model
            , Cmd.none
            , appMsgs
            )
                |> sequenceExtra (update language currentDate id isLabTech db) extraMsgs

        SetMalariaPreventionBoolInput formUpdateFunc value ->
            let
                updatedForm =
                    formUpdateFunc value model.malariaPreventionData.form

                updatedData =
                    model.malariaPreventionData
                        |> (\data -> { data | form = updatedForm })
            in
            ( { model | malariaPreventionData = updatedData }
            , Cmd.none
            , []
            )

        SaveMalariaPrevention personId saved ->
            let
                measurementId =
                    Maybe.map Tuple.first saved

                measurement =
                    getMeasurementValueFunc saved

                appMsgs =
                    model.malariaPreventionData.form
                        |> toMalariaPreventionValueWithDefault PhaseInitial measurement
                        |> unwrap
                            []
                            (\value ->
                                [ Backend.PrenatalEncounter.Model.SaveMalariaPrevention personId measurementId value
                                    |> Backend.Model.MsgPrenatalEncounter id
                                    |> App.Model.MsgIndexedDb
                                , App.Model.SetActivePage <| UserPage <| PrenatalEncounterPage id
                                ]
                            )
            in
            ( model
            , Cmd.none
            , appMsgs
            )

        SetDangerSign sign ->
            let
                form =
                    Dict.get id db.prenatalMeasurements
                        |> Maybe.withDefault NotAsked
                        |> RemoteData.toMaybe
                        |> Maybe.map
                            (.dangerSigns
                                >> getMeasurementValueFunc
                                >> dangerSignsFormWithDefault model.dangerSignsData.form
                            )
                        |> Maybe.withDefault model.dangerSignsData.form

                updatedForm =
                    setMultiSelectInputValue .signs
                        (\signs -> { form | signs = signs })
                        NoDangerSign
                        sign
                        form

                updatedData =
                    model.dangerSignsData
                        |> (\data -> { data | form = updatedForm })
            in
            ( { model | dangerSignsData = updatedData }
            , Cmd.none
            , []
            )

        SetPostpartumMotherDangerSign sign ->
            let
                form =
                    Dict.get id db.prenatalMeasurements
                        |> Maybe.withDefault NotAsked
                        |> RemoteData.toMaybe
                        |> Maybe.map
                            (.dangerSigns
                                >> getMeasurementValueFunc
                                >> dangerSignsFormWithDefault model.dangerSignsData.form
                            )
                        |> Maybe.withDefault model.dangerSignsData.form

                updatedForm =
                    setMultiSelectInputValue .postpartumMother
                        (\signs -> { form | postpartumMother = signs })
                        NoPostpartumMotherDangerSigns
                        sign
                        form

                updatedData =
                    model.dangerSignsData
                        |> (\data -> { data | form = updatedForm })
            in
            ( { model | dangerSignsData = updatedData }
            , Cmd.none
            , []
            )

        SetPostpartumChildDangerSign sign ->
            let
                form =
                    Dict.get id db.prenatalMeasurements
                        |> Maybe.withDefault NotAsked
                        |> RemoteData.toMaybe
                        |> Maybe.map
                            (.dangerSigns
                                >> getMeasurementValueFunc
                                >> dangerSignsFormWithDefault model.dangerSignsData.form
                            )
                        |> Maybe.withDefault model.dangerSignsData.form

                updatedForm =
                    setMultiSelectInputValue .postpartumChild
                        (\signs -> { form | postpartumChild = signs })
                        NoPostpartumChildDangerSigns
                        sign
                        form

                updatedData =
                    model.dangerSignsData
                        |> (\data -> { data | form = updatedForm })
            in
            ( { model | dangerSignsData = updatedData }
            , Cmd.none
            , []
            )

        SaveDangerSigns personId saved ->
            let
                measurementId =
                    Maybe.map Tuple.first saved

                measurement =
                    getMeasurementValueFunc saved

                appMsgs =
                    model.dangerSignsData.form
                        |> toDangerSignsValueWithDefault measurement
                        |> unwrap
                            []
                            (\value ->
                                [ Backend.PrenatalEncounter.Model.SaveDangerSigns personId measurementId value
                                    |> Backend.Model.MsgPrenatalEncounter id
                                    |> App.Model.MsgIndexedDb
                                , App.Model.SetActivePage <| UserPage <| PrenatalEncounterPage id
                                ]
                            )
            in
            ( model
            , Cmd.none
            , appMsgs
            )

        SavePrenatalPhoto personId maybePhotoId photoUrl ->
            let
                updatedData =
                    model.prenatalPhotoData
                        |> (\data -> { data | url = Nothing })
            in
            ( { model | prenatalPhotoData = updatedData }
            , Cmd.none
            , [ Backend.PrenatalEncounter.Model.SavePrenatalPhoto personId maybePhotoId photoUrl
                    |> Backend.Model.MsgPrenatalEncounter id
                    |> App.Model.MsgIndexedDb
              , App.Model.SetActivePage <| UserPage <| PrenatalEncounterPage id
              ]
            )

        SetBirthPlanBoolInput formUpdateFunc value ->
            let
                updatedData =
                    let
                        updatedForm =
                            formUpdateFunc value model.birthPlanData.form
                    in
                    model.birthPlanData
                        |> (\data -> { data | form = updatedForm })
            in
            ( { model | birthPlanData = updatedData }
            , Cmd.none
            , []
            )

        SetBirthPlanFamilyPlanning sign ->
            let
                form =
                    Dict.get id db.prenatalMeasurements
                        |> Maybe.withDefault NotAsked
                        |> RemoteData.toMaybe
                        |> Maybe.map
                            (.birthPlan
                                >> getMeasurementValueFunc
                                >> birthPlanFormWithDefault model.birthPlanData.form
                            )
                        |> Maybe.withDefault model.birthPlanData.form

                updatedForm =
                    setMultiSelectInputValue .familyPlanning
                        (\signs -> { form | familyPlanning = signs })
                        NoFamilyPlanning
                        sign
                        form

                updatedData =
                    model.birthPlanData
                        |> (\data -> { data | form = updatedForm })
            in
            ( { model | birthPlanData = updatedData }
            , Cmd.none
            , []
            )

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

        SetPregnancyTestResult value ->
            let
                updatedData =
                    let
                        result =
                            pregnancyTestResultFromString value

                        updatedForm =
                            model.laboratoryData.pregnancyTestForm
                                |> (\form -> { form | pregnancyTestResult = result })
                    in
                    model.laboratoryData
                        |> (\data -> { data | pregnancyTestForm = updatedForm })
            in
            ( { model | laboratoryData = updatedData }
            , Cmd.none
            , []
            )

        SaveBirthPlan personId saved ->
            let
                measurementId =
                    Maybe.map Tuple.first saved

                measurement =
                    getMeasurementValueFunc saved

                appMsgs =
                    model.birthPlanData.form
                        |> toBirthPlanValueWithDefault measurement
                        |> unwrap
                            []
                            (\value ->
                                [ Backend.PrenatalEncounter.Model.SaveBirthPlan personId measurementId value
                                    |> Backend.Model.MsgPrenatalEncounter id
                                    |> App.Model.MsgIndexedDb
                                , App.Model.SetActivePage <| UserPage <| PrenatalEncounterPage id
                                ]
                            )
            in
            ( model
            , Cmd.none
            , appMsgs
            )

        SavePregnancyTest personId saved ->
            let
                measurementId =
                    Maybe.map Tuple.first saved

                measurement =
                    getMeasurementValueFunc saved

                appMsgs =
                    model.laboratoryData.pregnancyTestForm
                        |> toPregnancyTestValueWithDefault measurement
                        |> unwrap
                            []
                            (\value ->
                                [ Backend.PrenatalEncounter.Model.SavePregnancyTest personId measurementId value
                                    |> Backend.Model.MsgPrenatalEncounter id
                                    |> App.Model.MsgIndexedDb
                                , App.Model.SetActivePage <| UserPage <| PrenatalEncounterPage id
                                ]
                            )
            in
            ( model
            , Cmd.none
            , appMsgs
            )

        SetHIVTestFormBoolInput formUpdateFunc value ->
            let
                form =
                    model.laboratoryData.hivTestForm

                updatedForm =
                    formUpdateFunc value form

                updatedData =
                    model.laboratoryData
                        |> (\data -> { data | hivTestForm = updatedForm })
            in
            ( { model | laboratoryData = updatedData }
            , Cmd.none
            , []
            )

        SetHIVTestExecutionNote value ->
            let
                form =
                    model.laboratoryData.hivTestForm

                updatedForm =
                    { form | executionNote = Just value, executionNoteDirty = True }

                updatedData =
                    model.laboratoryData
                        |> (\data -> { data | hivTestForm = updatedForm })
            in
            ( { model | laboratoryData = updatedData }
            , Cmd.none
            , []
            )

        SetHIVTestResult value ->
            let
                form =
                    model.laboratoryData.hivTestForm

                updatedForm =
                    { form
                        | testResult = testResultFromString value
                        , hivProgramHC = Nothing
                        , hivProgramHCDirty = True
                        , partnerHIVPositive = Nothing
                        , partnerHIVPositiveDirty = True
                        , partnerTakingARV = Nothing
                        , partnerTakingARVDirty = True
                        , partnerSurpressedViralLoad = Nothing
                        , partnerSurpressedViralLoadDirty = True
                    }

                updatedData =
                    model.laboratoryData
                        |> (\data -> { data | hivTestForm = updatedForm })
            in
            ( { model | laboratoryData = updatedData }
            , Cmd.none
            , []
            )

        SaveHIVTest personId saved nextTask ->
            let
                measurementId =
                    Maybe.map Tuple.first saved

                measurement =
                    getMeasurementValueFunc saved

                extraMsgs =
                    generateLaboratoryMsgs nextTask

                appMsgs =
                    model.laboratoryData.hivTestForm
                        |> toHIVTestValueUniversalWithDefault measurement
                        |> Maybe.map
                            (Backend.PrenatalEncounter.Model.SaveHIVTest personId measurementId
                                >> Backend.Model.MsgPrenatalEncounter id
                                >> App.Model.MsgIndexedDb
                                >> List.singleton
                            )
                        |> Maybe.withDefault []
            in
            ( model
            , Cmd.none
            , appMsgs
            )
                |> sequenceExtra (update language currentDate id isLabTech db) extraMsgs

        SetSyphilisTestFormBoolInput formUpdateFunc value ->
            let
                form =
                    model.laboratoryData.syphilisTestForm

                updatedForm =
                    formUpdateFunc value form

                updatedData =
                    model.laboratoryData
                        |> (\data -> { data | syphilisTestForm = updatedForm })
            in
            ( { model | laboratoryData = updatedData }
            , Cmd.none
            , []
            )

        SetSyphilisTestExecutionNote value ->
            let
                form =
                    model.laboratoryData.syphilisTestForm

                updatedForm =
                    { form | executionNote = Just value, executionNoteDirty = True }

                updatedData =
                    model.laboratoryData
                        |> (\data -> { data | syphilisTestForm = updatedForm })
            in
            ( { model | laboratoryData = updatedData }
            , Cmd.none
            , []
            )

        SetSyphilisTestResult value ->
            let
                form =
                    model.laboratoryData.syphilisTestForm

                updatedForm =
                    { form | testResult = testResultFromString value, testResultDirty = True, symptoms = Nothing, symptomsDirty = True }

                updatedData =
                    model.laboratoryData
                        |> (\data -> { data | syphilisTestForm = updatedForm })
            in
            ( { model | laboratoryData = updatedData }
            , Cmd.none
            , []
            )

        SetIllnessSymptom symptom ->
            let
                form =
                    Dict.get id db.prenatalMeasurements
                        |> Maybe.andThen RemoteData.toMaybe
                        |> Maybe.map
                            (.syphilisTest
                                >> getMeasurementValueFunc
                                >> syphilisTestFormWithDefault model.laboratoryData.syphilisTestForm
                            )
                        |> Maybe.withDefault model.laboratoryData.syphilisTestForm

                updatedForm =
                    setMultiSelectInputValue .symptoms
                        (\symptoms -> { form | symptoms = symptoms, symptomsDirty = True })
                        NoIllnessSymptoms
                        symptom
                        form

                updatedData =
                    model.laboratoryData
                        |> (\data -> { data | syphilisTestForm = updatedForm })
            in
            ( { model | laboratoryData = updatedData }
            , Cmd.none
            , []
            )

        SaveSyphilisTest personId saved nextTask ->
            let
                measurementId =
                    Maybe.map Tuple.first saved

                measurement =
                    getMeasurementValueFunc saved

                extraMsgs =
                    generateLaboratoryMsgs nextTask

                appMsgs =
                    model.laboratoryData.syphilisTestForm
                        |> toSyphilisTestValueWithDefault measurement
                        |> Maybe.map
                            (Backend.PrenatalEncounter.Model.SaveSyphilisTest personId measurementId
                                >> Backend.Model.MsgPrenatalEncounter id
                                >> App.Model.MsgIndexedDb
                                >> List.singleton
                            )
                        |> Maybe.withDefault []
            in
            ( model
            , Cmd.none
            , appMsgs
            )
                |> sequenceExtra (update language currentDate id isLabTech db) extraMsgs

        SetHepatitisBTestFormBoolInput formUpdateFunc value ->
            let
                form =
                    model.laboratoryData.hepatitisBTestForm

                updatedForm =
                    formUpdateFunc value form

                updatedData =
                    model.laboratoryData
                        |> (\data -> { data | hepatitisBTestForm = updatedForm })
            in
            ( { model | laboratoryData = updatedData }
            , Cmd.none
            , []
            )

        SetHepatitisBTestExecutionNote value ->
            let
                form =
                    model.laboratoryData.hepatitisBTestForm

                updatedForm =
                    { form | executionNote = Just value, executionNoteDirty = True }

                updatedData =
                    model.laboratoryData
                        |> (\data -> { data | hepatitisBTestForm = updatedForm })
            in
            ( { model | laboratoryData = updatedData }
            , Cmd.none
            , []
            )

        SetHepatitisBTestResult value ->
            let
                form =
                    model.laboratoryData.hepatitisBTestForm

                updatedForm =
                    { form | testResult = testResultFromString value, testResultDirty = True }

                updatedData =
                    model.laboratoryData
                        |> (\data -> { data | hepatitisBTestForm = updatedForm })
            in
            ( { model | laboratoryData = updatedData }
            , Cmd.none
            , []
            )

        SaveHepatitisBTest personId saved nextTask ->
            let
                measurementId =
                    Maybe.map Tuple.first saved

                measurement =
                    getMeasurementValueFunc saved

                extraMsgs =
                    generateLaboratoryMsgs nextTask

                appMsgs =
                    model.laboratoryData.hepatitisBTestForm
                        |> toHepatitisBTestValueWithDefault measurement
                        |> Maybe.map
                            (Backend.PrenatalEncounter.Model.SaveHepatitisBTest personId measurementId
                                >> Backend.Model.MsgPrenatalEncounter id
                                >> App.Model.MsgIndexedDb
                                >> List.singleton
                            )
                        |> Maybe.withDefault []
            in
            ( model
            , Cmd.none
            , appMsgs
            )
                |> sequenceExtra (update language currentDate id isLabTech db) extraMsgs

        SetMalariaTestFormBoolInput formUpdateFunc value ->
            let
                form =
                    model.laboratoryData.malariaTestForm

                updatedForm =
                    formUpdateFunc value form

                updatedData =
                    model.laboratoryData
                        |> (\data -> { data | malariaTestForm = updatedForm })
            in
            ( { model | laboratoryData = updatedData }
            , Cmd.none
            , []
            )

        SetMalariaTestExecutionNote value ->
            let
                form =
                    model.laboratoryData.malariaTestForm

                updatedForm =
                    { form | executionNote = Just value, executionNoteDirty = True }

                updatedData =
                    model.laboratoryData
                        |> (\data -> { data | malariaTestForm = updatedForm })
            in
            ( { model | laboratoryData = updatedData }
            , Cmd.none
            , []
            )

        SetMalariaTestResult value ->
            let
                form =
                    model.laboratoryData.malariaTestForm

                updatedForm =
                    { form | testResult = testResultFromString value }

                updatedData =
                    model.laboratoryData
                        |> (\data -> { data | malariaTestForm = updatedForm })
            in
            ( { model | laboratoryData = updatedData }
            , Cmd.none
            , []
            )

        SetBloodSmearResult value ->
            let
                form =
                    model.laboratoryData.malariaTestForm

                updatedForm =
                    { form
                        | bloodSmearResult = bloodSmearResultFromString value
                        , bloodSmearResultDirty = True
                        , executionDate = Just currentDate
                        , executionDateDirty = True
                    }

                updatedData =
                    model.laboratoryData
                        |> (\data -> { data | malariaTestForm = updatedForm })
            in
            ( { model | laboratoryData = updatedData }
            , Cmd.none
            , []
            )

        SaveMalariaTest personId saved nextTask ->
            let
                measurementId =
                    Maybe.map Tuple.first saved

                measurement =
                    getMeasurementValueFunc saved

                extraMsgs =
                    generateLaboratoryMsgs nextTask

                appMsgs =
                    model.laboratoryData.malariaTestForm
                        |> toMalariaTestValueWithDefault measurement
                        |> Maybe.map
                            (Backend.PrenatalEncounter.Model.SaveMalariaTest personId measurementId
                                >> Backend.Model.MsgPrenatalEncounter id
                                >> App.Model.MsgIndexedDb
                                >> List.singleton
                            )
                        |> Maybe.withDefault []
            in
            ( model
            , Cmd.none
            , appMsgs
            )
                |> sequenceExtra (update language currentDate id isLabTech db) extraMsgs

        SetBloodGpRsTestFormBoolInput formUpdateFunc value ->
            let
                form =
                    model.laboratoryData.bloodGpRsTestForm

                updatedForm =
                    formUpdateFunc value form

                updatedData =
                    model.laboratoryData
                        |> (\data -> { data | bloodGpRsTestForm = updatedForm })
            in
            ( { model | laboratoryData = updatedData }
            , Cmd.none
            , []
            )

        SetBloodGpRsTestExecutionNote value ->
            let
                form =
                    model.laboratoryData.bloodGpRsTestForm

                updatedForm =
                    { form | executionNote = Just value, executionNoteDirty = True }

                updatedData =
                    model.laboratoryData
                        |> (\data -> { data | bloodGpRsTestForm = updatedForm })
            in
            ( { model | laboratoryData = updatedData }
            , Cmd.none
            , []
            )

        SetBloodGroup value ->
            let
                form =
                    model.laboratoryData.bloodGpRsTestForm

                updatedForm =
                    { form | bloodGroup = bloodGroupFromString value, bloodGroupDirty = True }

                updatedData =
                    model.laboratoryData
                        |> (\data -> { data | bloodGpRsTestForm = updatedForm })
            in
            ( { model | laboratoryData = updatedData }
            , Cmd.none
            , []
            )

        SetRhesus value ->
            let
                form =
                    model.laboratoryData.bloodGpRsTestForm

                updatedForm =
                    { form | rhesus = rhesusFromString value, rhesusDirty = True }

                updatedData =
                    model.laboratoryData
                        |> (\data -> { data | bloodGpRsTestForm = updatedForm })
            in
            ( { model | laboratoryData = updatedData }
            , Cmd.none
            , []
            )

        SaveBloodGpRsTest personId saved nextTask ->
            let
                measurementId =
                    Maybe.map Tuple.first saved

                measurement =
                    getMeasurementValueFunc saved

                extraMsgs =
                    generateLaboratoryMsgs nextTask

                appMsgs =
                    model.laboratoryData.bloodGpRsTestForm
                        |> toBloodGpRsTestValueWithDefault measurement
                        |> Maybe.map
                            (Backend.PrenatalEncounter.Model.SaveBloodGpRsTest personId measurementId
                                >> Backend.Model.MsgPrenatalEncounter id
                                >> App.Model.MsgIndexedDb
                                >> List.singleton
                            )
                        |> Maybe.withDefault []
            in
            ( model
            , Cmd.none
            , appMsgs
            )
                |> sequenceExtra (update language currentDate id isLabTech db) extraMsgs

        SetUrineDipstickTestFormBoolInput formUpdateFunc value ->
            let
                form =
                    model.laboratoryData.urineDipstickTestForm

                updatedForm =
                    formUpdateFunc value form

                updatedData =
                    model.laboratoryData
                        |> (\data -> { data | urineDipstickTestForm = updatedForm })
            in
            ( { model | laboratoryData = updatedData }
            , Cmd.none
            , []
            )

        SetUrineDipstickTestExecutionNote value ->
            let
                form =
                    model.laboratoryData.urineDipstickTestForm

                updatedForm =
                    { form | executionNote = Just value, executionNoteDirty = True }

                updatedData =
                    model.laboratoryData
                        |> (\data -> { data | urineDipstickTestForm = updatedForm })
            in
            ( { model | laboratoryData = updatedData }
            , Cmd.none
            , []
            )

        SetUrineDipstickTestVariant value ->
            let
                form =
                    model.laboratoryData.urineDipstickTestForm

                updatedForm =
                    { form
                        | testVariant = Just value
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

                updatedData =
                    model.laboratoryData
                        |> (\data -> { data | urineDipstickTestForm = updatedForm })
            in
            ( { model | laboratoryData = updatedData }
            , Cmd.none
            , []
            )

        SetProtein value ->
            let
                form =
                    model.laboratoryData.urineDipstickTestForm

                updatedForm =
                    { form | protein = proteinValueFromString value, proteinDirty = True }

                updatedData =
                    model.laboratoryData
                        |> (\data -> { data | urineDipstickTestForm = updatedForm })
            in
            ( { model | laboratoryData = updatedData }
            , Cmd.none
            , []
            )

        SetPH value ->
            let
                form =
                    model.laboratoryData.urineDipstickTestForm

                updatedForm =
                    { form | ph = phValueFromString value, phDirty = True }

                updatedData =
                    model.laboratoryData
                        |> (\data -> { data | urineDipstickTestForm = updatedForm })
            in
            ( { model | laboratoryData = updatedData }
            , Cmd.none
            , []
            )

        SetGlucose value ->
            let
                form =
                    model.laboratoryData.urineDipstickTestForm

                updatedForm =
                    { form | glucose = glucoseValueFromString value, glucoseDirty = True }

                updatedData =
                    model.laboratoryData
                        |> (\data -> { data | urineDipstickTestForm = updatedForm })
            in
            ( { model | laboratoryData = updatedData }
            , Cmd.none
            , []
            )

        SetLeukocytes value ->
            let
                form =
                    model.laboratoryData.urineDipstickTestForm

                updatedForm =
                    { form | leukocytes = leukocytesValueFromString value, leukocytesDirty = True }

                updatedData =
                    model.laboratoryData
                        |> (\data -> { data | urineDipstickTestForm = updatedForm })
            in
            ( { model | laboratoryData = updatedData }
            , Cmd.none
            , []
            )

        SetNitrite value ->
            let
                form =
                    model.laboratoryData.urineDipstickTestForm

                updatedForm =
                    { form | nitrite = nitriteValueFromString value, nitriteDirty = True }

                updatedData =
                    model.laboratoryData
                        |> (\data -> { data | urineDipstickTestForm = updatedForm })
            in
            ( { model | laboratoryData = updatedData }
            , Cmd.none
            , []
            )

        SetUrobilinogen value ->
            let
                form =
                    model.laboratoryData.urineDipstickTestForm

                updatedForm =
                    { form | urobilinogen = urobilinogenValueFromString value, urobilinogenDirty = True }

                updatedData =
                    model.laboratoryData
                        |> (\data -> { data | urineDipstickTestForm = updatedForm })
            in
            ( { model | laboratoryData = updatedData }
            , Cmd.none
            , []
            )

        SetHaemoglobin value ->
            let
                form =
                    model.laboratoryData.urineDipstickTestForm

                updatedForm =
                    { form | haemoglobin = haemoglobinValueFromString value, haemoglobinDirty = True }

                updatedData =
                    model.laboratoryData
                        |> (\data -> { data | urineDipstickTestForm = updatedForm })
            in
            ( { model | laboratoryData = updatedData }
            , Cmd.none
            , []
            )

        SetKetone value ->
            let
                form =
                    model.laboratoryData.urineDipstickTestForm

                updatedForm =
                    { form | ketone = ketoneValueFromString value, ketoneDirty = True }

                updatedData =
                    model.laboratoryData
                        |> (\data -> { data | urineDipstickTestForm = updatedForm })
            in
            ( { model | laboratoryData = updatedData }
            , Cmd.none
            , []
            )

        SetBilirubin value ->
            let
                form =
                    model.laboratoryData.urineDipstickTestForm

                updatedForm =
                    { form | bilirubin = bilirubinValueFromString value, bilirubinDirty = True }

                updatedData =
                    model.laboratoryData
                        |> (\data -> { data | urineDipstickTestForm = updatedForm })
            in
            ( { model | laboratoryData = updatedData }
            , Cmd.none
            , []
            )

        SaveUrineDipstickTest personId saved nextTask ->
            let
                measurementId =
                    Maybe.map Tuple.first saved

                measurement =
                    getMeasurementValueFunc saved

                extraMsgs =
                    generateLaboratoryMsgs nextTask

                appMsgs =
                    model.laboratoryData.urineDipstickTestForm
                        |> toUrineDipstickTestValueUniversalWithDefault measurement
                        |> Maybe.map
                            (Backend.PrenatalEncounter.Model.SaveUrineDipstickTest personId measurementId
                                >> Backend.Model.MsgPrenatalEncounter id
                                >> App.Model.MsgIndexedDb
                                >> List.singleton
                            )
                        |> Maybe.withDefault []
            in
            ( model
            , Cmd.none
            , appMsgs
            )
                |> sequenceExtra (update language currentDate id isLabTech db) extraMsgs

        SetHemoglobinTestFormBoolInput formUpdateFunc value ->
            let
                form =
                    model.laboratoryData.hemoglobinTestForm

                updatedForm =
                    formUpdateFunc value form

                updatedData =
                    model.laboratoryData
                        |> (\data -> { data | hemoglobinTestForm = updatedForm })
            in
            ( { model | laboratoryData = updatedData }
            , Cmd.none
            , []
            )

        SetHemoglobinTestExecutionNote value ->
            let
                form =
                    model.laboratoryData.hemoglobinTestForm

                updatedForm =
                    { form | executionNote = Just value, executionNoteDirty = True }

                updatedData =
                    model.laboratoryData
                        |> (\data -> { data | hemoglobinTestForm = updatedForm })
            in
            ( { model | laboratoryData = updatedData }
            , Cmd.none
            , []
            )

        SetHemoglobinCount value ->
            let
                form =
                    model.laboratoryData.hemoglobinTestForm

                updatedForm =
                    { form | hemoglobinCount = String.toFloat value, hemoglobinCountDirty = True }

                updatedData =
                    model.laboratoryData
                        |> (\data -> { data | hemoglobinTestForm = updatedForm })
            in
            ( { model | laboratoryData = updatedData }
            , Cmd.none
            , []
            )

        SaveHemoglobinTest personId saved nextTask ->
            let
                measurementId =
                    Maybe.map Tuple.first saved

                measurement =
                    getMeasurementValueFunc saved

                extraMsgs =
                    generateLaboratoryMsgs nextTask

                appMsgs =
                    model.laboratoryData.hemoglobinTestForm
                        |> toHemoglobinTestValueWithDefault measurement
                        |> Maybe.map
                            (Backend.PrenatalEncounter.Model.SaveHemoglobinTest personId measurementId
                                >> Backend.Model.MsgPrenatalEncounter id
                                >> App.Model.MsgIndexedDb
                                >> List.singleton
                            )
                        |> Maybe.withDefault []
            in
            ( model
            , Cmd.none
            , appMsgs
            )
                |> sequenceExtra (update language currentDate id isLabTech db) extraMsgs

        SetRandomBloodSugarTestFormBoolInput formUpdateFunc value ->
            let
                form =
                    model.laboratoryData.randomBloodSugarTestForm

                updatedForm =
                    formUpdateFunc value form

                updatedData =
                    model.laboratoryData
                        |> (\data -> { data | randomBloodSugarTestForm = updatedForm })
            in
            ( { model | laboratoryData = updatedData }
            , Cmd.none
            , []
            )

        SetRandomBloodSugarTestExecutionNote value ->
            let
                form =
                    model.laboratoryData.randomBloodSugarTestForm

                updatedForm =
                    { form | executionNote = Just value, executionNoteDirty = True }

                updatedData =
                    model.laboratoryData
                        |> (\data -> { data | randomBloodSugarTestForm = updatedForm })
            in
            ( { model | laboratoryData = updatedData }
            , Cmd.none
            , []
            )

        SetRandomBloodSugarResult value ->
            let
                form =
                    model.laboratoryData.randomBloodSugarTestForm

                updatedForm =
                    { form | sugarCount = String.toFloat value, sugarCountDirty = True }

                updatedData =
                    model.laboratoryData
                        |> (\data -> { data | randomBloodSugarTestForm = updatedForm })
            in
            ( { model | laboratoryData = updatedData }
            , Cmd.none
            , []
            )

        SaveRandomBloodSugarTest personId saved nextTask ->
            let
                measurementId =
                    Maybe.map Tuple.first saved

                measurement =
                    getMeasurementValueFunc saved

                extraMsgs =
                    generateLaboratoryMsgs nextTask

                appMsgs =
                    model.laboratoryData.randomBloodSugarTestForm
                        |> toRandomBloodSugarTestValueUniversalWithDefault measurement
                        |> Maybe.map
                            (Backend.PrenatalEncounter.Model.SaveRandomBloodSugarTest personId measurementId
                                >> Backend.Model.MsgPrenatalEncounter id
                                >> App.Model.MsgIndexedDb
                                >> List.singleton
                            )
                        |> Maybe.withDefault []
            in
            ( model
            , Cmd.none
            , appMsgs
            )
                |> sequenceExtra (update language currentDate id isLabTech db) extraMsgs

        SetHIVPCRTestFormBoolInput formUpdateFunc value ->
            let
                form =
                    model.laboratoryData.hivPCRTestForm

                updatedForm =
                    formUpdateFunc value form

                updatedData =
                    model.laboratoryData
                        |> (\data -> { data | hivPCRTestForm = updatedForm })
            in
            ( { model | laboratoryData = updatedData }
            , Cmd.none
            , []
            )

        SetHIVPCRTestExecutionNote value ->
            let
                form =
                    model.laboratoryData.hivPCRTestForm

                updatedForm =
                    { form | executionNote = Just value, executionNoteDirty = True }

                updatedData =
                    model.laboratoryData
                        |> (\data -> { data | hivPCRTestForm = updatedForm })
            in
            ( { model | laboratoryData = updatedData }
            , Cmd.none
            , []
            )

        SetHIVViralLoadUndetectable undetectable ->
            let
                form =
                    model.laboratoryData.hivPCRTestForm

                status =
                    if undetectable then
                        ViralLoadUndetectable

                    else
                        ViralLoadDetectable

                updatedForm =
                    { form | hivViralLoadStatus = Just status, hivViralLoadStatusDirty = True }

                updatedData =
                    model.laboratoryData
                        |> (\data -> { data | hivPCRTestForm = updatedForm })
            in
            ( { model | laboratoryData = updatedData }
            , Cmd.none
            , []
            )

        SetHIVViralLoad value ->
            let
                form =
                    model.laboratoryData.hivPCRTestForm

                updatedForm =
                    { form | hivViralLoad = String.toFloat value, hivViralLoadDirty = True }

                updatedData =
                    model.laboratoryData
                        |> (\data -> { data | hivPCRTestForm = updatedForm })
            in
            ( { model | laboratoryData = updatedData }
            , Cmd.none
            , []
            )

        SaveHIVPCRTest personId saved nextTask ->
            let
                measurementId =
                    Maybe.map Tuple.first saved

                measurement =
                    getMeasurementValueFunc saved

                extraMsgs =
                    generateLaboratoryMsgs nextTask

                appMsgs =
                    model.laboratoryData.hivPCRTestForm
                        |> toHIVPCRTestValueWithDefault measurement
                        |> Maybe.map
                            (Backend.PrenatalEncounter.Model.SaveHIVPCRTest personId measurementId
                                >> Backend.Model.MsgPrenatalEncounter id
                                >> App.Model.MsgIndexedDb
                                >> List.singleton
                            )
                        |> Maybe.withDefault []
            in
            ( model
            , Cmd.none
            , appMsgs
            )
                |> sequenceExtra (update language currentDate id isLabTech db) extraMsgs

        SetPartnerHIVTestFormBoolInput formUpdateFunc value ->
            let
                form =
                    model.laboratoryData.partnerHIVTestForm

                updatedForm =
                    formUpdateFunc value form

                updatedData =
                    model.laboratoryData
                        |> (\data -> { data | partnerHIVTestForm = updatedForm })
            in
            ( { model | laboratoryData = updatedData }
            , Cmd.none
            , []
            )

        SetPartnerHIVTestExecutionNote value ->
            let
                form =
                    model.laboratoryData.partnerHIVTestForm

                updatedForm =
                    { form | executionNote = Just value, executionNoteDirty = True }

                updatedData =
                    model.laboratoryData
                        |> (\data -> { data | partnerHIVTestForm = updatedForm })
            in
            ( { model | laboratoryData = updatedData }
            , Cmd.none
            , []
            )

        SetPartnerHIVTestResult value ->
            let
                form =
                    model.laboratoryData.partnerHIVTestForm

                updatedForm =
                    { form
                        | testResult = testResultFromString value
                        , partnerTakingARV = Nothing
                        , partnerTakingARVDirty = True
                        , partnerSurpressedViralLoad = Nothing
                        , partnerSurpressedViralLoadDirty = True
                    }

                updatedData =
                    model.laboratoryData
                        |> (\data -> { data | partnerHIVTestForm = updatedForm })
            in
            ( { model | laboratoryData = updatedData }
            , Cmd.none
            , []
            )

        SavePartnerHIVTest personId saved nextTask ->
            let
                measurementId =
                    Maybe.map Tuple.first saved

                measurement =
                    getMeasurementValueFunc saved

                extraMsgs =
                    generateLaboratoryMsgs nextTask

                appMsgs =
                    model.laboratoryData.partnerHIVTestForm
                        |> toPartnerHIVTestValueWithDefault measurement
                        |> Maybe.map
                            (Backend.PrenatalEncounter.Model.SavePartnerHIVTest personId measurementId
                                >> Backend.Model.MsgPrenatalEncounter id
                                >> App.Model.MsgIndexedDb
                                >> List.singleton
                            )
                        |> Maybe.withDefault []
            in
            ( model
            , Cmd.none
            , appMsgs
            )
                |> sequenceExtra (update language currentDate id isLabTech db) extraMsgs

        SetLabsHistoryCompleted value ->
            let
                form =
                    model.laboratoryData.labsHistoryForm

                updatedForm =
                    { form | completed = Just value }

                updatedData =
                    model.laboratoryData
                        |> (\data -> { data | labsHistoryForm = updatedForm })
            in
            ( { model | laboratoryData = updatedData }
            , Cmd.none
            , []
            )

        SaveLabsHistory ->
            let
                updatedData =
                    model.laboratoryData
                        |> (\data -> { data | activeTask = Nothing })
            in
            ( { model | laboratoryData = updatedData }
            , Cmd.none
            , [ Backend.PrenatalEncounter.Model.SetLabsHistoryCompleted
                    |> Backend.Model.MsgPrenatalEncounter id
                    |> App.Model.MsgIndexedDb
              ]
            )

        SetHealthEducationBoolInput formUpdateFunc value ->
            let
                updatedForm =
                    formUpdateFunc value model.healthEducationData.form

                updatedData =
                    model.healthEducationData
                        |> (\data -> { data | form = updatedForm })
            in
            ( { model | healthEducationData = updatedData }
            , Cmd.none
            , []
            )

        SaveHealthEducation personId saved ->
            let
                measurementId =
                    Maybe.map Tuple.first saved

                measurement =
                    getMeasurementValueFunc saved

                appMsgs =
                    model.healthEducationData.form
                        |> toHealthEducationValueWithDefault measurement
                        |> unwrap
                            []
                            (\value ->
                                [ Backend.PrenatalEncounter.Model.SaveHealthEducation personId measurementId value
                                    |> Backend.Model.MsgPrenatalEncounter id
                                    |> App.Model.MsgIndexedDb
                                , App.Model.SetActivePage <| UserPage <| PrenatalEncounterPage id
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

        SetHealthEducationSubActivityBoolInput formUpdateFunc value ->
            let
                updatedForm =
                    formUpdateFunc value model.nextStepsData.healthEducationForm

                updatedData =
                    model.nextStepsData
                        |> (\data -> { data | healthEducationForm = updatedForm })
            in
            ( { model | nextStepsData = updatedData }
            , Cmd.none
            , []
            )

        SaveHealthEducationSubActivity personId saved secondPhaseRequired nextTask ->
            let
                measurementId =
                    Maybe.map Tuple.first saved

                measurement =
                    getMeasurementValueFunc saved

                extraMsgs =
                    generateNextStepsMsgs secondPhaseRequired nextTask

                appMsgs =
                    model.nextStepsData.healthEducationForm
                        |> toHealthEducationValueWithDefault measurement
                        |> Maybe.map
                            (Backend.PrenatalEncounter.Model.SaveHealthEducation personId measurementId
                                >> Backend.Model.MsgPrenatalEncounter id
                                >> App.Model.MsgIndexedDb
                                >> List.singleton
                            )
                        |> Maybe.withDefault []
            in
            ( model
            , Cmd.none
            , appMsgs
            )
                |> sequenceExtra (update language currentDate id isLabTech db) extraMsgs

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

        SaveFollowUp personId assesment saved secondPhaseRequired nextTask ->
            let
                measurementId =
                    Maybe.map Tuple.first saved

                measurement =
                    getMeasurementValueFunc saved

                extraMsgs =
                    generateNextStepsMsgs secondPhaseRequired nextTask

                form =
                    model.nextStepsData.followUpForm

                appMsgs =
                    toFollowUpValueWithDefault measurement { form | assesment = Just assesment }
                        |> Maybe.map
                            (Backend.PrenatalEncounter.Model.SaveFollowUp personId measurementId
                                >> Backend.Model.MsgPrenatalEncounter id
                                >> App.Model.MsgIndexedDb
                                >> List.singleton
                            )
                        |> Maybe.withDefault []
            in
            ( model
            , Cmd.none
            , appMsgs
            )
                |> sequenceExtra (update language currentDate id isLabTech db) extraMsgs

        SaveNewbornEnrollment secondPhaseRequired nextTask ->
            let
                extraMsgs =
                    generateNextStepsMsgs secondPhaseRequired nextTask
            in
            ( model
            , Cmd.none
            , []
            )
                |> sequenceExtra (update language currentDate id isLabTech db) extraMsgs

        SetReferralBoolInput updateFunc value ->
            let
                updatedForm =
                    updateFunc value model.nextStepsData.referralForm

                updatedData =
                    model.nextStepsData
                        |> (\data -> { data | referralForm = updatedForm })
            in
            ( { model | nextStepsData = updatedData }
            , Cmd.none
            , []
            )

        SetHealthCenterNonReferralReason value ->
            let
                form =
                    model.nextStepsData.referralForm

                updatedForm =
                    { form | reasonForNotSendingToHC = Just value }

                updatedData =
                    model.nextStepsData
                        |> (\data -> { data | referralForm = updatedForm })
            in
            ( { model | nextStepsData = updatedData }
            , Cmd.none
            , []
            )

        SetFacilityNonReferralReason currentValue facility reason ->
            let
                referralForm =
                    Dict.get id db.prenatalMeasurements
                        |> Maybe.andThen RemoteData.toMaybe
                        |> Maybe.map
                            (.sendToHC
                                >> getMeasurementValueFunc
                                >> referralFormWithDefault model.nextStepsData.referralForm
                            )
                        |> Maybe.withDefault model.nextStepsData.referralForm

                updatedValue =
                    nonReferralReasonToSign facility reason

                updatedFacilityNonReferralReasons =
                    Maybe.map
                        (\facilityNonReferralReasons ->
                            case currentValue of
                                Just value ->
                                    EverySet.remove (nonReferralReasonToSign facility value) facilityNonReferralReasons
                                        |> EverySet.insert updatedValue

                                Nothing ->
                                    EverySet.insert updatedValue facilityNonReferralReasons
                        )
                        referralForm.facilityNonReferralReasons
                        |> Maybe.withDefault (EverySet.singleton updatedValue)

                updatedForm =
                    { referralForm | facilityNonReferralReasons = Just updatedFacilityNonReferralReasons }

                updatedData =
                    model.nextStepsData
                        |> (\data -> { data | referralForm = updatedForm })
            in
            ( { model | nextStepsData = updatedData }
            , Cmd.none
            , []
            )

        SaveSendToHC personId saved secondPhaseRequired nextTask ->
            let
                measurementId =
                    Maybe.map Tuple.first saved

                measurement =
                    getMeasurementValueFunc saved

                extraMsgs =
                    generateNextStepsMsgs secondPhaseRequired nextTask

                appMsgs =
                    model.nextStepsData.referralForm
                        |> toPrenatalReferralValueWithDefault measurement
                        |> Maybe.map
                            (Backend.PrenatalEncounter.Model.SaveSendToHC personId measurementId
                                >> Backend.Model.MsgPrenatalEncounter id
                                >> App.Model.MsgIndexedDb
                                >> List.singleton
                            )
                        |> Maybe.withDefault []
            in
            ( model
            , Cmd.none
            , appMsgs
            )
                |> sequenceExtra (update language currentDate id isLabTech db) extraMsgs

        SetAppointmentDateSelectorState state ->
            let
                updatedForm =
                    model.nextStepsData.appointmentConfirmationForm
                        |> (\form -> { form | dateSelectorPopupState = state })

                updatedData =
                    model.nextStepsData
                        |> (\data -> { data | appointmentConfirmationForm = updatedForm })
            in
            ( { model | nextStepsData = updatedData }
            , Cmd.none
            , []
            )

        SetAppointmentConfirmation value ->
            let
                updatedForm =
                    model.nextStepsData.appointmentConfirmationForm
                        |> (\form -> { form | appointmentDate = Just value })

                updatedData =
                    model.nextStepsData
                        |> (\data -> { data | appointmentConfirmationForm = updatedForm })
            in
            ( { model | nextStepsData = updatedData }
            , Cmd.none
            , []
            )

        SaveAppointmentConfirmation personId saved secondPhaseRequired nextTask ->
            let
                measurementId =
                    Maybe.map Tuple.first saved

                measurement =
                    getMeasurementValueFunc saved

                extraMsgs =
                    generateNextStepsMsgs secondPhaseRequired nextTask

                appMsgs =
                    model.nextStepsData.appointmentConfirmationForm
                        |> toAppointmentConfirmationValueWithDefault measurement
                        |> Maybe.map
                            (Backend.PrenatalEncounter.Model.SaveAppointmentConfirmation personId measurementId
                                >> Backend.Model.MsgPrenatalEncounter id
                                >> App.Model.MsgIndexedDb
                                >> List.singleton
                            )
                        |> Maybe.withDefault []
            in
            ( model
            , Cmd.none
            , appMsgs
            )
                |> sequenceExtra (update language currentDate id isLabTech db) extraMsgs

        SetMedicationDistributionBoolInput formUpdateFunc value ->
            let
                updatedForm =
                    formUpdateFunc value model.nextStepsData.medicationDistributionForm

                updatedData =
                    model.nextStepsData
                        |> (\data -> { data | medicationDistributionForm = updatedForm })
            in
            ( { model | nextStepsData = updatedData }
            , Cmd.none
            , []
            )

        SetMedicationDistributionAdministrationNote currentValue medication reason ->
            let
                updatedValue =
                    nonAdministrationReasonToSign medication reason

                updatedNonAdministrationSigns =
                    medicationDistributionForm.nonAdministrationSigns
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
                    { medicationDistributionForm | nonAdministrationSigns = Just updatedNonAdministrationSigns }

                updatedData =
                    model.nextStepsData
                        |> (\data -> { data | medicationDistributionForm = updatedForm })
            in
            ( { model | nextStepsData = updatedData }
            , Cmd.none
            , []
            )

        SetRecommendedTreatmentSign allowedSigns sign ->
            let
                updatedSigns =
                    -- Since we may have values from recurrent phase of encounter, we make
                    -- sure to preserve them, before setting new value at inital phase.
                    Maybe.map
                        (\signs ->
                            List.filter (\sign_ -> not <| List.member sign_ allowedSigns) signs
                                |> List.append [ sign ]
                        )
                        medicationDistributionForm.recommendedTreatmentSigns
                        |> Maybe.withDefault [ sign ]

                updatedForm =
                    { medicationDistributionForm
                        | recommendedTreatmentSigns = Just updatedSigns
                        , hypertensionAvoidingGuidanceReason = Nothing
                        , hypertensionAvoidingGuidanceReasonDirty = True
                    }

                updatedData =
                    model.nextStepsData
                        |> (\data -> { data | medicationDistributionForm = updatedForm })
            in
            ( { model | nextStepsData = updatedData }
            , Cmd.none
            , []
            )

        SetAvoidingGuidanceReason value ->
            let
                updatedForm =
                    { medicationDistributionForm
                        | hypertensionAvoidingGuidanceReason = Just value
                        , hypertensionAvoidingGuidanceReasonDirty = True
                    }

                updatedData =
                    model.nextStepsData
                        |> (\data -> { data | medicationDistributionForm = updatedForm })
            in
            ( { model | nextStepsData = updatedData }
            , Cmd.none
            , []
            )

        SaveMedicationDistribution personId saved secondPhaseRequired nextTask ->
            let
                measurementId =
                    Maybe.map Tuple.first saved

                measurement =
                    getMeasurementValueFunc saved

                extraMsgs =
                    generateNextStepsMsgs secondPhaseRequired nextTask

                appMsgs =
                    model.nextStepsData.medicationDistributionForm
                        |> toMedicationDistributionValueWithDefaultInitialPhase measurement
                        |> Maybe.map
                            (Backend.PrenatalEncounter.Model.SaveMedicationDistribution personId measurementId
                                >> Backend.Model.MsgPrenatalEncounter id
                                >> App.Model.MsgIndexedDb
                                >> List.singleton
                            )
                        |> Maybe.withDefault []
            in
            ( model
            , Cmd.none
            , appMsgs
            )
                |> sequenceExtra (update language currentDate id isLabTech db) extraMsgs

        SaveWait personId measurementId updatedValue ->
            let
                extraMsgs =
                    -- When saving Wait activity, we pause the encounter, and
                    -- navigate to main page.
                    [ SetActivePage PinCodePage ]

                appMsgs =
                    [ Backend.PrenatalEncounter.Model.SaveLabsResults personId measurementId updatedValue
                        |> Backend.Model.MsgPrenatalEncounter id
                        |> App.Model.MsgIndexedDb
                    , Backend.PrenatalEncounter.Model.CloseEncounter
                        |> Backend.Model.MsgPrenatalEncounter id
                        |> App.Model.MsgIndexedDb
                    ]
            in
            ( model
            , Cmd.none
            , appMsgs
            )
                |> sequenceExtra (update language currentDate id isLabTech db) extraMsgs

        SetSymptomReviewStep step ->
            let
                updatedData =
                    model.symptomReviewData
                        |> (\data -> { data | step = step })

                extraMsgs =
                    if step == SymptomReviewStepQuestions then
                        let
                            symptomReviewForm =
                                Dict.get id db.prenatalMeasurements
                                    |> Maybe.andThen RemoteData.toMaybe
                                    |> Maybe.map
                                        (.symptomReview
                                            >> getMeasurementValueFunc
                                            >> symptomReviewFormWithDefault model.symptomReviewData.form
                                        )
                                    |> Maybe.withDefault model.symptomReviewData.form
                        in
                        Maybe.map
                            (\symptoms ->
                                if List.member CoughContinuous symptoms then
                                    [ SetWarningPopupState (Just WarningPopupTuberculosis) ]

                                else
                                    []
                            )
                            symptomReviewForm.symptoms
                            |> Maybe.withDefault []

                    else
                        []
            in
            ( { model | symptomReviewData = updatedData }
            , Cmd.none
            , []
            )
                |> sequenceExtra (update language currentDate id isLabTech db) extraMsgs

        SetPrenatalSymptom symptom ->
            let
                form =
                    Dict.get id db.prenatalMeasurements
                        |> Maybe.withDefault NotAsked
                        |> RemoteData.toMaybe
                        |> Maybe.map
                            (.symptomReview
                                >> getMeasurementValueFunc
                                >> symptomReviewFormWithDefault model.symptomReviewData.form
                            )
                        |> Maybe.withDefault model.symptomReviewData.form

                updatedForm =
                    setMultiSelectInputValue .symptoms
                        (Maybe.map (updateSymptomReviewFormWithSymptoms form)
                            >> Maybe.withDefault form
                        )
                        NoPrenatalSymptoms
                        symptom
                        form

                updatedData =
                    model.symptomReviewData
                        |> (\data -> { data | form = updatedForm })
            in
            ( { model | symptomReviewData = updatedData }
            , Cmd.none
            , []
            )

        SetPrenatalSymptomQuestionBoolInput formUpdateFunc value ->
            let
                updatedForm =
                    formUpdateFunc value model.symptomReviewData.form

                updatedData =
                    model.symptomReviewData
                        |> (\data -> { data | form = updatedForm })
            in
            ( { model | symptomReviewData = updatedData }
            , Cmd.none
            , []
            )

        SetFlankPainSign sign ->
            let
                form =
                    model.symptomReviewData.form

                updatedForm =
                    { form | flankPainSign = Just sign }

                updatedData =
                    model.symptomReviewData
                        |> (\data -> { data | form = updatedForm })
            in
            ( { model | symptomReviewData = updatedData }
            , Cmd.none
            , []
            )

        SaveSymptomReview personId saved ->
            let
                measurementId =
                    Maybe.map Tuple.first saved

                measurement =
                    getMeasurementValueFunc saved

                appMsgs =
                    model.symptomReviewData.form
                        |> toSymptomReviewValueWithDefault measurement
                        |> unwrap
                            []
                            (\value ->
                                [ Backend.PrenatalEncounter.Model.SaveSymptomReview personId measurementId value
                                    |> Backend.Model.MsgPrenatalEncounter id
                                    |> App.Model.MsgIndexedDb
                                , App.Model.SetActivePage <| UserPage <| PrenatalEncounterPage id
                                ]
                            )

                updatedData =
                    model.symptomReviewData
                        |> (\data -> { data | step = SymptomReviewStepSymptoms })
            in
            ( { model | symptomReviewData = updatedData }
            , Cmd.none
            , appMsgs
            )

        SetActiveTreatmentReviewTask task ->
            let
                updatedData =
                    model.treatmentReviewData
                        |> (\data -> { data | activeTask = Just task })
            in
            ( { model | treatmentReviewData = updatedData }
            , Cmd.none
            , []
            )

        SetMedicationSubActivityBoolInput formUpdateFunc value ->
            let
                updatedForm =
                    formUpdateFunc value model.treatmentReviewData.medicationForm

                updatedData =
                    model.treatmentReviewData
                        |> (\data -> { data | medicationForm = updatedForm })
            in
            ( { model | treatmentReviewData = updatedData }
            , Cmd.none
            , []
            )

        SetHIVMedicationNotGivenReason value ->
            let
                updatedForm =
                    model.treatmentReviewData.medicationForm
                        |> (\form -> { form | hivMedicationNotGivenReason = Just value, hivMedicationNotGivenReasonDirty = True })

                updatedData =
                    model.treatmentReviewData
                        |> (\data -> { data | medicationForm = updatedForm })
            in
            ( { model | treatmentReviewData = updatedData }
            , Cmd.none
            , []
            )

        SaveMedicationSubActivity personId saved nextTask ->
            let
                measurementId =
                    Maybe.map Tuple.first saved

                measurement =
                    getMeasurementValueFunc saved

                extraMsgs =
                    generateMedicationSubActivityMsgs nextTask

                appMsgs =
                    model.treatmentReviewData.medicationForm
                        |> toMedicationValueWithDefault measurement
                        |> Maybe.map
                            (Backend.PrenatalEncounter.Model.SaveMedication personId measurementId
                                >> Backend.Model.MsgPrenatalEncounter id
                                >> App.Model.MsgIndexedDb
                                >> List.singleton
                            )
                        |> Maybe.withDefault []
            in
            ( model
            , Cmd.none
            , appMsgs
            )
                |> sequenceExtra (update language currentDate id isLabTech db) extraMsgs

        SetMentalHealthStep step ->
            let
                updatedForm =
                    model.mentalHealthData.form
                        |> (\form -> { form | step = step })

                updatedData =
                    model.mentalHealthData
                        |> (\data -> { data | form = updatedForm })
            in
            ( { model | mentalHealthData = updatedData }
            , Cmd.none
            , []
            )

        SetMentalHealthOptionForQuestion question option ->
            let
                form =
                    Dict.get id db.prenatalMeasurements
                        |> Maybe.withDefault NotAsked
                        |> RemoteData.toMaybe
                        |> Maybe.map
                            (.mentalHealth
                                >> getMeasurementValueFunc
                                >> mentalHealthFormWithDefault model.mentalHealthData.form
                            )
                        |> Maybe.withDefault model.mentalHealthData.form

                updatedSigns =
                    Maybe.map (Dict.insert question option) form.signs
                        |> Maybe.withDefault (Dict.singleton question option)

                updatedForm =
                    { form | signs = Just updatedSigns }

                updatedData =
                    model.mentalHealthData
                        |> (\data -> { data | form = updatedForm })
            in
            ( { model | mentalHealthData = updatedData }
            , Cmd.none
            , []
            )

        SetSpecialistAtHC value ->
            let
                updatedForm =
                    model.mentalHealthData.form
                        |> (\form -> { form | specialistAtHC = Just value })

                updatedData =
                    model.mentalHealthData
                        |> (\data -> { data | form = updatedForm })
            in
            ( { model | mentalHealthData = updatedData }
            , Cmd.none
            , []
            )

        SaveMentalHealth personId saved ->
            let
                measurementId =
                    Maybe.map Tuple.first saved

                measurement =
                    getMeasurementValueFunc saved

                extraMsgs =
                    if List.isEmpty appMsgs then
                        []

                    else
                        [ SetActivePage <| UserPage <| PrenatalEncounterPage id
                        , SetWarningPopupState Nothing
                        ]

                appMsgs =
                    model.mentalHealthData.form
                        |> toPrenatalMentalHealthValueWithDefault measurement
                        |> Maybe.map
                            (Backend.PrenatalEncounter.Model.SaveMentalHealth personId measurementId
                                >> Backend.Model.MsgPrenatalEncounter id
                                >> App.Model.MsgIndexedDb
                                >> List.singleton
                            )
                        |> Maybe.withDefault []
            in
            ( model
            , Cmd.none
            , appMsgs
            )
                |> sequenceExtra (update language currentDate id isLabTech db) extraMsgs

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

        SaveTetanusImmunisation personId saved ->
            let
                measurement =
                    getMeasurementValueFunc saved

                appMsgs =
                    model.immunisationData.tetanusForm
                        |> toVaccinationValueWithDefault measurement
                        |> Maybe.map
                            (\value ->
                                let
                                    measurementId =
                                        Maybe.map Tuple.first saved
                                in
                                [ Backend.PrenatalEncounter.Model.SaveTetanusImmunisation personId measurementId value
                                    |> Backend.Model.MsgPrenatalEncounter id
                                    |> App.Model.MsgIndexedDb
                                , App.Model.SetActivePage <| UserPage <| PrenatalEncounterPage id
                                ]
                            )
                        |> Maybe.withDefault []
            in
            ( model
            , Cmd.none
            , appMsgs
            )

        SetPostpartumTreatmentReviewBoolInput formUpdateFunc value ->
            let
                updatedForm =
                    formUpdateFunc value model.postpartumTreatmentReviewData.form

                updatedData =
                    model.postpartumTreatmentReviewData
                        |> (\data -> { data | form = updatedForm })
            in
            ( { model | postpartumTreatmentReviewData = updatedData }
            , Cmd.none
            , []
            )

        SavePostpartumTreatmentReview personId saved ->
            let
                measurementId =
                    Maybe.map Tuple.first saved

                measurement =
                    getMeasurementValueFunc saved

                appMsgs =
                    model.postpartumTreatmentReviewData.form
                        |> toMedicationValueWithDefault measurement
                        |> unwrap
                            []
                            (\value ->
                                [ Backend.PrenatalEncounter.Model.SaveMedication personId measurementId value
                                    |> Backend.Model.MsgPrenatalEncounter id
                                    |> App.Model.MsgIndexedDb
                                , App.Model.SetActivePage <| UserPage <| PrenatalEncounterPage id
                                ]
                            )
            in
            ( model
            , Cmd.none
            , appMsgs
            )

        SetBreastfeedingBoolInput formUpdateFunc value ->
            let
                updatedForm =
                    formUpdateFunc value model.breastfeedingData.form

                updatedData =
                    model.breastfeedingData
                        |> (\data -> { data | form = updatedForm })
            in
            ( { model | breastfeedingData = updatedData }
            , Cmd.none
            , []
            )

        SetReasonForNotBreastfeeding value ->
            let
                form =
                    model.breastfeedingData.form

                updatedForm =
                    { form | reasonForNotBreastfeeding = Just value, reasonForNotBreastfeedingDirty = True }

                updatedData =
                    model.breastfeedingData
                        |> (\data -> { data | form = updatedForm })
            in
            ( { model | breastfeedingData = updatedData }
            , Cmd.none
            , []
            )

        SaveBreastfeeding personId saved ->
            let
                measurementId =
                    Maybe.map Tuple.first saved

                measurement =
                    getMeasurementValueFunc saved

                appMsgs =
                    model.breastfeedingData.form
                        |> toBreastfeedingValueWithDefault measurement
                        |> unwrap
                            []
                            (\value ->
                                [ Backend.PrenatalEncounter.Model.SaveBreastfeeding personId measurementId value
                                    |> Backend.Model.MsgPrenatalEncounter id
                                    |> App.Model.MsgIndexedDb
                                , App.Model.SetActivePage <| UserPage <| PrenatalEncounterPage id
                                ]
                            )
            in
            ( model
            , Cmd.none
            , appMsgs
            )

        SetSpecialityCareBoolInput formUpdateFunc value ->
            let
                updatedForm =
                    formUpdateFunc value model.specialityCareData.form

                updatedData =
                    model.specialityCareData
                        |> (\data -> { data | form = updatedForm })
            in
            ( { model | specialityCareData = updatedData }
            , Cmd.none
            , []
            )

        SaveSpecialityCare personId saved ->
            let
                measurementId =
                    Maybe.map Tuple.first saved

                measurement =
                    getMeasurementValueFunc saved

                appMsgs =
                    model.specialityCareData.form
                        |> toSpecialityCareValueWithDefault measurement
                        |> unwrap
                            []
                            (\value ->
                                [ Backend.PrenatalEncounter.Model.SaveSpecialityCare personId measurementId value
                                    |> Backend.Model.MsgPrenatalEncounter id
                                    |> App.Model.MsgIndexedDb
                                , App.Model.SetActivePage <| UserPage <| PrenatalEncounterPage id
                                ]
                            )
            in
            ( model
            , Cmd.none
            , appMsgs
            )
