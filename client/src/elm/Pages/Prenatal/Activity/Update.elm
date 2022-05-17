module Pages.Prenatal.Activity.Update exposing (update)

import App.Model
import AssocList as Dict
import Backend.Entities exposing (PrenatalEncounterId)
import Backend.IndividualEncounterParticipant.Model
import Backend.Measurement.Decoder exposing (pregnancyTestResultFromString)
import Backend.Measurement.Model
    exposing
        ( AbdomenCPESign(..)
        , BreastExamSign(..)
        , CSectionReason(..)
        , DangerSign(..)
        , FamilyPlanningSign(..)
        , HandsCPESign(..)
        , LegsCPESign(..)
        , LungsCPESign(..)
        , NeckCPESign(..)
        , PhotoUrl(..)
        , PostpartumChildDangerSign(..)
        , PostpartumMotherDangerSign(..)
        , PrenatalSymptom(..)
        , PreviousDeliveryPeriod(..)
        , SocialHistoryHivTestingResult(..)
        )
import Backend.Measurement.Utils exposing (getMeasurementValueFunc, prenatalTestResultFromString)
import Backend.Model exposing (ModelIndexedDb)
import Backend.PrenatalEncounter.Model
import Backend.PrenatalEncounter.Utils exposing (lmpToEDDDate)
import Date exposing (Unit(..))
import EverySet exposing (EverySet)
import Gizra.NominalDate exposing (NominalDate)
import Gizra.Update exposing (sequenceExtra)
import Maybe.Extra exposing (isJust, isNothing, unwrap)
import Measurement.Utils exposing (toSendToHCValueWithDefault, toVitalsValueWithDefault)
import Pages.AcuteIllness.Activity.Utils exposing (nonAdministrationReasonToSign)
import Pages.Page exposing (Page(..), UserPage(..))
import Pages.Prenatal.Activity.Model exposing (..)
import Pages.Prenatal.Activity.Types exposing (..)
import Pages.Prenatal.Activity.Utils exposing (..)
import Pages.Prenatal.Utils exposing (..)
import Pages.Utils exposing (setMultiSelectInputValue, tasksBarId)
import RemoteData exposing (RemoteData(..))
import Result exposing (Result)
import Translate exposing (Language, translate)


update : Language -> NominalDate -> PrenatalEncounterId -> ModelIndexedDb -> Msg -> Model -> ( Model, Cmd Msg, List App.Model.Msg )
update language currentDate id db msg model =
    let
        noChange =
            ( model, Cmd.none, [] )

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

        symptomReviewForm =
            Dict.get id db.prenatalMeasurements
                |> Maybe.andThen RemoteData.toMaybe
                |> Maybe.map
                    (.symptomReview
                        >> getMeasurementValueFunc
                        >> symptomReviewFormWithDefault model.symptomReviewData.form
                    )
                |> Maybe.withDefault model.symptomReviewData.form

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
            Maybe.map (\task -> [ SetActiveTreatmentReviewTask task ]) nextTask
                |> Maybe.withDefault [ SetActivePage <| UserPage <| PrenatalEncounterPage id ]
    in
    case msg of
        NoOp ->
            noChange

        DropZoneComplete result ->
            let
                updatedData =
                    model.prenatalPhotoData
                        |> (\data -> { data | url = Just (PhotoUrl result.url) })
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

        ViewWarningPopupForNonUrgentDiagnoses ->
            let
                nonUrgentDiagnoses =
                    Dict.get id db.prenatalEncounters
                        |> Maybe.andThen RemoteData.toMaybe
                        |> Maybe.map (.diagnoses >> EverySet.toList >> listNonUrgentDiagnoses)
                        |> Maybe.withDefault []

                extraMsgs =
                    if List.isEmpty nonUrgentDiagnoses then
                        []

                    else
                        let
                            message =
                                List.map (Translate.PrenatalDiagnosisNonUrgentMessage >> translate language) nonUrgentDiagnoses
                                    |> String.join ", "
                        in
                        [ SetWarningPopupState <| Just ( message, "" ) ]
            in
            sequenceExtra (update language currentDate id db) extraMsgs noChange

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

        SetConfirmLmpDate confirmedDate confirmed ->
            let
                updatedForm =
                    if confirmed then
                        model.pregnancyDatingData.form
                            |> (\form -> { form | chwLmpConfirmation = Just True, lmpDate = Just confirmedDate, lmpDateConfident = Just True })

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
                        |> (\form -> { form | lmpDateConfident = Just value })

                updatedData =
                    model.pregnancyDatingData
                        |> (\data -> { data | form = updatedForm })
            in
            ( { model | pregnancyDatingData = updatedData }
            , Cmd.none
            , []
            )

        SetLmpRange value ->
            let
                range =
                    decodeLmpRange value

                updatedForm =
                    model.pregnancyDatingData.form
                        |> (\form -> { form | lmpRange = range })

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
                                    |> Backend.Model.MsgIndividualSession prenatalParticipantId
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
                        |> (\data -> { data | activeTask = task })
            in
            ( { model | historyData = updatedData }
            , Cmd.none
            , []
            )

        SetCurrentlyPregnant value ->
            let
                updatedData =
                    case model.historyData.activeTask of
                        Obstetric ->
                            case model.historyData.obstetricHistoryStep of
                                ObstetricHistoryFirstStep ->
                                    let
                                        form =
                                            model.historyData.obstetricFormFirstStep

                                        updatedForm =
                                            { form | currentlyPregnant = Just value }
                                    in
                                    model.historyData
                                        |> (\data -> { data | obstetricFormFirstStep = updatedForm })

                                -- We should never get here.
                                -- Input is set on first step.
                                ObstetricHistorySecondStep ->
                                    model.historyData

                        _ ->
                            model.historyData
            in
            ( { model | historyData = updatedData }
            , Cmd.none
            , []
            )

        SetOBIntInput formUpdateFunc value ->
            let
                updatedData =
                    case model.historyData.activeTask of
                        Obstetric ->
                            case model.historyData.obstetricHistoryStep of
                                ObstetricHistoryFirstStep ->
                                    let
                                        form =
                                            model.historyData.obstetricFormFirstStep

                                        updatedForm =
                                            formUpdateFunc (String.toInt value) form
                                    in
                                    model.historyData
                                        |> (\data -> { data | obstetricFormFirstStep = updatedForm })

                                -- We should never get here.
                                -- Input is set on first step.
                                ObstetricHistorySecondStep ->
                                    model.historyData

                        _ ->
                            model.historyData
            in
            ( { model | historyData = updatedData }
            , Cmd.none
            , []
            )

        SaveOBHistoryStep1 personId saved ->
            let
                measurementId =
                    Maybe.map Tuple.first saved

                measurement =
                    getMeasurementValueFunc saved

                ( appMsgs, updatedData ) =
                    case model.historyData.obstetricHistoryStep of
                        ObstetricHistoryFirstStep ->
                            ( model.historyData.obstetricFormFirstStep
                                |> toObstetricHistoryValueWithDefault measurement
                                |> unwrap
                                    []
                                    (\value ->
                                        [ Backend.PrenatalEncounter.Model.SaveObstetricHistory personId measurementId value
                                            |> Backend.Model.MsgPrenatalEncounter id
                                            |> App.Model.MsgIndexedDb
                                        ]
                                    )
                            , model.historyData
                                |> (\data -> { data | obstetricHistoryStep = ObstetricHistorySecondStep })
                            )

                        -- Satisfy compiler.
                        ObstetricHistorySecondStep ->
                            ( [], model.historyData )
            in
            ( { model | historyData = updatedData }
            , Cmd.none
            , appMsgs
            )

        SetCSectionReason reason ->
            let
                updatedData =
                    case model.historyData.activeTask of
                        Obstetric ->
                            case model.historyData.obstetricHistoryStep of
                                ObstetricHistorySecondStep ->
                                    let
                                        form =
                                            model.historyData.obstetricFormSecondStep

                                        updatedReason =
                                            if form.cSectionReason == Just reason then
                                                Nothing

                                            else
                                                Just reason

                                        updatedForm =
                                            { form | cSectionReason = updatedReason }
                                    in
                                    model.historyData
                                        |> (\data -> { data | obstetricFormSecondStep = updatedForm })

                                -- We should never get here.
                                -- Input is set on second step.
                                ObstetricHistoryFirstStep ->
                                    model.historyData

                        _ ->
                            model.historyData
            in
            ( { model | historyData = updatedData }
            , Cmd.none
            , []
            )

        SetNumberOfCSections value ->
            let
                updatedData =
                    case model.historyData.activeTask of
                        Obstetric ->
                            case model.historyData.obstetricHistoryStep of
                                ObstetricHistorySecondStep ->
                                    let
                                        form =
                                            model.historyData.obstetricFormSecondStep

                                        updatedForm =
                                            let
                                                updatedValue =
                                                    String.toInt value
                                            in
                                            { form | cSections = updatedValue, cSectionsDirty = True }
                                    in
                                    model.historyData
                                        |> (\data -> { data | obstetricFormSecondStep = updatedForm })

                                -- We should never get here.
                                -- Input is set on first step.
                                ObstetricHistoryFirstStep ->
                                    model.historyData

                        _ ->
                            model.historyData
            in
            ( { model | historyData = updatedData }
            , Cmd.none
            , []
            )

        SetOBBoolInput formUpdateFunc value ->
            let
                updatedData =
                    case model.historyData.activeTask of
                        Obstetric ->
                            case model.historyData.obstetricHistoryStep of
                                ObstetricHistorySecondStep ->
                                    let
                                        updatedForm =
                                            formUpdateFunc value model.historyData.obstetricFormSecondStep
                                    in
                                    model.historyData
                                        |> (\data -> { data | obstetricFormSecondStep = updatedForm })

                                -- We should never get here.
                                -- Input is set on second step.
                                ObstetricHistoryFirstStep ->
                                    model.historyData

                        _ ->
                            model.historyData
            in
            ( { model | historyData = updatedData }
            , Cmd.none
            , []
            )

        SetPreviousDeliveryPeriod period ->
            let
                updatedData =
                    case model.historyData.activeTask of
                        Obstetric ->
                            case model.historyData.obstetricHistoryStep of
                                ObstetricHistorySecondStep ->
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
                                    in
                                    model.historyData
                                        |> (\data -> { data | obstetricFormSecondStep = updatedForm })

                                -- We should never get here.
                                -- Input is set on second step.
                                ObstetricHistoryFirstStep ->
                                    model.historyData

                        _ ->
                            model.historyData
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

        SaveOBHistoryStep2 personId saved nextTask_ ->
            let
                measurementId =
                    Maybe.map Tuple.first saved

                measurement =
                    getMeasurementValueFunc saved

                ( backToActivitiesMsg, nextTask ) =
                    nextTask_
                        |> Maybe.map (\task -> ( [], task ))
                        |> Maybe.withDefault
                            ( [ App.Model.SetActivePage <| UserPage <| PrenatalEncounterPage id ]
                            , Obstetric
                            )

                ( appMsgs, updatedData ) =
                    case model.historyData.obstetricHistoryStep of
                        -- Satisfy compiler.
                        ObstetricHistoryFirstStep ->
                            ( [], model.historyData )

                        ObstetricHistorySecondStep ->
                            ( model.historyData.obstetricFormSecondStep
                                |> toObstetricHistoryStep2ValueWithDefault measurement
                                |> unwrap
                                    []
                                    (\value ->
                                        (Backend.PrenatalEncounter.Model.SaveObstetricHistoryStep2 personId measurementId value
                                            |> Backend.Model.MsgPrenatalEncounter id
                                            |> App.Model.MsgIndexedDb
                                        )
                                            :: backToActivitiesMsg
                                    )
                            , model.historyData
                                |> (\data -> { data | obstetricHistoryStep = ObstetricHistoryFirstStep, activeTask = nextTask })
                            )
            in
            ( { model | historyData = updatedData }
            , Cmd.none
            , App.Model.ScrollToElement tasksBarId :: appMsgs
            )

        SetMedicalBoolInput formUpdateFunc value ->
            let
                updatedData =
                    let
                        updatedForm =
                            formUpdateFunc value model.historyData.medicalForm
                    in
                    model.historyData
                        |> (\data -> { data | medicalForm = updatedForm })
            in
            ( { model | historyData = updatedData }
            , Cmd.none
            , []
            )

        SaveMedicalHistory personId saved nextTask_ ->
            let
                measurementId =
                    Maybe.map Tuple.first saved

                measurement =
                    getMeasurementValueFunc saved

                ( backToActivitiesMsg, nextTask ) =
                    nextTask_
                        |> Maybe.map (\task -> ( [], task ))
                        |> Maybe.withDefault
                            ( [ App.Model.SetActivePage <| UserPage <| PrenatalEncounterPage id ]
                            , Obstetric
                            )

                appMsgs =
                    model.historyData.medicalForm
                        |> toMedicalHistoryValueWithDefault measurement
                        |> unwrap
                            []
                            (\value ->
                                (Backend.PrenatalEncounter.Model.SaveMedicalHistory personId measurementId value
                                    |> Backend.Model.MsgPrenatalEncounter id
                                    |> App.Model.MsgIndexedDb
                                )
                                    :: backToActivitiesMsg
                            )

                updatedData =
                    model.historyData
                        |> (\data -> { data | activeTask = nextTask })
            in
            ( { model | historyData = updatedData }
            , Cmd.none
            , appMsgs
            )

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

        SetSocialHivTestingResult value ->
            let
                result =
                    socialHistoryHivTestingResultFromString value

                updatedData =
                    let
                        updatedForm =
                            model.historyData.socialForm
                                |> (\form -> { form | partnerTestingResult = result })
                    in
                    model.historyData
                        |> (\data -> { data | socialForm = updatedForm })
            in
            ( { model | historyData = updatedData }
            , Cmd.none
            , []
            )

        SaveSocialHistory personId saved nextTask_ ->
            let
                measurementId =
                    Maybe.map Tuple.first saved

                measurement =
                    getMeasurementValueFunc saved

                updatedForm =
                    if isNothing model.historyData.socialForm.partnerTestingResult then
                        model.historyData.socialForm
                            |> (\form -> { form | partnerTestingResult = Just NoHivTesting })

                    else
                        model.historyData.socialForm

                ( backToActivitiesMsg, nextTask ) =
                    nextTask_
                        |> Maybe.map (\task -> ( [], task ))
                        |> Maybe.withDefault
                            ( [ App.Model.SetActivePage <| UserPage <| PrenatalEncounterPage id ]
                            , Obstetric
                            )

                appMsgs =
                    updatedForm
                        |> toSocialHistoryValueWithDefault measurement
                        |> unwrap
                            []
                            (\value ->
                                (Backend.PrenatalEncounter.Model.SaveSocialHistory personId measurementId value
                                    |> Backend.Model.MsgPrenatalEncounter id
                                    |> App.Model.MsgIndexedDb
                                )
                                    :: backToActivitiesMsg
                            )

                updatedData =
                    model.historyData
                        |> (\data -> { data | activeTask = nextTask })
            in
            ( { model | historyData = updatedData }
            , Cmd.none
            , appMsgs
            )

        SetActiveExaminationTask task ->
            let
                updatedData =
                    model.examinationData
                        |> (\data -> { data | activeTask = task })
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

        SaveVitals personId saved nextTask_ ->
            let
                measurementId =
                    Maybe.map Tuple.first saved

                measurement =
                    getMeasurementValueFunc saved

                ( backToActivitiesMsg, nextTask ) =
                    nextTask_
                        |> Maybe.map (\task -> ( [], task ))
                        |> Maybe.withDefault
                            ( [ App.Model.SetActivePage <| UserPage <| PrenatalEncounterPage id ]
                            , Vitals
                            )

                appMsgs =
                    model.examinationData.vitalsForm
                        |> toVitalsValueWithDefault measurement
                        |> unwrap
                            []
                            (\value ->
                                (Backend.PrenatalEncounter.Model.SaveVitals personId measurementId value
                                    |> Backend.Model.MsgPrenatalEncounter id
                                    |> App.Model.MsgIndexedDb
                                )
                                    :: backToActivitiesMsg
                            )

                updatedData =
                    model.examinationData
                        |> (\data -> { data | activeTask = nextTask })
            in
            ( { model | examinationData = updatedData }
            , Cmd.none
            , appMsgs
            )

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

        SaveNutritionAssessment personId saved maybeHeight nextTask_ ->
            let
                measurementId =
                    Maybe.map Tuple.first saved

                measurement =
                    getMeasurementValueFunc saved

                form_ =
                    model.examinationData.nutritionAssessmentForm

                form =
                    maybeHeight
                        |> Maybe.map (\height -> { form_ | height = Just height })
                        |> Maybe.withDefault form_

                ( backToActivitiesMsg, nextTask ) =
                    nextTask_
                        |> Maybe.map (\task -> ( [], task ))
                        |> Maybe.withDefault
                            ( [ App.Model.SetActivePage <| UserPage <| PrenatalEncounterPage id ]
                            , Vitals
                            )

                appMsgs =
                    form
                        |> toPrenatalNutritionValueWithDefault measurement
                        |> unwrap
                            []
                            (\value ->
                                (Backend.PrenatalEncounter.Model.SaveNutrition personId measurementId value
                                    |> Backend.Model.MsgPrenatalEncounter id
                                    |> App.Model.MsgIndexedDb
                                )
                                    :: backToActivitiesMsg
                            )

                updatedData =
                    model.examinationData
                        |> (\data -> { data | activeTask = nextTask })
            in
            ( { model | examinationData = updatedData }
            , Cmd.none
            , appMsgs
            )

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

        SaveCorePhysicalExam personId saved nextTask_ ->
            let
                measurementId =
                    Maybe.map Tuple.first saved

                measurement =
                    getMeasurementValueFunc saved

                ( backToActivitiesMsg, nextTask ) =
                    nextTask_
                        |> Maybe.map (\task -> ( [], task ))
                        |> Maybe.withDefault
                            ( [ App.Model.SetActivePage <| UserPage <| PrenatalEncounterPage id ]
                            , Vitals
                            )

                appMsgs =
                    model.examinationData.corePhysicalExamForm
                        |> toCorePhysicalExamValueWithDefault measurement
                        |> unwrap
                            []
                            (\value ->
                                (Backend.PrenatalEncounter.Model.SaveCorePhysicalExam personId measurementId value
                                    |> Backend.Model.MsgPrenatalEncounter id
                                    |> App.Model.MsgIndexedDb
                                )
                                    :: backToActivitiesMsg
                            )

                updatedData =
                    model.examinationData
                        |> (\data -> { data | activeTask = nextTask })
            in
            ( { model | examinationData = updatedData }
            , Cmd.none
            , appMsgs
            )

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
                        updatedForm =
                            formUpdateFunc (String.toInt value) model.examinationData.obstetricalExamForm
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

        SaveObstetricalExam personId saved nextTask_ ->
            let
                measurementId =
                    Maybe.map Tuple.first saved

                measurement =
                    getMeasurementValueFunc saved

                ( backToActivitiesMsg, nextTask ) =
                    nextTask_
                        |> Maybe.map (\task -> ( [], task ))
                        |> Maybe.withDefault
                            ( [ App.Model.SetActivePage <| UserPage <| PrenatalEncounterPage id ]
                            , Vitals
                            )

                appMsgs =
                    model.examinationData.obstetricalExamForm
                        |> toObstetricalExamValueWithDefault measurement
                        |> unwrap
                            []
                            (\value ->
                                (Backend.PrenatalEncounter.Model.SaveObstetricalExam personId measurementId value
                                    |> Backend.Model.MsgPrenatalEncounter id
                                    |> App.Model.MsgIndexedDb
                                )
                                    :: backToActivitiesMsg
                            )

                updatedData =
                    model.examinationData
                        |> (\data -> { data | activeTask = nextTask })
            in
            ( { model | examinationData = updatedData }
            , Cmd.none
            , appMsgs
            )

        SetBreastExamBoolInput formUpdateFunc value ->
            let
                updatedData =
                    let
                        updatedForm =
                            formUpdateFunc value model.examinationData.breastExamForm
                    in
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
                        |> Maybe.withDefault NotAsked
                        |> RemoteData.toMaybe
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

                updatedData =
                    model.examinationData
                        |> (\data -> { data | breastExamForm = updatedForm })
            in
            ( { model | examinationData = updatedData }
            , Cmd.none
            , []
            )

        SaveBreastExam personId saved nextTask_ ->
            let
                measurementId =
                    Maybe.map Tuple.first saved

                measurement =
                    getMeasurementValueFunc saved

                ( backToActivitiesMsg, nextTask ) =
                    nextTask_
                        |> Maybe.map (\task -> ( [], task ))
                        |> Maybe.withDefault
                            ( [ App.Model.SetActivePage <| UserPage <| PrenatalEncounterPage id ]
                            , Vitals
                            )

                appMsgs =
                    model.examinationData.breastExamForm
                        |> toBreastExamValueWithDefault measurement
                        |> unwrap
                            []
                            (\value ->
                                (Backend.PrenatalEncounter.Model.SaveBreastExam personId measurementId value
                                    |> Backend.Model.MsgPrenatalEncounter id
                                    |> App.Model.MsgIndexedDb
                                )
                                    :: backToActivitiesMsg
                            )

                updatedData =
                    model.examinationData
                        |> (\data -> { data | activeTask = nextTask })
            in
            ( { model | examinationData = updatedData }
            , Cmd.none
            , appMsgs
            )

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

        SetMedicationBoolInput formUpdateFunc value ->
            let
                updatedData =
                    let
                        updatedForm =
                            formUpdateFunc value model.medicationData.form
                    in
                    model.medicationData
                        |> (\data -> { data | form = updatedForm })
            in
            ( { model | medicationData = updatedData }
            , Cmd.none
            , []
            )

        SaveMedication personId saved ->
            let
                measurementId =
                    Maybe.map Tuple.first saved

                measurement =
                    getMeasurementValueFunc saved

                appMsgs =
                    model.medicationData.form
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

        SetMalariaPreventionBoolInput formUpdateFunc value ->
            let
                updatedData =
                    let
                        updatedForm =
                            formUpdateFunc value model.malariaPreventionData.form
                    in
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
                        |> toMalariaPreventionValueWithDefault measurement
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
                result =
                    pregnancyTestResultFromString value

                updatedData =
                    let
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

        SetHIVTestExecutionDate value ->
            let
                form =
                    model.laboratoryData.hivTestForm

                updatedForm =
                    { form | executionDate = Just value }

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
                        | testResult = prenatalTestResultFromString value
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

        SetHIVTestDateSelectorState state ->
            let
                form =
                    model.laboratoryData.hivTestForm

                defaultSelection =
                    Maybe.Extra.or form.executionDate (Maybe.andThen .dateDefault state)

                updatedForm =
                    { form | dateSelectorPopupState = state, executionDate = defaultSelection }

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
                        |> toPrenatalHIVTestValueWithDefault measurement
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
                |> sequenceExtra (update language currentDate id db) extraMsgs

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

        SetSyphilisTestExecutionDate value ->
            let
                form =
                    model.laboratoryData.syphilisTestForm

                updatedForm =
                    { form | executionDate = Just value }

                updatedData =
                    model.laboratoryData
                        |> (\data -> { data | syphilisTestForm = updatedForm })
            in
            ( { model | laboratoryData = updatedData }
            , Cmd.none
            , []
            )

        SetSyphilisTestDateSelectorState state ->
            let
                form =
                    model.laboratoryData.syphilisTestForm

                defaultSelection =
                    Maybe.Extra.or form.executionDate (Maybe.andThen .dateDefault state)

                updatedForm =
                    { form | dateSelectorPopupState = state, executionDate = defaultSelection }

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
                        |> toPrenatalNonRDTValueWithDefault measurement toSyphilisTestValueWithEmptyResults
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
                |> sequenceExtra (update language currentDate id db) extraMsgs

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

        SetHepatitisBTestExecutionDate value ->
            let
                form =
                    model.laboratoryData.hepatitisBTestForm

                updatedForm =
                    { form | executionDate = Just value }

                updatedData =
                    model.laboratoryData
                        |> (\data -> { data | hepatitisBTestForm = updatedForm })
            in
            ( { model | laboratoryData = updatedData }
            , Cmd.none
            , []
            )

        SetHepatitisBTestDateSelectorState state ->
            let
                form =
                    model.laboratoryData.hepatitisBTestForm

                defaultSelection =
                    Maybe.Extra.or form.executionDate (Maybe.andThen .dateDefault state)

                updatedForm =
                    { form | dateSelectorPopupState = state, executionDate = defaultSelection }

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
                        |> toPrenatalNonRDTValueWithDefault measurement toHepatitisBTestValueWithEmptyResults
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
                |> sequenceExtra (update language currentDate id db) extraMsgs

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

        SetMalariaTestExecutionDate value ->
            let
                form =
                    model.laboratoryData.malariaTestForm

                updatedForm =
                    { form | executionDate = Just value }

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
                    { form | testResult = prenatalTestResultFromString value }

                updatedData =
                    model.laboratoryData
                        |> (\data -> { data | malariaTestForm = updatedForm })
            in
            ( { model | laboratoryData = updatedData }
            , Cmd.none
            , []
            )

        SetMalariaTestDateSelectorState state ->
            let
                form =
                    model.laboratoryData.malariaTestForm

                defaultSelection =
                    Maybe.Extra.or form.executionDate (Maybe.andThen .dateDefault state)

                updatedForm =
                    { form | dateSelectorPopupState = state, executionDate = defaultSelection }

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
                        |> toPrenatalMalariaTestValueWithDefault measurement
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
                |> sequenceExtra (update language currentDate id db) extraMsgs

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

        SetBloodGpRsTestExecutionDate value ->
            let
                form =
                    model.laboratoryData.bloodGpRsTestForm

                updatedForm =
                    { form | executionDate = Just value }

                updatedData =
                    model.laboratoryData
                        |> (\data -> { data | bloodGpRsTestForm = updatedForm })
            in
            ( { model | laboratoryData = updatedData }
            , Cmd.none
            , []
            )

        SetBloodGpRsTestDateSelectorState state ->
            let
                form =
                    model.laboratoryData.bloodGpRsTestForm

                defaultSelection =
                    Maybe.Extra.or form.executionDate (Maybe.andThen .dateDefault state)

                updatedForm =
                    { form | dateSelectorPopupState = state, executionDate = defaultSelection }

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
                        |> toPrenatalNonRDTValueWithDefault measurement toBloodGpRsTestValueWithEmptyResults
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
                |> sequenceExtra (update language currentDate id db) extraMsgs

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
                    { form | testVariant = Just value }

                updatedData =
                    model.laboratoryData
                        |> (\data -> { data | urineDipstickTestForm = updatedForm })
            in
            ( { model | laboratoryData = updatedData }
            , Cmd.none
            , []
            )

        SetUrineDipstickTestExecutionDate value ->
            let
                form =
                    model.laboratoryData.urineDipstickTestForm

                updatedForm =
                    { form | executionDate = Just value }

                updatedData =
                    model.laboratoryData
                        |> (\data -> { data | urineDipstickTestForm = updatedForm })
            in
            ( { model | laboratoryData = updatedData }
            , Cmd.none
            , []
            )

        SetUrineDipstickTestDateSelectorState state ->
            let
                form =
                    model.laboratoryData.urineDipstickTestForm

                defaultSelection =
                    Maybe.Extra.or form.executionDate (Maybe.andThen .dateDefault state)

                updatedForm =
                    { form | dateSelectorPopupState = state, executionDate = defaultSelection }

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
                        |> toPrenatalUrineDipstickTestValueWithDefault measurement
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
                |> sequenceExtra (update language currentDate id db) extraMsgs

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

        SetHemoglobinTestExecutionDate value ->
            let
                form =
                    model.laboratoryData.hemoglobinTestForm

                updatedForm =
                    { form | executionDate = Just value }

                updatedData =
                    model.laboratoryData
                        |> (\data -> { data | hemoglobinTestForm = updatedForm })
            in
            ( { model | laboratoryData = updatedData }
            , Cmd.none
            , []
            )

        SetHemoglobinTestDateSelectorState state ->
            let
                form =
                    model.laboratoryData.hemoglobinTestForm

                defaultSelection =
                    Maybe.Extra.or form.executionDate (Maybe.andThen .dateDefault state)

                updatedForm =
                    { form | dateSelectorPopupState = state, executionDate = defaultSelection }

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
                        |> toPrenatalNonRDTValueWithDefault measurement toHemoglobinTestValueWithEmptyResults
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
                |> sequenceExtra (update language currentDate id db) extraMsgs

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

        SetRandomBloodSugarTestExecutionDate value ->
            let
                form =
                    model.laboratoryData.randomBloodSugarTestForm

                updatedForm =
                    { form | executionDate = Just value }

                updatedData =
                    model.laboratoryData
                        |> (\data -> { data | randomBloodSugarTestForm = updatedForm })
            in
            ( { model | laboratoryData = updatedData }
            , Cmd.none
            , []
            )

        SetRandomBloodSugarTestDateSelectorState state ->
            let
                form =
                    model.laboratoryData.randomBloodSugarTestForm

                defaultSelection =
                    Maybe.Extra.or form.executionDate (Maybe.andThen .dateDefault state)

                updatedForm =
                    { form | dateSelectorPopupState = state, executionDate = defaultSelection }

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
                        |> toPrenatalNonRDTValueWithDefault measurement toRandomBloodSugarTestValueWithEmptyResults
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
                |> sequenceExtra (update language currentDate id db) extraMsgs

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
                updatedData =
                    let
                        updatedForm =
                            formUpdateFunc value model.nextStepsData.healthEducationForm
                    in
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
                |> sequenceExtra (update language currentDate id db) extraMsgs

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

                appMsgs =
                    model.nextStepsData.followUpForm
                        |> toFollowUpValueWithDefault measurement
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
                |> sequenceExtra (update language currentDate id db) extraMsgs

        SaveNewbornEnrollment secondPhaseRequired nextTask ->
            let
                extraMsgs =
                    generateNextStepsMsgs secondPhaseRequired nextTask
            in
            ( model
            , Cmd.none
            , []
            )
                |> sequenceExtra (update language currentDate id db) extraMsgs

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

        SetAccompanyToHC value ->
            let
                form =
                    model.nextStepsData.sendToHCForm

                updatedForm =
                    { form | accompanyToHealthCenter = Just value }

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

        SaveSendToHC personId saved secondPhaseRequired nextTask ->
            let
                measurementId =
                    Maybe.map Tuple.first saved

                measurement =
                    getMeasurementValueFunc saved

                extraMsgs =
                    generateNextStepsMsgs secondPhaseRequired nextTask

                appMsgs =
                    model.nextStepsData.sendToHCForm
                        |> toSendToHCValueWithDefault measurement
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
                |> sequenceExtra (update language currentDate id db) extraMsgs

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
                |> sequenceExtra (update language currentDate id db) extraMsgs

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
                    { medicationDistributionForm | recommendedTreatmentSigns = Just updatedSigns }

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
                |> sequenceExtra (update language currentDate id db) extraMsgs

        SaveWait personId measurementId updatedValue secondPhaseRequired nextTask ->
            let
                extraMsgs =
                    -- When saving Wait activity, we pause the encounter, and
                    -- navigate to main page.
                    [ SetActivePage PinCodePage ]

                appMsgs =
                    [ Backend.PrenatalEncounter.Model.SaveLabsResults personId measurementId updatedValue
                        |> Backend.Model.MsgPrenatalEncounter id
                        |> App.Model.MsgIndexedDb
                    ]
            in
            ( model
            , Cmd.none
            , appMsgs
            )
                |> sequenceExtra (update language currentDate id db) extraMsgs

        SetSymptomReviewStep step ->
            let
                updatedData =
                    model.symptomReviewData
                        |> (\data -> { data | step = step })

                warningPopupState =
                    if step == SymptomReviewStepQuestions then
                        Maybe.map
                            (\symptoms ->
                                if List.member CoughContinuous symptoms then
                                    Just
                                        ( translate language Translate.TuberculosisWarning
                                        , translate language Translate.TuberculosisInstructions
                                        )

                                else
                                    model.warningPopupState
                            )
                            symptomReviewForm.symptoms
                            |> Maybe.withDefault model.warningPopupState

                    else
                        model.warningPopupState
            in
            ( { model | symptomReviewData = updatedData, warningPopupState = warningPopupState }
            , Cmd.none
            , []
            )

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
                |> sequenceExtra (update language currentDate id db) extraMsgs
