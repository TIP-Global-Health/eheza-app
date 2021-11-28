module Pages.PrenatalActivity.Update exposing (update)

import App.Model
import AssocList as Dict
import Backend.Entities exposing (..)
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
        , PreviousDeliveryPeriod(..)
        , SocialHistoryHivTestingResult(..)
        )
import Backend.Measurement.Utils exposing (getMeasurementValueFunc)
import Backend.Model exposing (ModelIndexedDb)
import Backend.PrenatalEncounter.Model
import Date exposing (Unit(..))
import Gizra.NominalDate exposing (NominalDate)
import Gizra.Update exposing (sequenceExtra)
import Maybe.Extra exposing (isJust, isNothing, unwrap)
import Measurement.Utils exposing (toSendToHCValueWithDefault, toVitalsValueWithDefault)
import Pages.Page exposing (Page(..), UserPage(..))
import Pages.PrenatalActivity.Model exposing (..)
import Pages.PrenatalActivity.Utils exposing (..)
import Pages.PrenatalEncounter.Utils exposing (calculateEDD)
import Pages.Utils exposing (setMultiSelectInputValue, tasksBarId)
import RemoteData exposing (RemoteData(..))
import Result exposing (Result)


update : NominalDate -> PrenatalEncounterId -> ModelIndexedDb -> Msg -> Model -> ( Model, Cmd Msg, List App.Model.Msg )
update currentDate id db msg model =
    let
        corePhysicalExamForm =
            Dict.get id db.prenatalMeasurements
                |> Maybe.withDefault NotAsked
                |> RemoteData.toMaybe
                |> Maybe.map
                    (.corePhysicalExam
                        >> getMeasurementValueFunc
                        >> corePhysicalExamFormWithDefault model.examinationData.corePhysicalExamForm
                    )
                |> Maybe.withDefault model.examinationData.corePhysicalExamForm

        --navigationMsgsByNextStep : (t -> msg) -> UserPage -> Maybe t -> ( List App.Model.Msg, List msg )
        navigationMsgsByNextStep setActiveTaskMsg encounterPage nextTask =
            nextTask
                |> Maybe.map
                    (\task ->
                        ( []
                        , [ setActiveTaskMsg task ]
                        )
                    )
                |> Maybe.withDefault
                    ( [ App.Model.SetActivePage <| UserPage encounterPage ]
                    , []
                    )
    in
    case msg of
        NoOp ->
            ( model
            , Cmd.none
            , []
            )

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

        ToggleDateSelector ->
            let
                updatedForm =
                    model.pregnancyDatingData.form
                        |> (\form -> { form | isDateSelectorOpen = not form.isDateSelectorOpen })

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

                today =
                    currentDate

                ( lmpDate, isDateSelectorOpen ) =
                    case range of
                        Just OneMonth ->
                            ( Date.add Months -1 today |> Just, True )

                        Just ThreeMonth ->
                            ( Date.add Months -3 today |> Just, True )

                        Just SixMonth ->
                            ( Date.add Months -6 today |> Just, True )

                        Nothing ->
                            ( Nothing, False )

                updatedForm =
                    model.pregnancyDatingData.form
                        |> (\form -> { form | lmpRange = range, lmpDate = lmpDate, isDateSelectorOpen = isDateSelectorOpen })

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
                                  calculateEDD lastMenstrualPeriodValue.date
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

        SetPregnancyTestResult value ->
            let
                result =
                    pregnancyTestResultFromString value

                updatedData =
                    let
                        updatedForm =
                            model.laboratoryData.form
                                |> (\form -> { form | pregnancyTestResult = result })
                    in
                    model.laboratoryData
                        |> (\data -> { data | form = updatedForm })
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

        SavePregnancyTesting personId saved ->
            let
                measurementId =
                    Maybe.map Tuple.first saved

                measurement =
                    getMeasurementValueFunc saved

                appMsgs =
                    model.laboratoryData.form
                        |> toPregnancyTestingValueWithDefault measurement
                        |> unwrap
                            []
                            (\value ->
                                [ Backend.PrenatalEncounter.Model.SavePregnancyTesting personId measurementId value
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

        SetHealthEducationBoolInput formUpdateFunc value ->
            let
                updatedData =
                    let
                        updatedForm =
                            formUpdateFunc value model.healthEducationData.form
                    in
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

        SaveHealthEducationSubActivity personId saved nextTask_ ->
            let
                measurementId =
                    Maybe.map Tuple.first saved

                measurement =
                    getMeasurementValueFunc saved

                ( backToActivitiesMsg, setActiveTaskMsg ) =
                    navigationMsgsByNextStep SetActiveNextStepsTask (PrenatalEncounterPage id) nextTask_

                appMsgs =
                    model.nextStepsData.healthEducationForm
                        |> toHealthEducationValueWithDefault measurement
                        |> unwrap
                            []
                            (\value ->
                                (Backend.PrenatalEncounter.Model.SaveHealthEducation personId measurementId value
                                    |> Backend.Model.MsgPrenatalEncounter id
                                    |> App.Model.MsgIndexedDb
                                )
                                    :: backToActivitiesMsg
                            )
            in
            ( model
            , Cmd.none
            , appMsgs
            )
                |> sequenceExtra (update currentDate id db) setActiveTaskMsg

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

        SaveFollowUp personId assesment saved nextTask_ ->
            let
                measurementId =
                    Maybe.map Tuple.first saved

                measurement =
                    getMeasurementValueFunc saved

                ( backToActivitiesMsg, setActiveTaskMsg ) =
                    navigationMsgsByNextStep SetActiveNextStepsTask (PrenatalEncounterPage id) nextTask_

                appMsgs =
                    model.nextStepsData.followUpForm
                        |> (\form -> { form | assesment = Just assesment })
                        |> toFollowUpValueWithDefault measurement
                        |> unwrap
                            []
                            (\value ->
                                (Backend.PrenatalEncounter.Model.SaveFollowUp personId measurementId value
                                    |> Backend.Model.MsgPrenatalEncounter id
                                    |> App.Model.MsgIndexedDb
                                )
                                    :: backToActivitiesMsg
                            )
            in
            ( model
            , Cmd.none
            , appMsgs
            )
                |> sequenceExtra (update currentDate id db) setActiveTaskMsg

        SaveNewbornEnrollment nextTask_ ->
            let
                ( backToActivitiesMsg, setActiveTaskMsg ) =
                    navigationMsgsByNextStep SetActiveNextStepsTask (PrenatalEncounterPage id) nextTask_
            in
            ( model
            , Cmd.none
            , backToActivitiesMsg
            )
                |> sequenceExtra (update currentDate id db) setActiveTaskMsg

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

        SaveSendToHC personId saved nextTask_ ->
            let
                measurementId =
                    Maybe.map Tuple.first saved

                measurement =
                    getMeasurementValueFunc saved

                ( backToActivitiesMsg, setActiveTaskMsg ) =
                    navigationMsgsByNextStep SetActiveNextStepsTask (PrenatalEncounterPage id) nextTask_

                appMsgs =
                    model.nextStepsData.sendToHCForm
                        |> toSendToHCValueWithDefault measurement
                        |> unwrap
                            []
                            (\value ->
                                (Backend.PrenatalEncounter.Model.SaveSendToHC personId measurementId value
                                    |> Backend.Model.MsgPrenatalEncounter id
                                    |> App.Model.MsgIndexedDb
                                )
                                    :: backToActivitiesMsg
                            )
            in
            ( model
            , Cmd.none
            , appMsgs
            )
                |> sequenceExtra (update currentDate id db) setActiveTaskMsg

        AppointmentToggleDateSelector ->
            let
                updatedForm =
                    model.nextStepsData.appointmentConfirmationForm
                        |> (\form -> { form | isDateSelectorOpen = not form.isDateSelectorOpen })

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

        SaveAppointmentConfirmation personId saved nextTask_ ->
            let
                measurementId =
                    Maybe.map Tuple.first saved

                measurement =
                    getMeasurementValueFunc saved

                ( backToActivitiesMsg, setActiveTaskMsg ) =
                    navigationMsgsByNextStep SetActiveNextStepsTask (PrenatalEncounterPage id) nextTask_

                appMsgs =
                    model.nextStepsData.appointmentConfirmationForm
                        |> toAppointmentConfirmationValueWithDefault measurement
                        |> unwrap
                            []
                            (\value ->
                                (Backend.PrenatalEncounter.Model.SaveAppointmentConfirmation personId measurementId value
                                    |> Backend.Model.MsgPrenatalEncounter id
                                    |> App.Model.MsgIndexedDb
                                )
                                    :: backToActivitiesMsg
                            )
            in
            ( model
            , Cmd.none
            , appMsgs
            )
                |> sequenceExtra (update currentDate id db) setActiveTaskMsg
