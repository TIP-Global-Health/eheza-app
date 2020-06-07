module Pages.AcuteIllnessActivity.Update exposing (update)

import App.Model
import AssocList as Dict
import Backend.AcuteIllnessEncounter.Model
import Backend.Entities exposing (..)
import Backend.IndividualEncounterParticipant.Model
import Backend.Measurement.Model
    exposing
        ( HCRecomendation(..)
        , ReasonForNotIsolating(..)
        , ResponsePeriod(..)
        , SymptomsGISign(..)
        , SymptomsGeneralSign(..)
        , SymptomsRespiratorySign(..)
        )
import Backend.Model exposing (ModelIndexedDb)
import Gizra.NominalDate exposing (NominalDate)
import Maybe.Extra exposing (isJust, isNothing, unwrap)
import Pages.AcuteIllnessActivity.Model exposing (..)
import Pages.AcuteIllnessActivity.Utils exposing (..)
import Pages.Page exposing (Page(..), UserPage(..))
import RemoteData exposing (RemoteData(..))
import Result exposing (Result)


update : NominalDate -> AcuteIllnessEncounterId -> ModelIndexedDb -> Msg -> Model -> ( Model, Cmd Msg, List App.Model.Msg )
update currentDate id db msg model =
    let
        noChange =
            ( model, Cmd.none, [] )

        symptomsGeneralForm =
            Dict.get id db.acuteIllnessMeasurements
                |> Maybe.withDefault NotAsked
                |> RemoteData.toMaybe
                |> Maybe.map
                    (.symptomsGeneral
                        >> Maybe.map (Tuple.second >> .value)
                        >> symptomsGeneralFormWithDefault model.symptomsData.symptomsGeneralForm
                    )
                |> Maybe.withDefault model.symptomsData.symptomsGeneralForm

        symptomsRespiratoryForm =
            Dict.get id db.acuteIllnessMeasurements
                |> Maybe.withDefault NotAsked
                |> RemoteData.toMaybe
                |> Maybe.map
                    (.symptomsRespiratory
                        >> Maybe.map (Tuple.second >> .value)
                        >> symptomsRespiratoryFormWithDefault model.symptomsData.symptomsRespiratoryForm
                    )
                |> Maybe.withDefault model.symptomsData.symptomsRespiratoryForm

        symptomsGIForm =
            Dict.get id db.acuteIllnessMeasurements
                |> Maybe.withDefault NotAsked
                |> RemoteData.toMaybe
                |> Maybe.map
                    (.symptomsGI
                        >> Maybe.map (Tuple.second >> .value)
                        >> symptomsGIFormWithDefault model.symptomsData.symptomsGIForm
                    )
                |> Maybe.withDefault model.symptomsData.symptomsGIForm
    in
    case msg of
        SetActivePage page ->
            ( model
            , Cmd.none
            , [ App.Model.SetActivePage page ]
            )

        SetAlertsDialogState isOpen ->
            ( { model | showAlertsDialog = isOpen }, Cmd.none, [] )

        SetCovid19PopupState isOpen ->
            ( { model | showCovid19Popup = isOpen }, Cmd.none, [] )

        SetActiveSymptomsTask task ->
            let
                updatedData =
                    model.symptomsData
                        |> (\data -> { data | activeTask = task })
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
                    { symptomsGeneralForm | signs = updatedSigns }

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
                    { symptomsGIForm | signs = updatedSigns }

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
                    { symptomsRespiratoryForm | signs = updatedSigns }

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

        SetSymptomsGIIntractableVomiting value ->
            let
                updatedForm =
                    { symptomsGIForm | intractableVomiting = Just value }

                updatedData =
                    model.symptomsData
                        |> (\data -> { data | symptomsGIForm = updatedForm })
            in
            ( { model | symptomsData = updatedData }
            , Cmd.none
            , []
            )

        SaveSymptomsGeneral personId saved nextTask_ ->
            let
                measurementId =
                    Maybe.map Tuple.first saved

                measurement =
                    Maybe.map (Tuple.second >> .value) saved

                ( backToActivitiesMsg, nextTask ) =
                    nextTask_
                        |> Maybe.map (\task -> ( [], task ))
                        |> Maybe.withDefault
                            ( [ App.Model.SetActivePage <| UserPage <| AcuteIllnessEncounterPage id ]
                            , SymptomsGeneral
                            )

                form =
                    model.symptomsData.symptomsGeneralForm

                value =
                    toSymptomsGeneralValueWithDefault measurement form

                appMsgs =
                    (Backend.AcuteIllnessEncounter.Model.SaveSymptomsGeneral personId measurementId value
                        |> Backend.Model.MsgAcuteIllnessEncounter id
                        |> App.Model.MsgIndexedDb
                    )
                        :: backToActivitiesMsg

                updatedData =
                    model.symptomsData
                        |> (\data -> { data | activeTask = nextTask })
            in
            ( { model | symptomsData = updatedData }
            , Cmd.none
            , appMsgs
            )

        SaveSymptomsRespiratory personId saved nextTask_ ->
            let
                measurementId =
                    Maybe.map Tuple.first saved

                measurement =
                    Maybe.map (Tuple.second >> .value) saved

                ( backToActivitiesMsg, nextTask ) =
                    nextTask_
                        |> Maybe.map (\task -> ( [], task ))
                        |> Maybe.withDefault
                            ( [ App.Model.SetActivePage <| UserPage <| AcuteIllnessEncounterPage id ]
                            , SymptomsGeneral
                            )

                form =
                    model.symptomsData.symptomsRespiratoryForm

                value =
                    toSymptomsRespiratoryValueWithDefault measurement form

                appMsgs =
                    (Backend.AcuteIllnessEncounter.Model.SaveSymptomsRespiratory personId measurementId value
                        |> Backend.Model.MsgAcuteIllnessEncounter id
                        |> App.Model.MsgIndexedDb
                    )
                        :: backToActivitiesMsg

                updatedData =
                    model.symptomsData
                        |> (\data -> { data | activeTask = nextTask })
            in
            ( { model | symptomsData = updatedData }
            , Cmd.none
            , appMsgs
            )

        SaveSymptomsGI personId saved nextTask_ ->
            let
                measurementId =
                    Maybe.map Tuple.first saved

                measurement =
                    Maybe.map (Tuple.second >> .value) saved

                ( backToActivitiesMsg, nextTask ) =
                    nextTask_
                        |> Maybe.map (\task -> ( [], task ))
                        |> Maybe.withDefault
                            ( [ App.Model.SetActivePage <| UserPage <| AcuteIllnessEncounterPage id ]
                            , SymptomsGeneral
                            )

                form =
                    model.symptomsData.symptomsGIForm

                value =
                    toSymptomsGIValueWithDefault measurement form

                appMsgs =
                    (Backend.AcuteIllnessEncounter.Model.SaveSymptomsGI personId measurementId value
                        |> Backend.Model.MsgAcuteIllnessEncounter id
                        |> App.Model.MsgIndexedDb
                    )
                        :: backToActivitiesMsg

                updatedData =
                    model.symptomsData
                        |> (\data -> { data | activeTask = nextTask })
            in
            ( { model | symptomsData = updatedData }
            , Cmd.none
            , appMsgs
            )

        SetActivePhysicalExamTask task ->
            let
                updatedData =
                    model.physicalExamData
                        |> (\data -> { data | activeTask = task })
            in
            ( { model | physicalExamData = updatedData }
            , Cmd.none
            , []
            )

        SetVitalsResporatoryRate value ->
            let
                form =
                    model.physicalExamData.vitalsForm

                updatedForm =
                    { form | respiratoryRate = String.toInt value }

                updatedData =
                    model.physicalExamData
                        |> (\data -> { data | vitalsForm = updatedForm })
            in
            ( { model | physicalExamData = updatedData }
            , Cmd.none
            , []
            )

        SetVitalsBodyTemperature value ->
            let
                form =
                    model.physicalExamData.vitalsForm

                updatedForm =
                    { form | bodyTemperature = String.toFloat value }

                updatedData =
                    model.physicalExamData
                        |> (\data -> { data | vitalsForm = updatedForm })
            in
            ( { model | physicalExamData = updatedData }
            , Cmd.none
            , []
            )

        SaveVitals personId saved ->
            let
                measurementId =
                    Maybe.map Tuple.first saved

                measurement =
                    Maybe.map (Tuple.second >> .value) saved

                appMsgs =
                    model.physicalExamData.vitalsForm
                        |> toVitalsValueWithDefault measurement
                        |> unwrap
                            []
                            (\value ->
                                [ Backend.AcuteIllnessEncounter.Model.SaveVitals personId measurementId value
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

        SetActiveLaboratoryTask task ->
            let
                updatedData =
                    model.laboratoryData
                        |> (\data -> { data | activeTask = task })
            in
            ( { model | laboratoryData = updatedData }
            , Cmd.none
            , []
            )

        SetRapidTestPositive value ->
            let
                form =
                    model.laboratoryData.malariaTestingForm

                updatedForm =
                    { form | rapidTestPositive = Just value }

                updatedData =
                    model.laboratoryData
                        |> (\data -> { data | malariaTestingForm = updatedForm })
            in
            ( { model | laboratoryData = updatedData }
            , Cmd.none
            , []
            )

        SaveMalariaTesting personId saved ->
            let
                measurementId =
                    Maybe.map Tuple.first saved

                measurement =
                    Maybe.map (Tuple.second >> .value) saved

                appMsgs =
                    model.laboratoryData.malariaTestingForm
                        |> toMalariaTestingValueWithDefault measurement
                        |> unwrap
                            []
                            (\value ->
                                [ Backend.AcuteIllnessEncounter.Model.SaveMalariaTesting personId measurementId value
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

        SetActiveExposureTask task ->
            let
                updatedData =
                    model.exposureData
                        |> (\data -> { data | activeTask = task })
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

        SaveTravelHistory personId saved nextTask_ ->
            let
                measurementId =
                    Maybe.map Tuple.first saved

                measurement =
                    Maybe.map (Tuple.second >> .value) saved

                ( backToActivitiesMsg, nextTask ) =
                    nextTask_
                        |> Maybe.map (\task -> ( [], task ))
                        |> Maybe.withDefault
                            ( [ App.Model.SetActivePage <| UserPage <| AcuteIllnessEncounterPage id ]
                            , ExposureTravel
                            )

                appMsgs =
                    model.exposureData.travelHistoryForm
                        |> toTravelHistoryValueWithDefault measurement
                        |> unwrap
                            []
                            (\value ->
                                (Backend.AcuteIllnessEncounter.Model.SaveTravelHistory personId measurementId value
                                    |> Backend.Model.MsgAcuteIllnessEncounter id
                                    |> App.Model.MsgIndexedDb
                                )
                                    :: backToActivitiesMsg
                            )

                updatedData =
                    model.exposureData
                        |> (\data -> { data | activeTask = nextTask })
            in
            ( { model | exposureData = updatedData }
            , Cmd.none
            , appMsgs
            )

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

        SetSimilarSymptoms value ->
            let
                form =
                    model.exposureData.exposureForm

                updatedForm =
                    { form | similarSymptoms = Just value }

                updatedData =
                    model.exposureData
                        |> (\data -> { data | exposureForm = updatedForm })
            in
            ( { model | exposureData = updatedData }
            , Cmd.none
            , []
            )

        SaveExposure personId saved nextTask_ ->
            let
                measurementId =
                    Maybe.map Tuple.first saved

                measurement =
                    Maybe.map (Tuple.second >> .value) saved

                ( backToActivitiesMsg, nextTask ) =
                    nextTask_
                        |> Maybe.map (\task -> ( [], task ))
                        |> Maybe.withDefault
                            ( [ App.Model.SetActivePage <| UserPage <| AcuteIllnessEncounterPage id ]
                            , ExposureTravel
                            )

                appMsgs =
                    model.exposureData.exposureForm
                        |> toExposureValueWithDefault measurement
                        |> unwrap
                            []
                            (\value ->
                                (Backend.AcuteIllnessEncounter.Model.SaveExposure personId measurementId value
                                    |> Backend.Model.MsgAcuteIllnessEncounter id
                                    |> App.Model.MsgIndexedDb
                                )
                                    :: backToActivitiesMsg
                            )

                updatedData =
                    model.exposureData
                        |> (\data -> { data | activeTask = nextTask })
            in
            ( { model | exposureData = updatedData }
            , Cmd.none
            , appMsgs
            )

        SetPatientIsolated value ->
            let
                form =
                    model.exposureData.isolationForm

                updatedForm =
                    { form | patientIsolated = Just value }

                updatedData =
                    model.exposureData
                        |> (\data -> { data | isolationForm = updatedForm })
            in
            ( { model | exposureData = updatedData }
            , Cmd.none
            , []
            )

        SetHealthEducation value ->
            let
                form =
                    model.exposureData.isolationForm

                updatedForm =
                    { form | healthEducation = Just value }

                updatedData =
                    model.exposureData
                        |> (\data -> { data | isolationForm = updatedForm })
            in
            ( { model | exposureData = updatedData }
            , Cmd.none
            , []
            )

        SetSignOnDoor value ->
            let
                form =
                    model.exposureData.isolationForm

                updatedForm =
                    { form | signOnDoor = Just value }

                updatedData =
                    model.exposureData
                        |> (\data -> { data | isolationForm = updatedForm })
            in
            ( { model | exposureData = updatedData }
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
                                >> Maybe.map (Tuple.second >> .value)
                                >> isolationFormWithDefault model.exposureData.isolationForm
                            )
                        |> Maybe.withDefault model.exposureData.isolationForm

                updatedForm =
                    case form.reasonsForNotIsolating of
                        Just reasons ->
                            case reasons of
                                [ IsolationReasonNotApplicable ] ->
                                    { form | reasonsForNotIsolating = [ reason ] |> Just }

                                _ ->
                                    if List.member reason reasons then
                                        let
                                            updated =
                                                if List.length reasons == 1 then
                                                    Nothing

                                                else
                                                    reasons |> List.filter ((/=) reason) |> Just
                                        in
                                        { form | reasonsForNotIsolating = updated }

                                    else
                                        { form | reasonsForNotIsolating = reason :: reasons |> Just }

                        Nothing ->
                            { form | reasonsForNotIsolating = Just [ reason ] }

                updatedData =
                    model.exposureData
                        |> (\data -> { data | isolationForm = updatedForm })
            in
            ( { model | exposureData = updatedData }
            , Cmd.none
            , []
            )

        SaveIsolation personId saved nextTask_ ->
            let
                measurementId =
                    Maybe.map Tuple.first saved

                measurement =
                    Maybe.map (Tuple.second >> .value) saved

                ( backToActivitiesMsg, nextTask ) =
                    nextTask_
                        |> Maybe.map (\task -> ( [], task ))
                        |> Maybe.withDefault
                            ( [ App.Model.SetActivePage <| UserPage <| AcuteIllnessEncounterPage id ]
                            , ExposureTravel
                            )

                appMsgs =
                    model.exposureData.isolationForm
                        |> toIsolationValueWithDefault measurement
                        |> unwrap
                            []
                            (\value ->
                                (Backend.AcuteIllnessEncounter.Model.SaveIsolation personId measurementId value
                                    |> Backend.Model.MsgAcuteIllnessEncounter id
                                    |> App.Model.MsgIndexedDb
                                )
                                    :: backToActivitiesMsg
                            )

                updatedData =
                    model.exposureData
                        |> (\data -> { data | activeTask = nextTask })
            in
            ( { model | exposureData = updatedData }
            , Cmd.none
            , appMsgs
            )

        SetContactedHC value ->
            let
                form =
                    model.exposureData.hcContactForm

                updatedForm =
                    { form | contactedHC = Just value }

                updatedData =
                    model.exposureData
                        |> (\data -> { data | hcContactForm = updatedForm })
            in
            ( { model | exposureData = updatedData }
            , Cmd.none
            , []
            )

        SetHCRecommendation value ->
            let
                form =
                    model.exposureData.hcContactForm

                updatedForm =
                    case form.recomendations of
                        Just period ->
                            if period == value then
                                { form | recomendations = Nothing }

                            else
                                { form | recomendations = Just value }

                        Nothing ->
                            { form | recomendations = Just value }

                updatedData =
                    model.exposureData
                        |> (\data -> { data | hcContactForm = updatedForm })
            in
            ( { model | exposureData = updatedData }
            , Cmd.none
            , []
            )

        SetResponsePeriod value ->
            let
                form =
                    model.exposureData.hcContactForm

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
                    model.exposureData
                        |> (\data -> { data | hcContactForm = updatedForm })
            in
            ( { model | exposureData = updatedData }
            , Cmd.none
            , []
            )

        SetAmbulanceArrivalPeriod value ->
            let
                form =
                    model.exposureData.hcContactForm

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
                    model.exposureData
                        |> (\data -> { data | hcContactForm = updatedForm })
            in
            ( { model | exposureData = updatedData }
            , Cmd.none
            , []
            )

        SaveHCContact personId saved nextTask_ ->
            let
                measurementId =
                    Maybe.map Tuple.first saved

                measurement =
                    Maybe.map (Tuple.second >> .value) saved

                ( backToActivitiesMsg, nextTask ) =
                    nextTask_
                        |> Maybe.map (\task -> ( [], task ))
                        |> Maybe.withDefault
                            ( [ App.Model.SetActivePage <| UserPage <| AcuteIllnessEncounterPage id ]
                            , ExposureTravel
                            )

                appMsgs =
                    model.exposureData.hcContactForm
                        |> toHCContactValueWithDefault measurement
                        |> unwrap
                            []
                            (\value ->
                                (Backend.AcuteIllnessEncounter.Model.SaveHCContact personId measurementId value
                                    |> Backend.Model.MsgAcuteIllnessEncounter id
                                    |> App.Model.MsgIndexedDb
                                )
                                    :: backToActivitiesMsg
                            )

                updatedData =
                    model.exposureData
                        |> (\data -> { data | activeTask = nextTask })
            in
            ( { model | exposureData = updatedData }
            , Cmd.none
            , appMsgs
            )
