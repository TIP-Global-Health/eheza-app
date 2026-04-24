module Pages.ChildScoreboard.Activity.Update exposing (update)

import App.Model
import AssocList as Dict
import Backend.ChildScoreboardEncounter.Model
import Backend.Entities exposing (..)
import Backend.Measurement.Model
    exposing
        ( AdministrationNote(..)
        , MuacInCm(..)
        , WeightInGrm(..)
        , WeightInKg(..)
        )
import Backend.Measurement.Utils exposing (getMeasurementValueFunc)
import Backend.Model exposing (ModelIndexedDb)
import Date
import EverySet
import Gizra.NominalDate exposing (NominalDate)
import Gizra.Update exposing (sequenceExtra)
import Maybe.Extra exposing (unwrap)
import Measurement.Model exposing (VaccinationFormViewMode(..))
import Measurement.Utils
    exposing
        ( ncdaFormWithDefault
        , toNCDAValueWithDefault
        , toVaccinationValueWithDefault
        , vaccinationFormWithDefault
        , vaccineDoseToComparable
        )
import Pages.ChildScoreboard.Activity.Model exposing (Model, Msg(..))
import Pages.ChildScoreboard.Activity.Utils exposing (getFormByVaccineTypeFunc, getMeasurementByVaccineTypeFunc, updateVaccinationFormByVaccineType)
import Pages.Page exposing (Page(..), UserPage(..))
import Pages.Utils exposing (insertIntoSet, setMuacValueForSite)
import RemoteData
import SyncManager.Model exposing (Site)


update : NominalDate -> Site -> ChildScoreboardEncounterId -> ModelIndexedDb -> Msg -> Model -> ( Model, Cmd Msg, List App.Model.Msg )
update currentDate site id db msg model =
    let
        ncdaForm =
            Dict.get id db.childScoreboardMeasurements
                |> Maybe.andThen RemoteData.toMaybe
                |> Maybe.map
                    (.ncda
                        >> getMeasurementValueFunc
                        >> ncdaFormWithDefault model.ncdaData.form
                    )
                |> Maybe.withDefault model.ncdaData.form

        resolveVaccinationForm vaccineType form =
            Dict.get id db.childScoreboardMeasurements
                |> Maybe.andThen RemoteData.toMaybe
                |> Maybe.map
                    (getMeasurementByVaccineTypeFunc vaccineType
                        >> vaccinationFormWithDefault form
                    )
                |> Maybe.withDefault form

        generateImmunisationMsgs nextTask =
            Maybe.map (\task -> [ SetActiveImmunisationTask task ]) nextTask
                |> Maybe.withDefault [ SetActivePage <| UserPage <| ChildScoreboardEncounterPage id ]
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

        SetWeight string ->
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

        SetMuac string ->
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
                                [ Backend.ChildScoreboardEncounter.Model.SaveNCDA personId measurementId value
                                    |> Backend.Model.MsgChildScoreboardEncounter id
                                    |> App.Model.MsgIndexedDb
                                , App.Model.SetActivePage <| UserPage <| ChildScoreboardEncounterPage id
                                ]
                            )
            in
            ( model
            , Cmd.none
            , appMsgs
            )

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
            let
                measurementId =
                    Maybe.map Tuple.first saved

                measurement =
                    getMeasurementValueFunc saved

                extraMsgs =
                    generateImmunisationMsgs nextTask

                appMsgs =
                    model.immunisationData.bcgForm
                        |> toVaccinationValueWithDefault measurement
                        |> Maybe.map
                            (Backend.ChildScoreboardEncounter.Model.SaveBCGImmunisation personId measurementId
                                >> Backend.Model.MsgChildScoreboardEncounter id
                                >> App.Model.MsgIndexedDb
                                >> List.singleton
                            )
                        |> Maybe.withDefault []
            in
            ( model
            , Cmd.none
            , appMsgs
            )
                |> sequenceExtra (update currentDate site id db) extraMsgs

        SaveDTPImmunisation personId saved nextTask ->
            let
                measurementId =
                    Maybe.map Tuple.first saved

                measurement =
                    getMeasurementValueFunc saved

                extraMsgs =
                    generateImmunisationMsgs nextTask

                appMsgs =
                    model.immunisationData.dtpForm
                        |> toVaccinationValueWithDefault measurement
                        |> Maybe.map
                            (Backend.ChildScoreboardEncounter.Model.SaveDTPImmunisation personId measurementId
                                >> Backend.Model.MsgChildScoreboardEncounter id
                                >> App.Model.MsgIndexedDb
                                >> List.singleton
                            )
                        |> Maybe.withDefault []
            in
            ( model
            , Cmd.none
            , appMsgs
            )
                |> sequenceExtra (update currentDate site id db) extraMsgs

        SaveDTPStandaloneImmunisation personId saved nextTask ->
            let
                measurementId =
                    Maybe.map Tuple.first saved

                measurement =
                    getMeasurementValueFunc saved

                extraMsgs =
                    generateImmunisationMsgs nextTask

                appMsgs =
                    model.immunisationData.dtpForm
                        |> toVaccinationValueWithDefault measurement
                        |> Maybe.map
                            (Backend.ChildScoreboardEncounter.Model.SaveDTPStandaloneImmunisation personId measurementId
                                >> Backend.Model.MsgChildScoreboardEncounter id
                                >> App.Model.MsgIndexedDb
                                >> List.singleton
                            )
                        |> Maybe.withDefault []
            in
            ( model
            , Cmd.none
            , appMsgs
            )
                |> sequenceExtra (update currentDate site id db) extraMsgs

        SaveIPVImmunisation personId saved nextTask ->
            let
                measurementId =
                    Maybe.map Tuple.first saved

                measurement =
                    getMeasurementValueFunc saved

                extraMsgs =
                    generateImmunisationMsgs nextTask

                appMsgs =
                    model.immunisationData.ipvForm
                        |> toVaccinationValueWithDefault measurement
                        |> Maybe.map
                            (Backend.ChildScoreboardEncounter.Model.SaveIPVImmunisation personId measurementId
                                >> Backend.Model.MsgChildScoreboardEncounter id
                                >> App.Model.MsgIndexedDb
                                >> List.singleton
                            )
                        |> Maybe.withDefault []
            in
            ( model
            , Cmd.none
            , appMsgs
            )
                |> sequenceExtra (update currentDate site id db) extraMsgs

        SaveMRImmunisation personId saved nextTask ->
            let
                measurementId =
                    Maybe.map Tuple.first saved

                measurement =
                    getMeasurementValueFunc saved

                extraMsgs =
                    generateImmunisationMsgs nextTask

                appMsgs =
                    model.immunisationData.mrForm
                        |> toVaccinationValueWithDefault measurement
                        |> Maybe.map
                            (Backend.ChildScoreboardEncounter.Model.SaveMRImmunisation personId measurementId
                                >> Backend.Model.MsgChildScoreboardEncounter id
                                >> App.Model.MsgIndexedDb
                                >> List.singleton
                            )
                        |> Maybe.withDefault []
            in
            ( model
            , Cmd.none
            , appMsgs
            )
                |> sequenceExtra (update currentDate site id db) extraMsgs

        SaveOPVImmunisation personId saved nextTask ->
            let
                measurementId =
                    Maybe.map Tuple.first saved

                measurement =
                    getMeasurementValueFunc saved

                extraMsgs =
                    generateImmunisationMsgs nextTask

                appMsgs =
                    model.immunisationData.opvForm
                        |> toVaccinationValueWithDefault measurement
                        |> Maybe.map
                            (Backend.ChildScoreboardEncounter.Model.SaveOPVImmunisation personId measurementId
                                >> Backend.Model.MsgChildScoreboardEncounter id
                                >> App.Model.MsgIndexedDb
                                >> List.singleton
                            )
                        |> Maybe.withDefault []
            in
            ( model
            , Cmd.none
            , appMsgs
            )
                |> sequenceExtra (update currentDate site id db) extraMsgs

        SavePCV13Immunisation personId saved nextTask ->
            let
                measurementId =
                    Maybe.map Tuple.first saved

                measurement =
                    getMeasurementValueFunc saved

                extraMsgs =
                    generateImmunisationMsgs nextTask

                appMsgs =
                    model.immunisationData.pcv13Form
                        |> toVaccinationValueWithDefault measurement
                        |> Maybe.map
                            (Backend.ChildScoreboardEncounter.Model.SavePCV13Immunisation personId measurementId
                                >> Backend.Model.MsgChildScoreboardEncounter id
                                >> App.Model.MsgIndexedDb
                                >> List.singleton
                            )
                        |> Maybe.withDefault []
            in
            ( model
            , Cmd.none
            , appMsgs
            )
                |> sequenceExtra (update currentDate site id db) extraMsgs

        SaveRotarixImmunisation personId saved nextTask ->
            let
                measurementId =
                    Maybe.map Tuple.first saved

                measurement =
                    getMeasurementValueFunc saved

                extraMsgs =
                    generateImmunisationMsgs nextTask

                appMsgs =
                    model.immunisationData.rotarixForm
                        |> toVaccinationValueWithDefault measurement
                        |> Maybe.map
                            (Backend.ChildScoreboardEncounter.Model.SaveRotarixImmunisation personId measurementId
                                >> Backend.Model.MsgChildScoreboardEncounter id
                                >> App.Model.MsgIndexedDb
                                >> List.singleton
                            )
                        |> Maybe.withDefault []
            in
            ( model
            , Cmd.none
            , appMsgs
            )
                |> sequenceExtra (update currentDate site id db) extraMsgs
