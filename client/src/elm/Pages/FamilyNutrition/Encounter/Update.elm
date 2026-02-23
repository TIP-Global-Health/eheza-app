module Pages.FamilyNutrition.Encounter.Update exposing (update)

import App.Model
import Backend.Entities exposing (..)
import Backend.FamilyNutritionActivity.Model exposing (FamilyNutritionActivity(..))
import Backend.FamilyNutritionEncounter.Model
import Backend.Measurement.Utils exposing (getMeasurementValueFunc)
import Backend.Model exposing (ModelIndexedDb)
import EverySet
import Gizra.Update exposing (sequenceExtra)
import Maybe.Extra exposing (unwrap)
import Measurement.Utils exposing (toAhezaValueWithDefault, toMuacValueWithDefault)
import Pages.FamilyNutrition.Encounter.Model exposing (..)
import Pages.FamilyNutrition.Encounter.Utils exposing (activitiesForFamilyMember, activityCompleted, generateAssembledData, nextFamilyMember)
import Pages.Page exposing (Page(..))
import Pages.Utils exposing (setMuacValueForSite)
import RemoteData
import SyncManager.Model exposing (Site)


update : Site -> FamilyNutritionEncounterId -> ModelIndexedDb -> Msg -> Model -> ( Model, Cmd Msg, List App.Model.Msg )
update site id db msg model =
    case msg of
        CloseEncounter encounterId ->
            ( model
            , Cmd.none
            , [ Backend.FamilyNutritionEncounter.Model.CloseFamilyNutritionEncounter
                    |> Backend.Model.MsgFamilyNutritionEncounter encounterId
                    |> App.Model.MsgIndexedDb
              , App.Model.SetActivePage PinCodePage
              ]
            )

        SaveAhezaChild personId saved ->
            let
                measurementId =
                    Maybe.map Tuple.first saved

                measurement =
                    getMeasurementValueFunc saved

                appMsgs =
                    toAhezaValueWithDefault measurement model.ahezaData.form
                        |> unwrap
                            []
                            (\value ->
                                [ Backend.FamilyNutritionEncounter.Model.SaveAhezaChild personId measurementId value
                                    |> Backend.Model.MsgFamilyNutritionEncounter id
                                    |> App.Model.MsgIndexedDb
                                ]
                            )

                extraMsgs =
                    if List.isEmpty appMsgs then
                        []

                    else
                        generateAutoAdvanceMsgs id FamilyNutritionAheza db model.selectedFamilyMember
            in
            ( { model | ahezaData = emptyAhezaData }
            , Cmd.none
            , appMsgs
            )
                |> sequenceExtra (update site id db) extraMsgs

        SaveAhezaMother personId saved ->
            let
                measurementId =
                    Maybe.map Tuple.first saved

                measurement =
                    getMeasurementValueFunc saved

                appMsgs =
                    toAhezaValueWithDefault measurement model.ahezaData.form
                        |> unwrap
                            []
                            (\value ->
                                [ Backend.FamilyNutritionEncounter.Model.SaveAhezaMother personId measurementId value
                                    |> Backend.Model.MsgFamilyNutritionEncounter id
                                    |> App.Model.MsgIndexedDb
                                ]
                            )

                extraMsgs =
                    if List.isEmpty appMsgs then
                        []

                    else
                        generateAutoAdvanceMsgs id FamilyNutritionAheza db model.selectedFamilyMember
            in
            ( { model | ahezaData = emptyAhezaData }
            , Cmd.none
            , appMsgs
            )
                |> sequenceExtra (update site id db) extraMsgs

        SaveMuacChild personId saved ->
            let
                measurementId =
                    Maybe.map Tuple.first saved

                measurement =
                    getMeasurementValueFunc saved

                appMsgs =
                    model.muacData.form
                        |> toMuacValueWithDefault measurement
                        |> unwrap
                            []
                            (\value ->
                                [ Backend.FamilyNutritionEncounter.Model.SaveMuacChild personId measurementId value
                                    |> Backend.Model.MsgFamilyNutritionEncounter id
                                    |> App.Model.MsgIndexedDb
                                ]
                            )

                extraMsgs =
                    if List.isEmpty appMsgs then
                        []

                    else
                        generateAutoAdvanceMsgs id FamilyNutritionMuac db model.selectedFamilyMember
            in
            ( { model | muacData = emptyMuacData }
            , Cmd.none
            , appMsgs
            )
                |> sequenceExtra (update site id db) extraMsgs

        SaveMuacMother personId saved ->
            let
                measurementId =
                    Maybe.map Tuple.first saved

                measurement =
                    getMeasurementValueFunc saved

                appMsgs =
                    model.muacData.form
                        |> toMuacValueWithDefault measurement
                        |> unwrap
                            []
                            (\value ->
                                [ Backend.FamilyNutritionEncounter.Model.SaveMuacMother personId measurementId value
                                    |> Backend.Model.MsgFamilyNutritionEncounter id
                                    |> App.Model.MsgIndexedDb
                                ]
                            )

                extraMsgs =
                    if List.isEmpty appMsgs then
                        []

                    else
                        generateAutoAdvanceMsgs id FamilyNutritionMuac db model.selectedFamilyMember
            in
            ( { model | muacData = emptyMuacData }
            , Cmd.none
            , appMsgs
            )
                |> sequenceExtra (update site id db) extraMsgs

        SetActivePage page ->
            ( model
            , Cmd.none
            , [ App.Model.SetActivePage page ]
            )
                |> sequenceExtra (update site id db) [ SetDialogState Nothing ]

        SetAheza string ->
            let
                updatedData =
                    let
                        updatedForm =
                            model.ahezaData.form
                                |> (\form ->
                                        { form
                                            | aheza =
                                                String.toInt string
                                                    |> Maybe.andThen
                                                        (\val ->
                                                            if val >= 0 then
                                                                Just (toFloat val)

                                                            else
                                                                Nothing
                                                        )
                                            , ahezaDirty = True
                                        }
                                   )
                    in
                    model.ahezaData
                        |> (\data -> { data | form = updatedForm })
            in
            ( { model | ahezaData = updatedData }
            , Cmd.none
            , []
            )

        SetDialogState state ->
            ( { model | dialogState = state }, Cmd.none, [] )

        SetMuac string ->
            let
                updatedData =
                    let
                        updatedForm =
                            model.muacData.form
                                |> (\form ->
                                        { form | muac = setMuacValueForSite site string, muacDirty = True }
                                   )
                    in
                    model.muacData
                        |> (\data -> { data | form = updatedForm })
            in
            ( { model | muacData = updatedData }
            , Cmd.none
            , []
            )

        SetSelectedActivity activity ->
            ( { model | selectedActivity = activity }, Cmd.none, [] )

        SetSelectedFamilyMember member ->
            ( { model
                | selectedFamilyMember = member
                , selectedActivity = Nothing
                , selectedTab = Pending
                , ahezaData = emptyAhezaData
                , muacData = emptyMuacData
              }
            , Cmd.none
            , []
            )

        SetSelectedTab tab ->
            ( { model | selectedTab = tab }, Cmd.none, [] )


generateAutoAdvanceMsgs : FamilyNutritionEncounterId -> FamilyNutritionActivity -> ModelIndexedDb -> FamilyMember -> List Msg
generateAutoAdvanceMsgs encounterId triggeringCompletedActivity db currentMember =
    generateAssembledData encounterId db
        |> RemoteData.toMaybe
        |> Maybe.map
            (\assembled ->
                let
                    applicableActivities =
                        activitiesForFamilyMember assembled.encounter.startDate currentMember assembled.children

                    allCompleted =
                        List.filter ((/=) triggeringCompletedActivity) applicableActivities
                            |> List.all (activityCompleted currentMember assembled.measurements)
                in
                if allCompleted then
                    nextFamilyMember currentMember assembled.children
                        |> SetSelectedFamilyMember
                        |> List.singleton

                else
                    []
            )
        |> Maybe.withDefault []
