module Pages.FamilyNutrition.Encounter.Update exposing (update)

import App.Model
import App.Ports exposing (bindDropZone)
import Backend.Entities exposing (..)
import Backend.FamilyNutritionActivity.Model exposing (FamilyNutritionActivity(..))
import Backend.FamilyNutritionEncounter.Model
import Backend.Measurement.Model exposing (ImageUrl(..))
import Backend.Measurement.Utils exposing (ahezaDistributionReasonFromString, getMeasurementValueFunc)
import Backend.Model exposing (ModelIndexedDb)
import EverySet
import Gizra.Update exposing (sequenceExtra)
import Maybe.Extra exposing (unwrap)
import Measurement.Utils exposing (toAhezaMotherValueWithDefault, toAhezaValueWithDefault, toMuacValueWithDefault)
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

        DropZoneComplete result ->
            let
                updatedData =
                    let
                        updatedForm =
                            model.photoData.form
                                |> (\form ->
                                        { form | url = Just (ImageUrl result.url) }
                                   )
                    in
                    model.photoData
                        |> (\data -> { data | form = updatedForm })
            in
            ( { model | photoData = updatedData }
            , Cmd.none
            , []
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
                    toAhezaMotherValueWithDefault measurement model.ahezaData.form
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

        SavePhoto personId saved ->
            let
                measurementId =
                    Maybe.map Tuple.first saved

                appMsgs =
                    case model.photoData.form.url of
                        Just url ->
                            [ Backend.FamilyNutritionEncounter.Model.SavePhoto personId measurementId url
                                |> Backend.Model.MsgFamilyNutritionEncounter id
                                |> App.Model.MsgIndexedDb
                            ]

                        Nothing ->
                            []

                extraMsgs =
                    if List.isEmpty appMsgs then
                        []

                    else
                        generateAutoAdvanceMsgs id FamilyNutritionPhoto db model.selectedFamilyMember
            in
            ( { model | photoData = emptyPhotoData }
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

        SetAhezaDistributionReason value ->
            let
                reason =
                    ahezaDistributionReasonFromString value

                updatedData =
                    let
                        updatedForm =
                            model.ahezaData.form
                                |> (\form ->
                                        { form
                                            | distributionReason = reason
                                            , distributionReasonDirty = True
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
            let
                cmd =
                    case activity of
                        Just FamilyNutritionPhoto ->
                            bindDropZone ()

                        _ ->
                            Cmd.none
            in
            ( { model | selectedActivity = activity }, cmd, [] )

        SetSelectedFamilyMember member ->
            let
                cmd =
                    case member of
                        FamilyMemberChild _ ->
                            bindDropZone ()

                        FamilyMemberMother ->
                            Cmd.none
            in
            ( { model
                | selectedFamilyMember = member
                , selectedActivity = Nothing
                , selectedTab = Pending
                , ahezaData = emptyAhezaData
                , muacData = emptyMuacData
                , photoData = emptyPhotoData
              }
            , cmd
            , []
            )

        SetSelectedTab tab ->
            let
                cmd =
                    case tab of
                        Completed ->
                            bindDropZone ()

                        Pending ->
                            bindDropZone ()

                        Reports ->
                            Cmd.none
            in
            ( { model | selectedTab = tab }, cmd, [] )


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
