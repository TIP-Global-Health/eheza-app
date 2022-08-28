module Pages.NCD.Activity.Update exposing (update)

import App.Model
import AssocList as Dict
import Backend.Entities exposing (..)
import Backend.IndividualEncounterParticipant.Model
import Backend.Measurement.Model
    exposing
        ( FamilyPlanningSign(..)
        , NCDDangerSign(..)
        , NCDGroup1Symptom(..)
        , NCDGroup2Symptom(..)
        , NCDPainSymptom(..)
        )
import Backend.Measurement.Utils exposing (getMeasurementValueFunc)
import Backend.Model exposing (ModelIndexedDb)
import Backend.NCDEncounter.Model
import Gizra.NominalDate exposing (NominalDate)
import Maybe.Extra exposing (isJust, isNothing, unwrap)
import Measurement.Utils exposing (familyPlanningFormWithDefault, toFamilyPlanningValueWithDefault)
import Pages.NCD.Activity.Model exposing (..)
import Pages.NCD.Activity.Utils exposing (..)
import Pages.Page exposing (Page(..), UserPage(..))
import Pages.Utils exposing (setMultiSelectInputValue)
import RemoteData exposing (RemoteData(..))


update : NominalDate -> NCDEncounterId -> ModelIndexedDb -> Msg -> Model -> ( Model, Cmd Msg, List App.Model.Msg )
update currentDate id db msg model =
    let
        symptomReviewForm =
            Dict.get id db.ncdMeasurements
                |> Maybe.andThen RemoteData.toMaybe
                |> Maybe.map
                    (.symptomReview
                        >> getMeasurementValueFunc
                        >> symptomReviewFormWithDefault model.symptomReviewData.form
                    )
                |> Maybe.withDefault model.symptomReviewData.form
    in
    case msg of
        SetActivePage page ->
            ( model
            , Cmd.none
            , [ App.Model.SetActivePage page ]
            )

        SetDangerSign sign ->
            let
                form =
                    Dict.get id db.ncdMeasurements
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
                        NoNCDDangerSigns
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
                                [ Backend.NCDEncounter.Model.SaveDangerSigns personId measurementId value
                                    |> Backend.Model.MsgNCDEncounter id
                                    |> App.Model.MsgIndexedDb
                                , App.Model.SetActivePage <| UserPage <| NCDEncounterPage id
                                ]
                            )
            in
            ( model
            , Cmd.none
            , appMsgs
            )

        SetGroup1Symptom sign ->
            let
                updatedForm =
                    setMultiSelectInputValue .group1Symptoms
                        (\symptoms -> { symptomReviewForm | group1Symptoms = symptoms })
                        NoNCDGroup1Symptoms
                        sign
                        symptomReviewForm

                updatedData =
                    model.symptomReviewData
                        |> (\data -> { data | form = updatedForm })
            in
            ( { model | symptomReviewData = updatedData }
            , Cmd.none
            , []
            )

        SetGroup2Symptom sign ->
            let
                updatedForm =
                    setMultiSelectInputValue .group2Symptoms
                        (\symptoms -> { symptomReviewForm | group2Symptoms = symptoms })
                        NoNCDGroup2Symptoms
                        sign
                        symptomReviewForm

                updatedData =
                    model.symptomReviewData
                        |> (\data -> { data | form = updatedForm })
            in
            ( { model | symptomReviewData = updatedData }
            , Cmd.none
            , []
            )

        SetPainSymptom sign ->
            let
                updatedForm =
                    setMultiSelectInputValue .painSymptoms
                        (\symptoms -> { symptomReviewForm | painSymptoms = symptoms })
                        NoNCDPainSymptoms
                        sign
                        symptomReviewForm

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
                                [ Backend.NCDEncounter.Model.SaveSymptomReview personId measurementId value
                                    |> Backend.Model.MsgNCDEncounter id
                                    |> App.Model.MsgIndexedDb
                                , App.Model.SetActivePage <| UserPage <| NCDEncounterPage id
                                ]
                            )
            in
            ( model
            , Cmd.none
            , appMsgs
            )

        SetFamilyPlanningSign sign ->
            let
                form =
                    Dict.get id db.ncdMeasurements
                        |> Maybe.andThen RemoteData.toMaybe
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
                                [ Backend.NCDEncounter.Model.SaveFamilyPlanning personId measurementId value
                                    |> Backend.Model.MsgNCDEncounter id
                                    |> App.Model.MsgIndexedDb
                                , App.Model.SetActivePage <| UserPage <| NCDEncounterPage id
                                ]
                            )
            in
            ( model
            , Cmd.none
            , appMsgs
            )
