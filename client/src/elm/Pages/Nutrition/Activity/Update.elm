module Pages.Nutrition.Activity.Update exposing (update)

import App.Model
import AssocList as Dict
import Backend.Entities exposing (..)
import Backend.Measurement.Model
    exposing
        ( ChildNutritionSign(..)
        , ContributingFactorsSign(..)
        , ImageUrl(..)
        , MuacInCm(..)
        , SkippedForm(..)
        , WeightInGrm(..)
        , WeightInKg(..)
        )
import Backend.Measurement.Utils exposing (getMeasurementValueFunc)
import Backend.Model exposing (ModelIndexedDb)
import Backend.NutritionEncounter.Model
import EverySet
import Gizra.NominalDate exposing (NominalDate)
import Gizra.Update exposing (sequenceExtra)
import Maybe.Extra exposing (unwrap)
import Measurement.Utils exposing (..)
import Pages.Nutrition.Activity.Model exposing (..)
import Pages.Page exposing (Page(..), UserPage(..))
import Pages.Utils exposing (setMuacValueForSite, setMultiSelectInputValue)
import RemoteData exposing (RemoteData(..))
import SyncManager.Model exposing (Site)


update : NominalDate -> Site -> NutritionEncounterId -> ModelIndexedDb -> Msg -> Model -> ( Model, Cmd Msg, List App.Model.Msg )
update currentDate site id db msg model =
    let
        ncdaForm =
            Dict.get id db.nutritionMeasurements
                |> Maybe.andThen RemoteData.toMaybe
                |> Maybe.map
                    (.ncda
                        >> getMeasurementValueFunc
                        >> ncdaFormWithDefault model.ncdaData.form
                    )
                |> Maybe.withDefault model.ncdaData.form

        generateNextStepsMsgs nextTask =
            Maybe.map (\task -> [ SetActiveNextStepsTask task ]) nextTask
                |> Maybe.withDefault [ SetActivePage <| UserPage <| NutritionEncounterPage id ]
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

        SetHeight string ->
            let
                updatedData =
                    let
                        updatedForm =
                            model.heightData.form
                                |> (\form ->
                                        { form | height = String.toFloat string, heightDirty = True, measurementNotTaken = Nothing }
                                   )
                    in
                    model.heightData
                        |> (\data -> { data | form = updatedForm })
            in
            ( { model | heightData = updatedData }
            , Cmd.none
            , []
            )

        ToggleHeightNotTaken ->
            let
                updatedData =
                    let
                        updatedForm =
                            model.heightData.form
                                |> (\form ->
                                        let
                                            notTaken =
                                                Maybe.map
                                                    (\measurementNotTaken ->
                                                        if measurementNotTaken then
                                                            Nothing

                                                        else
                                                            Just True
                                                    )
                                                    form.measurementNotTaken
                                                    |> Maybe.withDefault (Just True)
                                        in
                                        { form | height = Nothing, heightDirty = True, measurementNotTaken = notTaken }
                                   )
                    in
                    model.heightData
                        |> (\data -> { data | form = updatedForm })
            in
            ( { model | heightData = updatedData }
            , Cmd.none
            , []
            )

        SaveHeight skippedForms personId saved ->
            let
                measurementId =
                    Maybe.map Tuple.first saved

                measurement =
                    getMeasurementValueFunc saved

                form_ =
                    model.heightData.form

                appMsgs =
                    if form_.measurementNotTaken == Just True then
                        let
                            updateSkippedFormsMsg =
                                if EverySet.member SkippedHeight skippedForms then
                                    []

                                else
                                    [ Backend.NutritionEncounter.Model.AddSkippedForm SkippedHeight
                                        |> Backend.Model.MsgNutritionEncounter id
                                        |> App.Model.MsgIndexedDb
                                    ]
                        in
                        updateSkippedFormsMsg
                            ++ [ App.Model.SetActivePage <| UserPage <| NutritionEncounterPage id ]

                    else
                        toHeightValueWithDefault skippedForms measurement form_
                            |> unwrap
                                []
                                (\value ->
                                    [ Backend.NutritionEncounter.Model.SaveHeight personId measurementId value
                                        |> Backend.Model.MsgNutritionEncounter id
                                        |> App.Model.MsgIndexedDb
                                    , Backend.NutritionEncounter.Model.RemoveSkippedForm SkippedHeight
                                        |> Backend.Model.MsgNutritionEncounter id
                                        |> App.Model.MsgIndexedDb
                                    , App.Model.SetActivePage <| UserPage <| NutritionEncounterPage id
                                    ]
                                )
            in
            ( model
            , Cmd.none
            , appMsgs
            )

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

        SaveMuac personId saved ->
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
                                [ Backend.NutritionEncounter.Model.SaveMuac personId measurementId value
                                    |> Backend.Model.MsgNutritionEncounter id
                                    |> App.Model.MsgIndexedDb
                                , App.Model.SetActivePage <| UserPage <| NutritionEncounterPage id
                                ]
                            )
            in
            ( model
            , Cmd.none
            , appMsgs
            )

        SetNutritionSign sign ->
            let
                form =
                    Dict.get id db.nutritionMeasurements
                        |> Maybe.withDefault NotAsked
                        |> RemoteData.toMaybe
                        |> Maybe.map (.nutrition >> getMeasurementValueFunc >> nutritionFormWithDefault model.nutritionData.form)
                        |> Maybe.withDefault model.nutritionData.form

                updatedForm =
                    setMultiSelectInputValue .signs
                        (\signs -> { form | signs = signs })
                        NormalChildNutrition
                        sign
                        form

                updatedData =
                    model.nutritionData
                        |> (\data -> { data | form = updatedForm })
            in
            ( { model | nutritionData = updatedData }
            , Cmd.none
            , []
            )

        SaveNutrition personId saved assesment ->
            let
                measurementId =
                    Maybe.map Tuple.first saved

                measurement =
                    getMeasurementValueFunc saved

                appMsgs =
                    model.nutritionData.form
                        |> (\form -> { form | assesment = Just assesment })
                        |> toNutritionValueWithDefault measurement
                        |> unwrap
                            []
                            (\value ->
                                [ Backend.NutritionEncounter.Model.SaveNutrition personId measurementId value
                                    |> Backend.Model.MsgNutritionEncounter id
                                    |> App.Model.MsgIndexedDb
                                , App.Model.SetActivePage <| UserPage <| NutritionEncounterPage id
                                ]
                            )
            in
            ( model
            , Cmd.none
            , appMsgs
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

        SavePhoto personId maybePhotoId url ->
            ( { model | photoData = emptyPhotoData }
            , Cmd.none
            , [ Backend.NutritionEncounter.Model.SavePhoto personId maybePhotoId url
                    |> Backend.Model.MsgNutritionEncounter id
                    |> App.Model.MsgIndexedDb
              , App.Model.SetActivePage <| UserPage <| NutritionEncounterPage id
              ]
            )

        SetWeight string ->
            let
                updatedData =
                    let
                        updatedForm =
                            model.weightData.form
                                |> (\form ->
                                        { form | weight = String.toFloat string, weightDirty = True }
                                   )
                    in
                    model.weightData
                        |> (\data -> { data | form = updatedForm })
            in
            ( { model | weightData = updatedData }
            , Cmd.none
            , []
            )

        SaveWeight personId saved ->
            let
                measurementId =
                    Maybe.map Tuple.first saved

                measurement =
                    getMeasurementValueFunc saved

                appMsgs =
                    model.weightData.form
                        |> toWeightValueWithDefault measurement
                        |> unwrap
                            []
                            (\value ->
                                [ Backend.NutritionEncounter.Model.SaveWeight personId measurementId value
                                    |> Backend.Model.MsgNutritionEncounter id
                                    |> App.Model.MsgIndexedDb
                                , App.Model.SetActivePage <| UserPage <| NutritionEncounterPage id
                                ]
                            )
            in
            ( model
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
                    toNCDAValueWithDefault measurement model.ncdaData.form
                        |> unwrap
                            []
                            (\value ->
                                [ Backend.NutritionEncounter.Model.SaveNCDA personId measurementId value
                                    |> Backend.Model.MsgNutritionEncounter id
                                    |> App.Model.MsgIndexedDb
                                , App.Model.SetActivePage <| UserPage <| NutritionEncounterPage id
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
                            (Backend.NutritionEncounter.Model.SaveSendToHC personId measurementId
                                >> Backend.Model.MsgNutritionEncounter id
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
                            (Backend.NutritionEncounter.Model.SaveHealthEducation personId measurementId
                                >> Backend.Model.MsgNutritionEncounter id
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

        SetContributingFactorsSign sign ->
            let
                form =
                    Dict.get id db.nutritionMeasurements
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
            let
                measurementId =
                    Maybe.map Tuple.first saved

                measurement =
                    getMeasurementValueFunc saved

                extraMsgs =
                    generateNextStepsMsgs nextTask

                appMsgs =
                    toContributingFactorsValueWithDefault measurement model.nextStepsData.contributingFactorsForm
                        |> Maybe.map
                            (Backend.NutritionEncounter.Model.SaveContributingFactors personId measurementId
                                >> Backend.Model.MsgNutritionEncounter id
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
            let
                measurementId =
                    Maybe.map Tuple.first saved

                measurement =
                    getMeasurementValueFunc saved

                extraMsgs =
                    generateNextStepsMsgs nextTask

                form =
                    model.nextStepsData.followUpForm

                appMsgs =
                    toNutritionFollowUpValueWithDefault measurement { form | assesment = Just assesment }
                        |> Maybe.map
                            (Backend.NutritionEncounter.Model.SaveFollowUp personId measurementId
                                >> Backend.Model.MsgNutritionEncounter id
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
