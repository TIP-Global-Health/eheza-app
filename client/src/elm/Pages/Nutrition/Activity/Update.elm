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
        , WeightInGrm(..)
        , WeightInKg(..)
        )
import Backend.Measurement.Utils exposing (getMeasurementValueFunc)
import Backend.Model exposing (ModelIndexedDb)
import Backend.NutritionEncounter.Model
import EverySet
import Gizra.NominalDate exposing (NominalDate)
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
    in
    case msg of
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
                                        { form | height = String.toFloat string, heightDirty = True }
                                   )
                    in
                    model.heightData
                        |> (\data -> { data | form = updatedForm })
            in
            ( { model | heightData = updatedData }
            , Cmd.none
            , []
            )

        SaveHeight personId saved ->
            let
                measurementId =
                    Maybe.map Tuple.first saved

                measurement =
                    getMeasurementValueFunc saved

                appMsgs =
                    model.heightData.form
                        |> toHeightValueWithDefault measurement
                        |> unwrap
                            []
                            (\value ->
                                [ Backend.NutritionEncounter.Model.SaveHeight personId measurementId value
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
                    model.ncdaData.form
                        |> toNCDAValueWithDefault measurement
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

        SaveSendToHC personId saved nextTask_ ->
            let
                measurementId =
                    Maybe.map Tuple.first saved

                measurement =
                    getMeasurementValueFunc saved

                ( backToActivitiesMsg, nextTask ) =
                    nextTask_
                        |> Maybe.map (\task -> ( [], Just task ))
                        |> Maybe.withDefault
                            ( [ App.Model.SetActivePage <| UserPage <| NutritionEncounterPage id ]
                            , Nothing
                            )

                appMsgs =
                    model.nextStepsData.sendToHCForm
                        |> toSendToHCValueWithDefault measurement
                        |> unwrap
                            []
                            (\value ->
                                (Backend.NutritionEncounter.Model.SaveSendToHC personId measurementId value
                                    |> Backend.Model.MsgNutritionEncounter id
                                    |> App.Model.MsgIndexedDb
                                )
                                    :: backToActivitiesMsg
                            )

                updatedData =
                    model.nextStepsData
                        |> (\data -> { data | activeTask = nextTask })
            in
            ( { model | nextStepsData = updatedData }
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

        SaveHealthEducation personId saved nextTask_ ->
            let
                measurementId =
                    Maybe.map Tuple.first saved

                measurement =
                    getMeasurementValueFunc saved

                ( backToActivitiesMsg, nextTask ) =
                    nextTask_
                        |> Maybe.map (\task -> ( [], Just task ))
                        |> Maybe.withDefault
                            ( [ App.Model.SetActivePage <| UserPage <| NutritionEncounterPage id ]
                            , Nothing
                            )

                appMsgs =
                    model.nextStepsData.healthEducationForm
                        |> toHealthEducationValueWithDefault measurement
                        |> unwrap
                            []
                            (\value ->
                                (Backend.NutritionEncounter.Model.SaveHealthEducation personId measurementId value
                                    |> Backend.Model.MsgNutritionEncounter id
                                    |> App.Model.MsgIndexedDb
                                )
                                    :: backToActivitiesMsg
                            )

                updatedData =
                    model.nextStepsData
                        |> (\data -> { data | activeTask = nextTask })
            in
            ( { model | nextStepsData = updatedData }
            , Cmd.none
            , appMsgs
            )

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

        SaveContributingFactors personId saved nextTask_ ->
            let
                measurementId =
                    Maybe.map Tuple.first saved

                measurement =
                    getMeasurementValueFunc saved

                ( backToActivitiesMsg, nextTask ) =
                    nextTask_
                        |> Maybe.map (\task -> ( [], Just task ))
                        |> Maybe.withDefault
                            ( [ App.Model.SetActivePage <| UserPage <| NutritionEncounterPage id ]
                            , Nothing
                            )

                appMsgs =
                    model.nextStepsData.contributingFactorsForm
                        |> toContributingFactorsValueWithDefault measurement
                        |> unwrap
                            []
                            (\value ->
                                (Backend.NutritionEncounter.Model.SaveContributingFactors personId measurementId value
                                    |> Backend.Model.MsgNutritionEncounter id
                                    |> App.Model.MsgIndexedDb
                                )
                                    :: backToActivitiesMsg
                            )

                updatedData =
                    model.nextStepsData
                        |> (\data -> { data | activeTask = nextTask })
            in
            ( { model | nextStepsData = updatedData }
            , Cmd.none
            , appMsgs
            )

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

        SaveFollowUp personId saved assesment nextTask_ ->
            let
                measurementId =
                    Maybe.map Tuple.first saved

                measurement =
                    getMeasurementValueFunc saved

                ( backToActivitiesMsg, nextTask ) =
                    nextTask_
                        |> Maybe.map (\task -> ( [], Just task ))
                        |> Maybe.withDefault
                            ( [ App.Model.SetActivePage <| UserPage <| NutritionEncounterPage id ]
                            , Nothing
                            )

                appMsgs =
                    model.nextStepsData.followUpForm
                        |> (\form -> { form | assesment = Just assesment })
                        |> toNutritionFollowUpValueWithDefault measurement
                        |> unwrap
                            []
                            (\value ->
                                (Backend.NutritionEncounter.Model.SaveFollowUp personId measurementId value
                                    |> Backend.Model.MsgNutritionEncounter id
                                    |> App.Model.MsgIndexedDb
                                )
                                    :: backToActivitiesMsg
                            )

                updatedData =
                    model.nextStepsData
                        |> (\data -> { data | activeTask = nextTask })
            in
            ( { model | nextStepsData = updatedData }
            , Cmd.none
            , appMsgs
            )
