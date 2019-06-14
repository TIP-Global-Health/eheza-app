module Pages.PrenatalActivity.Update exposing (update)

import App.Model
import Backend.Entities exposing (PersonId)
import Backend.Model exposing (ModelIndexedDb)
import Maybe.Extra exposing (isJust)
import Pages.PrenatalActivity.Model exposing (..)
import PrenatalActivity.Model exposing (PrenatalActivity)
import Result exposing (Result)


update : PersonId -> PrenatalActivity -> ModelIndexedDb -> Msg -> Model -> ( Model, Cmd Msg, List App.Model.Msg )
update motherId activity db msg model =
    case msg of
        SetActivePage page ->
            ( model
            , Cmd.none
            , [ App.Model.SetActivePage page ]
            )

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

        SetLmpRange s ->
            let
                range =
                    decodeLmpRange s

                ( lmpDate, isDateSelectorOpen ) =
                    if isJust range then
                        ( model.pregnancyDatingData.form.lmpDate, True )

                    else
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

        SetAbortions value ->
            let
                updatedData =
                    case model.historyData.activeTask of
                        Obstetric ->
                            case model.historyData.obstetricForm of
                                FirstStep form ->
                                    let
                                        updatedForm =
                                            case String.toInt value of
                                                Ok number ->
                                                    { form | abortions = Just number }

                                                Err _ ->
                                                    form
                                    in
                                    model.historyData
                                        |> (\data -> { data | obstetricForm = FirstStep updatedForm })

                                -- We should never get here.
                                -- Input is set on first step.
                                SecondStep form ->
                                    model.historyData

                        _ ->
                            model.historyData
            in
            ( { model | historyData = updatedData }
            , Cmd.none
            , []
            )

        SetActiveHistoryTask taks ->
            let
                updatedData =
                    model.historyData
                        |> (\data -> { data | activeTask = taks })
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
                            case model.historyData.obstetricForm of
                                FirstStep form ->
                                    let
                                        updatedForm =
                                            { form | currentlyPregnant = Just value }
                                    in
                                    model.historyData
                                        |> (\data -> { data | obstetricForm = FirstStep updatedForm })

                                -- We should never get here.
                                -- Input is set on first step.
                                SecondStep form ->
                                    model.historyData

                        _ ->
                            model.historyData
            in
            ( { model | historyData = updatedData }
            , Cmd.none
            , []
            )

        SetLiveChildren value ->
            let
                updatedData =
                    case model.historyData.activeTask of
                        Obstetric ->
                            case model.historyData.obstetricForm of
                                FirstStep form ->
                                    let
                                        updatedForm =
                                            case String.toInt value of
                                                Ok number ->
                                                    { form | liveChildren = Just number }

                                                Err _ ->
                                                    form
                                    in
                                    model.historyData
                                        |> (\data -> { data | obstetricForm = FirstStep updatedForm })

                                -- We should never get here.
                                -- Input is set on first step.
                                SecondStep form ->
                                    model.historyData

                        _ ->
                            model.historyData
            in
            ( { model | historyData = updatedData }
            , Cmd.none
            , []
            )

        SetPreTermPregnancy value ->
            let
                updatedData =
                    case model.historyData.activeTask of
                        Obstetric ->
                            case model.historyData.obstetricForm of
                                FirstStep form ->
                                    let
                                        updatedForm =
                                            case String.toInt value of
                                                Ok number ->
                                                    { form | preTermPreganancy = Just number }

                                                Err _ ->
                                                    form
                                    in
                                    model.historyData
                                        |> (\data -> { data | obstetricForm = FirstStep updatedForm })

                                -- We should never get here.
                                -- Input is set on first step.
                                SecondStep form ->
                                    model.historyData

                        _ ->
                            model.historyData
            in
            ( { model | historyData = updatedData }
            , Cmd.none
            , []
            )

        SetStillbirthsAtTerm value ->
            let
                updatedData =
                    case model.historyData.activeTask of
                        Obstetric ->
                            case model.historyData.obstetricForm of
                                FirstStep form ->
                                    let
                                        updatedForm =
                                            case String.toInt value of
                                                Ok number ->
                                                    { form | stillbirthsAtTerm = Just number }

                                                Err _ ->
                                                    form
                                    in
                                    model.historyData
                                        |> (\data -> { data | obstetricForm = FirstStep updatedForm })

                                -- We should never get here.
                                -- Input is set on first step.
                                SecondStep form ->
                                    model.historyData

                        _ ->
                            model.historyData
            in
            ( { model | historyData = updatedData }
            , Cmd.none
            , []
            )

        SetStillbirthsPreTerm value ->
            let
                updatedData =
                    case model.historyData.activeTask of
                        Obstetric ->
                            case model.historyData.obstetricForm of
                                FirstStep form ->
                                    let
                                        updatedForm =
                                            case String.toInt value of
                                                Ok number ->
                                                    { form | stillbirthsPreTerm = Just number }

                                                Err _ ->
                                                    form
                                    in
                                    model.historyData
                                        |> (\data -> { data | obstetricForm = FirstStep updatedForm })

                                -- We should never get here.
                                -- Input is set on first step.
                                SecondStep form ->
                                    model.historyData

                        _ ->
                            model.historyData
            in
            ( { model | historyData = updatedData }
            , Cmd.none
            , []
            )

        SetTermPregnancy value ->
            let
                updatedData =
                    case model.historyData.activeTask of
                        Obstetric ->
                            case model.historyData.obstetricForm of
                                FirstStep form ->
                                    let
                                        updatedForm =
                                            case String.toInt value of
                                                Ok number ->
                                                    { form | termPreganancy = Just number }

                                                Err _ ->
                                                    form
                                    in
                                    model.historyData
                                        |> (\data -> { data | obstetricForm = FirstStep updatedForm })

                                -- We should never get here.
                                -- Input is set on first step.
                                SecondStep form ->
                                    model.historyData

                        _ ->
                            model.historyData
            in
            ( { model | historyData = updatedData }
            , Cmd.none
            , []
            )
