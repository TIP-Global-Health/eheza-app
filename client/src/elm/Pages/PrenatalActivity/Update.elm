module Pages.PrenatalActivity.Update exposing (update)

import App.Model
import Backend.Entities exposing (PersonId)
import Backend.Model exposing (ModelIndexedDb)
import Maybe.Extra exposing (isJust)
import Pages.PrenatalActivity.Model exposing (..)
import PrenatalActivity.Model exposing (PrenatalActivity)


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
                    model.pregnancyDatingForm
                        |> (\form -> { form | isDateSelectorOpen = not form.isDateSelectorOpen })
            in
            ( { model | pregnancyDatingForm = updatedForm }
            , Cmd.none
            , []
            )

        SetLmpDate value ->
            let
                updatedForm =
                    model.pregnancyDatingForm
                        |> (\form -> { form | lmpDate = Just value })
            in
            ( { model | pregnancyDatingForm = updatedForm }
            , Cmd.none
            , []
            )

        SetLmpDateConfident value ->
            let
                updatedForm =
                    model.pregnancyDatingForm
                        |> (\form -> { form | lmpDateConfident = Just value })
            in
            ( { model | pregnancyDatingForm = updatedForm }
            , Cmd.none
            , []
            )

        SetLmpRange s ->
            let
                range =
                    decodeLmpRange s

                ( lmpDate, isDateSelectorOpen ) =
                    if isJust range then
                        ( model.pregnancyDatingForm.lmpDate, True )

                    else
                        ( Nothing, False )

                updatedForm =
                    model.pregnancyDatingForm
                        |> (\form -> { form | lmpRange = range, lmpDate = lmpDate, isDateSelectorOpen = isDateSelectorOpen })
            in
            ( { model | pregnancyDatingForm = updatedForm }
            , Cmd.none
            , []
            )
