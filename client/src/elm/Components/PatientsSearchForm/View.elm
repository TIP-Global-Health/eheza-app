module Components.PatientsSearchForm.View exposing (view)

import Components.PatientsSearchForm.Model exposing (..)
import Html exposing (..)
import Html.Attributes exposing (..)
import Pages.Utils exposing (viewBoolInput, viewLabel, viewTextInput)
import Translate exposing (Language)


view : Language -> Model -> Html Msg
view language model =
    let
        placeholder =
            case model.mode of
                ModeSearchByName ->
                    Translate.PlaceholderEnterParticipantName

                ModeSearchByNationalId ->
                    Translate.PlaceholderEnterParticipantNationalId
    in
    div [ class "ui search form" ]
        [ viewLabel language Translate.SearchBy
        , viewBoolInput language
            (model.mode == ModeSearchByName |> Just)
            SetMode
            "search-by"
            (Just ( Translate.Name, Translate.NationalId ))
        , viewTextInput language model.input SetInput (Just placeholder) (Just "search-input")
        ]
