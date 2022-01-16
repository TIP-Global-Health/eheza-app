module Pages.PatientRecord.View exposing (view)

import AssocList as Dict exposing (Dict)
import Backend.Entities exposing (..)
import Backend.Model exposing (ModelIndexedDb)
import Backend.Person.Model exposing (Person)
import Backend.Person.Utils exposing (generateFullName)
import EverySet exposing (EverySet)
import Gizra.Html exposing (emptyNode)
import Gizra.NominalDate exposing (NominalDate)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Maybe.Extra exposing (isJust)
import Pages.Page exposing (Page(..), UserPage(..))
import Pages.PatientRecord.Model exposing (..)
import Pages.Utils
    exposing
        ( isTaskCompleted
        , taskCompleted
        , tasksBarId
        , viewBoolInput
        , viewCheckBoxMultipleSelectInput
        , viewCheckBoxSelectInput
        , viewCustomLabel
        , viewQuestionLabel
        , viewSaveAction
        )
import Pages.WellChildEncounter.View exposing (thumbnailDimensions, viewPersonDetails)
import RemoteData exposing (RemoteData)
import Translate exposing (Language, TranslationId, translate)
import Utils.Html exposing (thumbnailImage, viewModal)


view : Language -> NominalDate -> PersonId -> ModelIndexedDb -> Model -> Html Msg
view language currentDate id db model =
    let
        person =
            Dict.get id db.people
                |> Maybe.andThen RemoteData.toMaybe

        personDetails =
            Maybe.map (viewPersonDetails language currentDate)
                person
                |> Maybe.withDefault []

        content =
            div [ class "ui unstackable items" ] <|
                [ div [ class "item" ] personDetails ]
    in
    div [ class "page-activity trace-contact" ]
        [ viewHeader language
        , content
        ]


viewHeader : Language -> Html Msg
viewHeader language =
    div
        [ class "ui basic segment head" ]
        [ h1
            [ class "ui header" ]
            [ text <| translate language Translate.CovidContactTracing ]
        , span
            [ class "link-back"
            , onClick <| SetActivePage <| UserPage GlobalCaseManagementPage
            ]
            [ span [ class "icon-back" ] []
            , span [] []
            ]
        ]
