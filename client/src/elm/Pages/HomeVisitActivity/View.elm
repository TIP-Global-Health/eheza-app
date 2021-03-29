module Pages.HomeVisitActivity.View exposing (view)

import AssocList as Dict
import Backend.Entities exposing (..)
import Backend.HomeVisitActivity.Model exposing (HomeVisitActivity(..))
import Backend.HomeVisitEncounter.Model exposing (HomeVisitEncounter)
import Backend.IndividualEncounterParticipant.Model exposing (IndividualEncounterParticipant)
import Backend.Measurement.Model exposing (..)
import Backend.Model exposing (ModelIndexedDb)
import EverySet
import Gizra.NominalDate exposing (NominalDate)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Json.Decode
import Maybe.Extra exposing (isJust, isNothing, unwrap)
import Measurement.Utils exposing (..)
import Measurement.View
    exposing
        ( renderDatePart
        , viewContributingFactorsForm
        , viewFollowUpForm
        , viewHealthEducationForm
        , viewMeasurementFloatDiff
        , viewMuacIndication
        , viewSendToHCForm
        , zScoreForHeightOrLength
        )
import Pages.HomeVisitActivity.Model exposing (..)
import Pages.HomeVisitActivity.Utils exposing (..)
import Pages.HomeVisitEncounter.Model exposing (AssembledData)
import Pages.HomeVisitEncounter.Utils exposing (generateAssembledData)
import Pages.Page exposing (Page(..), UserPage(..))
import Pages.PrenatalEncounter.View exposing (viewPersonDetails)
import RemoteData exposing (RemoteData(..), WebData)
import Translate exposing (Language, TranslationId, translate)
import Utils.WebData exposing (viewWebData)


view : Language -> NominalDate -> HomeVisitEncounterId -> HomeVisitActivity -> Bool -> ModelIndexedDb -> Model -> Html Msg
view language currentDate id activity isChw db model =
    let
        data =
            generateAssembledData id db
    in
    viewWebData language (viewHeaderAndContent language currentDate id activity isChw db model) identity data


viewHeaderAndContent : Language -> NominalDate -> HomeVisitEncounterId -> HomeVisitActivity -> Bool -> ModelIndexedDb -> Model -> AssembledData -> Html Msg
viewHeaderAndContent language currentDate id activity isChw db model data =
    let
        header =
            viewHeader language id activity

        content =
            viewContent language currentDate id activity isChw db model data
    in
    div [ class "page-activity home-visit" ]
        [ header
        , content
        ]


viewHeader : Language -> HomeVisitEncounterId -> HomeVisitActivity -> Html Msg
viewHeader language id activity =
    div
        [ class "ui basic segment head" ]
        [ h1
            [ class "ui header" ]
            [ text <| translate language <| Translate.HomeVisitActivityTitle activity ]
        , a
            [ class "link-back"
            , onClick <| SetActivePage <| UserPage <| HomeVisitEncounterPage id
            ]
            [ span [ class "icon-back" ] []
            , span [] []
            ]
        ]


viewContent : Language -> NominalDate -> HomeVisitEncounterId -> HomeVisitActivity -> Bool -> ModelIndexedDb -> Model -> AssembledData -> Html Msg
viewContent language currentDate id activity isChw db model assembled =
    ((viewPersonDetails language currentDate assembled.person Nothing |> div [ class "item" ])
        :: viewActivity language currentDate id activity isChw assembled db model
    )
        |> div [ class "ui unstackable items" ]


viewActivity : Language -> NominalDate -> HomeVisitEncounterId -> HomeVisitActivity -> Bool -> AssembledData -> ModelIndexedDb -> Model -> List (Html Msg)
viewActivity language currentDate id activity isChw assembled db model =
    -- @todo
    []
