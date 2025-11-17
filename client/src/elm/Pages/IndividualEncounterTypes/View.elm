module Pages.IndividualEncounterTypes.View exposing (view)

import App.Model exposing (Msg(..))
import Backend.Entities exposing (..)
import Backend.IndividualEncounterParticipant.Model exposing (IndividualEncounterType(..))
import Backend.Utils
    exposing
        ( acuteIllnessEnabled
        , antenatalEnabled
        , hivManagementEnabled
        , ncdEnabled
        , ncdaEnabled
        , nutritionIndividualEnabled
        , tuberculosisManagementEnabled
        , wellChildEnabled
        )
import EverySet exposing (EverySet)
import Gizra.Html exposing (emptyNode)
import Gizra.NominalDate exposing (NominalDate)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick)
import Pages.Page exposing (Page(..), UserPage(..))
import Pages.Utils exposing (viewBySyncStatus)
import SyncManager.Model exposing (SiteFeature)
import Translate exposing (Language, translate)


view : Language -> NominalDate -> EverySet SiteFeature -> HealthCenterId -> Bool -> App.Model.Model -> Html App.Model.Msg
view language currentDate features healthCenterId isChw model =
    div
        [ class "wrap wrap-alt-2 page-encounter-types" ]
        [ viewHeader language
        , viewContent language currentDate features healthCenterId isChw model
            |> viewBySyncStatus language healthCenterId model.syncManager.syncInfoAuthorities
        ]


viewHeader : Language -> Html Msg
viewHeader language =
    div [ class "ui basic head segment" ]
        [ h1
            [ class "ui header" ]
            [ text <| translate language Translate.BeginNewEncounter ]
        , a
            [ class "link-back"
            , onClick <| SetActivePage <| UserPage ClinicalPage
            ]
            [ span [ class "icon-back" ] [] ]
        ]


viewContent : Language -> NominalDate -> EverySet SiteFeature -> HealthCenterId -> Bool -> App.Model.Model -> Html App.Model.Msg
viewContent language currentDate features healthCenterId isChw model =
    let
        encounterButton encounterType =
            button
                [ class "ui primary button encounter-type"
                , onClick <| SetActivePage <| UserPage <| IndividualEncounterParticipantsPage encounterType
                ]
                [ span [ class "text" ] [ text <| translate language <| Translate.IndividualEncounterType encounterType isChw ]
                , span [ class "icon-back" ] []
                ]

        buttons =
            let
                viewButtonIfFeatureEnabled ( enabledFunc, encounterType ) =
                    if enabledFunc features then
                        encounterButton encounterType

                    else
                        emptyNode
            in
            if isChw then
                List.map viewButtonIfFeatureEnabled
                    [ ( acuteIllnessEnabled, AcuteIllnessEncounter )
                    , ( antenatalEnabled, AntenatalEncounter )
                    , ( nutritionIndividualEnabled, NutritionEncounter )
                    , ( wellChildEnabled, WellChildEncounter )
                    , ( ncdaEnabled, ChildScoreboardEncounter )
                    , ( tuberculosisManagementEnabled, TuberculosisEncounter )
                    , ( hivManagementEnabled, HIVEncounter )
                    ]

            else
                List.map viewButtonIfFeatureEnabled
                    [ ( acuteIllnessEnabled, AcuteIllnessEncounter )
                    , ( antenatalEnabled, AntenatalEncounter )
                    , ( nutritionIndividualEnabled, NutritionEncounter )
                    , ( ncdEnabled, NCDEncounter )
                    , ( wellChildEnabled, WellChildEncounter )
                    ]
    in
    p [] [ text <| translate language Translate.SelectEncounterType ++ ":" ]
        :: buttons
        |> div [ class "ui full segment" ]
