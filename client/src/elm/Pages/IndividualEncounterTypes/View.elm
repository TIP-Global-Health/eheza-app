module Pages.IndividualEncounterTypes.View exposing (view)

import App.Model exposing (Msg(..))
import Backend.Entities exposing (..)
import Backend.IndividualEncounterParticipant.Model exposing (IndividualEncounterType(..))
import Backend.Model exposing (ncdaEnabled)
import Gizra.Html exposing (emptyNode)
import Gizra.NominalDate exposing (NominalDate)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick)
import Pages.Page exposing (Page(..), UserPage(..))
import Pages.Utils exposing (viewBySyncStatus)
import Translate exposing (Language, translate)


view : Language -> NominalDate -> HealthCenterId -> Bool -> App.Model.Model -> Html App.Model.Msg
view language currentDate healthCenterId isChw model =
    div
        [ class "wrap wrap-alt-2 page-encounter-types" ]
        [ viewHeader language
        , viewContent language currentDate healthCenterId isChw model
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


viewContent : Language -> NominalDate -> HealthCenterId -> Bool -> App.Model.Model -> Html App.Model.Msg
viewContent language currentDate healthCenterId isChw model =
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
            if isChw then
                let
                    childScoreboardButton =
                        -- @todo: remove when NCDA is launched.
                        if ncdaEnabled then
                            encounterButton ChildScoreboardEncounter

                        else
                            emptyNode
                in
                [ encounterButton AcuteIllnessEncounter
                , encounterButton AntenatalEncounter
                , encounterButton NutritionEncounter
                , encounterButton WellChildEncounter
                , childScoreboardButton
                ]

            else
                [ encounterButton AcuteIllnessEncounter
                , encounterButton AntenatalEncounter
                , encounterButton NutritionEncounter
                , encounterButton NCDEncounter
                , encounterButton WellChildEncounter
                ]
    in
    p [] [ text <| translate language Translate.SelectEncounterType ++ ":" ]
        :: buttons
        |> div [ class "ui full segment" ]
