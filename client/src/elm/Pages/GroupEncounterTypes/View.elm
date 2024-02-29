module Pages.GroupEncounterTypes.View exposing (view)

import App.Model exposing (Msg(..))
import Backend.Entities exposing (..)
import EverySet exposing (EverySet)
import Gizra.Html exposing (emptyNode)
import Gizra.NominalDate exposing (NominalDate)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick)
import Pages.GroupEncounterTypes.Model exposing (GroupEncounterType(..))
import Pages.Page exposing (Page(..), UserPage(..))
import Pages.Utils exposing (viewBySyncStatus)
import Translate exposing (Language, translate)


view : Language -> NominalDate -> HealthCenterId -> App.Model.Model -> Html App.Model.Msg
view language currentDate healthCenterId model =
    div
        [ class "wrap wrap-alt-2 page-encounter-types" ]
        [ viewHeader language
        , viewContent language currentDate healthCenterId
            |> viewBySyncStatus language healthCenterId model.syncManager.syncInfoAuthorities
        ]


viewHeader : Language -> Html Msg
viewHeader language =
    div [ class "ui basic head segment" ]
        [ h1
            [ class "ui header" ]
            [ text <| translate language Translate.BeginNewEncounter ]
        , span
            [ class "link-back"
            , onClick <| SetActivePage <| UserPage ClinicalPage
            ]
            [ span [ class "icon-back" ] [] ]
        ]


viewContent : Language -> NominalDate -> HealthCenterId -> Html App.Model.Msg
viewContent language currentDate healthCenterId =
    -- let
    --     encounterButton encounterType =
    --         button
    --             [ class "ui primary button encounter-type"
    --             , onClick <| SetActivePage <| UserPage <| GroupEncounterParticipantsPage encounterType
    --             ]
    --             [ span [ class "text" ] [ text <| translate language <| Translate.GroupEncounterType encounterType isChw ]
    --             , span [ class "icon-back" ] []
    --             ]
    --
    --     buttons =
    --         if isChw then
    --             let
    --                 childScoreboardButton =
    --                     if ncdaEnabled features then
    --                         encounterButton ChildScoreboardEncounter
    --
    --                     else
    --                         emptyNode
    --
    --                 tuberculosiskManagementButton =
    --                     if tuberculosisManagementEnabled features then
    --                         encounterButton TuberculosisEncounter
    --
    --                     else
    --                         emptyNode
    --             in
    --             [ encounterButton AcuteIllnessEncounter
    --             , encounterButton AntenatalEncounter
    --             , encounterButton NutritionEncounter
    --             , encounterButton WellChildEncounter
    --             , childScoreboardButton
    --             , tuberculosiskManagementButton
    --             ]
    --
    --         else
    --             [ encounterButton AcuteIllnessEncounter
    --             , encounterButton AntenatalEncounter
    --             , encounterButton NutritionEncounter
    --             , encounterButton NCDEncounter
    --             , encounterButton WellChildEncounter
    --             ]
    -- in
    -- p [] [ text <| translate language Translate.SelectEncounterType ++ ":" ]
    --     :: buttons
    --     |> div [ class "ui full segment" ]
    text "@todo"
