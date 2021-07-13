module Pages.IndividualEncounterTypes.View exposing (view)

import App.Model exposing (Msg(..))
import Backend.Entities exposing (..)
import Backend.IndividualEncounterParticipant.Model exposing (IndividualEncounterType(..))
import Backend.Model exposing (ModelIndexedDb)
import Backend.Person.Utils exposing (isPersonAFertileWoman)
import Gizra.Html exposing (emptyNode)
import Gizra.NominalDate exposing (NominalDate)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick)
import List.Extra
import List.Zipper as Zipper
import Pages.Page exposing (Page(..), UserPage(..))
import RemoteData exposing (RemoteData(..))
import Restful.Endpoint exposing (fromEntityUuid)
import SyncManager.Model exposing (SyncInfoStatus(..))
import Translate exposing (Language, translate)


view : Language -> NominalDate -> HealthCenterId -> Bool -> App.Model.Model -> Html App.Model.Msg
view language currentDate selectedHealthCenter isChw app =
    div
        [ class "wrap wrap-alt-2 page-encounter-types" ]
        [ viewHeader language
        , viewContent language currentDate selectedHealthCenter isChw app
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
            [ span [ class "icon-back" ] []
            , span [] []
            ]
        ]


viewContent : Language -> NominalDate -> HealthCenterId -> Bool -> App.Model.Model -> Html App.Model.Msg
viewContent language currentDate selectedHealthCenter isChw app =
    let
        selectedHealthCenterSyncInfo =
            app.syncManager.syncInfoAuthorities
                |> Maybe.andThen
                    (Zipper.toList >> List.Extra.find (\authorityInfo -> authorityInfo.uuid == fromEntityUuid selectedHealthCenter))

        showWarningMessage header message =
            div [ class "ui basic segment" ]
                [ div [ class "ui message warning" ]
                    [ div [ class "header" ] [ text <| translate language header ]
                    , text <| translate language message
                    ]
                ]
    in
    selectedHealthCenterSyncInfo
        |> Maybe.map
            (\syncInfo ->
                case syncInfo.status of
                    SyncManager.Model.NotAvailable ->
                        showWarningMessage Translate.SelectedHCNotSynced Translate.PleaseSync

                    SyncManager.Model.Uploading ->
                        showWarningMessage Translate.SelectedHCSyncing Translate.SelectedHCUploading

                    SyncManager.Model.Downloading ->
                        showWarningMessage Translate.SelectedHCSyncing Translate.SelectedHCDownloading

                    _ ->
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
                                    [ encounterButton AcuteIllnessEncounter
                                    , encounterButton AntenatalEncounter
                                    , encounterButton NutritionEncounter
                                    , encounterButton WellChildEncounter
                                    ]

                                else
                                    [ encounterButton AcuteIllnessEncounter
                                    , encounterButton AntenatalEncounter
                                    , encounterButton NutritionEncounter
                                    , encounterButton WellChildEncounter
                                    ]
                        in
                        p [] [ text <| translate language Translate.SelectEncounterType ++ ":" ]
                            :: buttons
                            |> div [ class "ui full segment" ]
            )
        |> Maybe.withDefault
            (showWarningMessage Translate.SelectedHCNotSynced Translate.PleaseSync)
