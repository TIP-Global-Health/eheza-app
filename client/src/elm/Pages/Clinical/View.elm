module Pages.Clinical.View exposing (view)

import App.Model exposing (Msg(..))
import AssocList as Dict
import Backend.Clinic.Model exposing (ClinicType(..))
import Backend.Entities exposing (..)
import Backend.Model exposing (ModelIndexedDb, MsgIndexedDb(..))
import Backend.Village.Utils exposing (getVillageClinicId)
import Gizra.Html exposing (emptyNode)
import Gizra.NominalDate exposing (NominalDate)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick)
import List.Extra
import List.Zipper as Zipper
import Pages.Page exposing (Page(..), SessionPage(..), UserPage(..))
import RemoteData exposing (RemoteData(..))
import Restful.Endpoint exposing (fromEntityUuid)
import SyncManager.Model exposing (SyncInfoStatus(..))
import Translate exposing (Language, translate)


view : Language -> NominalDate -> ( HealthCenterId, Maybe VillageId ) -> Bool -> App.Model.Model -> Html App.Model.Msg
view language currentDate ( healthCenterId, maybeVillageId ) isChw app =
    viewHeader language
        :: viewContent language currentDate ( healthCenterId, maybeVillageId ) isChw app
        |> div [ class "ui basic segment page-clinical" ]


viewHeader : Language -> Html Msg
viewHeader language =
    div [ class "ui basic head segment" ]
        [ h1 [ class "ui header" ]
            [ text <| translate language Translate.Clinical ]
        , a
            [ class "link-back"
            , onClick <| SetActivePage PinCodePage
            ]
            [ span [ class "icon-back" ] [] ]
        ]


viewContent : Language -> NominalDate -> ( HealthCenterId, Maybe VillageId ) -> Bool -> App.Model.Model -> List (Html App.Model.Msg)
viewContent language currentDate ( healthCenterId, maybeVillageId ) isChw app =
    let
        selectedHealthCenterSyncInfo =
            app.syncManager.syncInfoAuthorities
                |> Maybe.andThen
                    (Zipper.toList >> List.Extra.find (\authorityInfo -> authorityInfo.uuid == fromEntityUuid healthCenterId))

        showWarningMessage header message =
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
                            db =
                                app.indexedDb

                            groupAssessmentButtonAction =
                                maybeVillageId
                                    |> Maybe.andThen
                                        (\villageId ->
                                            -- There is one clinic for each village, so, if we got village ID,
                                            -- we should be able to find the ID of its clinic.
                                            getVillageClinicId villageId db
                                                |> Maybe.map
                                                    (\clinicId ->
                                                        let
                                                            clinicSessions =
                                                                db.sessionsByClinic
                                                                    |> Dict.get clinicId
                                                                    |> Maybe.withDefault NotAsked
                                                                    |> RemoteData.toMaybe
                                                                    |> Maybe.map Dict.toList
                                                                    |> Maybe.withDefault []

                                                            currentDaySessionId =
                                                                clinicSessions
                                                                    |> List.filter (Tuple.second >> .startDate >> (==) currentDate)
                                                                    |> List.head
                                                                    |> Maybe.map Tuple.first
                                                        in
                                                        currentDaySessionId
                                                            |> Maybe.map
                                                                (\sessionId ->
                                                                    SessionPage sessionId AttendancePage
                                                                        |> UserPage
                                                                        |> SetActivePage
                                                                )
                                                            |> Maybe.withDefault
                                                                ({ startDate = currentDate
                                                                 , endDate = Nothing
                                                                 , clinicId = clinicId
                                                                 , clinicType = Chw
                                                                 }
                                                                    |> PostSession
                                                                    |> MsgIndexedDb
                                                                )
                                                    )
                                        )
                                    |> Maybe.withDefault (SetActivePage <| UserPage <| ClinicsPage Nothing)

                            groupAssessmentButton =
                                button
                                    [ class "ui primary button group-assessment"
                                    , onClick groupAssessmentButtonAction
                                    ]
                                    [ span [ class "icon" ] []
                                    , span [ class "text" ] [ text <| translate language Translate.GroupAssessment ]
                                    , span [ class "icon-back" ] []
                                    ]

                            individualEncounterButton =
                                button
                                    [ class "ui primary button individual-assessment"
                                    , onClick <| SetActivePage <| UserPage IndividualEncounterTypesPage
                                    ]
                                    [ span [ class "icon" ] []
                                    , span [ class "text" ] [ text <| translate language Translate.IndividualEncounter ]
                                    , span [ class "icon-back" ] []
                                    ]
                        in
                        [ p [] [ text <| translate language Translate.WhatDoYouWantToDo ]
                        , individualEncounterButton
                        , groupAssessmentButton
                        ]
            )
        |> Maybe.withDefault
            (showWarningMessage Translate.SelectedHCNotSynced Translate.PleaseSync)
