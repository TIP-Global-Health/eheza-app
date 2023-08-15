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
import Pages.Page exposing (Page(..), SessionPage(..), UserPage(..))
import Pages.Utils exposing (viewBySyncStatus)
import RemoteData exposing (RemoteData(..))
import SyncManager.Model exposing (SyncInfoStatus(..))
import Translate exposing (Language, translate)


view : Language -> NominalDate -> ( HealthCenterId, Maybe VillageId ) -> Bool -> App.Model.Model -> Html App.Model.Msg
view language currentDate ( healthCenterId, maybeVillageId ) isChw model =
    div [ class "ui basic segment page-clinical" ]
        [ viewHeader language
        , viewContent language currentDate ( healthCenterId, maybeVillageId ) isChw model
            |> viewBySyncStatus language healthCenterId model.syncManager.syncInfoAuthorities
        ]


viewHeader : Language -> Html Msg
viewHeader language =
    div [ class "ui basic head segment" ]
        [ h1 [ class "ui header" ]
            [ text <| translate language Translate.Clinical ]
        , span
            [ class "link-back"
            , onClick <| SetActivePage PinCodePage
            ]
            [ span [ class "icon-back" ] [] ]
        ]


viewContent : Language -> NominalDate -> ( HealthCenterId, Maybe VillageId ) -> Bool -> App.Model.Model -> Html App.Model.Msg
viewContent language currentDate ( healthCenterId, maybeVillageId ) isChw model =
    let
        groupAssessmentButtonAction =
            Maybe.andThen
                (\villageId ->
                    -- There is one clinic for each village, so, if we got village ID,
                    -- we should be able to find the ID of its clinic.
                    getVillageClinicId villageId model.indexedDb
                        |> Maybe.map
                            (\clinicId ->
                                let
                                    clinicSessions =
                                        Dict.get clinicId model.indexedDb.sessionsByClinic
                                            |> Maybe.withDefault NotAsked
                                            |> RemoteData.toMaybe
                                            |> Maybe.map Dict.toList
                                            |> Maybe.withDefault []

                                    currentDaySessionId =
                                        List.filter (Tuple.second >> .startDate >> (==) currentDate) clinicSessions
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
                maybeVillageId
                |> Maybe.withDefault (SetActivePage <| UserPage ClinicsPage)

        viewButton label class_ action =
            button
                [ class <| "ui primary button " ++ class_
                , onClick action
                ]
                [ span [ class "icon" ] []
                , span [ class "text" ] [ text <| translate language label ]
                , span [ class "icon-back" ] []
                ]
    in
    div []
        [ p [] [ text <| translate language Translate.WhatDoYouWantToDo ]
        , viewButton Translate.IndividualEncounter "individual-assessment" (SetActivePage <| UserPage IndividualEncounterTypesPage)
        , viewButton Translate.GroupAssessment "group-assessment" groupAssessmentButtonAction
        ]
