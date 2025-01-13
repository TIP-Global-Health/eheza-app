module Pages.GroupEncounterTypes.View exposing (view)

import App.Model exposing (Msg(..))
import AssocList as Dict
import Backend.Clinic.Model exposing (ClinicType(..))
import Backend.EducationSession.Model exposing (emptyEducationSession)
import Backend.Entities exposing (..)
import Backend.Model exposing (MsgIndexedDb(..))
import Backend.Utils exposing (groupEducationEnabled)
import Backend.Village.Utils exposing (getVillageClinicId)
import EverySet exposing (EverySet)
import Gizra.Html exposing (emptyNode)
import Gizra.NominalDate exposing (NominalDate)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick)
import Pages.GroupEncounterTypes.Model exposing (GroupEncounterType(..))
import Pages.Page exposing (Page(..), SessionPage(..), UserPage(..))
import Pages.Utils exposing (viewBySyncStatus)
import RemoteData
import SyncManager.Model exposing (SiteFeature)
import Translate exposing (Language, translate)


view : Language -> NominalDate -> EverySet SiteFeature -> HealthCenterId -> NurseId -> App.Model.Model -> Html App.Model.Msg
view language currentDate features healthCenterId nurseId model =
    div
        [ class "wrap wrap-alt-2 page-encounter-types" ]
        [ viewHeader language
        , viewContent language currentDate features healthCenterId nurseId model
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


viewContent : Language -> NominalDate -> EverySet SiteFeature -> HealthCenterId -> NurseId -> App.Model.Model -> Html App.Model.Msg
viewContent language currentDate features healthCenterId nurseId model =
    let
        groupEncounterEducationButton =
            if groupEducationEnabled features then
                Maybe.map
                    (\villageId ->
                        emptyEducationSession currentDate nurseId villageId (Just healthCenterId)
                            |> PostEducationSession
                            |> MsgIndexedDb
                            |> encounterButton GroupEncounterEducation
                    )
                    model.villageId
                    |> Maybe.withDefault emptyNode

            else
                emptyNode

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
                                            |> Maybe.andThen RemoteData.toMaybe
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
                model.villageId
                |> Maybe.withDefault NoOp

        encounterButton encounterType action =
            button
                [ class "ui primary button encounter-type"
                , onClick action
                ]
                [ span [ class "text" ] [ text <| translate language <| Translate.GroupEncounterType encounterType ]
                , span [ class "icon-back" ] []
                ]
    in
    div [ class "ui full segment" ]
        [ p [] [ text <| translate language Translate.SelectEncounterType ++ ":" ]
        , encounterButton GroupEncounterNutrition groupAssessmentButtonAction
        , groupEncounterEducationButton
        ]
