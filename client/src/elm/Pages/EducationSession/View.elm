module Pages.EducationSession.View exposing (view)

import AssocList as Dict exposing (Dict)
import Backend.EducationSession.Model exposing (EducationSession)
import Backend.Entities exposing (..)
import Backend.Measurement.Utils exposing (getMeasurementValueFunc)
import Backend.Model exposing (ModelIndexedDb)
import EverySet
import Gizra.NominalDate exposing (NominalDate)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Pages.EducationSession.Model exposing (..)
import Pages.Page exposing (Page(..), UserPage(..))
import Pages.Utils exposing (viewEncounterActionButton)
import RemoteData exposing (RemoteData(..))
import Translate exposing (Language, translate)
import Utils.WebData exposing (viewWebData)


view : Language -> NominalDate -> EducationSessionId -> ModelIndexedDb -> Model -> Html Msg
view language currentDate id db model =
    let
        session =
            Dict.get id db.educationSessions
                |> Maybe.withDefault NotAsked
    in
    viewWebData language (viewHeaderAndContent language currentDate db model) identity session


viewHeaderAndContent : Language -> NominalDate -> ModelIndexedDb -> Model -> EducationSession -> Html Msg
viewHeaderAndContent language currentDate db model session =
    let
        header =
            viewHeader language viewMode session

        --
        -- content =
        --     viewContent language currentDate site db model assembled
        viewMode =
            Maybe.withDefault
                (if EverySet.isEmpty session.topics then
                    ModeTopics

                 else
                    ModeAttendance
                )
                model.viewMode
    in
    div [ class "page-encounter child-scoreboard" ]
        [ header

        -- , content
        -- , viewModal <| acuteIllnessEncounterPopup language assembled model.showAIEncounterPopup TriggerAcuteIllnessEncounter
        ]


viewHeader : Language -> ViewMode -> EducationSession -> Html Msg
viewHeader language viewMode session =
    let
        ( label, goBackAction ) =
            case viewMode of
                ModeTopics ->
                    ( Translate.HealthTopics
                    , SetActivePage <| UserPage GroupEncounterTypesPage
                    )

                ModeAttendance ->
                    ( Translate.Attendance
                    , SetViewMode ModeTopics
                    )
    in
    div
        [ class "ui basic segment head" ]
        [ h1
            [ class "ui header" ]
            [ text <| translate language label ]
        , span
            [ class "link-back"
            , onClick goBackAction
            ]
            [ span [ class "icon-back" ] [] ]
        ]
