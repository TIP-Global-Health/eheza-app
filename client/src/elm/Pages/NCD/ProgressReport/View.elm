module Pages.NCD.ProgressReport.View exposing (view)

import AssocList as Dict
import Backend.Entities exposing (..)
import Backend.Measurement.Utils exposing (getMeasurementValueFunc)
import Backend.Model exposing (ModelIndexedDb)
import Backend.NCDEncounter.Types exposing (NCDProgressReportInitiator(..))
import Backend.Person.Model exposing (Person)
import EverySet exposing (EverySet)
import Gizra.NominalDate exposing (NominalDate)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Maybe.Extra exposing (isJust)
import Pages.NCD.Model exposing (AssembledData)
import Pages.NCD.ProgressReport.Model exposing (..)
import Pages.NCD.Utils exposing (generateAssembledData)
import Pages.Page exposing (Page(..), UserPage(..))
import RemoteData exposing (RemoteData(..))
import Translate exposing (Language, TranslationId, translate)
import Utils.WebData exposing (viewWebData)


view : Language -> NominalDate -> NCDEncounterId -> NCDProgressReportInitiator -> ModelIndexedDb -> Model -> Html Msg
view language currentDate id initiator db model =
    let
        assembled =
            generateAssembledData id db

        header =
            viewHeader language initiator

        -- content =
        --     viewWebData language (viewContent language currentDate initiator model) identity assembled
        -- endEncounterDialog =
        --     if model.showEndEncounterDialog then
        --         Just <|
        --             viewEndEncounterDialog language
        --                 Translate.EndEncounterQuestion
        --                 Translate.OnceYouEndTheEncounter
        --                 (CloseEncounter id)
        --                 (SetEndEncounterDialogState False)
        --
        --     else
        --         Nothing
    in
    div [ class "page-report ncd" ] <|
        [ header

        -- , content
        -- , viewModal endEncounterDialog
        ]


viewHeader : Language -> NCDProgressReportInitiator -> Html Msg
viewHeader language initiator =
    let
        goBackPage =
            case initiator of
                InitiatorEncounterPage id ->
                    NCDEncounterPage id

                InitiatorRecurrentEncounterPage id ->
                    NCDRecurrentEncounterPage id
    in
    div
        [ class "ui basic segment head" ]
        [ h1 [ class "ui header" ]
            [ text <| translate language Translate.ProgressReport ]
        , span
            [ class "link-back" ]
            [ span
                [ class "icon-back"
                , onClick <| SetActivePage <| UserPage goBackPage
                ]
                []
            ]
        ]
