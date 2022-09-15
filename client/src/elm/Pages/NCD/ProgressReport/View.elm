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
import Pages.Types exposing (LabResultsCurrentMode(..), LabResultsHistoryMode(..), LabResultsMode(..), TestReport(..))
import Pages.Utils exposing (viewPersonDetailsExtended)
import RemoteData exposing (RemoteData(..))
import Translate exposing (Language, TranslationId, translate)
import Utils.WebData exposing (viewWebData)


view : Language -> NominalDate -> NCDEncounterId -> NCDProgressReportInitiator -> ModelIndexedDb -> Model -> Html Msg
view language currentDate id initiator db model =
    let
        assembled =
            generateAssembledData id db

        header =
            viewHeader language initiator model

        content =
            viewWebData language (viewContent language currentDate initiator model) identity assembled

        -- @todo
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
        , content

        -- @todo
        -- , viewModal endEncounterDialog
        ]


viewHeader : Language -> NCDProgressReportInitiator -> Model -> Html Msg
viewHeader language initiator model =
    let
        label =
            Maybe.map
                (\mode ->
                    case mode of
                        LabResultsCurrent currentMode ->
                            Translate.LabResults

                        LabResultsHistory _ ->
                            Translate.LabHistory
                )
                model.labResultsMode
                |> Maybe.withDefault Translate.NCDProgressReport

        backIcon =
            let
                iconForView action =
                    span
                        [ class "link-back" ]
                        [ span
                            [ class "icon-back"
                            , onClick action
                            ]
                            []
                        ]

                goBackActionByLabResultsState defaultAction =
                    Maybe.map
                        (\mode ->
                            let
                                backToCurrentMsg targetMode =
                                    SetLabResultsMode (Just (LabResultsCurrent targetMode))
                            in
                            case mode of
                                LabResultsCurrent currentMode ->
                                    case currentMode of
                                        LabResultsCurrentMain ->
                                            SetLabResultsMode Nothing

                                        LabResultsCurrentDipstickShort ->
                                            backToCurrentMsg LabResultsCurrentMain

                                        LabResultsCurrentDipstickLong ->
                                            backToCurrentMsg LabResultsCurrentMain

                                LabResultsHistory historyMode ->
                                    Maybe.withDefault LabResultsCurrentMain model.labResultsHistoryOrigin
                                        |> backToCurrentMsg
                        )
                        model.labResultsMode
                        |> Maybe.withDefault defaultAction
            in
            case initiator of
                InitiatorEncounterPage id ->
                    iconForView <| goBackActionByLabResultsState (SetActivePage <| UserPage <| NCDEncounterPage id)

                InitiatorRecurrentEncounterPage id ->
                    iconForView <| goBackActionByLabResultsState (SetActivePage <| UserPage <| NCDRecurrentEncounterPage id)

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
            [ text <| translate language label ]
        , backIcon
        ]


viewContent : Language -> NominalDate -> NCDProgressReportInitiator -> Model -> AssembledData -> Html Msg
viewContent language currentDate initiator model assembled =
    let
        derivedContent =
            -- @todo
            -- case model.labResultsMode of
            --     Just mode ->
            --         case mode of
            --             LabResultsCurrent currentMode ->
            --                 [ viewLabResultsPane language currentDate currentMode assembled ]
            --
            --             LabResultsHistory historyMode ->
            --                 [ viewLabResultsHistoryPane language currentDate historyMode ]
            --
            --     Nothing ->
            --         let
            --             firstEncounterMeasurements =
            --                 getFirstEncounterMeasurements isChw assembled
            --
            --             actions =
            --                 case initiator of
            --                     InitiatorEncounterPage _ ->
            --                         let
            --                             ( completedActivities, pendingActivities ) =
            --                                 getAllActivities assembled
            --                                     |> List.filter (Pages.NCD.Activity.Utils.expectActivity currentDate assembled)
            --                                     |> List.partition (Pages.NCD.Activity.Utils.activityCompleted currentDate assembled)
            --                         in
            --                         viewActionButton language
            --                             pendingActivities
            --                             completedActivities
            --                             (SetActivePage PinCodePage)
            --                             SetEndEncounterDialogState
            --                             assembled
            --
            --                     InitiatorRecurrentEncounterPage _ ->
            --                         let
            --                             ( completedActivities, pendingActivities ) =
            --                                 Pages.NCD.RecurrentEncounter.Utils.allActivities
            --                                     |> List.filter (Pages.NCD.RecurrentActivity.Utils.expectActivity currentDate assembled)
            --                                     |> List.partition (Pages.NCD.RecurrentActivity.Utils.activityCompleted currentDate assembled)
            --
            --                             allowEndEcounter =
            --                                 List.isEmpty pendingActivities
            --                         in
            --                         viewEndEncounterButton language allowEndEcounter SetEndEncounterDialogState
            --
            --                     InitiatorNewEncounter encounterId ->
            --                         div [ class "actions" ]
            --                             [ button
            --                                 [ class "ui fluid primary button"
            --                                 , onClick <| SetActivePage <| UserPage <| NCDEncounterPage encounterId
            --                                 ]
            --                                 [ text <| translate language Translate.Reviewed ]
            --                             ]
            --
            --                     Backend.NCDEncounter.Model.InitiatorPatientRecord _ ->
            --                         emptyNode
            --         in
            --         [ viewRiskFactorsPane language currentDate firstEncounterMeasurements
            --         , viewMedicalDiagnosisPane language currentDate isChw firstEncounterMeasurements assembled
            --         , viewObstetricalDiagnosisPane language currentDate isChw firstEncounterMeasurements assembled
            --         , viewChwActivityPane language currentDate isChw assembled
            --         , viewPatientProgressPane language currentDate isChw assembled
            --         , viewLabsPane language currentDate assembled
            --         , viewProgressPhotosPane language currentDate isChw assembled
            --         , actions
            --         ]
            []
    in
    div [ class "ui unstackable items" ] <|
        viewPersonInfoPane language currentDate assembled.person
            :: derivedContent


viewPersonInfoPane : Language -> NominalDate -> Person -> Html any
viewPersonInfoPane language currentDate person =
    div [ class "pane person-details" ]
        [ viewPaneHeading language Translate.PatientInformation
        , div [ class "patient-info" ] <|
            viewPersonDetailsExtended language currentDate person
        ]


viewPaneHeading : Language -> TranslationId -> Html any
viewPaneHeading language label =
    div [ class <| "pane-heading" ]
        [ text <| translate language label ]
