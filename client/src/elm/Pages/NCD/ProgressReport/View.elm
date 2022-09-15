module Pages.NCD.ProgressReport.View exposing (view)

import AssocList as Dict
import Backend.Entities exposing (..)
import Backend.Measurement.Utils exposing (getMeasurementValueFunc)
import Backend.Model exposing (ModelIndexedDb)
import Backend.NCDEncounter.Types exposing (NCDProgressReportInitiator(..))
import Backend.Person.Model exposing (Person)
import EverySet exposing (EverySet)
import Gizra.Html exposing (emptyNode)
import Gizra.NominalDate exposing (NominalDate)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Maybe.Extra exposing (isJust)
import Pages.NCD.Model exposing (AssembledData)
import Pages.NCD.ProgressReport.Model exposing (..)
import Pages.NCD.Utils exposing (generateAssembledData)
import Pages.Page exposing (Page(..), UserPage(..))
import Pages.Report.Types exposing (LabResultsCurrentMode(..), LabResultsHistoryMode(..), LabResultsMode(..), TestReport(..))
import Pages.Report.View exposing (..)
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
            case model.labResultsMode of
                Just mode ->
                    case mode of
                        LabResultsCurrent currentMode ->
                            [ viewLabResultsPane language currentDate currentMode assembled ]

                        LabResultsHistory historyMode ->
                            [ viewLabResultsHistoryPane language currentDate historyMode ]

                Nothing ->
                    let
                        -- @todo
                        actions =
                            --     case initiator of
                            --         InitiatorEncounterPage _ ->
                            --             let
                            --                 ( completedActivities, pendingActivities ) =
                            --                     getAllActivities assembled
                            --                         |> List.filter (Pages.NCD.Activity.Utils.expectActivity currentDate assembled)
                            --                         |> List.partition (Pages.NCD.Activity.Utils.activityCompleted currentDate assembled)
                            --             in
                            --             viewActionButton language
                            --                 pendingActivities
                            --                 completedActivities
                            --                 (SetActivePage PinCodePage)
                            --                 SetEndEncounterDialogState
                            --                 assembled
                            --
                            --         InitiatorRecurrentEncounterPage _ ->
                            --             let
                            --                 ( completedActivities, pendingActivities ) =
                            --                     Pages.NCD.RecurrentEncounter.Utils.allActivities
                            --                         |> List.filter (Pages.NCD.RecurrentActivity.Utils.expectActivity currentDate assembled)
                            --                         |> List.partition (Pages.NCD.RecurrentActivity.Utils.activityCompleted currentDate assembled)
                            --
                            --                 allowEndEcounter =
                            --                     List.isEmpty pendingActivities
                            --             in
                            --             viewEndEncounterButton language allowEndEcounter SetEndEncounterDialogState
                            emptyNode
                    in
                    [ -- @todo
                      -- viewRiskFactorsPane language currentDate firstEncounterMeasurements
                      -- , viewMedicalDiagnosisPane language currentDate isChw firstEncounterMeasurements assembled
                      -- , viewObstetricalDiagnosisPane language currentDate isChw firstEncounterMeasurements assembled
                      -- , viewChwActivityPane language currentDate isChw assembled
                      -- , viewPatientProgressPane language currentDate isChw assembled
                      viewLabsPane language currentDate SetLabResultsMode

                    -- @todo
                    -- , viewProgressPhotosPane language currentDate isChw assembled
                    -- , actions
                    ]
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


viewLabResultsPane : Language -> NominalDate -> LabResultsCurrentMode -> AssembledData -> Html Msg
viewLabResultsPane language currentDate mode assembled =
    -- let
    --     heading =
    --         div [ class "heading" ]
    --             [ div [ class "name" ] [ translateText language Translate.TestName ]
    --             , div [ class "date" ] [ translateText language Translate.TestDate ]
    --             , div [ class "result" ] [ translateText language Translate.Result ]
    --             , div [ class "normal-range" ] [ translateText language Translate.NormalRange ]
    --             ]
    --
    --     measurementsWithLabResults =
    --         assembled.measurements
    --             :: List.map (\( _, _, measurements ) -> measurements) assembled.nursePreviousMeasurementsWithDates
    --
    --     getTestResults getMeasurementFunc getResultFunc =
    --         List.filterMap (getMeasurementFunc >> getMeasurementValueFunc)
    --             measurementsWithLabResults
    --             |> List.filterMap
    --                 (\value ->
    --                     if List.member value.executionNote [ TestNoteRunToday, TestNoteRunPreviously ] then
    --                         Maybe.map (\executionDate -> ( executionDate, getResultFunc value ))
    --                             value.executionDate
    --
    --                     else
    --                         Nothing
    --                 )
    --             |> List.sortWith sortTuplesByDateDesc
    --
    --     getTestResultsKnownAsPositive getMeasurementFunc getResultFunc =
    --         List.filterMap (getMeasurementFunc >> getMeasurementValueFunc)
    --             measurementsWithLabResults
    --             |> List.filterMap
    --                 (\value ->
    --                     if value.executionNote == TestNoteKnownAsPositive then
    --                         Just ( currentDate, Just TestNotPerformedKnownAsPositive )
    --
    --                     else if List.member value.executionNote [ TestNoteRunToday, TestNoteRunPreviously ] then
    --                         Maybe.map (\executionDate -> ( executionDate, getResultFunc value |> Maybe.map TestPerformed ))
    --                             value.executionDate
    --
    --                     else
    --                         Nothing
    --                 )
    --             |> List.sortWith sortTuplesByDateDesc
    --
    --     hivTestResults =
    --         getTestResultsKnownAsPositive .hivTest .testResult
    --
    --     hivPCRTestResults =
    --         getTestResults .hivPCRTest
    --             (\value ->
    --                 Maybe.andThen
    --                     (\status ->
    --                         case status of
    --                             ViralLoadUndetectable ->
    --                                 Just ResultSuppressedViralLoad
    --
    --                             ViralLoadDetectable ->
    --                                 Maybe.map ResultDetectibleViralLoad value.hivViralLoad
    --                     )
    --                     value.hivViralLoadStatus
    --             )
    --
    --     syphilisTestResults =
    --         getTestResults .syphilisTest .testResult
    --
    --     hepatitisBTestResults =
    --         getTestResultsKnownAsPositive .hepatitisBTest .testResult
    --
    --     malariaTestResults =
    --         getTestResults .malariaTest .testResult
    --
    --     urineDipstickTestValues =
    --         List.filterMap (.urineDipstickTest >> getMeasurementValueFunc)
    --             measurementsWithLabResults
    --
    --     urineDipstickTestResults =
    --         List.filterMap
    --             (\value ->
    --                 if List.member value.executionNote [ TestNoteRunToday, TestNoteRunPreviously ] then
    --                     Maybe.map (\executionDate -> ( executionDate, ( value.protein, value.ph, value.glucose ) ))
    --                         value.executionDate
    --
    --                 else
    --                     Nothing
    --             )
    --             urineDipstickTestValues
    --             |> List.sortWith sortTuplesByDateDesc
    --
    --     longUrineDipstickTestResults =
    --         List.filterMap
    --             (\value ->
    --                 if value.testVariant == Just VariantLongTest && List.member value.executionNote [ TestNoteRunToday, TestNoteRunPreviously ] then
    --                     Maybe.map (\executionDate -> ( executionDate, value ))
    --                         value.executionDate
    --
    --                 else
    --                     Nothing
    --             )
    --             urineDipstickTestValues
    --             |> List.sortWith sortTuplesByDateDesc
    --
    --     proteinResults =
    --         List.map (\( date, ( protein, _, _ ) ) -> ( date, protein )) urineDipstickTestResults
    --
    --     phResults =
    --         List.map (\( date, ( _, ph, _ ) ) -> ( date, ph )) urineDipstickTestResults
    --
    --     glucoseResults =
    --         List.map (\( date, ( _, _, glucose ) ) -> ( date, glucose )) urineDipstickTestResults
    --
    --     leukocytesResults =
    --         List.map (\( date, value ) -> ( date, value.leukocytes )) longUrineDipstickTestResults
    --
    --     nitriteResults =
    --         List.map (\( date, value ) -> ( date, value.nitrite )) longUrineDipstickTestResults
    --
    --     urobilinogenResults =
    --         List.map (\( date, value ) -> ( date, value.urobilinogen )) longUrineDipstickTestResults
    --
    --     haemoglobinResults =
    --         List.map (\( date, value ) -> ( date, value.haemoglobin )) longUrineDipstickTestResults
    --
    --     ketoneResults =
    --         List.map (\( date, value ) -> ( date, value.ketone )) longUrineDipstickTestResults
    --
    --     bilirubinResults =
    --         List.map (\( date, value ) -> ( date, value.bilirubin )) longUrineDipstickTestResults
    --
    --     randomBloodSugarResults =
    --         getTestResults .randomBloodSugarTest .sugarCount
    --
    --     hemoglobinResults =
    --         getTestResults .hemoglobinTest .hemoglobinCount
    --
    --     bloodGpRsResults =
    --         getTestResults .bloodGpRsTest (\value -> ( value.bloodGroup, value.rhesus ))
    --
    --     bloodGroupResults =
    --         List.map (\( date, ( bloodGroup, _ ) ) -> ( date, bloodGroup )) bloodGpRsResults
    --
    --     rhesusResults =
    --         List.map (\( date, ( _, rhesus ) ) -> ( date, rhesus )) bloodGpRsResults
    --
    --     content =
    --         case mode of
    --             LabResultsCurrentMain ->
    --                 [ viewLabResultsEntry language currentDate SetLabResultsMode (LabResultsHistoryHIV hivTestResults)
    --                 , viewLabResultsEntry language currentDate SetLabResultsMode (LabResultsHistoryHIVPCR hivPCRTestResults)
    --                 , viewLabResultsEntry language currentDate SetLabResultsMode (LabResultsHistorySyphilis syphilisTestResults)
    --                 , viewLabResultsEntry language currentDate SetLabResultsMode (LabResultsHistoryHepatitisB hepatitisBTestResults)
    --                 , viewLabResultsEntry language currentDate SetLabResultsMode (LabResultsHistoryMalaria malariaTestResults)
    --                 , dipstickShortEntry
    --                 , dipstickLongEntry
    --                 , viewLabResultsEntry language currentDate SetLabResultsMode (LabResultsHistoryRandomBloodSugar randomBloodSugarResults)
    --                 , viewLabResultsEntry language currentDate SetLabResultsMode (LabResultsHistoryHemoglobin hemoglobinResults)
    --                 , viewLabResultsEntry language currentDate SetLabResultsMode (LabResultsHistoryBloodGroup bloodGroupResults)
    --                 , viewLabResultsEntry language currentDate SetLabResultsMode (LabResultsHistoryRhesus rhesusResults)
    --                 ]
    --
    --             LabResultsCurrentDipstickShort ->
    --                 [ proteinEntry
    --                 , phEntry
    --                 , glucoseEntry
    --                 ]
    --
    --             LabResultsCurrentDipstickLong ->
    --                 [ proteinEntry
    --                 , phEntry
    --                 , glucoseEntry
    --                 , viewLabResultsEntry language currentDate SetLabResultsMode (LabResultsHistoryLeukocytes leukocytesResults)
    --                 , viewLabResultsEntry language currentDate SetLabResultsMode (LabResultsHistoryNitrite nitriteResults)
    --                 , viewLabResultsEntry language currentDate SetLabResultsMode (LabResultsHistoryUrobilinogen urobilinogenResults)
    --                 , viewLabResultsEntry language currentDate SetLabResultsMode (LabResultsHistoryHaemoglobin haemoglobinResults)
    --                 , viewLabResultsEntry language currentDate SetLabResultsMode (LabResultsHistoryKetone ketoneResults)
    --                 , viewLabResultsEntry language currentDate SetLabResultsMode (LabResultsHistoryBilirubin bilirubinResults)
    --                 ]
    --
    --     proteinEntry =
    --         viewLabResultsEntry language currentDate SetLabResultsMode (LabResultsHistoryProtein proteinResults)
    --
    --     phEntry =
    --         viewLabResultsEntry language currentDate SetLabResultsMode (LabResultsHistoryPH phResults)
    --
    --     glucoseEntry =
    --         viewLabResultsEntry language currentDate SetLabResultsMode (LabResultsHistoryGlucose glucoseResults)
    --
    --     dipstickShortEntry =
    --         List.head proteinResults
    --             |> Maybe.map
    --                 (\( date, proteinResult ) ->
    --                     let
    --                         proteinResultNormal_ =
    --                             Maybe.map proteinResultNormal proteinResult
    --                                 |> -- It's ok not to have a result, because test
    --                                    -- may have been not performed yet.
    --                                    Maybe.withDefault True
    --
    --                         resultsNormal =
    --                             if not proteinResultNormal_ then
    --                                 False
    --
    --                             else
    --                                 Maybe.map2
    --                                     (\phResult glucoseResult ->
    --                                         phResultNormal phResult && glucoseResultNormal glucoseResult
    --                                     )
    --                                     (List.head phResults |> Maybe.andThen Tuple.second)
    --                                     (List.head glucoseResults |> Maybe.andThen Tuple.second)
    --                                     |> -- We should never get here since protein result
    --                                        -- existed, and all 3 are entered together.
    --                                        Maybe.withDefault False
    --
    --                         result =
    --                             if resultsNormal then
    --                                 translate language Translate.Normal
    --
    --                             else
    --                                 translate language Translate.Abnormal
    --                     in
    --                     viewDipstickEntry (Translate.PrenatalUrineDipstickTestLabel VariantShortTest)
    --                         (formatDDMMYYYY date)
    --                         result
    --                         (Just <| SetLabResultsMode <| Just <| LabResultsCurrent LabResultsCurrentDipstickShort)
    --                         resultsNormal
    --                 )
    --             |> Maybe.withDefault (emptyDipstickEntry (Translate.PrenatalUrineDipstickTestLabel VariantShortTest))
    --
    --     dipstickLongEntry =
    --         List.head leukocytesResults
    --             |> Maybe.map
    --                 (\( date, leukocytesResult ) ->
    --                     let
    --                         leukocytesResultNormal_ =
    --                             Maybe.map leukocytesResultNormal leukocytesResult
    --                                 |> -- It's ok not to have a result, because test
    --                                    -- may have been not performed yet.
    --                                    Maybe.withDefault True
    --
    --                         resultsNormal =
    --                             if not leukocytesResultNormal_ then
    --                                 False
    --
    --                             else
    --                                 let
    --                                     firstGroupResultsNormal =
    --                                         Maybe.map5
    --                                             (\proteinResult phResult glucoseResult nitriteResult urobilinogenResult ->
    --                                                 proteinResultNormal proteinResult
    --                                                     && phResultNormal phResult
    --                                                     && glucoseResultNormal glucoseResult
    --                                                     && nitriteResultNormal nitriteResult
    --                                                     && urobilinogenResultNormal urobilinogenResult
    --                                             )
    --                                             (List.head proteinResults |> Maybe.andThen Tuple.second)
    --                                             (List.head phResults |> Maybe.andThen Tuple.second)
    --                                             (List.head glucoseResults |> Maybe.andThen Tuple.second)
    --                                             (List.head nitriteResults |> Maybe.andThen Tuple.second)
    --                                             (List.head urobilinogenResults |> Maybe.andThen Tuple.second)
    --                                             |> -- We should never get here since leukocytes result
    --                                                -- existed, and all the results are entered together.
    --                                                Maybe.withDefault False
    --
    --                                     secondGroupResultsNormal =
    --                                         Maybe.map3
    --                                             (\haemoglobinResult ketoneResult bilirubinResult ->
    --                                                 urineHaemoglobinValueResultNormal haemoglobinResult
    --                                                     && ketoneResultNormal ketoneResult
    --                                                     && bilirubinResultNormal bilirubinResult
    --                                             )
    --                                             (List.head haemoglobinResults |> Maybe.andThen Tuple.second)
    --                                             (List.head ketoneResults |> Maybe.andThen Tuple.second)
    --                                             (List.head bilirubinResults |> Maybe.andThen Tuple.second)
    --                                             |> -- We should never get here since leukocytes result
    --                                                -- existed, and all the results are entered together.
    --                                                Maybe.withDefault False
    --                                 in
    --                                 firstGroupResultsNormal && secondGroupResultsNormal
    --
    --                         result =
    --                             if resultsNormal then
    --                                 translate language Translate.Normal
    --
    --                             else
    --                                 translate language Translate.Abnormal
    --                     in
    --                     viewDipstickEntry (Translate.PrenatalUrineDipstickTestLabel VariantLongTest)
    --                         (formatDDMMYYYY date)
    --                         result
    --                         (Just <| SetLabResultsMode <| Just <| LabResultsCurrent LabResultsCurrentDipstickLong)
    --                         resultsNormal
    --                 )
    --             |> Maybe.withDefault (emptyDipstickEntry (Translate.PrenatalUrineDipstickTestLabel VariantLongTest))
    --
    --     emptyDipstickEntry label =
    --         viewDipstickEntry label "--/--/----" "---" Nothing True
    --
    --     viewDipstickEntry label date result maybeAction resultNormal =
    --         let
    --             forwardIcon =
    --                 Maybe.map
    --                     (\action ->
    --                         div
    --                             [ class "icon-forward"
    --                             , onClick action
    --                             ]
    --                             []
    --                     )
    --                     maybeAction
    --                     |> Maybe.withDefault emptyNode
    --         in
    --         div [ classList [ ( "entry", True ), ( "warning", not resultNormal ) ] ]
    --             [ div [ class "name" ] [ translateText language label ]
    --             , div [ class "date" ] [ text date ]
    --             , div [ class "result" ] [ text result ]
    --             , div [ class "normal-range" ] [ translateText language Translate.Normal ]
    --             , forwardIcon
    --             ]
    -- in
    div [ class "lab-results" ]
        [ viewItemHeading language (Translate.LabResultsPaneHeader mode) "blue"

        -- , div [ class "pane-content" ] [ heading ]
        -- , div [ class "group-content" ] content
        ]
