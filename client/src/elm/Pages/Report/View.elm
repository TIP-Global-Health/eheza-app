module Pages.Report.View exposing (viewAcuteIllnessDiagnosisEntry, viewEntries, viewItemHeading, viewLabResultsHistoryPane, viewLabResultsPane, viewLabsPane)

import Backend.AcuteIllnessEncounter.Types exposing (AcuteIllnessProgressReportInitiator)
import Backend.Entities exposing (..)
import Backend.Measurement.Model exposing (..)
import Backend.Measurement.Utils exposing (labExpirationPeriod)
import Backend.Model exposing (ModelIndexedDb)
import Date exposing (Unit(..))
import Gizra.Html exposing (emptyNode, showIf)
import Gizra.NominalDate exposing (NominalDate, formatDDMMYYYY)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Measurement.Model exposing (LaboratoryTask(..))
import Measurement.Utils exposing (bloodSmearResultNotSet, testPerformedByExecutionNote)
import Pages.Page exposing (Page(..), UserPage(..))
import Pages.Report.Model exposing (LabResultsCurrentMode(..), LabResultsHistoryMode(..), LabResultsMode(..), LabsResultsDisplayConfig, LabsResultsValues, PaneEntryStatus, TestReport(..))
import Pages.Report.Utils exposing (altResultNormal, astResultNormal, bilirubinResultNormal, bloodSmearResultNormal, bunResultNormal, creatinineResultNormal, diagnosisEntryStatusToString, getAcuteIllnessDiagnosisForEncounters, getAcuteIllnessEncountersForParticipant, getRandomBloodSugarResultValue, glucoseResultNormal, hba1cResultNormal, hdlCholesterolResultNormal, hemoglobinResultNormal, hepatitisBResultNormal, hivPCRResultNormal, hivResultNormal, ketoneResultNormal, ldlCholesterolResultNormal, leukocytesResultNormal, malariaResultNormal, nitriteResultNormal, partnerHIVResultNormal, phResultNormal, pregnancyResultNormal, proteinResultNormal, randomBloodSugarResultFromValue, randomBloodSugarResultNormal, rhesusResultsNormal, syphilisResultNormal, totalCholesterolResultNormal, triglyceridesResultNormal, urineHaemoglobinValueResultNormal, urobilinogenResultNormal)
import Translate exposing (Language, TranslationId, translate, translateText)
import Utils.NominalDate exposing (sortTuplesByDateDesc)


viewLabsPane : Language -> (Maybe LabResultsMode -> msg) -> Html msg
viewLabsPane language setLabResultsModeMsg =
    div [ class "labs" ] <|
        [ viewItemHeading language Translate.LabResults "blue"
        , div [ class "pane-content" ]
            [ div
                [ class "ui primary button"
                , onClick <| setLabResultsModeMsg <| Just (LabResultsCurrent LabResultsCurrentMain)
                ]
                [ text <| translate language Translate.SeeLabResults ]
            ]
        ]


viewItemHeading : Language -> TranslationId -> String -> Html any
viewItemHeading language label color =
    div [ class <| "pane-heading " ++ color ]
        [ text <| translate language label ]


viewLabResultsEntry : Language -> NominalDate -> Bool -> (Maybe LabResultsMode -> msg) -> LabResultsHistoryMode -> Html msg
viewLabResultsEntry language currentDate isResultsReviewer setLabResultsModeMsg results =
    let
        config =
            case results of
                LabResultsHistoryHIV assembled ->
                    let
                        recentResultValue =
                            List.head assembled |> Maybe.andThen Tuple.second
                    in
                    { label = Translate.LaboratoryTaskLabel TaskHIVTest
                    , recentResult = Maybe.map (translateTestReport language) recentResultValue
                    , knownAsPositive = recentResultValue == Just TestNotPerformedKnownAsPositive
                    , recentResultDate = List.head assembled |> Maybe.map Tuple.first
                    , totalResults = List.length assembled
                    , recentResultNormal =
                        Maybe.map hivResultNormal recentResultValue
                            |> Maybe.withDefault True
                    }

                LabResultsHistoryHIVPCR assembled ->
                    let
                        recentResultValue =
                            List.head assembled |> Maybe.andThen Tuple.second
                    in
                    { label = Translate.LaboratoryTaskLabel TaskHIVPCRTest
                    , recentResult = Maybe.map (Translate.HIVPCRResult >> translate language) recentResultValue
                    , knownAsPositive = False
                    , recentResultDate = List.head assembled |> Maybe.map Tuple.first
                    , totalResults = List.length assembled
                    , recentResultNormal =
                        Maybe.map hivPCRResultNormal recentResultValue
                            |> Maybe.withDefault True
                    }

                LabResultsHistoryPartnerHIV assembled ->
                    let
                        recentResultValue =
                            List.head assembled |> Maybe.andThen Tuple.second
                    in
                    { label = Translate.LaboratoryTaskLabel TaskPartnerHIVTest
                    , recentResult = Maybe.map (Translate.TestResult >> translate language) recentResultValue
                    , knownAsPositive = False
                    , recentResultDate = List.head assembled |> Maybe.map Tuple.first
                    , totalResults = List.length assembled
                    , recentResultNormal =
                        Maybe.map partnerHIVResultNormal recentResultValue
                            |> Maybe.withDefault True
                    }

                LabResultsHistorySyphilis assembled ->
                    let
                        recentResultValue =
                            List.head assembled |> Maybe.andThen Tuple.second
                    in
                    { label = Translate.LaboratoryTaskLabel TaskSyphilisTest
                    , recentResult = Maybe.map (Translate.TestResult >> translate language) recentResultValue
                    , knownAsPositive = False
                    , recentResultDate = List.head assembled |> Maybe.map Tuple.first
                    , totalResults = List.length assembled
                    , recentResultNormal =
                        Maybe.map syphilisResultNormal recentResultValue
                            |> Maybe.withDefault True
                    }

                LabResultsHistoryHepatitisB assembled ->
                    let
                        recentResultValue =
                            List.head assembled |> Maybe.andThen Tuple.second
                    in
                    { label = Translate.LaboratoryTaskLabel TaskHepatitisBTest
                    , recentResult = Maybe.map (translateTestReport language) recentResultValue
                    , knownAsPositive = recentResultValue == Just TestNotPerformedKnownAsPositive
                    , recentResultDate = List.head assembled |> Maybe.map Tuple.first
                    , totalResults = List.length assembled
                    , recentResultNormal =
                        Maybe.map hepatitisBResultNormal recentResultValue
                            |> Maybe.withDefault True
                    }

                LabResultsHistoryMalaria assembled ->
                    let
                        recentResultValue =
                            List.head assembled |> Maybe.andThen Tuple.second
                    in
                    { label = Translate.LaboratoryTaskLabel TaskMalariaTest
                    , recentResult = Maybe.map (Translate.TestResult >> translate language) recentResultValue
                    , knownAsPositive = False
                    , recentResultDate = List.head assembled |> Maybe.map Tuple.first
                    , totalResults = List.length assembled
                    , recentResultNormal =
                        Maybe.map malariaResultNormal recentResultValue
                            |> Maybe.withDefault True
                    }

                LabResultsHistoryBloodSmear assembled ->
                    let
                        recentResultValue =
                            List.head assembled |> Maybe.andThen Tuple.second
                    in
                    { label = Translate.BloodSmearLabel
                    , recentResult = Maybe.map (Translate.BloodSmearResult >> translate language) recentResultValue
                    , knownAsPositive = False
                    , recentResultDate = List.head assembled |> Maybe.map Tuple.first
                    , totalResults = List.length assembled
                    , recentResultNormal =
                        Maybe.map bloodSmearResultNormal recentResultValue
                            |> Maybe.withDefault True
                    }

                LabResultsHistoryProtein assembled ->
                    let
                        recentResultValue =
                            List.head assembled |> Maybe.andThen Tuple.second
                    in
                    { label = Translate.LaboratoryProteinLabel
                    , recentResult = Maybe.map (Translate.LaboratoryProteinValue >> translate language) recentResultValue
                    , knownAsPositive = False
                    , recentResultDate = List.head assembled |> Maybe.map Tuple.first
                    , totalResults = List.length assembled
                    , recentResultNormal =
                        Maybe.map proteinResultNormal recentResultValue
                            |> Maybe.withDefault True
                    }

                LabResultsHistoryPH assembled ->
                    let
                        recentResultValue =
                            List.head assembled |> Maybe.andThen Tuple.second
                    in
                    { label = Translate.LaboratoryPHLabel
                    , recentResult = Maybe.map (Translate.LaboratoryPHValue >> translate language) recentResultValue
                    , knownAsPositive = False
                    , recentResultDate = List.head assembled |> Maybe.map Tuple.first
                    , totalResults = List.length assembled
                    , recentResultNormal =
                        Maybe.map phResultNormal recentResultValue
                            |> Maybe.withDefault True
                    }

                LabResultsHistoryGlucose assembled ->
                    let
                        recentResultValue =
                            List.head assembled |> Maybe.andThen Tuple.second
                    in
                    { label = Translate.LaboratoryGlucoseLabel
                    , recentResult = Maybe.map (Translate.LaboratoryGlucoseValue >> translate language) recentResultValue
                    , knownAsPositive = False
                    , recentResultDate = List.head assembled |> Maybe.map Tuple.first
                    , totalResults = List.length assembled
                    , recentResultNormal =
                        Maybe.map glucoseResultNormal recentResultValue
                            |> Maybe.withDefault True
                    }

                LabResultsHistoryLeukocytes assembled ->
                    let
                        recentResultValue =
                            List.head assembled |> Maybe.andThen Tuple.second
                    in
                    { label = Translate.LaboratoryLeukocytesLabel
                    , recentResult = Maybe.map (Translate.LaboratoryLeukocytesValue >> translate language) recentResultValue
                    , knownAsPositive = False
                    , recentResultDate = List.head assembled |> Maybe.map Tuple.first
                    , totalResults = List.length assembled
                    , recentResultNormal =
                        Maybe.map leukocytesResultNormal recentResultValue
                            |> Maybe.withDefault True
                    }

                LabResultsHistoryNitrite assembled ->
                    let
                        recentResultValue =
                            List.head assembled |> Maybe.andThen Tuple.second
                    in
                    { label = Translate.LaboratoryNitriteLabel
                    , recentResult = Maybe.map (Translate.LaboratoryNitriteValue >> translate language) recentResultValue
                    , knownAsPositive = False
                    , recentResultDate = List.head assembled |> Maybe.map Tuple.first
                    , totalResults = List.length assembled
                    , recentResultNormal =
                        Maybe.map nitriteResultNormal recentResultValue
                            |> Maybe.withDefault True
                    }

                LabResultsHistoryUrobilinogen assembled ->
                    let
                        recentResultValue =
                            List.head assembled |> Maybe.andThen Tuple.second
                    in
                    { label = Translate.LaboratoryUrobilinogenLabel
                    , recentResult = Maybe.map (Translate.LaboratoryUrobilinogenValue >> translate language) recentResultValue
                    , knownAsPositive = False
                    , recentResultDate = List.head assembled |> Maybe.map Tuple.first
                    , totalResults = List.length assembled
                    , recentResultNormal =
                        Maybe.map urobilinogenResultNormal recentResultValue
                            |> Maybe.withDefault True
                    }

                LabResultsHistoryHaemoglobin assembled ->
                    let
                        recentResultValue =
                            List.head assembled |> Maybe.andThen Tuple.second
                    in
                    { label = Translate.LaboratoryHaemoglobinLabel
                    , recentResult = Maybe.map (Translate.LaboratoryHaemoglobinValue >> translate language) recentResultValue
                    , knownAsPositive = False
                    , recentResultDate = List.head assembled |> Maybe.map Tuple.first
                    , totalResults = List.length assembled
                    , recentResultNormal =
                        Maybe.map urineHaemoglobinValueResultNormal recentResultValue
                            |> Maybe.withDefault True
                    }

                LabResultsHistoryKetone assembled ->
                    let
                        recentResultValue =
                            List.head assembled |> Maybe.andThen Tuple.second
                    in
                    { label = Translate.LaboratoryKetoneLabel
                    , recentResult = Maybe.map (Translate.LaboratoryKetoneValue >> translate language) recentResultValue
                    , knownAsPositive = False
                    , recentResultDate = List.head assembled |> Maybe.map Tuple.first
                    , totalResults = List.length assembled
                    , recentResultNormal =
                        Maybe.map ketoneResultNormal recentResultValue
                            |> Maybe.withDefault True
                    }

                LabResultsHistoryBilirubin assembled ->
                    let
                        recentResultValue =
                            List.head assembled |> Maybe.andThen Tuple.second
                    in
                    { label = Translate.LaboratoryBilirubinLabel
                    , recentResult = Maybe.map (Translate.LaboratoryBilirubinValue >> translate language) recentResultValue
                    , knownAsPositive = False
                    , recentResultDate = List.head assembled |> Maybe.map Tuple.first
                    , totalResults = List.length assembled
                    , recentResultNormal =
                        Maybe.map bilirubinResultNormal recentResultValue
                            |> Maybe.withDefault True
                    }

                LabResultsHistoryRandomBloodSugar assembled ->
                    let
                        recentResultValue =
                            List.head assembled |> Maybe.andThen Tuple.second
                    in
                    { label = Translate.LaboratoryTaskLabel TaskRandomBloodSugarTest
                    , recentResult = Maybe.map (getRandomBloodSugarResultValue >> String.fromFloat) recentResultValue
                    , knownAsPositive = False
                    , recentResultDate = List.head assembled |> Maybe.map Tuple.first
                    , totalResults = List.length assembled
                    , recentResultNormal =
                        Maybe.map randomBloodSugarResultNormal recentResultValue
                            |> Maybe.withDefault True
                    }

                LabResultsHistoryHemoglobin assembled ->
                    let
                        recentResultValue =
                            List.head assembled |> Maybe.andThen Tuple.second
                    in
                    { label = Translate.LaboratoryTaskLabel TaskHemoglobinTest
                    , recentResult = Maybe.map String.fromFloat recentResultValue
                    , knownAsPositive = False
                    , recentResultDate = List.head assembled |> Maybe.map Tuple.first
                    , totalResults = List.length assembled
                    , recentResultNormal =
                        Maybe.map hemoglobinResultNormal recentResultValue
                            |> Maybe.withDefault True
                    }

                LabResultsHistoryBloodGroup assembled ->
                    let
                        recentResultValue =
                            List.head assembled |> Maybe.andThen Tuple.second
                    in
                    { label = Translate.LaboratoryBloodGroupLabel
                    , recentResult = Maybe.map (Translate.LaboratoryBloodGroup >> translate language) recentResultValue
                    , recentResultDate = List.head assembled |> Maybe.map Tuple.first
                    , knownAsPositive = False
                    , totalResults = List.length assembled
                    , recentResultNormal = True
                    }

                LabResultsHistoryRhesus assembled ->
                    let
                        recentResultValue =
                            List.head assembled |> Maybe.andThen Tuple.second
                    in
                    { label = Translate.LaboratoryRhesusLabel
                    , recentResult = Maybe.map (Translate.LaboratoryRhesus >> translate language) recentResultValue
                    , knownAsPositive = False
                    , recentResultDate = List.head assembled |> Maybe.map Tuple.first
                    , totalResults = List.length assembled
                    , recentResultNormal =
                        Maybe.map rhesusResultsNormal recentResultValue
                            |> Maybe.withDefault True
                    }

                LabResultsHistoryCreatinine assembled ->
                    let
                        recentResultValue =
                            List.head assembled |> Maybe.andThen Tuple.second
                    in
                    { label = Translate.LaboratoryCreatinineLabel
                    , recentResult = Maybe.map String.fromFloat recentResultValue
                    , knownAsPositive = False
                    , recentResultDate = List.head assembled |> Maybe.map Tuple.first
                    , totalResults = List.length assembled
                    , recentResultNormal =
                        Maybe.map creatinineResultNormal recentResultValue
                            |> Maybe.withDefault True
                    }

                LabResultsHistoryBUN assembled ->
                    let
                        recentResultValue =
                            List.head assembled |> Maybe.andThen Tuple.second
                    in
                    { label = Translate.LaboratoryBUNLabel
                    , recentResult = Maybe.map String.fromFloat recentResultValue
                    , knownAsPositive = False
                    , recentResultDate = List.head assembled |> Maybe.map Tuple.first
                    , totalResults = List.length assembled
                    , recentResultNormal =
                        Maybe.map bunResultNormal recentResultValue
                            |> Maybe.withDefault True
                    }

                LabResultsHistoryALT assembled ->
                    let
                        recentResultValue =
                            List.head assembled |> Maybe.andThen Tuple.second
                    in
                    { label = Translate.LaboratoryALTLabel
                    , recentResult = Maybe.map String.fromFloat recentResultValue
                    , knownAsPositive = False
                    , recentResultDate = List.head assembled |> Maybe.map Tuple.first
                    , totalResults = List.length assembled
                    , recentResultNormal =
                        Maybe.map altResultNormal recentResultValue
                            |> Maybe.withDefault True
                    }

                LabResultsHistoryAST assembled ->
                    let
                        recentResultValue =
                            List.head assembled |> Maybe.andThen Tuple.second
                    in
                    { label = Translate.LaboratoryASTLabel
                    , recentResult = Maybe.map String.fromFloat recentResultValue
                    , knownAsPositive = False
                    , recentResultDate = List.head assembled |> Maybe.map Tuple.first
                    , totalResults = List.length assembled
                    , recentResultNormal =
                        Maybe.map astResultNormal recentResultValue
                            |> Maybe.withDefault True
                    }

                LabResultsHistoryPregnancy assembled ->
                    let
                        recentResultValue =
                            List.head assembled |> Maybe.andThen Tuple.second
                    in
                    { label = Translate.LaboratoryPregnancyLabel
                    , recentResult = Maybe.map (translateTestReport language) recentResultValue
                    , knownAsPositive = recentResultValue == Just TestNotPerformedKnownAsPositive
                    , recentResultDate = List.head assembled |> Maybe.map Tuple.first
                    , totalResults = List.length assembled
                    , recentResultNormal =
                        Maybe.map pregnancyResultNormal recentResultValue
                            |> Maybe.withDefault True
                    }

                LabResultsHistoryHbA1c assembled ->
                    let
                        recentResultValue =
                            List.head assembled |> Maybe.andThen Tuple.second
                    in
                    { label = Translate.HbA1c
                    , recentResult = Maybe.map String.fromFloat recentResultValue
                    , knownAsPositive = False
                    , recentResultDate = List.head assembled |> Maybe.map Tuple.first
                    , totalResults = List.length assembled
                    , recentResultNormal =
                        Maybe.map hba1cResultNormal recentResultValue
                            |> Maybe.withDefault True
                    }

                LabResultsHistoryTotalCholesterol assembled ->
                    let
                        recentResultValue =
                            List.head assembled |> Maybe.andThen Tuple.second
                    in
                    { label = Translate.LaboratoryLipidPanelTotalCholesterolLabel
                    , recentResult = Maybe.map String.fromFloat recentResultValue
                    , knownAsPositive = False
                    , recentResultDate = List.head assembled |> Maybe.map Tuple.first
                    , totalResults = List.length assembled
                    , recentResultNormal =
                        Maybe.map totalCholesterolResultNormal recentResultValue
                            |> Maybe.withDefault True
                    }

                LabResultsHistoryLDLCholesterol assembled ->
                    let
                        recentResultValue =
                            List.head assembled |> Maybe.andThen Tuple.second
                    in
                    { label = Translate.LaboratoryLipidPanelLDLCholesterolLabel
                    , recentResult = Maybe.map String.fromFloat recentResultValue
                    , knownAsPositive = False
                    , recentResultDate = List.head assembled |> Maybe.map Tuple.first
                    , totalResults = List.length assembled
                    , recentResultNormal =
                        Maybe.map ldlCholesterolResultNormal recentResultValue
                            |> Maybe.withDefault True
                    }

                LabResultsHistoryHDLCholesterol assembled ->
                    let
                        recentResultValue =
                            List.head assembled |> Maybe.andThen Tuple.second
                    in
                    { label = Translate.LaboratoryLipidPanelHDLCholesterolLabel
                    , recentResult = Maybe.map String.fromFloat recentResultValue
                    , knownAsPositive = False
                    , recentResultDate = List.head assembled |> Maybe.map Tuple.first
                    , totalResults = List.length assembled
                    , recentResultNormal =
                        Maybe.map hdlCholesterolResultNormal recentResultValue
                            |> Maybe.withDefault True
                    }

                LabResultsHistoryTriglycerides assembled ->
                    let
                        recentResultValue =
                            List.head assembled |> Maybe.andThen Tuple.second
                    in
                    { label = Translate.LaboratoryLipidPanelTriglyceridesLabel
                    , recentResult = Maybe.map String.fromFloat recentResultValue
                    , knownAsPositive = False
                    , recentResultDate = List.head assembled |> Maybe.map Tuple.first
                    , totalResults = List.length assembled
                    , recentResultNormal =
                        Maybe.map triglyceridesResultNormal recentResultValue
                            |> Maybe.withDefault True
                    }
    in
    if isResultsReviewer && config.totalResults == 0 then
        emptyNode

    else
        let
            dateCell =
                Maybe.map (formatDDMMYYYY >> text) config.recentResultDate
                    |> Maybe.withDefault (text "")

            resultCell =
                if config.totalResults == 0 then
                    text ""

                else if config.knownAsPositive then
                    viewKnownAsPositiveResult language

                else
                    Maybe.map text config.recentResult
                        |> Maybe.withDefault (viewUncompetedResult language currentDate config.recentResultDate)

            historyResultsIcon =
                if not isResultsReviewer && config.totalResults > 1 then
                    div
                        [ class "icon-forward"
                        , onClick <| setLabResultsModeMsg <| Just (LabResultsHistory results)
                        ]
                        []

                else
                    emptyNode

            normalRange =
                case results of
                    LabResultsHistoryRandomBloodSugar assembled ->
                        List.head assembled
                            |> Maybe.andThen Tuple.second
                            |> Maybe.map Translate.RandomBloodSugarResultNormalRange
                            |> Maybe.withDefault Translate.EmptyString

                    _ ->
                        Translate.LabResultsNormalRange results
        in
        div [ classList [ ( "entry", True ), ( "warning", not config.recentResultNormal ) ] ]
            [ div [ class "name" ] [ translateText language config.label ]
            , div [ class "date" ] [ dateCell ]
            , div [ class "result" ] [ resultCell ]
            , div [ class "normal-range" ] [ text <| translate language normalRange ]
            , historyResultsIcon
            ]


translateTestReport : Language -> TestReport -> String
translateTestReport language report =
    case report of
        TestPerformed result ->
            Translate.TestResult result
                |> translate language

        TestNotPerformedKnownAsPositive ->
            -- We don't need this, since this result is displayed
            -- using viewKnownAsPositiveResult funciton.
            ""


viewKnownAsPositiveResult : Language -> Html any
viewKnownAsPositiveResult language =
    span [ class "known-positive" ]
        [ text <| translate language Translate.KnownPositive ]


viewUncompetedResult : Language -> NominalDate -> Maybe NominalDate -> Html any
viewUncompetedResult language currentDate resultDate =
    let
        transId =
            Maybe.map
                (\date ->
                    if Date.diff Days date currentDate >= labExpirationPeriod then
                        Translate.ResultsMissing

                    else
                        Translate.ResultsPending
                )
                resultDate
                |> Maybe.withDefault Translate.ResultsPending
    in
    span [ class "uncompleted" ] [ translateText language transId ]


viewLabResultsPane :
    Language
    -> NominalDate
    -> Bool
    -> LabResultsCurrentMode
    -> (Maybe LabResultsMode -> msg)
    -> LabsResultsDisplayConfig
    -> LabsResultsValues encounterId
    -> Html msg
viewLabResultsPane language currentDate viewForConfirmation mode setLabResultsModeMsg displayConfig data =
    let
        heading =
            div [ class "heading" ]
                [ div [ class "name" ] [ translateText language Translate.TestName ]
                , div [ class "date" ] [ translateText language Translate.TestDate ]
                , div [ class "result" ] [ translateText language Translate.Result ]
                , div [ class "normal-range" ] [ translateText language Translate.NormalRange ]
                ]

        getTestResults getValueFunc getResultFunc =
            getValueFunc data
                |> List.filterMap
                    (\value ->
                        if testPerformedByExecutionNote value.executionNote then
                            Maybe.map (\executionDate -> ( executionDate, getResultFunc value ))
                                value.executionDate

                        else
                            Nothing
                    )
                |> List.sortWith sortTuplesByDateDesc

        getTestResultsKnownAsPositive getValueFunc getResultFunc =
            getValueFunc data
                |> List.filterMap
                    (\value ->
                        if value.executionNote == TestNoteKnownAsPositive then
                            Just ( currentDate, Just TestNotPerformedKnownAsPositive )

                        else if testPerformedByExecutionNote value.executionNote then
                            Maybe.map (\executionDate -> ( executionDate, getResultFunc value |> Maybe.map TestPerformed ))
                                value.executionDate

                        else
                            Nothing
                    )
                |> List.sortWith sortTuplesByDateDesc

        urineDipstickTestResults =
            List.filterMap
                (\value ->
                    if testPerformedByExecutionNote value.executionNote then
                        Maybe.map (\executionDate -> ( executionDate, ( value.protein, value.ph, value.glucose ) ))
                            value.executionDate

                    else
                        Nothing
                )
                data.urineDipstick
                |> List.sortWith sortTuplesByDateDesc

        longUrineDipstickTestResults =
            List.filterMap
                (\value ->
                    if value.testVariant == Just VariantLongTest && testPerformedByExecutionNote value.executionNote then
                        Maybe.map (\executionDate -> ( executionDate, value ))
                            value.executionDate

                    else
                        Nothing
                )
                data.urineDipstick
                |> List.sortWith sortTuplesByDateDesc

        proteinResults =
            List.map (\( date, ( protein, _, _ ) ) -> ( date, protein )) urineDipstickTestResults

        phResults =
            List.map (\( date, ( _, ph, _ ) ) -> ( date, ph )) urineDipstickTestResults

        glucoseResults =
            List.map (\( date, ( _, _, glucose ) ) -> ( date, glucose )) urineDipstickTestResults

        leukocytesResults =
            List.map (\( date, value ) -> ( date, value.leukocytes )) longUrineDipstickTestResults

        nitriteResults =
            List.map (\( date, value ) -> ( date, value.nitrite )) longUrineDipstickTestResults

        urobilinogenResults =
            List.map (\( date, value ) -> ( date, value.urobilinogen )) longUrineDipstickTestResults

        haemoglobinResults =
            List.map (\( date, value ) -> ( date, value.haemoglobin )) longUrineDipstickTestResults

        ketoneResults =
            List.map (\( date, value ) -> ( date, value.ketone )) longUrineDipstickTestResults

        bilirubinResults =
            List.map (\( date, value ) -> ( date, value.bilirubin )) longUrineDipstickTestResults

        lipidPanelTestResults =
            List.filterMap
                (\value ->
                    if testPerformedByExecutionNote value.executionNote then
                        Maybe.map
                            (\executionDate ->
                                ( executionDate
                                , { totalCholesterol = value.totalCholesterolResult
                                  , ldlCholesterol = value.ldlCholesterolResult
                                  , hdlCholesterol = value.hdlCholesterolResult
                                  , triglycerides = value.triglyceridesResult
                                  }
                                )
                            )
                            value.executionDate

                    else
                        Nothing
                )
                data.lipidPanel
                |> List.sortWith sortTuplesByDateDesc

        totalCholesterolResults =
            List.map (Tuple.mapSecond .totalCholesterol) lipidPanelTestResults

        ldlCholesterolResults =
            List.map (Tuple.mapSecond .ldlCholesterol) lipidPanelTestResults

        hdlCholesterolResults =
            List.map (Tuple.mapSecond .hdlCholesterol) lipidPanelTestResults

        triglyceridesResults =
            List.map (Tuple.mapSecond .triglycerides) lipidPanelTestResults

        content =
            case mode of
                LabResultsCurrentMain ->
                    let
                        dipstickShortEntry =
                            let
                                emptyEntry =
                                    if viewForConfirmation then
                                        emptyNode

                                    else
                                        emptyConsolidatedEntry (Translate.UrineDipstickTestLabel VariantShortTest)
                            in
                            List.head proteinResults
                                |> Maybe.map
                                    (\( date, proteinResult ) ->
                                        let
                                            proteinResultNormal_ =
                                                Maybe.map proteinResultNormal proteinResult
                                                    |> -- It's ok not to have a result, because test
                                                       -- may have been not performed yet.
                                                       Maybe.withDefault True

                                            resultsNormal =
                                                if not proteinResultNormal_ then
                                                    False

                                                else
                                                    Maybe.map2
                                                        (\phResult glucoseResult ->
                                                            phResultNormal phResult && glucoseResultNormal glucoseResult
                                                        )
                                                        (List.head phResults |> Maybe.andThen Tuple.second)
                                                        (List.head glucoseResults |> Maybe.andThen Tuple.second)
                                                        |> -- We should never get here since protein result
                                                           -- existed, and all 3 are entered together.
                                                           Maybe.withDefault False

                                            result =
                                                if resultsNormal then
                                                    translate language Translate.Normal

                                                else
                                                    translate language Translate.Abnormal
                                        in
                                        viewConsolidatedEntry (Translate.UrineDipstickTestLabel VariantShortTest)
                                            (formatDDMMYYYY date)
                                            result
                                            (Just <| setLabResultsModeMsg <| Just <| LabResultsCurrent LabResultsCurrentDipstickShort)
                                            resultsNormal
                                    )
                                |> Maybe.withDefault emptyEntry

                        dipstickLongEntry =
                            let
                                emptyEntry =
                                    if viewForConfirmation then
                                        emptyNode

                                    else
                                        emptyConsolidatedEntry (Translate.UrineDipstickTestLabel VariantLongTest)
                            in
                            List.head leukocytesResults
                                |> Maybe.map
                                    (\( date, leukocytesResult ) ->
                                        let
                                            leukocytesResultNormal_ =
                                                Maybe.map leukocytesResultNormal leukocytesResult
                                                    |> -- It's ok not to have a result, because test
                                                       -- may have been not performed yet.
                                                       Maybe.withDefault True

                                            resultsNormal =
                                                if not leukocytesResultNormal_ then
                                                    False

                                                else
                                                    let
                                                        firstGroupResultsNormal =
                                                            Maybe.map5
                                                                (\proteinResult phResult glucoseResult nitriteResult urobilinogenResult ->
                                                                    proteinResultNormal proteinResult
                                                                        && phResultNormal phResult
                                                                        && glucoseResultNormal glucoseResult
                                                                        && nitriteResultNormal nitriteResult
                                                                        && urobilinogenResultNormal urobilinogenResult
                                                                )
                                                                (List.head proteinResults |> Maybe.andThen Tuple.second)
                                                                (List.head phResults |> Maybe.andThen Tuple.second)
                                                                (List.head glucoseResults |> Maybe.andThen Tuple.second)
                                                                (List.head nitriteResults |> Maybe.andThen Tuple.second)
                                                                (List.head urobilinogenResults |> Maybe.andThen Tuple.second)
                                                                |> -- We should never get here since leukocytes result
                                                                   -- existed, and all the results are entered together.
                                                                   Maybe.withDefault False

                                                        secondGroupResultsNormal =
                                                            Maybe.map3
                                                                (\haemoglobinResult ketoneResult bilirubinResult ->
                                                                    urineHaemoglobinValueResultNormal haemoglobinResult
                                                                        && ketoneResultNormal ketoneResult
                                                                        && bilirubinResultNormal bilirubinResult
                                                                )
                                                                (List.head haemoglobinResults |> Maybe.andThen Tuple.second)
                                                                (List.head ketoneResults |> Maybe.andThen Tuple.second)
                                                                (List.head bilirubinResults |> Maybe.andThen Tuple.second)
                                                                |> -- We should never get here since leukocytes result
                                                                   -- existed, and all the results are entered together.
                                                                   Maybe.withDefault False
                                                    in
                                                    firstGroupResultsNormal && secondGroupResultsNormal

                                            result =
                                                if resultsNormal then
                                                    translate language Translate.Normal

                                                else
                                                    translate language Translate.Abnormal
                                        in
                                        viewConsolidatedEntry (Translate.UrineDipstickTestLabel VariantLongTest)
                                            (formatDDMMYYYY date)
                                            result
                                            (Just <| setLabResultsModeMsg <| Just <| LabResultsCurrent LabResultsCurrentDipstickLong)
                                            resultsNormal
                                    )
                                |> Maybe.withDefault emptyEntry

                        dipstickEntries =
                            if viewForConfirmation then
                                -- Confirmation view shows results only of current encounter.
                                -- Therefore, we should show either short or long entry instead of both.
                                if List.isEmpty longUrineDipstickTestResults then
                                    [ dipstickShortEntry ]

                                else
                                    [ dipstickLongEntry ]

                            else
                                [ dipstickShortEntry, dipstickLongEntry ]

                        hivTestResults =
                            getTestResultsKnownAsPositive .hiv .testResult

                        partnerHIVTestResults =
                            getTestResults .partnerHIV .testResult

                        syphilisTestResults =
                            getTestResults .syphilis .testResult

                        hepatitisBTestResults =
                            getTestResultsKnownAsPositive .hepatitisB .testResult

                        malariaTestResults =
                            getTestResults .malaria .testResult

                        randomBloodSugarResults =
                            List.filterMap randomBloodSugarResultFromValue data.randomBloodSugar
                                |> List.sortWith sortTuplesByDateDesc

                        hemoglobinResults =
                            getTestResults .hemoglobin .hemoglobinCount

                        pregnancyTestResults =
                            getTestResultsKnownAsPositive .pregnancy .testResult

                        hba1cResults =
                            getTestResults .hba1c .hba1cResult

                        lipidPanelEntry =
                            List.head totalCholesterolResults
                                |> Maybe.map
                                    (\( date, totalCholesterolResult ) ->
                                        let
                                            totalCholesterolResultsNormal_ =
                                                Maybe.map totalCholesterolResultNormal totalCholesterolResult
                                                    |> -- It's ok not to have a result, because test
                                                       -- may have been not performed yet.
                                                       Maybe.withDefault True

                                            resultsNormal =
                                                if not totalCholesterolResultsNormal_ then
                                                    False

                                                else
                                                    Maybe.map3
                                                        (\ldlCholesterolResult hdlCholesterolResult triglyceridesResult ->
                                                            ldlCholesterolResultNormal ldlCholesterolResult
                                                                && hdlCholesterolResultNormal hdlCholesterolResult
                                                                && triglyceridesResultNormal triglyceridesResult
                                                        )
                                                        (List.head ldlCholesterolResults |> Maybe.andThen Tuple.second)
                                                        (List.head hdlCholesterolResults |> Maybe.andThen Tuple.second)
                                                        (List.head triglyceridesResults |> Maybe.andThen Tuple.second)
                                                        |> -- We should never get here since protein result
                                                           -- existed, and all 4 are entered together.
                                                           Maybe.withDefault False

                                            result =
                                                if resultsNormal then
                                                    translate language Translate.Normal

                                                else
                                                    translate language Translate.Abnormal
                                        in
                                        viewConsolidatedEntry Translate.LipidPanel
                                            (formatDDMMYYYY date)
                                            result
                                            (Just <| setLabResultsModeMsg <| Just <| LabResultsCurrent LabResultsCurrentLipidPanel)
                                            resultsNormal
                                    )
                                |> Maybe.withDefault (emptyConsolidatedEntry Translate.LipidPanel)

                        creatinineTestResults =
                            getTestResults .creatinine (\value -> ( value.creatinineResult, value.bunResult ))

                        creatinineResults =
                            List.map (\( date, ( creatinineResult, _ ) ) -> ( date, creatinineResult )) creatinineTestResults

                        bunResults =
                            List.map (\( date, ( _, bunResult ) ) -> ( date, bunResult )) creatinineTestResults

                        liverFunctionResults =
                            getTestResults .liverFunction (\value -> ( value.altResult, value.astResult ))

                        altResults =
                            List.map (\( date, ( altResult, _ ) ) -> ( date, altResult )) liverFunctionResults

                        astResults =
                            List.map (\( date, ( _, astResult ) ) -> ( date, astResult )) liverFunctionResults

                        bloodGpRsResults =
                            getTestResults .bloodGpRs (\value -> ( value.bloodGroup, value.rhesus ))

                        bloodGroupResults =
                            List.map (\( date, ( bloodGroup, _ ) ) -> ( date, bloodGroup )) bloodGpRsResults

                        rhesusResults =
                            List.map (\( date, ( _, rhesus ) ) -> ( date, rhesus )) bloodGpRsResults

                        hivPCRTestResults =
                            getTestResults .hivPCR
                                (\value ->
                                    Maybe.andThen
                                        (\status ->
                                            case status of
                                                ViralLoadUndetectable ->
                                                    Just ResultSuppressedViralLoad

                                                ViralLoadDetectable ->
                                                    Maybe.map ResultDetectibleViralLoad value.hivViralLoad
                                        )
                                        value.hivViralLoadStatus
                                )

                        bloodSmearTestResults =
                            data.malaria
                                |> List.filterMap
                                    (\value ->
                                        if
                                            (not <| testPerformedByExecutionNote value.executionNote)
                                                && bloodSmearResultNotSet value.bloodSmearResult
                                        then
                                            Maybe.map (\executionDate -> ( executionDate, Just value.bloodSmearResult ))
                                                value.executionDate

                                        else
                                            Nothing
                                    )
                                |> List.sortWith sortTuplesByDateDesc
                    in
                    [ viewLabResultsEntry language currentDate viewForConfirmation setLabResultsModeMsg (LabResultsHistoryHIV hivTestResults)
                    , viewLabResultsEntry language currentDate viewForConfirmation setLabResultsModeMsg (LabResultsHistoryHIVPCR hivPCRTestResults)
                        |> showIf displayConfig.hivPCR
                    , viewLabResultsEntry language currentDate viewForConfirmation setLabResultsModeMsg (LabResultsHistoryPartnerHIV partnerHIVTestResults)
                    , viewLabResultsEntry language currentDate viewForConfirmation setLabResultsModeMsg (LabResultsHistorySyphilis syphilisTestResults)
                        |> showIf displayConfig.syphilis
                    , viewLabResultsEntry language currentDate viewForConfirmation setLabResultsModeMsg (LabResultsHistoryHepatitisB hepatitisBTestResults)
                        |> showIf displayConfig.hepatitisB
                    , viewLabResultsEntry language currentDate viewForConfirmation setLabResultsModeMsg (LabResultsHistoryMalaria malariaTestResults)
                        |> showIf displayConfig.malaria
                    , viewLabResultsEntry language currentDate viewForConfirmation setLabResultsModeMsg (LabResultsHistoryBloodSmear bloodSmearTestResults)
                        |> showIf ((not <| List.isEmpty bloodSmearTestResults) && displayConfig.malaria)
                    ]
                        ++ dipstickEntries
                        ++ [ viewLabResultsEntry language currentDate viewForConfirmation setLabResultsModeMsg (LabResultsHistoryRandomBloodSugar randomBloodSugarResults)
                           , viewLabResultsEntry language currentDate viewForConfirmation setLabResultsModeMsg (LabResultsHistoryHemoglobin hemoglobinResults)
                                |> showIf displayConfig.hemoglobin
                           , viewLabResultsEntry language currentDate viewForConfirmation setLabResultsModeMsg (LabResultsHistoryBloodGroup bloodGroupResults)
                                |> showIf displayConfig.bloodGpRs
                           , viewLabResultsEntry language currentDate viewForConfirmation setLabResultsModeMsg (LabResultsHistoryRhesus rhesusResults)
                                |> showIf displayConfig.bloodGpRs
                           , viewLabResultsEntry language currentDate viewForConfirmation setLabResultsModeMsg (LabResultsHistoryCreatinine creatinineResults)
                                |> showIf displayConfig.creatinine
                           , viewLabResultsEntry language currentDate viewForConfirmation setLabResultsModeMsg (LabResultsHistoryBUN bunResults)
                                |> showIf displayConfig.creatinine
                           , viewLabResultsEntry language currentDate viewForConfirmation setLabResultsModeMsg (LabResultsHistoryALT altResults)
                                |> showIf displayConfig.liverFunction
                           , viewLabResultsEntry language currentDate viewForConfirmation setLabResultsModeMsg (LabResultsHistoryAST astResults)
                                |> showIf displayConfig.liverFunction
                           , viewLabResultsEntry language currentDate viewForConfirmation setLabResultsModeMsg (LabResultsHistoryPregnancy pregnancyTestResults)
                                |> showIf displayConfig.pregnancy
                           , viewLabResultsEntry language currentDate viewForConfirmation setLabResultsModeMsg (LabResultsHistoryHbA1c hba1cResults)
                                |> showIf displayConfig.hba1c
                           , lipidPanelEntry
                                |> showIf displayConfig.lipidPanel
                           ]

                LabResultsCurrentDipstickShort ->
                    [ proteinEntry
                    , phEntry
                    , glucoseEntry
                    ]

                LabResultsCurrentDipstickLong ->
                    [ proteinEntry
                    , phEntry
                    , glucoseEntry
                    , viewLabResultsEntry language currentDate viewForConfirmation setLabResultsModeMsg (LabResultsHistoryLeukocytes leukocytesResults)
                    , viewLabResultsEntry language currentDate viewForConfirmation setLabResultsModeMsg (LabResultsHistoryNitrite nitriteResults)
                    , viewLabResultsEntry language currentDate viewForConfirmation setLabResultsModeMsg (LabResultsHistoryUrobilinogen urobilinogenResults)
                    , viewLabResultsEntry language currentDate viewForConfirmation setLabResultsModeMsg (LabResultsHistoryHaemoglobin haemoglobinResults)
                    , viewLabResultsEntry language currentDate viewForConfirmation setLabResultsModeMsg (LabResultsHistoryKetone ketoneResults)
                    , viewLabResultsEntry language currentDate viewForConfirmation setLabResultsModeMsg (LabResultsHistoryBilirubin bilirubinResults)
                    ]

                LabResultsCurrentLipidPanel ->
                    [ viewLabResultsEntry language currentDate viewForConfirmation setLabResultsModeMsg (LabResultsHistoryTotalCholesterol totalCholesterolResults)
                    , viewLabResultsEntry language currentDate viewForConfirmation setLabResultsModeMsg (LabResultsHistoryLDLCholesterol ldlCholesterolResults)
                    , viewLabResultsEntry language currentDate viewForConfirmation setLabResultsModeMsg (LabResultsHistoryHDLCholesterol hdlCholesterolResults)
                    , viewLabResultsEntry language currentDate viewForConfirmation setLabResultsModeMsg (LabResultsHistoryTriglycerides triglyceridesResults)
                    ]

        proteinEntry =
            viewLabResultsEntry language currentDate viewForConfirmation setLabResultsModeMsg (LabResultsHistoryProtein proteinResults)

        phEntry =
            viewLabResultsEntry language currentDate viewForConfirmation setLabResultsModeMsg (LabResultsHistoryPH phResults)

        glucoseEntry =
            viewLabResultsEntry language currentDate viewForConfirmation setLabResultsModeMsg (LabResultsHistoryGlucose glucoseResults)

        emptyConsolidatedEntry label =
            viewConsolidatedEntry label "--/--/----" "---" Nothing True

        viewConsolidatedEntry label date result maybeAction resultNormal =
            let
                forwardIcon =
                    Maybe.map
                        (\action ->
                            div
                                [ class "icon-forward"
                                , onClick action
                                ]
                                []
                        )
                        maybeAction
                        |> Maybe.withDefault emptyNode
            in
            div [ classList [ ( "entry", True ), ( "warning", not resultNormal ) ] ]
                [ div [ class "name" ] [ translateText language label ]
                , div [ class "date" ] [ text date ]
                , div [ class "result" ] [ text result ]
                , div [ class "normal-range" ] [ translateText language Translate.Normal ]
                , forwardIcon
                ]
    in
    div [ class "lab-results" ]
        [ viewItemHeading language (Translate.LabResultsPaneHeader mode) "blue"
        , div [ class "pane-content" ] [ heading ]
        , div [ class "group-content" ] content
        ]


viewLabResultsHistoryPane : Language -> NominalDate -> LabResultsHistoryMode -> Html any
viewLabResultsHistoryPane language currentDate mode =
    let
        heading =
            div [ class "heading" ]
                [ div [ class "date" ] [ translateText language Translate.TestDate ]
                , div [ class "result" ] [ translateText language Translate.Result ]
                , div [ class "normal-range" ] [ translateText language Translate.NormalRange ]
                ]

        entries =
            case mode of
                LabResultsHistoryHIV assembled ->
                    List.map (viewEntry (translateTestReport language) hivResultNormal) assembled

                LabResultsHistoryHIVPCR assembled ->
                    List.map (viewEntry (Translate.HIVPCRResult >> translate language) hivPCRResultNormal) assembled

                LabResultsHistoryPartnerHIV assembled ->
                    List.map (viewEntry (Translate.TestResult >> translate language) partnerHIVResultNormal) assembled

                LabResultsHistorySyphilis assembled ->
                    List.map (viewEntry (Translate.TestResult >> translate language) syphilisResultNormal) assembled

                LabResultsHistoryHepatitisB assembled ->
                    List.map (viewEntry (translateTestReport language) hepatitisBResultNormal) assembled

                LabResultsHistoryMalaria assembled ->
                    List.map (viewEntry (Translate.TestResult >> translate language) malariaResultNormal) assembled

                LabResultsHistoryBloodSmear assembled ->
                    List.map (viewEntry (Translate.BloodSmearResult >> translate language) bloodSmearResultNormal) assembled

                LabResultsHistoryProtein assembled ->
                    List.map (viewEntry (Translate.LaboratoryProteinValue >> translate language) proteinResultNormal) assembled

                LabResultsHistoryPH assembled ->
                    List.map (viewEntry (Translate.LaboratoryPHValue >> translate language) phResultNormal) assembled

                LabResultsHistoryGlucose assembled ->
                    List.map (viewEntry (Translate.LaboratoryGlucoseValue >> translate language) glucoseResultNormal) assembled

                LabResultsHistoryLeukocytes assembled ->
                    List.map (viewEntry (Translate.LaboratoryLeukocytesValue >> translate language) leukocytesResultNormal) assembled

                LabResultsHistoryNitrite assembled ->
                    List.map (viewEntry (Translate.LaboratoryNitriteValue >> translate language) nitriteResultNormal) assembled

                LabResultsHistoryUrobilinogen assembled ->
                    List.map (viewEntry (Translate.LaboratoryUrobilinogenValue >> translate language) urobilinogenResultNormal) assembled

                LabResultsHistoryHaemoglobin assembled ->
                    List.map (viewEntry (Translate.LaboratoryHaemoglobinValue >> translate language) urineHaemoglobinValueResultNormal) assembled

                LabResultsHistoryKetone assembled ->
                    List.map (viewEntry (Translate.LaboratoryKetoneValue >> translate language) ketoneResultNormal) assembled

                LabResultsHistoryBilirubin assembled ->
                    List.map (viewEntry (Translate.LaboratoryBilirubinValue >> translate language) bilirubinResultNormal) assembled

                LabResultsHistoryRandomBloodSugar assembled ->
                    List.map
                        (viewCustomEntry (getRandomBloodSugarResultValue >> String.fromFloat)
                            randomBloodSugarResultNormal
                            (Maybe.map Translate.RandomBloodSugarResultNormalRange
                                >> Maybe.withDefault Translate.EmptyString
                            )
                        )
                        assembled

                LabResultsHistoryHemoglobin assembled ->
                    List.map (viewEntry String.fromFloat hemoglobinResultNormal) assembled

                LabResultsHistoryBloodGroup assembled ->
                    List.map (viewEntry (Translate.LaboratoryBloodGroup >> translate language) (always True)) assembled

                LabResultsHistoryRhesus assembled ->
                    List.map (viewEntry (Translate.LaboratoryRhesus >> translate language) rhesusResultsNormal) assembled

                LabResultsHistoryCreatinine assembled ->
                    List.map (viewEntry String.fromFloat creatinineResultNormal) assembled

                LabResultsHistoryBUN assembled ->
                    List.map (viewEntry String.fromFloat bunResultNormal) assembled

                LabResultsHistoryALT assembled ->
                    List.map (viewEntry String.fromFloat altResultNormal) assembled

                LabResultsHistoryAST assembled ->
                    List.map (viewEntry String.fromFloat astResultNormal) assembled

                LabResultsHistoryPregnancy assembled ->
                    List.map (viewEntry (translateTestReport language) pregnancyResultNormal) assembled

                LabResultsHistoryHbA1c assembled ->
                    List.map (viewEntry String.fromFloat hba1cResultNormal) assembled

                LabResultsHistoryTotalCholesterol assembled ->
                    List.map (viewEntry String.fromFloat totalCholesterolResultNormal) assembled

                LabResultsHistoryLDLCholesterol assembled ->
                    List.map (viewEntry String.fromFloat ldlCholesterolResultNormal) assembled

                LabResultsHistoryHDLCholesterol assembled ->
                    List.map (viewEntry String.fromFloat hdlCholesterolResultNormal) assembled

                LabResultsHistoryTriglycerides assembled ->
                    List.map (viewEntry String.fromFloat triglyceridesResultNormal) assembled

        viewEntry resultToStringFunc resultNormalFunc ( date, maybeResult ) =
            viewCustomEntry resultToStringFunc
                resultNormalFunc
                (always (Translate.LabResultsNormalRange mode))
                ( date, maybeResult )

        viewCustomEntry resultToStringFunc resultNormalFunc normalRangeFunc ( date, maybeResult ) =
            let
                resultCell =
                    Maybe.map (resultToStringFunc >> text) maybeResult
                        |> Maybe.withDefault (viewUncompetedResult language currentDate (Just date))

                resultNormal =
                    Maybe.map resultNormalFunc maybeResult
                        |> Maybe.withDefault True

                warningIcon =
                    if not resultNormal then
                        img [ class "icon-warning", src "assets/images/exclamation-red.png" ] []

                    else
                        emptyNode

                normalRange =
                    normalRangeFunc maybeResult
            in
            div [ classList [ ( "entry", True ), ( "warning", not resultNormal ) ] ]
                [ div [ class "date" ] [ text <| formatDDMMYYYY date ]
                , div [ class "result" ] [ resultCell ]
                , div [ class "normal-range" ] [ text <| translate language normalRange ]
                , warningIcon
                ]
    in
    div [ class "lab-results-history" ]
        [ viewItemHeading language (Translate.LabResultsHistoryModeLabel mode) "blue"
        , div [ class "pane-content" ] [ heading ]
        , div [ class "group-content" ] entries
        ]


viewAcuteIllnessDiagnosisEntry :
    Language
    -> AcuteIllnessProgressReportInitiator
    -> ModelIndexedDb
    -> (Page -> msg)
    -> ( IndividualEncounterParticipantId, PaneEntryStatus )
    -> Maybe ( NominalDate, Html msg )
viewAcuteIllnessDiagnosisEntry language acuteIllnessProgressReportInitiator db setActivePageMsg ( participantId, status ) =
    let
        encounters =
            getAcuteIllnessEncountersForParticipant db participantId

        maybeLastEncounterId =
            List.head encounters
                |> Maybe.map Tuple.first

        diagnosisData =
            getAcuteIllnessDiagnosisForEncounters encounters
    in
    Maybe.map2
        (\( date, diagnosis ) lastEncounterId ->
            ( date
            , div [ class "entry diagnosis" ]
                [ div [ class "cell assesment" ] [ text <| translate language <| Translate.AcuteIllnessDiagnosis diagnosis ]
                , div [ class <| "cell status " ++ diagnosisEntryStatusToString status ]
                    [ text <| translate language <| Translate.EntryStatusDiagnosis status ]
                , div [ class "cell date" ] [ text <| formatDDMMYYYY date ]
                , div
                    [ class "icon-forward"
                    , onClick <|
                        setActivePageMsg <|
                            UserPage <|
                                AcuteIllnessProgressReportPage
                                    acuteIllnessProgressReportInitiator
                                    lastEncounterId
                    ]
                    []
                ]
            )
        )
        diagnosisData
        maybeLastEncounterId


viewEntries : Language -> List (Html any) -> List (Html any)
viewEntries language entries =
    if List.isEmpty entries then
        [ div [ class "entry no-matches" ] [ text <| translate language Translate.NoMatchesFound ] ]

    else
        entries
