module Pages.Report.View exposing (..)

import AssocList as Dict
import Backend.Entities exposing (..)
import Backend.Measurement.Model exposing (..)
import Backend.Measurement.Utils exposing (getMeasurementValueFunc, labExpirationPeriod)
import Backend.Model exposing (ModelIndexedDb)
import Backend.NCDEncounter.Types exposing (NCDProgressReportInitiator(..))
import Backend.NutritionEncounter.Utils exposing (sortTuplesByDateDesc)
import Backend.Person.Model exposing (Person)
import Date exposing (Interval(..), Unit(..))
import EverySet exposing (EverySet)
import Gizra.Html exposing (emptyNode, showIf)
import Gizra.NominalDate exposing (NominalDate, formatDDMMYYYY)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Maybe.Extra exposing (isJust)
import Measurement.Model exposing (LaboratoryTask(..))
import Pages.Page exposing (Page(..), UserPage(..))
import Pages.Report.Model exposing (..)
import Pages.Report.Model exposing (LabResultsCurrentMode(..), LabResultsHistoryMode(..), LabResultsMode(..), TestReport(..))
import Pages.Report.Utils exposing (..)
import RemoteData exposing (RemoteData(..))
import Translate exposing (Language, TranslationId, translate, translateText)
import Utils.WebData exposing (viewWebData)


viewLabsPane : Language -> NominalDate -> (Maybe LabResultsMode -> msg) -> Html msg
viewLabsPane language currentDate setLabResultsModeMsg =
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


viewLabResultsEntry : Language -> NominalDate -> (Maybe LabResultsMode -> msg) -> LabResultsHistoryMode -> Html msg
viewLabResultsEntry language currentDate setLabResultsModeMsg results =
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

                LabResultsHistoryProtein assembled ->
                    let
                        recentResultValue =
                            List.head assembled |> Maybe.andThen Tuple.second
                    in
                    { label = Translate.PrenatalLaboratoryProteinLabel
                    , recentResult = Maybe.map (Translate.PrenatalLaboratoryProteinValue >> translate language) recentResultValue
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
                    { label = Translate.PrenatalLaboratoryPHLabel
                    , recentResult = Maybe.map (Translate.PrenatalLaboratoryPHValue >> translate language) recentResultValue
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
                    { label = Translate.PrenatalLaboratoryGlucoseLabel
                    , recentResult = Maybe.map (Translate.PrenatalLaboratoryGlucoseValue >> translate language) recentResultValue
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
                    { label = Translate.PrenatalLaboratoryLeukocytesLabel
                    , recentResult = Maybe.map (Translate.PrenatalLaboratoryLeukocytesValue >> translate language) recentResultValue
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
                    { label = Translate.PrenatalLaboratoryNitriteLabel
                    , recentResult = Maybe.map (Translate.PrenatalLaboratoryNitriteValue >> translate language) recentResultValue
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
                    { label = Translate.PrenatalLaboratoryUrobilinogenLabel
                    , recentResult = Maybe.map (Translate.PrenatalLaboratoryUrobilinogenValue >> translate language) recentResultValue
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
                    { label = Translate.PrenatalLaboratoryHaemoglobinLabel
                    , recentResult = Maybe.map (Translate.PrenatalLaboratoryHaemoglobinValue >> translate language) recentResultValue
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
                    { label = Translate.PrenatalLaboratoryKetoneLabel
                    , recentResult = Maybe.map (Translate.PrenatalLaboratoryKetoneValue >> translate language) recentResultValue
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
                    { label = Translate.PrenatalLaboratoryBilirubinLabel
                    , recentResult = Maybe.map (Translate.PrenatalLaboratoryBilirubinValue >> translate language) recentResultValue
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
                    , recentResult = Maybe.map String.fromFloat recentResultValue
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
                    { label = Translate.PrenatalLaboratoryBloodGroupLabel
                    , recentResult = Maybe.map (Translate.PrenatalLaboratoryBloodGroup >> translate language) recentResultValue
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
                    { label = Translate.PrenatalLaboratoryRhesusLabel
                    , recentResult = Maybe.map (Translate.PrenatalLaboratoryRhesus >> translate language) recentResultValue
                    , knownAsPositive = False
                    , recentResultDate = List.head assembled |> Maybe.map Tuple.first
                    , totalResults = List.length assembled
                    , recentResultNormal =
                        Maybe.map rhesusResultsNormal recentResultValue
                            |> Maybe.withDefault True
                    }

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
            if config.totalResults > 1 then
                div
                    [ class "icon-forward"
                    , onClick <| setLabResultsModeMsg <| Just (LabResultsHistory results)
                    ]
                    []

            else
                emptyNode
    in
    div [ classList [ ( "entry", True ), ( "warning", not config.recentResultNormal ) ] ]
        [ div [ class "name" ] [ translateText language config.label ]
        , div [ class "date" ] [ dateCell ]
        , div [ class "result" ] [ resultCell ]
        , div [ class "normal-range" ] [ text <| translate language <| Translate.LabResultsNormalRange results ]
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
    -> LabResultsCurrentMode
    -> (Maybe LabResultsMode -> msg)
    -> LabsResultsDisplayConfig
    -> LabsResultsValues encounterId
    -> Html msg
viewLabResultsPane language currentDate mode setLabResultsModeMsg displayConfig data =
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
                        if List.member value.executionNote [ TestNoteRunToday, TestNoteRunPreviously ] then
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

                        else if List.member value.executionNote [ TestNoteRunToday, TestNoteRunPreviously ] then
                            Maybe.map (\executionDate -> ( executionDate, getResultFunc value |> Maybe.map TestPerformed ))
                                value.executionDate

                        else
                            Nothing
                    )
                |> List.sortWith sortTuplesByDateDesc

        hivTestResults =
            getTestResultsKnownAsPositive .hiv .testResult

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

        syphilisTestResults =
            getTestResults .syphilis .testResult

        hepatitisBTestResults =
            getTestResultsKnownAsPositive .hepatitisB .testResult

        malariaTestResults =
            getTestResults .malaria .testResult

        urineDipstickTestResults =
            List.filterMap
                (\value ->
                    if List.member value.executionNote [ TestNoteRunToday, TestNoteRunPreviously ] then
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
                    if value.testVariant == Just VariantLongTest && List.member value.executionNote [ TestNoteRunToday, TestNoteRunPreviously ] then
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

        randomBloodSugarResults =
            getTestResults .randomBloodSugar .sugarCount

        hemoglobinResults =
            getTestResults .hemoglobin .hemoglobinCount

        bloodGpRsResults =
            getTestResults .bloodGpRs (\value -> ( value.bloodGroup, value.rhesus ))

        bloodGroupResults =
            List.map (\( date, ( bloodGroup, _ ) ) -> ( date, bloodGroup )) bloodGpRsResults

        rhesusResults =
            List.map (\( date, ( _, rhesus ) ) -> ( date, rhesus )) bloodGpRsResults

        content =
            case mode of
                LabResultsCurrentMain ->
                    [ viewLabResultsEntry language currentDate setLabResultsModeMsg (LabResultsHistoryHIV hivTestResults)
                    , viewLabResultsEntry language currentDate setLabResultsModeMsg (LabResultsHistoryHIVPCR hivPCRTestResults)
                        |> showIf displayConfig.hivPCR
                    , viewLabResultsEntry language currentDate setLabResultsModeMsg (LabResultsHistorySyphilis syphilisTestResults)
                        |> showIf displayConfig.syphilis
                    , viewLabResultsEntry language currentDate setLabResultsModeMsg (LabResultsHistoryHepatitisB hepatitisBTestResults)
                        |> showIf displayConfig.hepatitisB
                    , viewLabResultsEntry language currentDate setLabResultsModeMsg (LabResultsHistoryMalaria malariaTestResults)
                        |> showIf displayConfig.malaria
                    , dipstickShortEntry
                    , dipstickLongEntry
                    , viewLabResultsEntry language currentDate setLabResultsModeMsg (LabResultsHistoryRandomBloodSugar randomBloodSugarResults)
                    , viewLabResultsEntry language currentDate setLabResultsModeMsg (LabResultsHistoryHemoglobin hemoglobinResults)
                        |> showIf displayConfig.hemoglobin
                    , viewLabResultsEntry language currentDate setLabResultsModeMsg (LabResultsHistoryBloodGroup bloodGroupResults)
                        |> showIf displayConfig.bloodGpRs
                    , viewLabResultsEntry language currentDate setLabResultsModeMsg (LabResultsHistoryRhesus rhesusResults)
                        |> showIf displayConfig.bloodGpRs
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
                    , viewLabResultsEntry language currentDate setLabResultsModeMsg (LabResultsHistoryLeukocytes leukocytesResults)
                    , viewLabResultsEntry language currentDate setLabResultsModeMsg (LabResultsHistoryNitrite nitriteResults)
                    , viewLabResultsEntry language currentDate setLabResultsModeMsg (LabResultsHistoryUrobilinogen urobilinogenResults)
                    , viewLabResultsEntry language currentDate setLabResultsModeMsg (LabResultsHistoryHaemoglobin haemoglobinResults)
                    , viewLabResultsEntry language currentDate setLabResultsModeMsg (LabResultsHistoryKetone ketoneResults)
                    , viewLabResultsEntry language currentDate setLabResultsModeMsg (LabResultsHistoryBilirubin bilirubinResults)
                    ]

        proteinEntry =
            viewLabResultsEntry language currentDate setLabResultsModeMsg (LabResultsHistoryProtein proteinResults)

        phEntry =
            viewLabResultsEntry language currentDate setLabResultsModeMsg (LabResultsHistoryPH phResults)

        glucoseEntry =
            viewLabResultsEntry language currentDate setLabResultsModeMsg (LabResultsHistoryGlucose glucoseResults)

        dipstickShortEntry =
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
                        viewDipstickEntry (Translate.PrenatalUrineDipstickTestLabel VariantShortTest)
                            (formatDDMMYYYY date)
                            result
                            (Just <| setLabResultsModeMsg <| Just <| LabResultsCurrent LabResultsCurrentDipstickShort)
                            resultsNormal
                    )
                |> Maybe.withDefault (emptyDipstickEntry (Translate.PrenatalUrineDipstickTestLabel VariantShortTest))

        dipstickLongEntry =
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
                        viewDipstickEntry (Translate.PrenatalUrineDipstickTestLabel VariantLongTest)
                            (formatDDMMYYYY date)
                            result
                            (Just <| setLabResultsModeMsg <| Just <| LabResultsCurrent LabResultsCurrentDipstickLong)
                            resultsNormal
                    )
                |> Maybe.withDefault (emptyDipstickEntry (Translate.PrenatalUrineDipstickTestLabel VariantLongTest))

        emptyDipstickEntry label =
            viewDipstickEntry label "--/--/----" "---" Nothing True

        viewDipstickEntry label date result maybeAction resultNormal =
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

                LabResultsHistorySyphilis assembled ->
                    List.map (viewEntry (Translate.TestResult >> translate language) syphilisResultNormal) assembled

                LabResultsHistoryHepatitisB assembled ->
                    List.map (viewEntry (translateTestReport language) hepatitisBResultNormal) assembled

                LabResultsHistoryMalaria assembled ->
                    List.map (viewEntry (Translate.TestResult >> translate language) malariaResultNormal) assembled

                LabResultsHistoryProtein assembled ->
                    List.map (viewEntry (Translate.PrenatalLaboratoryProteinValue >> translate language) proteinResultNormal) assembled

                LabResultsHistoryPH assembled ->
                    List.map (viewEntry (Translate.PrenatalLaboratoryPHValue >> translate language) phResultNormal) assembled

                LabResultsHistoryGlucose assembled ->
                    List.map (viewEntry (Translate.PrenatalLaboratoryGlucoseValue >> translate language) glucoseResultNormal) assembled

                LabResultsHistoryLeukocytes assembled ->
                    List.map (viewEntry (Translate.PrenatalLaboratoryLeukocytesValue >> translate language) leukocytesResultNormal) assembled

                LabResultsHistoryNitrite assembled ->
                    List.map (viewEntry (Translate.PrenatalLaboratoryNitriteValue >> translate language) nitriteResultNormal) assembled

                LabResultsHistoryUrobilinogen assembled ->
                    List.map (viewEntry (Translate.PrenatalLaboratoryUrobilinogenValue >> translate language) urobilinogenResultNormal) assembled

                LabResultsHistoryHaemoglobin assembled ->
                    List.map (viewEntry (Translate.PrenatalLaboratoryHaemoglobinValue >> translate language) urineHaemoglobinValueResultNormal) assembled

                LabResultsHistoryKetone assembled ->
                    List.map (viewEntry (Translate.PrenatalLaboratoryKetoneValue >> translate language) ketoneResultNormal) assembled

                LabResultsHistoryBilirubin assembled ->
                    List.map (viewEntry (Translate.PrenatalLaboratoryBilirubinValue >> translate language) bilirubinResultNormal) assembled

                LabResultsHistoryRandomBloodSugar assembled ->
                    List.map (viewEntry String.fromFloat randomBloodSugarResultNormal) assembled

                LabResultsHistoryHemoglobin assembled ->
                    List.map (viewEntry String.fromFloat hemoglobinResultNormal) assembled

                LabResultsHistoryBloodGroup assembled ->
                    List.map (viewEntry (Translate.PrenatalLaboratoryBloodGroup >> translate language) (always True)) assembled

                LabResultsHistoryRhesus assembled ->
                    List.map (viewEntry (Translate.PrenatalLaboratoryRhesus >> translate language) rhesusResultsNormal) assembled

        viewEntry resultToStringFunc resultNormalFunc ( date, maybeResult ) =
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
            in
            div [ classList [ ( "entry", True ), ( "warning", not resultNormal ) ] ]
                [ div [ class "date" ] [ text <| formatDDMMYYYY date ]
                , div [ class "result" ] [ resultCell ]
                , div [ class "normal-range" ] [ text <| translate language <| Translate.LabResultsNormalRange mode ]
                , warningIcon
                ]
    in
    div [ class "lab-results-history" ]
        [ viewItemHeading language (Translate.LabResultsHistoryModeLabel mode) "blue"
        , div [ class "pane-content" ] [ heading ]
        , div [ class "group-content" ] entries
        ]
