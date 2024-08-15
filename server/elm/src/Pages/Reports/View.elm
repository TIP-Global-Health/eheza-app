module Pages.Reports.View exposing (view)

import App.Types exposing (Language, Site)
import AssocList as Dict exposing (Dict)
import Backend.Model exposing (ModelBackend)
import Backend.Reports.Model
    exposing
        ( AcuteIllnessEncounterType(..)
        , BackendGeneratedNutritionReportTableDate
        , Gender(..)
        , NutritionReportTableType(..)
        , PatientData
        , PrenatalEncounterType(..)
        , PrenatalParticipantData
        , ReportsData
        , SelectedEntity(..)
        )
import Backend.Reports.Utils exposing (allAcuteIllnessDiagnoses)
import Date exposing (Interval(..), Unit(..))
import DateSelector.SelectorPopup exposing (viewCalendarPopup)
import Gizra.Html exposing (emptyNode)
import Gizra.NominalDate exposing (NominalDate, customFormatDDMMYYYY, formatDDMMYYYY, sortByDateDesc)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick)
import List.Extra
import Maybe.Extra exposing (isJust, isNothing)
import Pages.Model exposing (MetricsResultsTableData)
import Pages.Reports.Model exposing (..)
import Pages.Reports.Utils exposing (..)
import Pages.Utils
    exposing
        ( generateReportsHeaderImage
        , viewCustomCells
        , viewCustomLabel
        , viewSelectListInput
        , viewStandardCells
        , viewStandardRow
        , wrapSelectListInput
        )
import RemoteData exposing (RemoteData(..))
import Round
import Time exposing (Month(..))
import Translate exposing (TranslationId, translate)
import Utils.Html exposing (viewModal)


view : Language -> NominalDate -> String -> ModelBackend -> Model -> Html Msg
view language currentDate themePath modelBackend model =
    case modelBackend.reportsData of
        Just (Ok data) ->
            viewReportsData language currentDate themePath data model

        Just (Err err) ->
            text <| Debug.toString err

        Nothing ->
            emptyNode


viewReportsData : Language -> NominalDate -> String -> ReportsData -> Model -> Html Msg
viewReportsData language currentDate themePath data model =
    let
        topBar =
            div [ class "top-bar" ]
                [ div [ class "new-selection" ]
                    [ a [ href "/admin/reports/statistical-queries" ]
                        [ button []
                            [ text <| translate language Translate.NewScope ]
                        ]
                    ]
                , div [ class "scope" ]
                    [ text <| translate language Translate.Scope ++ ": " ++ scopeLabel ]
                ]

        scopeLabel =
            case data.entityType of
                EntityGlobal ->
                    translate language Translate.Global

                EntityHealthCenter ->
                    data.entityName

                _ ->
                    data.entityName ++ " " ++ (String.toLower <| translate language (Translate.SelectedScope data.entityType))

        -- The date system became live, and first content was uploaded.
        launchDate =
            Date.fromCalendarDate 2018 Jan 1

        dateInputs =
            Maybe.map
                (\reportType ->
                    if reportType == ReportNutrition then
                        []

                    else
                        let
                            startDateInput =
                                let
                                    dateSelectorConfig =
                                        { select = SetStartDate
                                        , close = SetStartDateSelectorState Nothing
                                        , dateFrom = launchDate
                                        , dateTo = currentDate
                                        , dateDefault = Just launchDate
                                        }

                                    dateForView =
                                        Maybe.map formatDDMMYYYY model.startDate
                                            |> Maybe.withDefault ""
                                in
                                div
                                    [ class "form-input date"
                                    , onClick <| SetStartDateSelectorState (Just dateSelectorConfig)
                                    ]
                                    [ text dateForView ]
                                    |> wrapSelectListInput language Translate.SelectStartDate False

                            limitDateInput =
                                if
                                    -- Reports requires setting start date before
                                    -- limit date can be shown.
                                    isNothing model.startDate
                                then
                                    emptyNode

                                else
                                    let
                                        dateFrom =
                                            Maybe.withDefault (Date.add Years -6 currentDate) model.startDate

                                        dateSelectorConfig =
                                            { select = SetLimitDate
                                            , close = SetLimitDateSelectorState Nothing
                                            , dateFrom = dateFrom
                                            , dateTo = currentDate
                                            , dateDefault = Just currentDate
                                            }

                                        limitDateForView =
                                            Maybe.map formatDDMMYYYY model.limitDate
                                                |> Maybe.withDefault ""
                                    in
                                    div
                                        [ class "form-input date"
                                        , onClick <| SetLimitDateSelectorState (Just dateSelectorConfig)
                                        ]
                                        [ text limitDateForView ]
                                        |> wrapSelectListInput language Translate.SelectLimitDate False
                        in
                        [ startDateInput, limitDateInput ]
                )
                model.reportType
                |> Maybe.withDefault []

        ( startDateByReportType, limitDateByReportType ) =
            if model.reportType == Just ReportNutrition then
                -- Nutrition report does not allow selecting limit date, so
                -- we force it to be today.
                ( Just launchDate, Just currentDate )

            else
                ( model.startDate, model.limitDate )

        content =
            if
                isJust model.startDateSelectorPopupState
                    || isJust model.limitDateSelectorPopupState
            then
                -- Date selector is open, so no need to calcualte
                -- intermediate results.
                emptyNode

            else
                Maybe.map3
                    (\reportType startDate limitDate ->
                        let
                            recordsTillLimitDate =
                                if
                                    (Date.compare startDate launchDate == EQ)
                                        && (Date.compare limitDate currentDate == EQ)
                                then
                                    data.records

                                else
                                    List.filterMap
                                        (\record ->
                                            if
                                                -- Patient was created not before the FROM date and
                                                -- not after the TO date.
                                                (not <| Date.compare record.created startDate == LT)
                                                    && (not <| Date.compare record.created limitDate == GT)
                                            then
                                                let
                                                    filterPrenatalData =
                                                        Maybe.map
                                                            (List.filterMap
                                                                (\participantData ->
                                                                    -- Pregnancy was created not after the TO date.
                                                                    if Date.compare participantData.created limitDate == GT then
                                                                        Nothing

                                                                    else
                                                                        let
                                                                            dateConcluded =
                                                                                -- If pregnancy was concluded, but conclusion date is
                                                                                -- after TO date, we mark pregnancy as not concluded.
                                                                                Maybe.andThen
                                                                                    (\date ->
                                                                                        if Date.compare limitDate date == LT then
                                                                                            Nothing

                                                                                        else
                                                                                            Just date
                                                                                    )
                                                                                    participantData.dateConcluded

                                                                            filteredEncounters =
                                                                                List.filter
                                                                                    (\encounterData ->
                                                                                        -- Encounter was created not before the FROM date and
                                                                                        -- not after the TO date.
                                                                                        (not <| Date.compare encounterData.startDate startDate == LT)
                                                                                            && (not <| Date.compare encounterData.startDate limitDate == GT)
                                                                                    )
                                                                                    participantData.encounters
                                                                        in
                                                                        if List.isEmpty filteredEncounters then
                                                                            Nothing

                                                                        else
                                                                            Just { participantData | dateConcluded = dateConcluded, encounters = filteredEncounters }
                                                                )
                                                            )

                                                    filterIndividualBy resolveDateFunc =
                                                        Maybe.map
                                                            (List.map
                                                                (List.filter
                                                                    (\encounterData ->
                                                                        let
                                                                            encounterDate =
                                                                                resolveDateFunc encounterData
                                                                        in
                                                                        (not <| Date.compare encounterDate startDate == LT)
                                                                            && (not <| Date.compare encounterDate limitDate == GT)
                                                                    )
                                                                )
                                                            )

                                                    filterGroupBy resolveDateFunc =
                                                        Maybe.map
                                                            (List.filter
                                                                (\encounterData ->
                                                                    let
                                                                        encounterDate =
                                                                            resolveDateFunc encounterData
                                                                    in
                                                                    (not <| Date.compare encounterDate startDate == LT)
                                                                        && (not <| Date.compare encounterDate limitDate == GT)
                                                                )
                                                            )
                                                in
                                                Just
                                                    { record
                                                        | acuteIllnessData = filterIndividualBy .startDate record.acuteIllnessData
                                                        , prenatalData = filterPrenatalData record.prenatalData
                                                        , homeVisitData = filterIndividualBy identity record.homeVisitData
                                                        , wellChildData = filterIndividualBy .startDate record.wellChildData
                                                        , childScorecardData = filterIndividualBy identity record.childScorecardData
                                                        , ncdData = filterIndividualBy identity record.ncdData
                                                        , hivData = filterIndividualBy identity record.hivData
                                                        , tuberculosisData = filterIndividualBy identity record.tuberculosisData
                                                        , individualNutritionData = filterIndividualBy .startDate record.individualNutritionData
                                                        , groupNutritionPmtctData = filterGroupBy .startDate record.groupNutritionPmtctData
                                                        , groupNutritionFbfData = filterGroupBy .startDate record.groupNutritionFbfData
                                                        , groupNutritionSorwatheData = filterGroupBy .startDate record.groupNutritionSorwatheData
                                                        , groupNutritionChwData = filterGroupBy .startDate record.groupNutritionChwData
                                                        , groupNutritionAchiData = filterGroupBy .startDate record.groupNutritionAchiData
                                                    }

                                            else
                                                Nothing
                                        )
                                        data.records
                        in
                        case reportType of
                            ReportAcuteIllness ->
                                viewAcuteIllnessReport language limitDate startDate scopeLabel recordsTillLimitDate

                            ReportDemographics ->
                                viewDemographicsReport language limitDate scopeLabel recordsTillLimitDate

                            ReportNutrition ->
                                viewNutritionReport language limitDate scopeLabel data.nutritionReportData model.nutritionReportData

                            ReportPrenatal ->
                                viewPrenatalReport language limitDate scopeLabel recordsTillLimitDate
                    )
                    model.reportType
                    startDateByReportType
                    limitDateByReportType
                    |> Maybe.withDefault
                        (if isWideScope data.entityType then
                            viewCustomLabel language Translate.WideScopeNote "" "label wide-scope"

                         else
                            emptyNode
                        )
    in
    div [ class "page-content reports" ]
        [ generateReportsHeaderImage themePath
        , topBar
        , div [ class "inputs" ] <|
            [ viewSelectListInput language
                model.reportType
                [ ReportAcuteIllness
                , ReportPrenatal
                , ReportDemographics
                , ReportNutrition
                ]
                reportTypeToString
                SetReportType
                Translate.ReportType
                "select-input"
                |> wrapSelectListInput language Translate.ReportTypeLabel False
            ]
                ++ dateInputs
                ++ [ content ]
        , viewModal <| viewCalendarPopup language model.startDateSelectorPopupState model.startDate
        , viewModal <| viewCalendarPopup language model.limitDateSelectorPopupState model.limitDate
        ]


viewDemographicsReport : Language -> NominalDate -> String -> List PatientData -> Html Msg
viewDemographicsReport language limitDate scopeLabel records =
    let
        demographicsReportPatientsData =
            generateDemographicsReportPatientsData language limitDate records

        demographicsReportEncountersData =
            generateDemographicsReportEncountersData language records

        csvFileName =
            "demographics-report-"
                ++ (String.toLower <| String.replace " " "-" scopeLabel)
                ++ "-"
                ++ customFormatDDMMYYYY "-" limitDate
                ++ ".csv"

        csvContent =
            demographicsReportPatientsDataToCSV demographicsReportPatientsData
                ++ "\n\n\n"
                ++ demographicsReportEncountersDataToCSV demographicsReportEncountersData
    in
    div [ class "report demographics" ] <|
        viewDemographicsReportPatients language limitDate demographicsReportPatientsData
            ++ viewDemographicsReportEncounters language demographicsReportEncountersData
            ++ [ viewDownloadCSVButton language csvFileName csvContent ]


generateDemographicsReportPatientsData :
    Language
    -> NominalDate
    -> List PatientData
    ->
        { heading : String
        , tables :
            List
                { captions : List String
                , name : String
                , rows : List (List String)
                , totals : ( String, String )
                }
        }
generateDemographicsReportPatientsData language limitDate records =
    let
        ( males, females ) =
            List.partition (.gender >> (==) Male) records

        males1MonthAndLess =
            List.filter (\patient -> Date.diff Months patient.birthDate limitDate == 0)
                males

        females1MonthAndLess =
            List.filter (\patient -> Date.diff Months patient.birthDate limitDate == 0)
                females

        males1Month2Years =
            List.filter
                (\patient ->
                    (Date.diff Months patient.birthDate limitDate > 0)
                        && (Date.diff Years patient.birthDate limitDate < 2)
                )
                males

        females1Month2Years =
            List.filter
                (\patient ->
                    (Date.diff Months patient.birthDate limitDate > 0)
                        && (Date.diff Years patient.birthDate limitDate < 2)
                )
                females

        males2Years5Years =
            List.filter
                (\patient ->
                    (Date.diff Years patient.birthDate limitDate >= 2)
                        && (Date.diff Years patient.birthDate limitDate < 5)
                )
                males

        females2Years5Years =
            List.filter
                (\patient ->
                    (Date.diff Years patient.birthDate limitDate >= 2)
                        && (Date.diff Years patient.birthDate limitDate < 5)
                )
                females

        males5Years10Years =
            List.filter
                (\patient ->
                    (Date.diff Years patient.birthDate limitDate >= 5)
                        && (Date.diff Years patient.birthDate limitDate < 10)
                )
                males

        females5Years10Years =
            List.filter
                (\patient ->
                    (Date.diff Years patient.birthDate limitDate >= 5)
                        && (Date.diff Years patient.birthDate limitDate < 10)
                )
                females

        males10Years20Years =
            List.filter
                (\patient ->
                    (Date.diff Years patient.birthDate limitDate >= 10)
                        && (Date.diff Years patient.birthDate limitDate < 20)
                )
                males

        females10Years20Years =
            List.filter
                (\patient ->
                    (Date.diff Years patient.birthDate limitDate >= 10)
                        && (Date.diff Years patient.birthDate limitDate < 20)
                )
                females

        males20Years50Years =
            List.filter
                (\patient ->
                    (Date.diff Years patient.birthDate limitDate >= 20)
                        && (Date.diff Years patient.birthDate limitDate < 50)
                )
                males

        females20Years50Years =
            List.filter
                (\patient ->
                    (Date.diff Years patient.birthDate limitDate >= 20)
                        && (Date.diff Years patient.birthDate limitDate < 50)
                )
                females

        males50YearsOrMore =
            List.filter
                (\patient -> Date.diff Years patient.birthDate limitDate >= 50)
                males

        females50YearsOrMore =
            List.filter
                (\patient -> Date.diff Years patient.birthDate limitDate >= 50)
                females

        malesImpacted1MonthAndLess =
            filterImpacted males1MonthAndLess

        femalesImpacted1MonthAndLess =
            filterImpacted females1MonthAndLess

        malesImpacted1Month2Years =
            filterImpacted males1Month2Years

        femalesImpacted1Month2Years =
            filterImpacted females1Month2Years

        malesImpacted2Years5Years =
            filterImpacted males2Years5Years

        femalesImpacted2Years5Years =
            filterImpacted females2Years5Years

        malesImpacted5Years10Years =
            filterImpacted males5Years10Years

        femalesImpacted5Years10Years =
            filterImpacted females5Years10Years

        malesImpacted10Years20Years =
            filterImpacted males10Years20Years

        femalesImpacted10Years20Years =
            filterImpacted females10Years20Years

        malesImpacted20Years50Years =
            filterImpacted males20Years50Years

        femalesImpacted20Years50Years =
            filterImpacted females20Years50Years

        malesImpacted50YearsOrMore =
            filterImpacted males50YearsOrMore

        femalesImpacted50YearsOrMore =
            filterImpacted females50YearsOrMore

        filterImpacted =
            List.filter (\patient -> countTotalEncounters patient > 1)

        patientsImpacted =
            malesImpacted1MonthAndLess
                ++ femalesImpacted1MonthAndLess
                ++ malesImpacted1Month2Years
                ++ femalesImpacted1Month2Years
                ++ malesImpacted2Years5Years
                ++ femalesImpacted2Years5Years
                ++ malesImpacted5Years10Years
                ++ femalesImpacted5Years10Years
                ++ malesImpacted10Years20Years
                ++ femalesImpacted10Years20Years
                ++ malesImpacted20Years50Years
                ++ femalesImpacted20Years50Years
                ++ malesImpacted50YearsOrMore
                ++ femalesImpacted50YearsOrMore

        labels =
            [ "0 - 1M"
            , "1M - 2Y"
            , "2Y - 5Y"
            , "5Y - 10Y"
            , "10Y - 20Y"
            , "20Y - 50Y"
            , "50Y +"
            ]

        generateRow label ( valueMales, valueFemales ) =
            [ label, String.fromInt <| List.length valueMales, String.fromInt <| List.length valueFemales ]
    in
    { heading = translate language Translate.RegisteredPatients ++ ":"
    , tables =
        [ { name = "registered"
          , captions = List.map (translate language) [ Translate.Registered, Translate.Male, Translate.Female ]
          , rows =
                List.map2 generateRow
                    labels
                    [ ( males1MonthAndLess, females1MonthAndLess )
                    , ( males1Month2Years, females1Month2Years )
                    , ( males2Years5Years, females2Years5Years )
                    , ( males5Years10Years, females5Years10Years )
                    , ( males10Years20Years, females10Years20Years )
                    , ( males20Years50Years, females20Years50Years )
                    , ( males50YearsOrMore, females50YearsOrMore )
                    ]
          , totals = ( translate language Translate.Total, String.fromInt <| List.length <| males ++ females )
          }
        , { name = "impacted"
          , captions = List.map (translate language) [ Translate.Impacted, Translate.Male, Translate.Female ]
          , rows =
                List.map2 generateRow
                    labels
                    [ ( malesImpacted1MonthAndLess, femalesImpacted1MonthAndLess )
                    , ( malesImpacted1Month2Years, femalesImpacted1Month2Years )
                    , ( malesImpacted2Years5Years, femalesImpacted2Years5Years )
                    , ( malesImpacted5Years10Years, femalesImpacted5Years10Years )
                    , ( malesImpacted10Years20Years, femalesImpacted10Years20Years )
                    , ( malesImpacted20Years50Years, femalesImpacted20Years50Years )
                    , ( malesImpacted50YearsOrMore, femalesImpacted50YearsOrMore )
                    ]
          , totals = ( translate language Translate.Total, String.fromInt <| List.length patientsImpacted )
          }
        ]
    }


viewDemographicsReportPatients :
    Language
    -> NominalDate
    ->
        { heading : String
        , tables :
            List
                { captions : List String
                , name : String
                , rows : List (List String)
                , totals : ( String, String )
                }
        }
    -> List (Html Msg)
viewDemographicsReportPatients language limitDate data =
    let
        viewTable tableData =
            div [ class <| "table " ++ tableData.name ] <|
                [ div [ class "row captions" ] <|
                    viewStandardCells tableData.captions
                ]
                    ++ List.map viewStandardRow tableData.rows
    in
    div [ class "section heading" ] [ text data.heading ]
        :: List.map viewTable data.tables


demographicsReportPatientsDataToCSV :
    { heading : String
    , tables :
        List
            { captions : List String
            , name : String
            , rows : List (List String)
            , totals : ( String, String )
            }
    }
    -> String
demographicsReportPatientsDataToCSV data =
    let
        tableDataToCSV tableData =
            [ String.join "," tableData.captions
            , List.map (String.join ",")
                tableData.rows
                |> String.join "\n"
            , Tuple.first tableData.totals ++ "," ++ Tuple.second tableData.totals
            ]
                |> String.join "\n"
    in
    [ data.heading ++ "\n"
    , List.map tableDataToCSV data.tables
        |> String.join "\n\n"
    ]
        |> String.join "\n"


generateDemographicsReportEncountersData :
    Language
    -> List PatientData
    ->
        { heading : String
        , captions : List String
        , rows : List ( List String, Bool )
        , totals : { label : String, total : String, unique : String }
        }
generateDemographicsReportEncountersData language records =
    let
        prenatalDataNurseEncounters =
            List.filterMap
                (.prenatalData
                    >> Maybe.map
                        (List.map .encounters
                            >> List.concat
                            >> List.filter
                                (\encounter ->
                                    List.member encounter.encounterType [ NurseEncounter, NursePostpartumEncounter ]
                                )
                        )
                )
                records

        prenatalDataChwEncounters =
            List.filterMap
                (.prenatalData
                    >> Maybe.map
                        (List.map .encounters
                            >> List.concat
                            >> List.filter
                                (\encounter ->
                                    not <| List.member encounter.encounterType [ NurseEncounter, NursePostpartumEncounter ]
                                )
                        )
                )
                records

        prenatalDataNurseEncountersTotal =
            countTotal prenatalDataNurseEncounters

        prenatalDataNurseEncountersUnique =
            countUnique prenatalDataNurseEncounters

        prenatalDataChwEncountersTotal =
            countTotal prenatalDataChwEncounters

        prenatalDataChwEncountersUnique =
            countUnique prenatalDataChwEncounters

        acuteIllnessDataNurseEncounters =
            List.filterMap
                (.acuteIllnessData
                    >> Maybe.map
                        (List.concat
                            >> List.filter
                                (\encounter ->
                                    List.member encounter.encounterType [ AcuteIllnessEncounterNurse, AcuteIllnessEncounterNurseSubsequent ]
                                )
                        )
                )
                records

        acuteIllnessDataChwEncounters =
            List.filterMap
                (.acuteIllnessData
                    >> Maybe.map
                        (List.concat
                            >> List.filter
                                (\encounter ->
                                    not <| List.member encounter.encounterType [ AcuteIllnessEncounterNurse, AcuteIllnessEncounterNurseSubsequent ]
                                )
                        )
                )
                records

        acuteIllnessDataNurseEncountersTotal =
            countTotal acuteIllnessDataNurseEncounters

        acuteIllnessDataNurseEncountersUnique =
            countUnique acuteIllnessDataNurseEncounters

        acuteIllnessDataChwEncountersTotal =
            countTotal acuteIllnessDataChwEncounters

        acuteIllnessDataChwEncountersUnique =
            countUnique acuteIllnessDataChwEncounters

        wellChildEncountersData =
            List.filterMap
                (.wellChildData >> Maybe.map List.concat)
                records

        wellChildDataEncountersTotal =
            countTotal wellChildEncountersData

        wellChildDataEncountersUnique =
            countUnique wellChildEncountersData

        homeVisitEncountersData =
            List.filterMap
                (.homeVisitData >> Maybe.map List.concat)
                records

        homeVisitDataEncountersTotal =
            countTotal homeVisitEncountersData

        homeVisitDataEncountersUnique =
            countUnique homeVisitEncountersData

        childScorecardEncountersData =
            List.filterMap
                (.childScorecardData >> Maybe.map List.concat)
                records

        childScorecardDataEncountersTotal =
            countTotal childScorecardEncountersData

        childScorecardDataEncountersUnique =
            countUnique childScorecardEncountersData

        ncdEncountersData =
            List.filterMap
                (.ncdData >> Maybe.map List.concat)
                records

        ncdDataEncountersTotal =
            countTotal ncdEncountersData

        ncdDataEncountersUnique =
            countUnique ncdEncountersData

        hivEncountersData =
            List.filterMap
                (.hivData >> Maybe.map List.concat)
                records

        hivDataEncountersTotal =
            countTotal hivEncountersData

        hivDataEncountersUnique =
            countUnique hivEncountersData

        tuberculosisEncountersData =
            List.filterMap
                (.tuberculosisData >> Maybe.map List.concat)
                records

        tuberculosisDataEncountersTotal =
            countTotal tuberculosisEncountersData

        tuberculosisDataEncountersUnique =
            countUnique tuberculosisEncountersData

        nutritionIndividualEncountersData =
            List.filterMap
                (.individualNutritionData >> Maybe.map List.concat)
                records

        nutritionIndividualEncountersTotal =
            countTotal nutritionIndividualEncountersData

        nutritionIndividualEncountersUnique =
            countUnique nutritionIndividualEncountersData

        nutritionGroupPmtctEncountersData =
            List.filterMap
                (.groupNutritionPmtctData >> Maybe.map identity)
                records

        nutritionGroupPmtctEncountersTotal =
            countTotal nutritionGroupPmtctEncountersData

        nutritionGroupPmtctEncountersUnique =
            countUnique nutritionGroupPmtctEncountersData

        nutritionGroupFbfEncountersData =
            List.filterMap
                (.groupNutritionFbfData >> Maybe.map identity)
                records

        nutritionGroupFbfEncountersTotal =
            countTotal nutritionGroupFbfEncountersData

        nutritionGroupFbfEncountersUnique =
            countUnique nutritionGroupFbfEncountersData

        nutritionGroupSorwatheEncountersData =
            List.filterMap
                (.groupNutritionSorwatheData >> Maybe.map identity)
                records

        nutritionGroupSorwatheEncountersTotal =
            countTotal nutritionGroupSorwatheEncountersData

        nutritionGroupSorwatheEncountersUnique =
            countUnique nutritionGroupSorwatheEncountersData

        nutritionGroupChwEncountersData =
            List.filterMap
                (.groupNutritionChwData >> Maybe.map identity)
                records

        nutritionGroupChwEncountersTotal =
            countTotal nutritionGroupChwEncountersData

        nutritionGroupChwEncountersUnique =
            countUnique nutritionGroupChwEncountersData

        nutritionGroupAchiEncountersData =
            List.filterMap
                (.groupNutritionAchiData >> Maybe.map identity)
                records

        nutritionGroupAchiEncountersTotal =
            countTotal nutritionGroupAchiEncountersData

        nutritionGroupAchiEncountersUnique =
            countUnique nutritionGroupAchiEncountersData

        overallNutritionTotal =
            nutritionIndividualEncountersTotal
                + nutritionGroupPmtctEncountersTotal
                + nutritionGroupFbfEncountersTotal
                + nutritionGroupSorwatheEncountersTotal
                + nutritionGroupChwEncountersTotal
                + nutritionGroupAchiEncountersTotal

        overallNutritionUnique =
            nutritionIndividualEncountersUnique
                + nutritionGroupPmtctEncountersUnique
                + nutritionGroupFbfEncountersUnique
                + nutritionGroupSorwatheEncountersUnique
                + nutritionGroupChwEncountersUnique
                + nutritionGroupAchiEncountersUnique

        overallTotal =
            prenatalDataNurseEncountersTotal
                + prenatalDataChwEncountersTotal
                + acuteIllnessDataNurseEncountersTotal
                + acuteIllnessDataChwEncountersTotal
                + wellChildDataEncountersTotal
                + homeVisitDataEncountersTotal
                + childScorecardDataEncountersTotal
                + ncdDataEncountersTotal
                + hivDataEncountersTotal
                + tuberculosisDataEncountersTotal
                + overallNutritionTotal

        overallUnique =
            prenatalDataNurseEncountersUnique
                + prenatalDataChwEncountersUnique
                + acuteIllnessDataNurseEncountersUnique
                + acuteIllnessDataChwEncountersUnique
                + wellChildDataEncountersUnique
                + homeVisitDataEncountersUnique
                + childScorecardDataEncountersUnique
                + ncdDataEncountersUnique
                + hivDataEncountersUnique
                + tuberculosisDataEncountersUnique
                + overallNutritionUnique

        countTotal =
            List.map List.length >> List.sum

        countUnique =
            List.filter (not << List.isEmpty) >> List.length

        generateRow labelTransId all unique shiftLeft =
            ( [ translate language labelTransId, String.fromInt all, String.fromInt unique ], shiftLeft )
    in
    { heading = translate language Translate.Encounters ++ ":"
    , captions = List.map (translate language) [ Translate.EncounterType, Translate.All, Translate.Unique ]
    , rows =
        [ generateRow Translate.ANCTotal
            (prenatalDataNurseEncountersTotal + prenatalDataChwEncountersTotal)
            (prenatalDataNurseEncountersUnique + prenatalDataChwEncountersUnique)
            False
        , generateRow Translate.HealthCenter prenatalDataNurseEncountersTotal prenatalDataNurseEncountersUnique True
        , generateRow Translate.CHW prenatalDataChwEncountersTotal prenatalDataChwEncountersUnique True
        , generateRow Translate.AcuteIllnessTotal
            (acuteIllnessDataNurseEncountersTotal + acuteIllnessDataChwEncountersTotal)
            (acuteIllnessDataNurseEncountersUnique + acuteIllnessDataChwEncountersUnique)
            False
        , generateRow Translate.HealthCenter acuteIllnessDataNurseEncountersTotal acuteIllnessDataNurseEncountersUnique True
        , generateRow Translate.CHW acuteIllnessDataChwEncountersTotal acuteIllnessDataChwEncountersUnique True
        , generateRow Translate.StandardPediatricVisit wellChildDataEncountersTotal wellChildDataEncountersUnique False
        , generateRow Translate.HomeVisit homeVisitDataEncountersTotal homeVisitDataEncountersUnique False
        , generateRow Translate.ChildScorecard childScorecardDataEncountersTotal childScorecardDataEncountersUnique False
        , generateRow Translate.NCD ncdDataEncountersTotal ncdDataEncountersUnique False
        , generateRow Translate.HIV hivDataEncountersTotal hivDataEncountersUnique False
        , generateRow Translate.Tuberculosis tuberculosisDataEncountersTotal tuberculosisDataEncountersUnique False
        , generateRow Translate.NutritionTotal overallNutritionTotal overallNutritionUnique False
        , generateRow Translate.PMTCT nutritionGroupPmtctEncountersTotal nutritionGroupPmtctEncountersUnique True
        , generateRow Translate.FBF nutritionGroupFbfEncountersTotal nutritionGroupFbfEncountersUnique True
        , generateRow Translate.Sorwathe nutritionGroupSorwatheEncountersTotal nutritionGroupSorwatheEncountersUnique True
        , generateRow Translate.CBNP nutritionGroupChwEncountersTotal nutritionGroupChwEncountersUnique True
        , generateRow Translate.ACHI nutritionGroupAchiEncountersTotal nutritionGroupAchiEncountersUnique True
        , generateRow Translate.Individual nutritionIndividualEncountersTotal nutritionIndividualEncountersUnique True
        ]
    , totals =
        { label = translate language Translate.Total
        , total = String.fromInt overallTotal
        , unique = String.fromInt overallUnique
        }
    }


viewDemographicsReportEncounters :
    Language
    ->
        { heading : String
        , captions : List String
        , rows : List ( List String, Bool )
        , totals : { label : String, total : String, unique : String }
        }
    -> List (Html Msg)
viewDemographicsReportEncounters language data =
    let
        viewRow ( cells, shiftLeft ) =
            div [ class "row" ] <|
                List.indexedMap
                    (\index cellText ->
                        div
                            [ classList
                                [ ( "item", True )
                                , ( "label", index == 0 )
                                , ( "ml-5", index == 0 && shiftLeft )
                                , ( "value", index /= 0 )
                                ]
                            ]
                            [ text cellText ]
                    )
                    cells
    in
    [ div [ class "section heading" ] [ text data.heading ]
    , div [ class "table encounters" ] <|
        [ div [ class "row captions" ] <|
            viewStandardCells data.captions
        ]
            ++ List.map viewRow data.rows
            ++ [ div [ class "row encounters-totals" ] <|
                    viewStandardCells [ data.totals.label, data.totals.total, data.totals.unique ]
               ]
    ]


demographicsReportEncountersDataToCSV :
    { heading : String
    , captions : List String
    , rows : List ( List String, Bool )
    , totals : { label : String, total : String, unique : String }
    }
    -> String
demographicsReportEncountersDataToCSV data =
    [ data.heading ++ "\n"
    , String.join "," data.captions
    , List.map (Tuple.first >> String.join ",")
        data.rows
        |> String.join "\n"
    , String.join "," [ data.totals.label, data.totals.total, data.totals.unique ]
    ]
        |> String.join "\n"


viewNutritionReport : Language -> NominalDate -> String -> Maybe (List BackendGeneratedNutritionReportTableDate) -> RemoteData String NutritionReportData -> Html Msg
viewNutritionReport language currentDate scopeLabel mBackendGeneratedData reportData =
    let
        generatedData =
            Maybe.map (generareNutritionReportDataFromBackendGeneratedData language currentDate) mBackendGeneratedData
                |> Maybe.withDefault (generareNutritionReportDataFromRawData language currentDate reportData)

        csvFileName =
            "nutrition-report-"
                ++ (String.toLower <| String.replace " " "-" scopeLabel)
                ++ "-"
                ++ customFormatDDMMYYYY "-" currentDate
                ++ ".csv"

        csvContent =
            reportTablesDataToCSV generatedData
    in
    div [ class "report nutrition" ] <|
        (List.map viewNutritionMetricsResultsTable generatedData
            |> List.concat
        )
            ++ [ viewDownloadCSVButton language csvFileName csvContent ]


generareNutritionReportDataFromRawData :
    Language
    -> NominalDate
    -> RemoteData String NutritionReportData
    -> List MetricsResultsTableData
generareNutritionReportDataFromRawData language currentDate reportData =
    case reportData of
        Success data ->
            let
                encountersByMonthForImpacted =
                    Dict.map
                        (\_ encounter ->
                            { encounter
                                | stuntingNormal = List.filter (\id -> List.member id data.impacted) encounter.stuntingNormal
                                , stuntingModerate = List.filter (\id -> List.member id data.impacted) encounter.stuntingModerate
                                , stuntingSevere = List.filter (\id -> List.member id data.impacted) encounter.stuntingSevere
                                , wastingNormal = List.filter (\id -> List.member id data.impacted) encounter.wastingNormal
                                , wastingModerate = List.filter (\id -> List.member id data.impacted) encounter.wastingModerate
                                , wastingSevere = List.filter (\id -> List.member id data.impacted) encounter.wastingSevere
                                , underweightNormal = List.filter (\id -> List.member id data.impacted) encounter.underweightNormal
                                , underweightModerate = List.filter (\id -> List.member id data.impacted) encounter.underweightModerate
                                , underweightSevere = List.filter (\id -> List.member id data.impacted) encounter.underweightSevere
                            }
                        )
                        data.encountersByMonth

                prevalenceByMonthOneVisitOrMoreData =
                    generateMonthlyPrevalenceTableData language currentDate Translate.PrevalenceByMonthOneVisitOrMore data.encountersByMonth

                prevalenceByMonthTwoVisitsOrMoreData =
                    generateMonthlyPrevalenceTableData language currentDate Translate.PrevalenceByMonthTwoVisitsOrMore encountersByMonthForImpacted

                incidenceByMonthOneVisitOrMoreData =
                    generateMonthlyIncidenceTableData language currentDate Translate.IncidenceByMonthOneVisitOrMore data.encountersByMonth

                incidenceByMonthTwoVisitsOrMoreData =
                    generateMonthlyIncidenceTableData language currentDate Translate.IncidenceByMonthTwoVisitsOrMore encountersByMonthForImpacted

                incidenceByQuarterOneVisitOrMoreData =
                    generateQuarterlyIncidenceTableData language currentDate Translate.IncidenceByQuarterOneVisitOrMore data.encountersByMonth

                incidenceByQuarterTwoVisitsOrMoreData =
                    generateQuarterlyIncidenceTableData language currentDate Translate.IncidenceByQuarterTwoVisitsOrMore encountersByMonthForImpacted

                incidenceByYearOneVisitOrMoreData =
                    generateYearlyIncidenceTableData language currentDate Translate.IncidenceByYearOneVisitOrMore data.encountersByMonth

                incidenceByYearTwoVisitsOrMore =
                    generateYearlyIncidenceTableData language currentDate Translate.IncidenceByYearTwoVisitsOrMore encountersByMonthForImpacted
            in
            [ prevalenceByMonthOneVisitOrMoreData
            , prevalenceByMonthTwoVisitsOrMoreData
            , incidenceByMonthOneVisitOrMoreData
            , incidenceByMonthTwoVisitsOrMoreData
            , incidenceByQuarterOneVisitOrMoreData
            , incidenceByQuarterTwoVisitsOrMoreData
            , incidenceByYearOneVisitOrMoreData
            , incidenceByYearTwoVisitsOrMore
            ]

        _ ->
            []


generareNutritionReportDataFromBackendGeneratedData :
    Language
    -> NominalDate
    -> List BackendGeneratedNutritionReportTableDate
    -> List MetricsResultsTableData
generareNutritionReportDataFromBackendGeneratedData language currentDate data =
    let
        nutritionTableTypeToNumber tableType =
            case tableType of
                NutritionTablePrevalanceOneOrMore ->
                    1

                NutritionTablePrevalanceTwoOrMore ->
                    2

                NutritionTableIncidenceMonthOneOrMore ->
                    3

                NutritionTableIncidenceMonthTwoOrMore ->
                    4

                NutritionTableIncidenceQuarterOneOrMore ->
                    5

                NutritionTableIncidenceQuarterTwoOrMore ->
                    6

                NutritionTableIncidenceYearOneOrMore ->
                    7

                NutritionTableIncidenceYearTwoOrMore ->
                    8
    in
    List.sortBy (.tableType >> nutritionTableTypeToNumber) data
        |> List.map (backendGeneratedNutritionReportTableDateToMetricsResultsTableData language)


backendGeneratedNutritionReportTableDateToMetricsResultsTableData : Language -> BackendGeneratedNutritionReportTableDate -> MetricsResultsTableData
backendGeneratedNutritionReportTableDateToMetricsResultsTableData language backendTableData =
    let
        translateCaption caption =
            if List.member backendTableData.tableType [ NutritionTableIncidenceQuarterOneOrMore, NutritionTableIncidenceQuarterTwoOrMore ] then
                case String.split "-" caption of
                    [ year, quarter ] ->
                        Maybe.map2 (\q y -> Translate.QuarterYear q y |> translate language)
                            (String.toInt quarter)
                            (String.toInt year)
                            |> Maybe.withDefault ""

                    _ ->
                        ""

            else if List.member backendTableData.tableType [ NutritionTableIncidenceYearOneOrMore, NutritionTableIncidenceYearTwoOrMore ] then
                String.toInt caption
                    |> Maybe.map (Translate.Year >> translate language)
                    |> Maybe.withDefault ""

            else
                case String.split "-" caption of
                    [ year, month ] ->
                        Maybe.map2 (\m y -> Translate.MonthYear m y True |> translate language)
                            (String.toInt month)
                            (String.toInt year)
                            |> Maybe.withDefault ""

                    _ ->
                        ""
    in
    { heading = translate language <| Translate.NutritionReportTableType backendTableData.tableType
    , captions = "" :: List.map translateCaption backendTableData.captions
    , rows =
        [ translate language Translate.StuntingModerate :: backendTableData.stuntingModerate
        , translate language Translate.StuntingSevere :: backendTableData.stuntingSevere
        , translate language Translate.WastingModerate :: backendTableData.stuntingModerate
        , translate language Translate.WastingSevere :: backendTableData.stuntingSevere
        , translate language Translate.UnderweightModerate :: backendTableData.stuntingModerate
        , translate language Translate.UnderweightSevere :: backendTableData.stuntingSevere
        ]
    }


generateMonthlyPrevalenceTableData :
    Language
    -> NominalDate
    -> TranslationId
    -> Dict ( Int, Int ) NutritionMetrics
    -> MetricsResultsTableData
generateMonthlyPrevalenceTableData language currentDate heading encountersByMonth =
    List.range 1 12
        |> List.map
            (\index ->
                let
                    selectedDate =
                        Date.add Months (-1 * index) currentDate

                    year =
                        Date.year selectedDate

                    monthNumber =
                        Date.monthNumber selectedDate
                in
                ( Translate.MonthYear monthNumber year True
                , resolveDataSetForMonth currentDate index encountersByMonth
                    |> generatePrevalenceNutritionMetricsResults
                )
            )
        |> toMetricsResultsTableData language heading


generateMonthlyIncidenceTableData :
    Language
    -> NominalDate
    -> TranslationId
    -> Dict ( Int, Int ) NutritionMetrics
    -> MetricsResultsTableData
generateMonthlyIncidenceTableData language currentDate heading encountersByMonth =
    List.range 1 12
        |> List.map
            (\index ->
                let
                    selectedDate =
                        Date.add Months (-1 * index) currentDate

                    year =
                        Date.year selectedDate

                    monthNumber =
                        Date.monthNumber selectedDate
                in
                ( Translate.MonthYear monthNumber year True
                , generateIncidenceNutritionMetricsResults
                    (resolveDataSetForMonth currentDate index encountersByMonth)
                    -- Per definition, for month, previous data set contains
                    -- data of 3 months that came prior.
                    (resolvePreviousDataSetForMonth currentDate index encountersByMonth)
                )
            )
        |> toMetricsResultsTableData language heading


generateQuarterlyIncidenceTableData :
    Language
    -> NominalDate
    -> TranslationId
    -> Dict ( Int, Int ) NutritionMetrics
    -> MetricsResultsTableData
generateQuarterlyIncidenceTableData language currentDate heading encountersByMonth =
    let
        dataSetsByQuarter =
            -- We show data of previous 4 quarters. So, if at Q2-2024, we show
            -- data for Q1-2024, Q4-2023, Q3-2023 and Q2-2023  We calculate set
            -- for 5 quarters (so claculating Q1-2023 as well), as for incidence
            -- each quarter requires a set of previous quarters.
            List.range 1 5
                |> List.map
                    (\index ->
                        resolveDataSetForQuarter currentDate index encountersByMonth
                    )
    in
    -- Showing data of previous 4 quarters.
    List.range 1 4
        |> List.map
            (\index ->
                let
                    selectedDate =
                        Date.add Months (-3 * index) currentDate

                    year =
                        Date.year selectedDate

                    quarter =
                        Date.quarter selectedDate

                    dataSet =
                        List.Extra.getAt (index - 1) dataSetsByQuarter
                            |> Maybe.withDefault emptyNutritionMetrics

                    previousDataSet =
                        List.Extra.getAt index dataSetsByQuarter
                            |> Maybe.withDefault emptyNutritionMetrics
                in
                ( Translate.QuarterYear quarter year
                , generateIncidenceNutritionMetricsResults dataSet previousDataSet
                )
            )
        |> toMetricsResultsTableData language heading


generateYearlyIncidenceTableData :
    Language
    -> NominalDate
    -> TranslationId
    -> Dict ( Int, Int ) NutritionMetrics
    -> MetricsResultsTableData
generateYearlyIncidenceTableData language currentDate heading encountersByMonth =
    let
        dataSetsByYear =
            -- We show data of previous 2 years. So, if at 2024, we show
            -- data for 2023 and 2022. We calculate set for 3 years (so claculating
            -- 2021 as well), as for incidence each year requires a set of previous year.
            List.range 1 3
                |> List.map
                    (\index ->
                        resolveDataSetForYear currentDate index encountersByMonth
                    )
    in
    -- Showing data of previous 2 years.
    List.range 1 2
        |> List.map
            (\index ->
                let
                    selectedDate =
                        Date.add Years (-1 * index) currentDate

                    year =
                        Date.year selectedDate

                    dataSet =
                        List.Extra.getAt (index - 1) dataSetsByYear
                            |> Maybe.withDefault emptyNutritionMetrics

                    previousDataSet =
                        List.Extra.getAt index dataSetsByYear
                            |> Maybe.withDefault emptyNutritionMetrics
                in
                ( Translate.Year year
                , generateIncidenceNutritionMetricsResults dataSet previousDataSet
                )
            )
        |> toMetricsResultsTableData language heading


toMetricsResultsTableData :
    Language
    -> TranslationId
    -> List ( TranslationId, NutritionMetricsResults )
    -> MetricsResultsTableData
toMetricsResultsTableData language heading data =
    let
        captions =
            List.map
                (\( label, _ ) ->
                    translate language label
                )
                data
                |> List.append [ "" ]

        generateRow label =
            List.map
                (\value ->
                    Round.round 3 value ++ "%"
                )
                >> List.append [ translate language label ]
    in
    { heading = translate language heading ++ ":"
    , captions = captions
    , rows =
        [ List.map (Tuple.second >> .stuntingModerate) data
            |> generateRow Translate.StuntingModerate
        , List.map (Tuple.second >> .stuntingSevere) data
            |> generateRow Translate.StuntingSevere
        , List.map (Tuple.second >> .wastingModerate) data
            |> generateRow Translate.WastingModerate
        , List.map (Tuple.second >> .wastingSevere) data
            |> generateRow Translate.WastingSevere
        , List.map (Tuple.second >> .underweightModerate) data
            |> generateRow Translate.UnderweightModerate
        , List.map (Tuple.second >> .underweightSevere) data
            |> generateRow Translate.UnderweightSevere
        ]
    }


viewNutritionMetricsResultsTable : MetricsResultsTableData -> List (Html Msg)
viewNutritionMetricsResultsTable data =
    let
        captionsRow =
            div [ class "row" ] <|
                viewCustomCells "row-label" "heading" data.captions

        viewRow cells =
            div [ class "row" ] <|
                viewCustomCells "row-label" "value" cells
    in
    [ div [ class "section heading" ] [ text data.heading ]
    , div [ class "table wide" ] <|
        captionsRow
            :: List.map viewRow data.rows
    ]


viewPrenatalReport : Language -> NominalDate -> String -> List PatientData -> Html Msg
viewPrenatalReport language limitDate scopeLabel records =
    let
        data =
            generatePrenatalReportData language limitDate records

        viewTable tableData =
            [ div [ class "section heading" ] [ text tableData.heading ]
            , div [ class "table anc" ] <|
                (div [ class "row captions" ] <|
                    viewStandardCells tableData.captions
                )
                    :: List.map viewStandardRow tableData.rows
            ]

        csvFileName =
            "anc-report-"
                ++ (String.toLower <| String.replace " " "-" scopeLabel)
                ++ "-"
                ++ customFormatDDMMYYYY "-" limitDate
                ++ ".csv"

        csvContent =
            reportTablesDataToCSV data
    in
    div [ class "report prenatal" ] <|
        (List.map viewTable data
            |> List.concat
        )
            ++ [ viewDownloadCSVButton language csvFileName csvContent ]


generatePrenatalReportData :
    Language
    -> NominalDate
    -> List PatientData
    -> List MetricsResultsTableData
generatePrenatalReportData language limitDate records =
    let
        filtered =
            List.map .prenatalData records
                |> Maybe.Extra.values
                |> List.concat
                |> List.filterMap
                    (\participantData ->
                        if isJust participantData.eddDate then
                            let
                                filteredEncounters =
                                    List.filter
                                        (\encounter ->
                                            not <| List.member encounter.encounterType [ NursePostpartumEncounter, ChwPostpartumEncounter ]
                                        )
                                        participantData.encounters
                            in
                            Just { participantData | encounters = filteredEncounters }

                        else
                            Nothing
                    )

        ( completed, active ) =
            List.partition
                (\participantData ->
                    -- Pregnancy is considered completed if
                    -- either conclusion date was set.
                    isJust participantData.dateConcluded
                        || (-- Or it's been 30 days or more since estimated delivery date.
                            Maybe.map
                                (\eddDate ->
                                    Date.compare (Date.add Days 30 eddDate) limitDate == LT
                                )
                                participantData.eddDate
                                |> -- We never get here, as filtered, only contains pregnancies
                                   -- that got EDD set.
                                   Maybe.withDefault False
                           )
                )
                filtered

        ( partitionedVisitsForActiveNurse, partitionedVisitsForActiveChw ) =
            countVisitsByType active
                |> partitionByNumberOfVisits

        ( partitionedVisitsForCompletedNurse, partitionedVisitsForCompletedChw ) =
            countVisitsByType completed
                |> partitionByNumberOfVisits

        countVisitsByType data =
            List.map
                (\participantData ->
                    let
                        totalEncounters =
                            List.length participantData.encounters

                        nurseEncounters =
                            List.filter (.encounterType >> (==) NurseEncounter)
                                participantData.encounters
                                |> List.length
                    in
                    { nurse = nurseEncounters
                    , chw = totalEncounters - nurseEncounters
                    }
                )
                data

        partitionByNumberOfVisits =
            List.foldl
                (\countedVisits ( nurseDict, chwDict ) ->
                    let
                        resolveKeyForValue value =
                            if value > 5 then
                                -1

                            else
                                value

                        updateDict value dict =
                            if value == 0 then
                                dict

                            else
                                let
                                    key =
                                        resolveKeyForValue value
                                in
                                Dict.get key dict
                                    |> Maybe.map
                                        (\total ->
                                            Dict.insert key (total + 1) dict
                                        )
                                    |> Maybe.withDefault (Dict.insert key 1 dict)
                    in
                    ( updateDict countedVisits.nurse nurseDict
                    , updateDict countedVisits.chw chwDict
                    )
                )
                ( Dict.empty, Dict.empty )

        resolveValueFromDict key =
            Dict.get key >> Maybe.withDefault 0

        activeNurseVisits1 =
            resolveValueFromDict 1 partitionedVisitsForActiveNurse

        activeNurseVisits2 =
            resolveValueFromDict 2 partitionedVisitsForActiveNurse

        activeNurseVisits3 =
            resolveValueFromDict 3 partitionedVisitsForActiveNurse

        activeNurseVisits4 =
            resolveValueFromDict 4 partitionedVisitsForActiveNurse

        activeNurseVisits5 =
            resolveValueFromDict 5 partitionedVisitsForActiveNurse

        activeNurseVisits5AndMore =
            resolveValueFromDict -1 partitionedVisitsForActiveNurse

        activeChwVisits1 =
            resolveValueFromDict 1 partitionedVisitsForActiveChw

        activeChwVisits2 =
            resolveValueFromDict 2 partitionedVisitsForActiveChw

        activeChwVisits3 =
            resolveValueFromDict 3 partitionedVisitsForActiveChw

        activeChwVisits4 =
            resolveValueFromDict 4 partitionedVisitsForActiveChw

        activeChwVisits5 =
            resolveValueFromDict 5 partitionedVisitsForActiveChw

        activeChwVisits5AndMore =
            resolveValueFromDict -1 partitionedVisitsForActiveChw

        completedNurseVisits1 =
            resolveValueFromDict 1 partitionedVisitsForCompletedNurse

        completedNurseVisits2 =
            resolveValueFromDict 2 partitionedVisitsForCompletedNurse

        completedNurseVisits3 =
            resolveValueFromDict 3 partitionedVisitsForCompletedNurse

        completedNurseVisits4 =
            resolveValueFromDict 4 partitionedVisitsForCompletedNurse

        completedNurseVisits5 =
            resolveValueFromDict 5 partitionedVisitsForCompletedNurse

        completedNurseVisits5AndMore =
            resolveValueFromDict -1 partitionedVisitsForCompletedNurse

        completedChwVisits1 =
            resolveValueFromDict 1 partitionedVisitsForCompletedChw

        completedChwVisits2 =
            resolveValueFromDict 2 partitionedVisitsForCompletedChw

        completedChwVisits3 =
            resolveValueFromDict 3 partitionedVisitsForCompletedChw

        completedChwVisits4 =
            resolveValueFromDict 4 partitionedVisitsForCompletedChw

        completedChwVisits5 =
            resolveValueFromDict 5 partitionedVisitsForCompletedChw

        completedChwVisits5AndMore =
            resolveValueFromDict -1 partitionedVisitsForCompletedChw

        generateTableData heading values =
            let
                generateRowData labelTransId valueChw valueNurse =
                    [ translate language labelTransId
                    , String.fromInt valueChw
                    , String.fromInt valueNurse
                    , String.fromInt <| valueChw + valueNurse
                    ]
            in
            { heading = translate language heading ++ ":"
            , captions = List.map (translate language) [ Translate.NumberOfVisitsLabel, Translate.CHW, Translate.HC, Translate.All ]
            , rows =
                List.indexedMap
                    (\index ( chwValue, nurseValue ) ->
                        generateRowData (Translate.NumberOfVisits (index + 1)) chwValue nurseValue
                    )
                    values
            }
    in
    [ generateTableData Translate.PregnanciesAll
        [ ( activeChwVisits1 + completedChwVisits1, activeNurseVisits1 + completedNurseVisits1 )
        , ( activeChwVisits2 + completedChwVisits2, activeNurseVisits2 + completedNurseVisits2 )
        , ( activeChwVisits3 + completedChwVisits3, activeNurseVisits3 + completedNurseVisits3 )
        , ( activeChwVisits4 + completedChwVisits4, activeNurseVisits4 + completedNurseVisits4 )
        , ( activeChwVisits5 + completedChwVisits5, activeNurseVisits5 + completedNurseVisits5 )
        , ( activeChwVisits5AndMore + completedChwVisits5AndMore, activeNurseVisits5AndMore + completedNurseVisits5AndMore )
        ]
    , generateTableData Translate.PregnanciesActive
        [ ( activeChwVisits1, activeNurseVisits1 )
        , ( activeChwVisits2, activeNurseVisits2 )
        , ( activeChwVisits3, activeNurseVisits3 )
        , ( activeChwVisits4, activeNurseVisits4 )
        , ( activeChwVisits5, activeNurseVisits5 )
        , ( activeChwVisits5AndMore, activeNurseVisits5AndMore )
        ]
    , generateTableData Translate.PregnanciesCompleted
        [ ( completedChwVisits1, completedNurseVisits1 )
        , ( completedChwVisits2, completedNurseVisits2 )
        , ( completedChwVisits3, completedNurseVisits3 )
        , ( completedChwVisits4, completedNurseVisits4 )
        , ( completedChwVisits5, completedNurseVisits5 )
        , ( completedChwVisits5AndMore, completedNurseVisits5AndMore )
        ]
    ]


viewAcuteIllnessReport : Language -> NominalDate -> NominalDate -> String -> List PatientData -> Html Msg
viewAcuteIllnessReport language limitDate startDate scopeLabel records =
    let
        data =
            generateAcuteIllnessReportData language startDate records

        captionsRow =
            viewStandardCells data.captions
                |> div [ class "row captions" ]

        csvFileName =
            "acute-illness-report-"
                ++ (String.toLower <| String.replace " " "-" scopeLabel)
                ++ "-"
                ++ customFormatDDMMYYYY "-" startDate
                ++ "-to-"
                ++ customFormatDDMMYYYY "-" limitDate
                ++ ".csv"

        csvContent =
            reportTableDataToCSV data
    in
    div [ class "report acute-illness" ] <|
        [ div [ class "table" ] <|
            captionsRow
                :: List.map viewStandardRow data.rows
        ]
            ++ [ viewDownloadCSVButton language csvFileName csvContent ]


generateAcuteIllnessReportData :
    Language
    -> NominalDate
    -> List PatientData
    -> MetricsResultsTableData
generateAcuteIllnessReportData language startDate records =
    let
        -- Records grouped by participant (illness).
        acuteIllnessParticipantRecords =
            List.map .acuteIllnessData records
                |> Maybe.Extra.values
                |> List.concat

        diagnosesCountDict =
            List.concat acuteIllnessParticipantRecords
                |> List.map .diagnosis
                |> Maybe.Extra.values
                |> List.foldl
                    (\diagnosis accum ->
                        Dict.get diagnosis accum
                            |> Maybe.map
                                (\value ->
                                    Dict.insert diagnosis (value + 1) accum
                                )
                            |> Maybe.withDefault (Dict.insert diagnosis 1 accum)
                    )
                    Dict.empty

        -- Initial encounter always determines a diagnosis.
        -- Here we count the illnesses for which no diagnosis was determined.
        illnessesWithNoDiagnosis =
            List.filter
                (\encountersList ->
                    List.sortWith (sortByDateDesc .startDate) encountersList
                        |> List.head
                        |> Maybe.map
                            (\encounter ->
                                (not <| Date.compare encounter.startDate startDate == LT)
                                    && isNothing encounter.diagnosis
                            )
                        |> Maybe.withDefault False
                )
                acuteIllnessParticipantRecords
                |> List.length

        rows =
            List.map
                (\diagnosis ->
                    Dict.get diagnosis diagnosesCountDict
                        |> Maybe.withDefault 0
                        |> generateRow (Translate.AcuteIllnessDiagnosis diagnosis)
                )
                allAcuteIllnessDiagnoses

        totalsRow =
            Dict.values diagnosesCountDict
                |> List.sum
                |> generateRow Translate.Total

        noneRow =
            generateRow Translate.NoDiagnosis illnessesWithNoDiagnosis

        generateRow label value =
            [ translate language label
            , String.fromInt value
            ]
    in
    { heading = ""
    , captions =
        [ translate language Translate.Diagnosis
        , translate language Translate.Total
        ]
    , rows = rows ++ [ totalsRow ] ++ [ noneRow ]
    }


viewDownloadCSVButton : Language -> String -> String -> Html Msg
viewDownloadCSVButton language csvFileName csvContent =
    div [ class "download-csv-wrapper" ]
        [ button
            [ class "download-csv"
            , onClick <| DownloadCSV csvFileName csvContent
            ]
            [ text <| translate language Translate.DownloadCSV ]
        ]


reportTablesDataToCSV : List MetricsResultsTableData -> String
reportTablesDataToCSV =
    List.map reportTableDataToCSV
        >> String.join "\n\n"


reportTableDataToCSV : MetricsResultsTableData -> String
reportTableDataToCSV tableData =
    [ tableData.heading
    , String.join "," tableData.captions
    , List.map (String.join ",") tableData.rows
        |> String.join "\n"
    ]
        |> String.join "\n"
