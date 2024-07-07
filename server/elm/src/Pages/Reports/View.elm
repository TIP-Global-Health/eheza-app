module Pages.Reports.View exposing (view)

import App.Types exposing (Language, Site)
import AssocList as Dict exposing (Dict)
import Backend.Model exposing (ModelBackend)
import Backend.Reports.Model
    exposing
        ( AcuteIllnessEncounterType(..)
        , Gender(..)
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
import Gizra.NominalDate exposing (NominalDate, formatDDMMYYYY, sortByDateDesc)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick)
import List.Extra
import Maybe.Extra exposing (isJust, isNothing)
import Pages.Reports.Model exposing (..)
import Pages.Reports.Utils exposing (..)
import Pages.Utils exposing (viewCustomLabel, viewSelectListInput, wrapSelectListInput)
import RemoteData exposing (RemoteData(..))
import Round
import Translate exposing (TranslationId, translate)
import Utils.Html exposing (viewModal)


view : Language -> NominalDate -> ModelBackend -> Model -> Html Msg
view language currentDate modelBackend model =
    case modelBackend.reportsData of
        Just (Ok data) ->
            viewReportsData language currentDate data model

        Just (Err err) ->
            text <| Debug.toString err

        Nothing ->
            emptyNode


viewReportsData : Language -> NominalDate -> ReportsData -> Model -> Html Msg
viewReportsData language currentDate data model =
    let
        topBar =
            let
                scopeLabel =
                    case data.entityType of
                        EntityGlobal ->
                            translate language Translate.Global

                        EntityHealthCenter ->
                            data.entityName

                        _ ->
                            data.entityName ++ " " ++ (String.toLower <| translate language (Translate.SelectedScope data.entityType))
            in
            div [ class "top-bar" ]
                [ div [ class "new-selection" ]
                    [ a [ href "/admin/reports/aggregated-reports" ]
                        [ button []
                            [ text <| translate language Translate.NewScope ]
                        ]
                    ]
                , div [ class "scope" ]
                    [ text <| translate language Translate.Scope ++ ": " ++ scopeLabel ]
                ]

        dateInputs =
            Maybe.map
                (\reportType ->
                    let
                        startDateInput =
                            if reportType == ReportAcuteIllness then
                                let
                                    dateSelectorConfig =
                                        let
                                            sixYearsAgo =
                                                Date.add Years -6 currentDate
                                        in
                                        { select = SetStartDate
                                        , close = SetStartDateSelectorState Nothing
                                        , dateFrom = sixYearsAgo
                                        , dateTo = currentDate
                                        , dateDefault = Just sixYearsAgo
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

                            else
                                emptyNode

                        limitDateInput =
                            if
                                -- Nutrition report does not allow selecting limit date, so
                                -- we do not show limit date input when report is selected.
                                (reportType == ReportNutrition)
                                    || -- Acute Illness report requires setting start date before
                                       -- limit date can be shown.
                                       (reportType == ReportAcuteIllness && isNothing model.startDate)
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

        limitDateByReportType =
            if model.reportType == Just ReportNutrition then
                -- Nutrition report does not allow selecting limit date, so
                -- we force it to be today.
                Just currentDate

            else
                model.limitDate

        content =
            if
                isJust model.startDateSelectorPopupState
                    || isJust model.limitDateSelectorPopupState
            then
                -- Date selector is open, so no need to calcualte
                -- intermediate results.
                emptyNode

            else
                Maybe.map2
                    (\reportType limitDate ->
                        let
                            recordsTillLimitDate =
                                if Date.compare limitDate currentDate == EQ then
                                    data.records

                                else
                                    List.filterMap
                                        (\record ->
                                            if Date.compare record.created limitDate == LT then
                                                let
                                                    filterPrenatalData =
                                                        Maybe.map
                                                            (List.filterMap
                                                                (\participantData ->
                                                                    if Date.compare participantData.created limitDate == LT then
                                                                        Nothing

                                                                    else
                                                                        let
                                                                            dateConcluded =
                                                                                -- If pregnancy was concluded, but conclusion date is
                                                                                -- after limit date, we mark pregnancy as not concluded.
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
                                                                                        Date.compare encounterData.startDate limitDate == LT
                                                                                    )
                                                                                    participantData.encounters
                                                                        in
                                                                        Just { participantData | dateConcluded = dateConcluded, encounters = filteredEncounters }
                                                                )
                                                            )

                                                    filterIndividualBy resolveDateFunc =
                                                        Maybe.map
                                                            (List.map
                                                                (List.filter
                                                                    (\encounterData ->
                                                                        Date.compare (resolveDateFunc encounterData) limitDate == LT
                                                                    )
                                                                )
                                                            )

                                                    filterGroupBy resolveDateFunc =
                                                        Maybe.map
                                                            (List.filter
                                                                (\encounterData ->
                                                                    Date.compare (resolveDateFunc encounterData) limitDate == LT
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
                                Maybe.map
                                    (\startDate ->
                                        viewAcuteIllnessReport language startDate recordsTillLimitDate
                                    )
                                    model.startDate
                                    |> Maybe.withDefault emptyNode

                            ReportDemographics ->
                                viewDemographicsReport language limitDate recordsTillLimitDate

                            ReportNutrition ->
                                viewNutritionReport language limitDate model.nutritionReportData

                            ReportPrenatal ->
                                viewPrenatalReport language limitDate recordsTillLimitDate
                    )
                    model.reportType
                    limitDateByReportType
                    |> Maybe.withDefault
                        (if isWideScope data.entityType then
                            viewCustomLabel language Translate.WideScopeNote "" "label wide-scope"

                         else
                            emptyNode
                        )
    in
    div [ class "page-content" ]
        [ topBar
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


viewDemographicsReport : Language -> NominalDate -> List PatientData -> Html Msg
viewDemographicsReport language limitDate records =
    let
        demographicsReportPatientsData =
            generateDemographicsReportPatientsData language limitDate records
    in
    div [ class "report demographics" ] <|
        viewDemographicsReportPatients language limitDate demographicsReportPatientsData
            ++ viewDemographicsReportEncounters language records


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

        viewRow label valueMales valueFemales =
            div [ class "row" ]
                [ div [ class "item label" ] [ text label ]
                , div [ class "item value" ] [ text <| String.fromInt <| List.length valueMales ]
                , div [ class "item value" ] [ text <| String.fromInt <| List.length valueFemales ]
                ]
    in
    { heading = translate language Translate.RegisteredPatients
    , tables =
        [ { name = "registered"
          , captions = List.map (translate language) [ Translate.Registered, Translate.Male, Translate.Female ]
          , rows =
                [ [ "0 - 1M", String.fromInt <| List.length males1MonthAndLess, String.fromInt <| List.length females1MonthAndLess ]
                , [ "1M - 2Y", String.fromInt <| List.length males1Month2Years, String.fromInt <| List.length females1Month2Years ]
                , [ "2Y - 5Y", String.fromInt <| List.length males2Years5Years, String.fromInt <| List.length females2Years5Years ]
                , [ "5Y - 10Y", String.fromInt <| List.length males5Years10Years, String.fromInt <| List.length females5Years10Years ]
                , [ "10Y - 20Y", String.fromInt <| List.length males10Years20Years, String.fromInt <| List.length females10Years20Years ]
                , [ "20Y - 50Y", String.fromInt <| List.length males20Years50Years, String.fromInt <| List.length females20Years50Years ]
                , [ "50Y +", String.fromInt <| List.length males50YearsOrMore, String.fromInt <| List.length females50YearsOrMore ]
                ]
          , totals = ( translate language Translate.Total, String.fromInt <| List.length <| males ++ females )
          }
        , { name = "impacted"
          , captions = List.map (translate language) [ Translate.Impacted, Translate.Male, Translate.Female ]
          , rows =
                [ [ "0 - 1M", String.fromInt <| List.length malesImpacted1MonthAndLess, String.fromInt <| List.length femalesImpacted1MonthAndLess ]
                , [ "1M - 2Y", String.fromInt <| List.length malesImpacted1Month2Years, String.fromInt <| List.length femalesImpacted1Month2Years ]
                , [ "2Y - 5Y", String.fromInt <| List.length malesImpacted2Years5Years, String.fromInt <| List.length femalesImpacted2Years5Years ]
                , [ "5Y - 10Y", String.fromInt <| List.length malesImpacted5Years10Years, String.fromInt <| List.length femalesImpacted5Years10Years ]
                , [ "10Y - 20Y", String.fromInt <| List.length malesImpacted10Years20Years, String.fromInt <| List.length femalesImpacted10Years20Years ]
                , [ "20Y - 50Y", String.fromInt <| List.length malesImpacted20Years50Years, String.fromInt <| List.length femalesImpacted20Years50Years ]
                , [ "50Y +", String.fromInt <| List.length malesImpacted50YearsOrMore, String.fromInt <| List.length femalesImpacted50YearsOrMore ]
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
                    List.map (\caption -> div [ class "item label" ] [ text caption ]) tableData.captions
                ]
                    ++ List.map
                        (\cells ->
                            div [ class "row" ] <|
                                List.indexedMap
                                    (\index cellText ->
                                        div
                                            [ classList
                                                [ ( "item", True )
                                                , ( "label", index == 0 )
                                                , ( "value", index /= 0 )
                                                ]
                                            ]
                                            [ text cellText ]
                                    )
                                    cells
                        )
                        tableData.rows
                    ++ [ div [ class "row totals" ]
                            [ div [ class "item label" ] [ text <| Tuple.first tableData.totals ]
                            , div [ class "item value" ] [ text <| Tuple.second tableData.totals ]
                            ]
                       ]
    in
    div [ class "section heading" ] [ text data.heading ]
        :: List.map viewTable data.tables


viewDemographicsReportEncounters : Language -> List PatientData -> List (Html Msg)
viewDemographicsReportEncounters language records =
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

        viewRow =
            viewCustomRow "row"

        viewCustomRow rowClass labelTransId all unique shiftLeft =
            div [ class rowClass ]
                [ div
                    [ classList
                        [ ( "item label", True )
                        , ( "ml-5", shiftLeft )
                        ]
                    ]
                    [ text <| translate language labelTransId ]
                , div [ class "item value" ] [ text <| String.fromInt all ]
                , div [ class "item value" ] [ text <| String.fromInt unique ]
                ]
    in
    [ viewCustomLabel language Translate.Encounters ":" "section heading"
    , div [ class "table encounters" ]
        [ div [ class "row captions" ]
            [ div [ class "item label" ] [ text <| translate language Translate.EncounterType ]
            , div [ class "item value" ] [ text <| translate language Translate.All ]
            , div [ class "item value" ] [ text <| translate language Translate.Unique ]
            ]
        , viewRow Translate.ANCTotal
            (prenatalDataNurseEncountersTotal + prenatalDataChwEncountersTotal)
            (prenatalDataNurseEncountersUnique + prenatalDataChwEncountersUnique)
            False
        , viewRow Translate.HealthCenter prenatalDataNurseEncountersTotal prenatalDataNurseEncountersUnique True
        , viewRow Translate.CHW prenatalDataChwEncountersTotal prenatalDataChwEncountersUnique True
        , viewRow Translate.AcuteIllnessTotal
            (acuteIllnessDataNurseEncountersTotal + acuteIllnessDataChwEncountersTotal)
            (acuteIllnessDataNurseEncountersUnique + acuteIllnessDataChwEncountersUnique)
            False
        , viewRow Translate.HealthCenter acuteIllnessDataNurseEncountersTotal acuteIllnessDataNurseEncountersUnique True
        , viewRow Translate.CHW acuteIllnessDataChwEncountersTotal acuteIllnessDataChwEncountersUnique True
        , viewRow Translate.StandardPediatricVisit wellChildDataEncountersTotal wellChildDataEncountersUnique False
        , viewRow Translate.HomeVisit homeVisitDataEncountersTotal homeVisitDataEncountersUnique False
        , viewRow Translate.ChildScorecard childScorecardDataEncountersTotal childScorecardDataEncountersUnique False
        , viewRow Translate.NCD ncdDataEncountersTotal ncdDataEncountersUnique False
        , viewRow Translate.HIV hivDataEncountersTotal hivDataEncountersUnique False
        , viewRow Translate.Tuberculosis tuberculosisDataEncountersTotal tuberculosisDataEncountersUnique False
        , viewRow Translate.NutritionTotal overallNutritionTotal overallNutritionUnique False
        , viewRow Translate.PMTCT nutritionGroupPmtctEncountersTotal nutritionGroupPmtctEncountersUnique True
        , viewRow Translate.FBF nutritionGroupFbfEncountersTotal nutritionGroupFbfEncountersUnique True
        , viewRow Translate.Sorwathe nutritionGroupSorwatheEncountersTotal nutritionGroupSorwatheEncountersUnique True
        , viewRow Translate.CBNP nutritionGroupChwEncountersTotal nutritionGroupChwEncountersUnique True
        , viewRow Translate.ACHI nutritionGroupAchiEncountersTotal nutritionGroupAchiEncountersUnique True
        , viewRow Translate.Individual nutritionIndividualEncountersTotal nutritionIndividualEncountersUnique True
        , viewCustomRow "row encounters-totals" Translate.Total overallTotal overallUnique False
        ]
    ]


viewNutritionReport : Language -> NominalDate -> RemoteData String NutritionReportData -> Html Msg
viewNutritionReport language currentDate reportData =
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
            in
            div [ class "report nutrition" ]
                [ viewCustomLabel language Translate.PrevalenceByMonthOneVisitOrMore ":" "section heading"
                , viewMonthlyPrevalenceTable language currentDate data.encountersByMonth
                , viewCustomLabel language Translate.PrevalenceByMonthTwoVisitsOrMore ":" "section heading"
                , viewMonthlyPrevalenceTable language currentDate encountersByMonthForImpacted
                , viewCustomLabel language Translate.IncidenceByMonthOneVisitOrMore ":" "section heading"
                , viewMonthlyIncidenceTable language currentDate data.encountersByMonth
                , viewCustomLabel language Translate.IncidenceByMonthTwoVisitsOrMore ":" "section heading"
                , viewMonthlyIncidenceTable language currentDate encountersByMonthForImpacted
                , viewCustomLabel language Translate.IncidenceByQuarterOneVisitOrMore ":" "section heading"
                , viewQuarterlyIncidenceTable language currentDate data.encountersByMonth
                , viewCustomLabel language Translate.IncidenceByQuarterTwoVisitsOrMore ":" "section heading"
                , viewQuarterlyIncidenceTable language currentDate encountersByMonthForImpacted
                , viewCustomLabel language Translate.IncidenceByYearOneVisitOrMore ":" "section heading"
                , viewYearlyIncidenceTable language currentDate data.encountersByMonth
                , viewCustomLabel language Translate.IncidenceByYearTwoVisitsOrMore ":" "section heading"
                , viewYearlyIncidenceTable language currentDate encountersByMonthForImpacted
                ]

        _ ->
            div [ class "report nutrition" ]
                [ viewCustomLabel language Translate.PrevalenceByMonthOneVisitOrMore ":" "section heading" ]


viewMonthlyPrevalenceTable : Language -> NominalDate -> Dict ( Int, Int ) NutritionMetrics -> Html Msg
viewMonthlyPrevalenceTable language currentDate encountersByMonth =
    List.range 1 12
        |> List.map
            (\index ->
                let
                    selectedDate =
                        Date.add Months (-1 * index) currentDate

                    year =
                        Date.year selectedDate

                    month =
                        Date.month selectedDate

                    monthNumber =
                        Date.monthNumber selectedDate
                in
                ( Translate.MonthYear month year True
                , resolveDataSetForMonth currentDate index encountersByMonth
                    |> generatePrevalenceNutritionMetricsResults
                )
            )
        |> viewNutritionMetricsResultsTable language currentDate


viewMonthlyIncidenceTable : Language -> NominalDate -> Dict ( Int, Int ) NutritionMetrics -> Html Msg
viewMonthlyIncidenceTable language currentDate encountersByMonth =
    List.range 1 12
        |> List.map
            (\index ->
                let
                    selectedDate =
                        Date.add Months (-1 * index) currentDate

                    year =
                        Date.year selectedDate

                    month =
                        Date.month selectedDate
                in
                ( Translate.MonthYear month year True
                , generateIncidenceNutritionMetricsResults
                    (resolveDataSetForMonth currentDate index encountersByMonth)
                    -- Per definition, for month, previous data set contains
                    -- data of 3 months that came prior.
                    (resolvePreviousDataSetForMonth currentDate index encountersByMonth)
                )
            )
        |> viewNutritionMetricsResultsTable language currentDate


viewQuarterlyIncidenceTable : Language -> NominalDate -> Dict ( Int, Int ) NutritionMetrics -> Html Msg
viewQuarterlyIncidenceTable language currentDate encountersByMonth =
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
        |> viewNutritionMetricsResultsTable language currentDate


viewYearlyIncidenceTable : Language -> NominalDate -> Dict ( Int, Int ) NutritionMetrics -> Html Msg
viewYearlyIncidenceTable language currentDate encountersByMonth =
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
        |> viewNutritionMetricsResultsTable language currentDate


viewNutritionMetricsResultsTable : Language -> NominalDate -> List ( TranslationId, NutritionMetricsResults ) -> Html Msg
viewNutritionMetricsResultsTable language currentDate data =
    let
        headerRow =
            List.map
                (\( label, _ ) ->
                    div [ class "item heading" ] [ text <| translate language label ]
                )
                data
                |> List.append [ div [ class "item row-label" ] [ text "" ] ]
                |> div [ class "row" ]

        viewRow label =
            List.map
                (\value ->
                    div [ class "item value" ] [ text <| Round.round 3 value ++ "%" ]
                )
                >> List.append [ div [ class "item row-label" ] [ text <| translate language label ] ]
                >> div [ class "row" ]
    in
    div [ class "table wide" ]
        [ headerRow
        , List.map (Tuple.second >> .stuntingModerate) data
            |> viewRow Translate.StuntingModerate
        , List.map (Tuple.second >> .stuntingSevere) data
            |> viewRow Translate.StuntingSevere
        , List.map (Tuple.second >> .wastingModerate) data
            |> viewRow Translate.WastingModerate
        , List.map (Tuple.second >> .wastingSevere) data
            |> viewRow Translate.WastingSevere
        , List.map (Tuple.second >> .underweightModerate) data
            |> viewRow Translate.UnderweightModerate
        , List.map (Tuple.second >> .underweightSevere) data
            |> viewRow Translate.UnderweightSevere
        ]


viewPrenatalReport : Language -> NominalDate -> List PatientData -> Html Msg
viewPrenatalReport language limitDate records =
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

        viewTable caption values =
            let
                rows =
                    List.indexedMap
                        (\index ( chwValue, nurseValue ) ->
                            viewRow (Translate.NumberOfVisits (index + 1)) chwValue nurseValue
                        )
                        values
            in
            [ viewCustomLabel language caption ":" "section heading"
            , div [ class "table anc" ] <|
                div [ class "row captions" ]
                    [ div [ class "item label" ] [ text <| translate language Translate.NumberOfVisitsLabel ]
                    , div [ class "item value" ] [ text <| translate language Translate.CHW ]
                    , div [ class "item value" ] [ text <| translate language Translate.HC ]
                    , div [ class "item value" ] [ text <| translate language Translate.All ]
                    ]
                    :: rows
            ]

        viewRow labelTransId valueChw valueNurse =
            div [ class "row" ]
                [ div [ class "item label" ] [ text <| translate language labelTransId ]
                , div [ class "item value" ] [ text <| String.fromInt valueChw ]
                , div [ class "item value" ] [ text <| String.fromInt valueNurse ]
                , div [ class "item value" ] [ text <| String.fromInt <| valueChw + valueNurse ]
                ]
    in
    div [ class "report prenatal" ] <|
        viewTable Translate.PregnanciesAll
            [ ( activeChwVisits1 + completedChwVisits1, activeNurseVisits1 + completedNurseVisits1 )
            , ( activeChwVisits2 + completedChwVisits2, activeNurseVisits2 + completedNurseVisits2 )
            , ( activeChwVisits3 + completedChwVisits3, activeNurseVisits3 + completedNurseVisits3 )
            , ( activeChwVisits4 + completedChwVisits4, activeNurseVisits4 + completedNurseVisits4 )
            , ( activeChwVisits5 + completedChwVisits5, activeNurseVisits5 + completedNurseVisits5 )
            , ( activeChwVisits5AndMore + completedChwVisits5AndMore, activeNurseVisits5AndMore + completedNurseVisits5AndMore )
            ]
            ++ viewTable Translate.PregnanciesActive
                [ ( activeChwVisits1, activeNurseVisits1 )
                , ( activeChwVisits2, activeNurseVisits2 )
                , ( activeChwVisits3, activeNurseVisits3 )
                , ( activeChwVisits4, activeNurseVisits4 )
                , ( activeChwVisits5, activeNurseVisits5 )
                , ( activeChwVisits5AndMore, activeNurseVisits5AndMore )
                ]
            ++ viewTable Translate.PregnanciesCompleted
                [ ( completedChwVisits1, completedNurseVisits1 )
                , ( completedChwVisits2, completedNurseVisits2 )
                , ( completedChwVisits3, completedNurseVisits3 )
                , ( completedChwVisits4, completedNurseVisits4 )
                , ( completedChwVisits5, completedNurseVisits5 )
                , ( completedChwVisits5AndMore, completedNurseVisits5AndMore )
                ]


viewAcuteIllnessReport : Language -> NominalDate -> List PatientData -> Html Msg
viewAcuteIllnessReport language startDate records =
    let
        acuteIllnessDataRecords =
            List.map .acuteIllnessData records
                |> Maybe.Extra.values

        filtered =
            -- We got recordes filtered by limit date (end date).
            -- Now we need to filter from start date.
            List.concat acuteIllnessDataRecords
                |> List.concat
                |> List.filter
                    (\encounter ->
                        not <| Date.compare encounter.startDate startDate == LT
                    )

        diagnosesCountDict =
            List.map .diagnosis filtered
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

        -- Initial encounter always determine a diagnosis.
        -- Here we count the illnesses for which no diagnosis was determined.
        illnessesWithNoDiagnosis =
            List.concat acuteIllnessDataRecords
                |> List.filter
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
                |> List.length

        rows =
            List.map
                (\diagnosis ->
                    Dict.get diagnosis diagnosesCountDict
                        |> Maybe.withDefault 0
                        |> viewRow (Translate.AcuteIllnessDiagnosis diagnosis)
                )
                allAcuteIllnessDiagnoses

        totalsRow =
            Dict.values diagnosesCountDict
                |> List.sum
                |> viewRow Translate.Total

        noneRow =
            viewRow Translate.NoDiagnosis illnessesWithNoDiagnosis

        viewRow label value =
            div [ class "row" ]
                [ div [ class "item label" ] [ text <| translate language label ]
                , div [ class "item value" ] [ text <| String.fromInt value ]
                ]
    in
    div [ class "report acute-illness" ]
        [ div [ class "table" ] <|
            div [ class "row captions" ]
                [ div [ class "item label" ] [ text <| translate language Translate.Diagnosis ]
                , div [ class "item value" ] [ text <| translate language Translate.Total ]
                ]
                :: (rows ++ [ totalsRow, noneRow ])
        ]
