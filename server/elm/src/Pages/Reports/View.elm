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
        , ReportsData
        )
import Date exposing (Interval(..), Unit(..))
import DateSelector.SelectorPopup exposing (viewCalendarPopup)
import Gizra.Html exposing (emptyNode)
import Gizra.NominalDate exposing (NominalDate, formatDDMMYYYY)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick)
import Maybe.Extra exposing (isJust)
import Pages.Reports.Model exposing (..)
import Pages.Reports.Utils exposing (..)
import Pages.Utils exposing (viewCustomLabel, viewSelectListInput, wrapSelectListInput)
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
            div [ class "top-bar" ]
                [ div [ class "new-selection" ]
                    [ a [ href "/admin/reports/aggregated-reports" ]
                        [ button []
                            [ text <| translate language Translate.NewSelection ]
                        ]
                    ]
                ]

        dateSelectorConfig =
            { select = SetLimitDate
            , close = SetLimitDateSelectorState Nothing
            , dateFrom = Date.add Years -6 currentDate
            , dateTo = currentDate
            , dateDefault = Just currentDate
            }

        limitDateForView =
            Maybe.map formatDDMMYYYY model.limitDate
                |> Maybe.withDefault ""

        content =
            if isJust model.dateSelectorPopupState then
                -- Date selector is open, so no need to calcualte
                -- intermediate results.
                emptyNode

            else
                Maybe.map2
                    (\reportType limitDate ->
                        let
                            recordsTillLimitDate =
                                List.filterMap
                                    (\record ->
                                        if Date.compare record.created limitDate == LT then
                                            let
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
                                                    , prenatalData = filterIndividualBy .startDate record.prenatalData
                                                    , homeVisitData = filterIndividualBy identity record.homeVisitData
                                                    , wellChildData = filterIndividualBy .startDate record.wellChildData
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
                            ReportDemographics ->
                                viewDemographicsReport language limitDate recordsTillLimitDate

                            ReportNutrition ->
                                viewNutritionReport language limitDate recordsTillLimitDate
                    )
                    model.reportType
                    model.limitDate
                    |> Maybe.withDefault emptyNode
    in
    div [ class "page-content" ]
        [ topBar
        , div [ class "inputs" ]
            [ viewSelectListInput language
                model.reportType
                [ ReportDemographics, ReportNutrition ]
                reportTypeToString
                SetReportType
                Translate.ReportType
                "select-input"
                |> wrapSelectListInput language Translate.ReportTypeLabel False
            , div
                [ class "form-input date"
                , onClick <| SetLimitDateSelectorState (Just dateSelectorConfig)
                ]
                [ text limitDateForView ]
                |> wrapSelectListInput language Translate.SelectLimitDate False
            , content
            ]
        , viewModal <| viewCalendarPopup language model.dateSelectorPopupState model.limitDate
        ]


viewDemographicsReport : Language -> NominalDate -> List PatientData -> Html Msg
viewDemographicsReport language limitDate records =
    div [ class "report demographics" ] <|
        viewDemographicsReportPatients language limitDate records
            ++ viewDemographicsReportEncounters language records


viewDemographicsReportPatients : Language -> NominalDate -> List PatientData -> List (Html Msg)
viewDemographicsReportPatients language limitDate records =
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
            List.filter (\patient -> countTotalEncounetrs patient > 1)

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
    [ viewCustomLabel language Translate.RegisteredPatients ":" "section heading"
    , div [ class "table registered" ]
        [ div [ class "row captions" ]
            [ div [ class "item label" ] [ text <| translate language Translate.Registered ]
            , div [ class "item value" ] [ text <| translate language Translate.Male ]
            , div [ class "item value" ] [ text <| translate language Translate.Female ]
            ]
        , viewRow "0 - 1M" males1MonthAndLess females1MonthAndLess
        , viewRow "1M - 2Y" males1Month2Years females1Month2Years
        , viewRow "2Y - 5Y" males2Years5Years females2Years5Years
        , viewRow "5Y - 10Y" males5Years10Years females5Years10Years
        , viewRow "10Y - 20Y" males10Years20Years females10Years20Years
        , viewRow "20Y - 50Y" males20Years50Years females20Years50Years
        , viewRow "50Y +" males50YearsOrMore females50YearsOrMore
        , div [ class "row totals" ]
            [ div [ class "item label" ] [ text <| translate language Translate.Total ]
            , div [ class "item value" ] [ text <| String.fromInt <| List.length <| males ++ females ]
            ]
        ]
    , div [ class "table impacted" ]
        [ div [ class "row captions" ]
            [ div [ class "item label" ] [ text <| translate language Translate.Impacted ]
            , div [ class "item value" ] [ text <| translate language Translate.Male ]
            , div [ class "item value" ] [ text <| translate language Translate.Female ]
            ]
        , viewRow "0 - 1M" malesImpacted1MonthAndLess femalesImpacted1MonthAndLess
        , viewRow "1M - 2Y" malesImpacted1Month2Years femalesImpacted1Month2Years
        , viewRow "2Y - 5Y" malesImpacted2Years5Years femalesImpacted2Years5Years
        , viewRow "5Y - 10Y" malesImpacted5Years10Years femalesImpacted5Years10Years
        , viewRow "10Y - 20Y" malesImpacted10Years20Years femalesImpacted10Years20Years
        , viewRow "20Y - 50Y" malesImpacted20Years50Years femalesImpacted20Years50Years
        , viewRow "50Y +" malesImpacted50YearsOrMore femalesImpacted50YearsOrMore
        , div [ class "row totals" ]
            [ div [ class "item label" ] [ text <| translate language Translate.Total ]
            , div [ class "item value" ] [ text <| String.fromInt <| List.length patientsImpacted ]
            ]
        ]
    ]


viewDemographicsReportEncounters : Language -> List PatientData -> List (Html Msg)
viewDemographicsReportEncounters language records =
    let
        prenatalDataNurseEncounters =
            List.filterMap
                (.prenatalData
                    >> Maybe.map
                        (List.concat
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
                        (List.concat
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
                + overallNutritionTotal

        overallUnique =
            prenatalDataNurseEncountersUnique
                + prenatalDataChwEncountersUnique
                + acuteIllnessDataNurseEncountersUnique
                + acuteIllnessDataChwEncountersUnique
                + wellChildDataEncountersUnique
                + homeVisitDataEncountersUnique
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


viewNutritionReport : Language -> NominalDate -> List PatientData -> Html Msg
viewNutritionReport language limitDate records =
    let
        metricsFor2021 =
            List.filter
                (\record ->
                    Date.diff Years record.birthDate limitDate < 6
                )
                records
                |> List.map
                    (\record ->
                        let
                            filterIndividualBy resolveDateFunc =
                                Maybe.map
                                    (List.map
                                        (List.filter
                                            (\encounterData ->
                                                Date.year (resolveDateFunc encounterData) == 2021
                                            )
                                        )
                                    )

                            filterGroupBy resolveDateFunc =
                                Maybe.map
                                    (List.filter
                                        (\encounterData ->
                                            Date.year (resolveDateFunc encounterData) == 2021
                                        )
                                    )
                        in
                        { record
                            | wellChildData = filterIndividualBy .startDate record.wellChildData
                            , individualNutritionData = filterIndividualBy .startDate record.individualNutritionData
                            , groupNutritionPmtctData = filterGroupBy .startDate record.groupNutritionPmtctData
                            , groupNutritionFbfData = filterGroupBy .startDate record.groupNutritionFbfData
                            , groupNutritionSorwatheData = filterGroupBy .startDate record.groupNutritionSorwatheData
                            , groupNutritionChwData = filterGroupBy .startDate record.groupNutritionChwData
                            , groupNutritionAchiData = filterGroupBy .startDate record.groupNutritionAchiData
                        }
                    )
                |> List.map calcualteNutritionMetricsForPatient
                |> sumNutritionMetrics
                |> Debug.log "all"

        stuntungTotal =
            metricsFor2021.stuntingNormal
                + metricsFor2021.stuntingModerate
                + metricsFor2021.stuntingSevere
                |> Debug.log "stuntungTotal"
    in
    div [ class "report nutrition" ]
        [ text "" ]
