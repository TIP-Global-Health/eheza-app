module Pages.Scoreboard.View exposing (view)

import App.Types exposing (Language)
import AssocList as Dict exposing (Dict)
import Gizra.Html exposing (emptyNode, showIf)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick, onInput)
import Icons
import Maybe.Extra exposing (isJust, isNothing)
import Pages.Scoreboard.Model exposing (..)
import Pages.Scoreboard.Utils exposing (..)
import Pages.Utils exposing (emptySelectOption, viewActionButton, viewLabel)
import Restful.Endpoint exposing (fromEntityId, toEntityId)
import Time exposing (Month(..))
import Translate exposing (TranslationId, translate)
import Utils.GeoLocation exposing (GeoLocationId, filterGeoLocationDictByParent, geoInfo, geoLocationDictToOptions)


view : Language -> Model -> Html Msg
view language model =
    case model.displayMode of
        DisplayViewSelection ->
            viewDisplayViewSelection language model

        DisplayResultTable value ->
            viewDisplayResultTable language value model


viewDisplayViewSelection : Language -> Model -> Html Msg
viewDisplayViewSelection language model =
    let
        provinceInput =
            let
                options =
                    geoLocationDictToOptions geoInfo.provinces
            in
            viewSelectListInput language
                model.form.province
                options
                (SetGeoLocation
                    (\value form ->
                        { form
                            | province =
                                String.toInt value |> Maybe.map toEntityId
                        }
                    )
                )
                Translate.Province
                (isJust model.form.district)

        districtInput =
            Maybe.map
                (\parentId ->
                    let
                        options =
                            filterGeoLocationDictByParent (fromEntityId parentId) geoInfo.districts
                                |> geoLocationDictToOptions
                    in
                    viewSelectListInput language
                        model.form.district
                        options
                        (SetGeoLocation
                            (\value form ->
                                { form
                                    | district = String.toInt value |> Maybe.map toEntityId
                                }
                            )
                        )
                        Translate.District
                        (isJust model.form.sector)
                )
                model.form.province
                |> Maybe.withDefault emptyNode

        sectorInput =
            Maybe.map
                (\parentId ->
                    let
                        options =
                            filterGeoLocationDictByParent (fromEntityId parentId) geoInfo.sectors
                                |> geoLocationDictToOptions
                    in
                    viewSelectListInput language
                        model.form.sector
                        options
                        (SetGeoLocation
                            (\value form ->
                                { form
                                    | sector = String.toInt value |> Maybe.map toEntityId
                                }
                            )
                        )
                        Translate.Sector
                        (isJust model.form.cell)
                )
                model.form.district
                |> Maybe.withDefault emptyNode

        cellInput =
            Maybe.map
                (\parentId ->
                    let
                        options =
                            filterGeoLocationDictByParent (fromEntityId parentId) geoInfo.cells
                                |> geoLocationDictToOptions
                    in
                    viewSelectListInput language
                        model.form.cell
                        options
                        (SetGeoLocation
                            (\value form ->
                                { form
                                    | cell = String.toInt value |> Maybe.map toEntityId
                                }
                            )
                        )
                        Translate.Cell
                        (isJust model.form.village)
                )
                model.form.sector
                |> Maybe.withDefault emptyNode

        villageInput =
            Maybe.map
                (\parentId ->
                    let
                        options =
                            filterGeoLocationDictByParent (fromEntityId parentId) geoInfo.villages
                                |> geoLocationDictToOptions
                    in
                    viewSelectListInput language
                        model.form.village
                        options
                        (SetGeoLocation
                            (\value form ->
                                { form
                                    | village = String.toInt value |> Maybe.map toEntityId
                                }
                            )
                        )
                        Translate.Village
                        False
                )
                model.form.cell
                |> Maybe.withDefault emptyNode
    in
    div [ class "page-content" ] <|
        [ div [ class "header" ] [ text "Please select desired view mode:" ]
        , div [ class "inputs" ]
            [ provinceInput
            , districtInput
            , sectorInput
            , cellInput
            , villageInput
            ]
        , viewActionButton language Translate.GenerateReport True GenerateReport
            |> showIf (isJust model.form.province && isJust model.form.district)
        ]


viewSelectListInput :
    Language
    -> Maybe GeoLocationId
    -> List ( String, String )
    -> (String -> Msg)
    -> TranslationId
    -> Bool
    -> Html Msg
viewSelectListInput language currentValue options setMsg labelTransId disabled =
    let
        selectOptions =
            emptyOption
                :: List.map
                    (\option_ ->
                        let
                            isSelected =
                                Tuple.first option_
                                    |> String.toInt
                                    |> Maybe.map
                                        (\id ->
                                            currentValue == (Just <| toEntityId id)
                                        )
                                    |> Maybe.withDefault False
                        in
                        option
                            [ value <| Tuple.first option_
                            , selected isSelected
                            ]
                            [ text <| Tuple.second option_ ]
                    )
                    options

        emptyOption =
            emptySelectOption (currentValue == Nothing)
    in
    div
        [ classList
            [ ( "select-input-wrapper", True )
            , ( "disabled", disabled )
            ]
        ]
        [ viewLabel language labelTransId
        , select
            [ onInput setMsg
            , class "select-input"
            ]
            selectOptions
        ]


viewDisplayResultTable : Language -> ViewSelectionValue -> Model -> Html Msg
viewDisplayResultTable language value model =
    let
        ( entityId, entityType ) =
            case value.village of
                Just id ->
                    ( id, EntityVillage )

                Nothing ->
                    case value.cell of
                        Just id ->
                            ( id, EntityCell )

                        Nothing ->
                            case value.sector of
                                Just id ->
                                    ( id, EntitySector )

                                Nothing ->
                                    ( value.district, EntityDistrict )
    in
    div [ class "page-content" ]
        [ viewAggregatedChildScoreboardPane language ( entityId, entityType )
        , viewDemographicsPane language entityType
        , viewAcuteMalnutritionPane language entityType
        , viewStuntingPane language entityType
        ]


viewAggregatedChildScoreboardPane :
    Language
    -> ( GeoLocationId, SelectedEntity )
    -> Html any
viewAggregatedChildScoreboardPane language ( entityId, entityType ) =
    let
        entityName =
            case entityType of
                EntityDistrict ->
                    resolveEnityName entityId geoInfo.districts

                EntitySector ->
                    resolveEnityName entityId geoInfo.sectors

                EntityCell ->
                    resolveEnityName entityId geoInfo.cells

                EntityVillage ->
                    resolveEnityName entityId geoInfo.villages

        resolveEnityName id dict =
            Dict.get id dict
                |> Maybe.map .name
                |> Maybe.withDefault ""
    in
    div [ class "pane" ]
        [ viewPaneHeading language Translate.AggregatedChildScoreboard
        , div [ class "pane-content" ]
            [ div []
                [ span [ class "selected-entity" ] [ text <| (translate language <| Translate.SelectedEntity entityType) ++ ":" ]
                , span [] [ text entityName ]
                ]
            ]
        ]


viewDemographicsPane : Language -> SelectedEntity -> Html any
viewDemographicsPane language entityType =
    let
        rows =
            List.map2
                (\item itemValues ->
                    viewTableRow language (Translate.NCDADemographicsItemLabel item) itemValues
                )
                [ ChildrenUnder2, NewbornsThisMonth, LowBirthWeigh ]
                values

        values =
            case entityType of
                EntityVillage ->
                    [ [ 12, 12, 14, 13, 15, 15, 15, 12, 13, 13, 14, 14 ]
                    , [ 11, 11, 17, 15, 16, 16, 16, 11, 15, 15, 17, 17 ]
                    , [ 5, 8, 6, 7, 1, 4, 3, 5, 8, 3, 1, 6 ]
                    ]

                EntityCell ->
                    [ [ 98, 98, 122, 100, 173, 173, 173, 98, 100, 100, 122, 122 ]
                    , [ 97, 97, 126, 106, 176, 176, 176, 97, 102, 102, 132, 132 ]
                    , [ 25, 34, 32, 21, 23, 34, 45, 13, 34, 56, 12, 34 ]
                    ]

                EntitySector ->
                    [ [ 203, 203, 239, 220, 256, 256, 256, 203, 220, 220, 239, 239 ]
                    , [ 205, 205, 238, 227, 266, 266, 266, 205, 227, 227, 238, 238 ]
                    , [ 145, 146, 124, 145, 124, 145, 123, 145, 134, 135, 123, 234 ]
                    ]

                EntityDistrict ->
                    [ [ 530, 530, 491, 455, 640, 640, 640, 530, 455, 455, 491, 491 ]
                    , [ 531, 531, 516, 455, 640, 640, 640, 531, 455, 455, 516, 516 ]
                    , [ 345, 345, 356, 455, 214, 256, 289, 278, 267, 256, 256, 245 ]
                    ]
    in
    div [ class "pane cyan" ]
        [ viewPaneHeading language Translate.Demographics
        , div [ class "pane-content" ] <|
            viewTableHeader language
                :: rows
        ]


viewAcuteMalnutritionPane : Language -> SelectedEntity -> Html any
viewAcuteMalnutritionPane language entityType =
    let
        rows =
            List.map2
                (\item itemValues ->
                    viewTableRow language (Translate.NCDAAcuteMalnutritionItemLabel item) itemValues
                )
                [ SevereAcuteMalnutrition, ModerateAcuteMalnutrition, GoodNutrition ]
                values

        values =
            case entityType of
                EntityVillage ->
                    [ [ 11, 17, 19, 15, 15, 7, 8, 12, 11, 17, 11, 12 ]
                    , [ 3, 8, 2, 0, 7, 6, 1, 5, 9, 4, 2, 3 ]
                    , [ 9, 6, 2, 8, 12, 1, 25, 3, 24, 5, 7, 11 ]
                    ]

                EntityCell ->
                    [ [ 98, 129, 100, 123, 112, 145, 173, 98, 145, 134, 135, 122 ]
                    , [ 98, 98, 122, 100, 173, 173, 173, 98, 100, 100, 122, 122 ]
                    , [ 35, 72, 98, 41, 84, 63, 52, 77, 96, 88, 55, 47 ]
                    ]

                EntitySector ->
                    [ [ 203, 257, 234, 245, 245, 256, 124, 145, 124, 145, 239, 240 ]
                    , [ 203, 203, 239, 220, 256, 256, 256, 203, 220, 220, 239, 239 ]
                    , [ 213, 243, 239, 221, 246, 236, 266, 223, 229, 221, 229, 234 ]
                    ]

                EntityDistrict ->
                    [ [ 491, 455, 640, 678, 524, 491, 545, 640, 563, 640, 455, 491 ]
                    , [ 530, 530, 491, 455, 640, 640, 640, 530, 455, 455, 491, 491 ]
                    , [ 223, 569, 854, 732, 988, 622, 901, 775, 666, 444, 888, 998 ]
                    ]
    in
    div [ class "pane orange" ]
        [ viewPaneHeading language Translate.AcuteMalnutrition
        , div [ class "pane-content" ] <|
            viewTableHeader language
                :: rows
        ]


viewStuntingPane : Language -> SelectedEntity -> Html any
viewStuntingPane language entityType =
    let
        rows =
            List.map2
                (\item itemValues ->
                    viewTableRow language (Translate.NCDAStuntingItemLabel item) itemValues
                )
                [ SevereStunting, ModerateStunting, NoStunting ]
                values

        values =
            case entityType of
                EntityVillage ->
                    [ [ 23, 21, 17, 14, 9, 12, 18, 21, 16, 13, 19, 22 ]
                    , [ 8, 14, 7, 18, 13, 17, 12, 15, 19, 16, 11, 10 ]
                    , [ 19, 23, 18, 13, 15, 21, 14, 17, 22, 16, 11, 20 ]
                    ]

                EntityCell ->
                    [ [ 153, 129, 102, 124, 148, 115, 149, 178, 162, 148, 161, 138 ]
                    , [ 102, 125, 136, 129, 149, 131, 125, 117, 144, 146, 137, 108 ]
                    , [ 116, 123, 151, 135, 112, 141, 152, 126, 123, 135, 146, 148 ]
                    ]

                EntitySector ->
                    [ [ 270, 245, 214, 231, 265, 238, 249, 218, 221, 267, 236, 260 ]
                    , [ 246, 269, 240, 232, 258, 215, 207, 236, 274, 252, 214, 233 ]
                    , [ 238, 245, 214, 260, 219, 231, 241, 237, 218, 238, 255, 261 ]
                    ]

                EntityDistrict ->
                    [ [ 605, 596, 562, 640, 621, 546, 661, 592, 635, 539, 587, 612 ]
                    , [ 595, 581, 562, 605, 656, 576, 593, 635, 625, 655, 620, 575 ]
                    , [ 604, 642, 553, 655, 577, 622, 600, 571, 598, 621, 542, 596 ]
                    ]
    in
    div [ class "pane velvet" ]
        [ viewPaneHeading language Translate.Stunting
        , div [ class "pane-content" ] <|
            viewTableHeader language
                :: rows
        ]



--
-- viewANCNewbornPane :
--     Language
--     -> NominalDate
--     -> ModelIndexedDb
--     -> Person
--     -> List ( NominalDate, NCDAValue )
--     -> Html any
-- viewANCNewbornPane language currentDate db child allNCDAQuestionnaires =
--     let
--         pregnancyValues =
--             List.repeat 9 NCDACellValueEmpty
--
--         pregnancyValuesForANCSign sign =
--             if List.isEmpty allNCDAQuestionnaires then
--                 List.repeat 9 NCDACellValueDash
--
--             else
--                 let
--                     signConfirmed =
--                         List.any (\( _, value ) -> EverySet.member sign value.signs) allNCDAQuestionnaires
--                 in
--                 if signConfirmed then
--                     List.repeat 9 NCDACellValueV
--
--                 else
--                     List.repeat 9 NCDACellValueX
--
--         zeroToFiveValues =
--             List.repeat 6 NCDACellValueDash
--
--         sixToTwentyFourValues =
--             List.repeat 19 NCDACellValueDash
--     in
--     div [ class "pane anc-newborn" ]
--         [ viewPaneHeading language Translate.ANCNewborn
--         , div [ class "pane-content" ]
--             [ viewTableHeader
--             , viewTableRow language
--                 (Translate.NCDAANCNewbornItemLabel RegularCheckups)
--                 (pregnancyValuesForANCSign NCDARegularPrenatalVisits)
--                 zeroToFiveValues
--                 sixToTwentyFourValues
--             , viewTableRow language
--                 (Translate.NCDAANCNewbornItemLabel IronDuringPregnancy)
--                 (pregnancyValuesForANCSign NCDAIronSupplementsDuringPregnancy)
--                 zeroToFiveValues
--                 sixToTwentyFourValues
--             ]
--         ]
--
--
--
--
-- viewNutritionBehaviorPane :
--     Language
--     -> NominalDate
--     -> Person
--     -> Maybe (Dict Int NCDAValue)
--     -> Html any
-- viewNutritionBehaviorPane language currentDate child questionnairesByAgeInMonths =
--     let
--         pregnancyValues =
--             List.repeat 9 NCDACellValueDash
--
--         breastfedForSixMonthsValues =
--             generateValues currentDate child questionnairesByAgeInMonths (.signs >> EverySet.member NCDABreastfedForSixMonths)
--
--         appropriateComplementaryFeedingValues =
--             generateValues currentDate child questionnairesByAgeInMonths (.signs >> EverySet.member NCDAAppropriateComplementaryFeeding)
--
--         mealsADayValues =
--             generateValues currentDate
--                 child
--                 questionnairesByAgeInMonths
--                 (\questionnaire ->
--                     List.any (\sign -> EverySet.member sign questionnaire.signs)
--                         [ NCDAMealFrequency6to8Months
--                         , NCDAMealFrequency9to11Months
--                         , NCDAMealFrequency12MonthsOrMore
--                         ]
--                 )
--
--         diverseDietValues =
--             generateValues currentDate child questionnairesByAgeInMonths (.signs >> EverySet.member NCDAFiveFoodGroups)
--
--         -- Here we are interested only at answer given when child was 6 months old.
--         -- For months before that, and after, will show dahses, in case child has
--         -- reached the age for which value is given (empty value otherwise).
--         ( breastfedForSixMonthsFirstPeriod, breastfedForSixMonthsSecondPeriod ) =
--             let
--                 firstPeriod =
--                     List.take 6 breastfedForSixMonthsValues
--                         |> List.map setDashIfNotEmpty
--
--                 secondPeriod =
--                     let
--                         generated =
--                             List.drop 6 breastfedForSixMonthsValues
--                     in
--                     List.take 1 generated
--                         ++ (List.drop 1 generated
--                                 |> List.map setDashIfNotEmpty
--                            )
--             in
--             ( firstPeriod, secondPeriod )
--
--         -- Here we are interested only at answer given when child has reached
--         -- the age of 7 months.
--         -- For prior period we show dahses, in case child has reached
--         -- the age for which value is given (empty value otherwise).
--         ( appropriateComplementaryFeedingFirstPeriod, appropriateComplementaryFeedingSecondPeriod ) =
--             let
--                 firstPeriod =
--                     List.take 6 appropriateComplementaryFeedingValues
--                         |> List.map setDashIfNotEmpty
--
--                 secondPeriod =
--                     let
--                         generated =
--                             List.drop 6 appropriateComplementaryFeedingValues
--                     in
--                     (List.take 1 generated
--                         |> List.map setDashIfNotEmpty
--                     )
--                         ++ List.drop 1 generated
--             in
--             ( firstPeriod, secondPeriod )
--
--         -- generateValues() may generate values at certain periods that are
--         -- not relevant, which we want to replace them with dashes.
--         -- However, if child has not yeat reach the age of month for which
--         -- value is presented, generateValues() will preperly set
--         -- NCDACellValueEmpty there, and we want to keep it.
--         setDashIfNotEmpty value =
--             if value == NCDACellValueEmpty then
--                 value
--
--             else
--                 NCDACellValueDash
--     in
--     div [ class "pane nutrition-behavior" ]
--         [ viewPaneHeading language Translate.NutritionBehavior
--         , div [ class "pane-content" ]
--             [ viewTableHeader
--             , viewTableRow language
--                 (Translate.NCDANutritionBehaviorItemLabel BreastfedSixMonths)
--                 pregnancyValues
--                 breastfedForSixMonthsFirstPeriod
--                 breastfedForSixMonthsSecondPeriod
--             , viewTableRow language
--                 (Translate.NCDANutritionBehaviorItemLabel AppropriateComplementaryFeeding)
--                 pregnancyValues
--                 appropriateComplementaryFeedingFirstPeriod
--                 appropriateComplementaryFeedingSecondPeriod
--             , viewTableRow language
--                 (Translate.NCDANutritionBehaviorItemLabel DiverseDiet)
--                 pregnancyValues
--                 (List.take 6 diverseDietValues)
--                 (List.drop 6 diverseDietValues)
--             , viewTableRow language
--                 (Translate.NCDANutritionBehaviorItemLabel MealsADay)
--                 pregnancyValues
--                 (List.take 6 mealsADayValues)
--                 (List.drop 6 mealsADayValues)
--             ]
--         ]
--
--
-- viewInfrastructureEnvironmentWashPane :
--     Language
--     -> NominalDate
--     -> Person
--     -> Maybe (Dict Int NCDAValue)
--     -> Html any
-- viewInfrastructureEnvironmentWashPane language currentDate child questionnairesByAgeInMonths =
--     let
--         pregnancyValues =
--             List.repeat 9 NCDACellValueDash
--
--         hasToilets =
--             generateValues currentDate child questionnairesByAgeInMonths (.signs >> EverySet.member NCDAHasToilets)
--
--         hasCleanWater =
--             generateValues currentDate child questionnairesByAgeInMonths (.signs >> EverySet.member NCDAHasCleanWater)
--
--         hasHandwashingFacility =
--             generateValues currentDate child questionnairesByAgeInMonths (.signs >> EverySet.member NCDAHasHandwashingFacility)
--
--         hasKitchenGarden =
--             generateValues currentDate child questionnairesByAgeInMonths (.signs >> EverySet.member NCDAHasKitchenGarden)
--
--         insecticideTreatedBedNets =
--             let
--                 byMonths =
--                     generateValues currentDate
--                         child
--                         questionnairesByAgeInMonths
--                         (.signs >> EverySet.member NCDAInsecticideTreatedBednetsDuringPregnancy)
--
--                 answer =
--                     List.foldl
--                         (\cellValue answerSoFar ->
--                             if List.member cellValue [ NCDACellValueV, NCDACellValueX ] then
--                                 Just cellValue
--
--                             else
--                                 answerSoFar
--                         )
--                         Nothing
--                         byMonths
--             in
--             -- This question is asked once. If answer was given,
--             -- we display it throughout the whole period.
--             Maybe.map
--                 (\answer_ ->
--                     List.map
--                         (\monthValue ->
--                             if monthValue /= NCDACellValueEmpty then
--                                 answer_
--
--                             else
--                                 NCDACellValueEmpty
--                         )
--                         byMonths
--                 )
--                 answer
--                 |> Maybe.withDefault byMonths
--     in
--     div [ class "pane infrastructure-environment-wash" ]
--         [ viewPaneHeading language Translate.InfrastructureEnvironmentWash
--         , div [ class "pane-content" ]
--             [ viewTableHeader
--             , viewTableRow language
--                 (Translate.NCDAInfrastructureEnvironmentWashItemLabel HasToilets)
--                 pregnancyValues
--                 (List.take 6 hasToilets)
--                 (List.drop 6 hasToilets)
--             , viewTableRow language
--                 (Translate.NCDAInfrastructureEnvironmentWashItemLabel HasCleanWater)
--                 pregnancyValues
--                 (List.take 6 hasCleanWater)
--                 (List.drop 6 hasCleanWater)
--             , viewTableRow language
--                 (Translate.NCDAInfrastructureEnvironmentWashItemLabel HasHandwashingFacility)
--                 pregnancyValues
--                 (List.take 6 hasHandwashingFacility)
--                 (List.drop 6 hasHandwashingFacility)
--             , viewTableRow language
--                 (Translate.NCDAInfrastructureEnvironmentWashItemLabel InsecticideTreatedBedNets)
--                 pregnancyValues
--                 (List.take 6 insecticideTreatedBedNets)
--                 (List.drop 6 insecticideTreatedBedNets)
--             , viewTableRow language
--                 (Translate.NCDAInfrastructureEnvironmentWashItemLabel HasKitchenGarden)
--                 pregnancyValues
--                 (List.take 6 hasKitchenGarden)
--                 (List.drop 6 hasKitchenGarden)
--             ]
--         ]
--
--
-- viewTargetedInterventionsPane :
--     Language
--     -> NominalDate
--     -> Person
--     -> ModelIndexedDb
--     -> Maybe (Dict Int NCDAValue)
--     -> ChildMeasurementList
--     -> List ( NominalDate, ( NutritionEncounterId, NutritionMeasurements ) )
--     -> List ( NominalDate, ( WellChildEncounterId, WellChildMeasurements ) )
--     -> List ( IndividualEncounterParticipantId, IndividualEncounterParticipant )
--     -> Html any
-- viewTargetedInterventionsPane language currentDate child db questionnairesByAgeInMonths groupNutritionMeasurements individualNutritionMeasurementsWithDates individualWellChildMeasurementsWithDates acuteIllnesses =
--     let
--         pregnancyValues =
--             List.repeat 9 NCDACellValueDash
--
--         fbfsByAgeInMonths =
--             Dict.values groupNutritionMeasurements.fbfs
--                 |> List.map
--                     (\fbf ->
--                         if fbf.value.distributedAmount > 0 then
--                             ( fbf.dateMeasured, NCDACellValueV )
--
--                         else
--                             ( fbf.dateMeasured, NCDACellValueX )
--                     )
--                 |> distributeByAgeInMonths child
--
--         malnutritionTreatmentsByAgeInMonths =
--             groupMalnutritionTreatmentData
--                 ++ individualMalnutritionTreatmentData
--                 |> distributeByAgeInMonths child
--
--         groupMalnutritionTreatmentData =
--             let
--                 malnutritionAssessmentDates =
--                     Dict.values groupNutritionMeasurements.nutritions
--                         |> List.filterMap
--                             (\nutrition ->
--                                 if
--                                     List.any (\assessment -> EverySet.member assessment nutrition.value.assesment)
--                                         [ AssesmentAcuteMalnutritionModerate
--                                         , AssesmentAcuteMalnutritionSevere
--                                         ]
--                                 then
--                                     Just nutrition.dateMeasured
--
--                                 else
--                                     Nothing
--                             )
--             in
--             Dict.values groupNutritionMeasurements.sendToHC
--                 |> List.filterMap
--                     (\sendToHC ->
--                         if
--                             -- Sent to HC measurement was taken on same day
--                             -- malnutrition assessment was made.
--                             List.member sendToHC.dateMeasured malnutritionAssessmentDates
--                         then
--                             if EverySet.member ReferToHealthCenter sendToHC.value.signs then
--                                 Just ( sendToHC.dateMeasured, NCDACellValueV )
--
--                             else
--                                 Just ( sendToHC.dateMeasured, NCDACellValueX )
--
--                         else
--                             Nothing
--                     )
--
--         individualMalnutritionTreatmentData =
--             generateIndividualMalnutritionTreatmentData individualNutritionMeasurementsWithDates
--                 ++ generateIndividualMalnutritionTreatmentData individualWellChildMeasurementsWithDates
--
--         generateIndividualMalnutritionTreatmentData measurementsWithDates =
--             List.filterMap
--                 (\( date, ( _, measurements ) ) ->
--                     getMeasurementValueFunc measurements.nutrition
--                         |> Maybe.andThen
--                             (\nutritionValue ->
--                                 if
--                                     List.any (\assessment -> EverySet.member assessment nutritionValue.assesment)
--                                         [ AssesmentAcuteMalnutritionModerate
--                                         , AssesmentAcuteMalnutritionSevere
--                                         ]
--                                 then
--                                     getMeasurementValueFunc measurements.sendToHC
--                                         |> Maybe.map
--                                             (\sendToHCValue ->
--                                                 if EverySet.member ReferToHealthCenter sendToHCValue.signs then
--                                                     ( date, NCDACellValueV )
--
--                                                 else
--                                                     ( date, NCDACellValueX )
--                                             )
--
--                                 else
--                                     Nothing
--                             )
--                 )
--                 measurementsWithDates
--
--         diarrheaTreatmenByAgeInMonths =
--             Maybe.andThen
--                 (\birthDate ->
--                     List.filter
--                         (\( participantId, participant ) ->
--                             diffMonths birthDate participant.startDate < 24
--                         )
--                         acuteIllnesses
--                         |> List.map
--                             (\( participantId, participant ) ->
--                                 Dict.get participantId db.acuteIllnessEncountersByParticipant
--                                     |> Maybe.andThen RemoteData.toMaybe
--                                     |> Maybe.map
--                                         (Dict.toList
--                                             >> List.filterMap
--                                                 (\( encounterId, encounter ) ->
--                                                     -- We need to fetch measurements of encounters where Uncomplicated
--                                                     -- Gastrointestinal Infection was diagnosed, to check if treatment was given.
--                                                     if encounter.diagnosis == DiagnosisGastrointestinalInfectionUncomplicated then
--                                                         Dict.get encounterId db.acuteIllnessMeasurements
--                                                             |> Maybe.andThen RemoteData.toMaybe
--                                                             |> Maybe.andThen
--                                                                 (.medicationDistribution
--                                                                     >> getMeasurementValueFunc
--                                                                     >> Maybe.map
--                                                                         (\value ->
--                                                                             if
--                                                                                 List.any (\sign -> EverySet.member sign value.distributionSigns)
--                                                                                     [ ORS, Zinc ]
--                                                                             then
--                                                                                 ( encounter.startDate, NCDACellValueV )
--
--                                                                             else
--                                                                                 ( encounter.startDate, NCDACellValueX )
--                                                                         )
--                                                                 )
--
--                                                     else
--                                                         Nothing
--                                                 )
--                                         )
--                                     |> Maybe.withDefault []
--                             )
--                         |> List.concat
--                         |> distributeByAgeInMonths child
--                 )
--                 child.birthDate
--
--         fbfValues =
--             generateValues currentDate child fbfsByAgeInMonths ((==) NCDACellValueV)
--
--         malnutritionTreatmentValues =
--             generateValues currentDate child malnutritionTreatmentsByAgeInMonths ((==) NCDACellValueV)
--
--         diarrheaTreatmentValues =
--             generateValues currentDate child diarrheaTreatmenByAgeInMonths ((==) NCDACellValueV)
--
--         supportChildWithDisabilityValues =
--             generateValues currentDate child questionnairesByAgeInMonths (.signs >> EverySet.member NCDASupportChildWithDisability)
--
--         conditionalCashTransferValues =
--             generateValues currentDate child questionnairesByAgeInMonths (.signs >> EverySet.member NCDAConditionalCashTransfer)
--
--         conditionalFoodItemsValues =
--             generateValues currentDate child questionnairesByAgeInMonths (.signs >> EverySet.member NCDAConditionalFoodItems)
--     in
--     div [ class "pane targeted-interventions" ]
--         [ viewPaneHeading language Translate.TargetedInterventions
--         , div [ class "pane-content" ]
--             [ viewTableHeader
--             , viewTableRow language
--                 (Translate.NCDATargetedInterventionsItemLabel FBFGiven)
--                 pregnancyValues
--                 (List.take 6 fbfValues)
--                 (List.drop 6 fbfValues)
--             , viewTableRow language
--                 (Translate.NCDATargetedInterventionsItemLabel TreatmentForAcuteMalnutrition)
--                 pregnancyValues
--                 (List.take 6 malnutritionTreatmentValues)
--                 (List.drop 6 malnutritionTreatmentValues)
--             , viewTableRow language
--                 (Translate.NCDATargetedInterventionsItemLabel TreatmentForDiarrhea)
--                 pregnancyValues
--                 (List.take 6 diarrheaTreatmentValues)
--                 (List.drop 6 diarrheaTreatmentValues)
--             , viewTableRow language
--                 (Translate.NCDATargetedInterventionsItemLabel SupportChildWithDisability)
--                 pregnancyValues
--                 (List.take 6 supportChildWithDisabilityValues)
--                 (List.drop 6 supportChildWithDisabilityValues)
--             , viewTableRow language
--                 (Translate.NCDATargetedInterventionsItemLabel ConditionalCashTransfer)
--                 pregnancyValues
--                 (List.take 6 conditionalCashTransferValues)
--                 (List.drop 6 conditionalCashTransferValues)
--             , viewTableRow language
--                 (Translate.NCDATargetedInterventionsItemLabel ConditionalFoodItems)
--                 pregnancyValues
--                 (List.take 6 conditionalFoodItemsValues)
--                 (List.drop 6 conditionalFoodItemsValues)
--             ]
--         ]
--
--
-- viewUniversalInterventionsPane :
--     Language
--     -> NominalDate
--     -> Person
--     -> ModelIndexedDb
--     -> Maybe (Dict Int NCDAValue)
--     -> Maybe AssembledData
--     -> List ( WellChildEncounterId, WellChildEncounter )
--     -> List ( NominalDate, ( WellChildEncounterId, WellChildMeasurements ) )
--     -> Html any
-- viewUniversalInterventionsPane language currentDate child db questionnairesByAgeInMonths maybeAssembled wellChildEncounters individualWellChildMeasurementsWithDates =
--     let
--         pregnancyValues =
--             List.repeat 9 NCDACellValueDash
--
--         immunizationByAgeInMonths =
--             Maybe.andThen
--                 (\birthDate ->
--                     Maybe.map
--                         (\assembled ->
--                             List.repeat 25 ""
--                                 |> List.indexedMap
--                                     (\index _ ->
--                                         let
--                                             referenceDate =
--                                                 -- We use it to determine if child was
--                                                 -- behind on any of vaccines at that month.
--                                                 resolveLastDayForMonthX (index + 1) birthDate
--
--                                             -- Filter out vaccinations that were performed
--                                             -- after the reference date.
--                                             vaccinationProgressOnReferrenceDate =
--                                                 Dict.map
--                                                     (\vaccineType dosesDict ->
--                                                         Dict.filter
--                                                             (\dose administeredDate ->
--                                                                 Date.compare administeredDate referenceDate == LT
--                                                             )
--                                                             dosesDict
--                                                     )
--                                                     assembled.vaccinationProgress
--
--                                             futureVaccinations =
--                                                 generateFutureVaccinationsData currentDate child False vaccinationProgressOnReferrenceDate
--
--                                             closestDateForVaccination =
--                                                 List.filterMap (Tuple.second >> Maybe.map Tuple.second) futureVaccinations
--                                                     |> List.sortWith Date.compare
--                                                     |> List.head
--                                         in
--                                         Maybe.map
--                                             (\closestDate ->
--                                                 if Date.compare closestDate referenceDate == GT then
--                                                     -- Closest date when vaccine is required is after
--                                                     -- current month, which means that att current month
--                                                     -- we're not behind on vaccination.
--                                                     ( referenceDate, NCDACellValueV )
--
--                                                 else
--                                                     ( referenceDate, NCDACellValueX )
--                                             )
--                                             closestDateForVaccination
--                                             |> Maybe.withDefault
--                                                 -- This indicates that there're no future vaccinations to be
--                                                 -- done, and therefore, we're on track at current month.
--                                                 ( referenceDate, NCDACellValueV )
--                                     )
--                         )
--                         maybeAssembled
--                         |> Maybe.withDefault
--                             -- We get here if there were no SPV encounters performed,
--                             -- which means that no vaccinations were recorded.
--                             -- Therefore, we're for sure behind on vaccinations
--                             -- for any given month.
--                             (List.repeat 25 ""
--                                 |> List.indexedMap
--                                     (\index _ ->
--                                         ( resolveLastDayForMonthX (index + 1) birthDate
--                                         , NCDACellValueX
--                                         )
--                                     )
--                             )
--                         |> distributeByAgeInMonths child
--                 )
--                 child.birthDate
--
--         resolveLastDayForMonthX monthX childBirthDate =
--             -- This is the date for last day of month X.
--             -- For example, for X = 0, this is
--             -- the last day, before child turns 1 month old.
--             Date.add Date.Months monthX childBirthDate
--                 |> Date.add Date.Days -1
--
--         vitaminAByAgeInMonths =
--             Maybe.andThen Tuple.first medicineByAgeInMonths
--
--         dewormerByAgeInMonths =
--             Maybe.andThen Tuple.second medicineByAgeInMonths
--
--         medicineByAgeInMonths =
--             Maybe.map
--                 (\assembled ->
--                     let
--                         generateMeasurementValues measurementFunc =
--                             List.filterMap
--                                 (measurementFunc
--                                     >> Maybe.map
--                                         (\( _, measurement ) ->
--                                             ( measurement.dateMeasured, measurement.value )
--                                         )
--                                 )
--                                 allMeasurements
--
--                         allMeasurements =
--                             assembled.measurements
--                                 :: List.map (Tuple.second >> Tuple.second)
--                                     assembled.previousMeasurementsWithDates
--                     in
--                     ( generateMeasurementValues .vitaminA
--                         |> distributeByAgeInMonths child
--                     , generateMeasurementValues .mebendezole
--                         |> distributeByAgeInMonths child
--                     )
--                 )
--                 maybeAssembled
--
--         immunizationValues =
--             generateValues currentDate child immunizationByAgeInMonths ((==) NCDACellValueV)
--
--         vitaminAValues =
--             let
--                 administeredMonths =
--                     List.indexedMap
--                         (\index value ->
--                             if value == NCDACellValueV then
--                                 Just index
--
--                             else
--                                 Nothing
--                         )
--                         rawValues
--                         |> Maybe.Extra.values
--
--                 rawValues =
--                     generateValues currentDate child vitaminAByAgeInMonths ((==) AdministeredToday)
--             in
--             List.indexedMap
--                 -- Vitamin A is not administered before age of 6 months.
--                 (postProcessMedicineRawValue 6 administeredMonths)
--                 rawValues
--
--         dewormerValues =
--             let
--                 administeredMonths =
--                     List.indexedMap
--                         (\index value ->
--                             if value == NCDACellValueV then
--                                 Just index
--
--                             else
--                                 Nothing
--                         )
--                         rawValues
--                         |> Maybe.Extra.values
--
--                 rawValues =
--                     generateValues currentDate child dewormerByAgeInMonths ((==) AdministeredToday)
--             in
--             List.indexedMap
--                 -- Dewormer is not administered before age of 12 months.
--                 (postProcessMedicineRawValue 12 administeredMonths)
--                 rawValues
--
--         postProcessMedicineRawValue startingMonth administeredMonths processingMonth value =
--             if value == NCDACellValueEmpty then
--                 -- This means that child did not reach this age yet.
--                 value
--
--             else if processingMonth < startingMonth then
--                 -- Medicine is not administered yet.
--                 NCDACellValueDash
--
--             else if
--                 List.any
--                     (\administeredMonth ->
--                         -- Child was given medicine within past 6 months
--                         processingMonth >= administeredMonth && processingMonth - administeredMonth < 6
--                     )
--                     administeredMonths
--             then
--                 NCDACellValueV
--
--             else
--                 NCDACellValueX
--
--         ongeraMNPValues =
--             generateValues currentDate child questionnairesByAgeInMonths (.signs >> EverySet.member NCDAOngeraMNP)
--
--         ecdValues =
--             Maybe.map2
--                 (\assembled ageMonths ->
--                     let
--                         milestonesToCurrentDateWithStatus =
--                             generateECDMilestonesWithStatus currentDate
--                                 child
--                                 wellChildEncounters
--                                 individualWellChildMeasurementsWithDates
--                                 |> Dict.fromList
--
--                         milestoneWithStatusToCellValues ( milestone, status ) =
--                             let
--                                 cellValue =
--                                     case status of
--                                         StatusOnTrack ->
--                                             NCDACellValueV
--
--                                         NoECDStatus ->
--                                             NCDACellValueEmpty
--
--                                         _ ->
--                                             NCDACellValueX
--                             in
--                             case milestone of
--                                 -- Covers age of 2 and 3 months.
--                                 Milestone6Weeks ->
--                                     List.repeat 2 cellValue
--
--                                 -- Covers age of 4 and 5 months.
--                                 Milestone14Weeks ->
--                                     List.repeat 2 cellValue
--
--                                 -- Covers age of 6, 7 and 8 months.
--                                 Milestone6Months ->
--                                     List.repeat 3 cellValue
--
--                                 -- Covers age of 9, 10 and 11 months.
--                                 Milestone9Months ->
--                                     List.repeat 3 cellValue
--
--                                 -- Covers age of 12, 13 and 14 months.
--                                 Milestone12Months ->
--                                     List.repeat 3 cellValue
--
--                                 --    Covers age of 15, 16 and 17 months.
--                                 Milestone15Months ->
--                                     List.repeat 3 cellValue
--
--                                 --    Covers age of 18 to 23 months.
--                                 Milestone18Months ->
--                                     List.repeat 6 cellValue
--
--                                 --    Covers age of 24 and 25 months.
--                                 Milestone2Years ->
--                                     List.repeat 2 cellValue
--
--                                 -- Not in range.
--                                 Milestone3Years ->
--                                     []
--
--                                 -- Not in range.
--                                 Milestone4Years ->
--                                     []
--
--                         allMilestones =
--                             [ Milestone6Weeks
--                             , Milestone14Weeks
--                             , Milestone6Months
--                             , Milestone9Months
--                             , Milestone12Months
--                             , Milestone15Months
--                             , Milestone18Months
--                             , Milestone2Years
--                             ]
--                     in
--                     -- For first month, there's no ECD milestone.
--                     NCDACellValueDash
--                         :: (List.map
--                                 (\milestone ->
--                                     ( milestone
--                                     , Dict.get milestone milestonesToCurrentDateWithStatus
--                                         |> Maybe.withDefault NoECDStatus
--                                     )
--                                 )
--                                 allMilestones
--                                 |> List.map milestoneWithStatusToCellValues
--                                 |> List.concat
--                            )
--                         |> List.indexedMap
--                             (\month value ->
--                                 if ageMonths < month then
--                                     NCDACellValueEmpty
--
--                                 else
--                                     value
--                             )
--                 )
--                 maybeAssembled
--                 (ageInMonths currentDate child)
--                 |> Maybe.withDefault emptyNCDAValuesForChild
--     in
--     div [ class "pane universal-interventions" ]
--         [ viewPaneHeading language Translate.UniversalInterventions
--         , div [ class "pane-content" ]
--             [ viewTableHeader
--             , viewTableRow language
--                 (Translate.NCDAUniversalInterventionsItemLabel Immunization)
--                 pregnancyValues
--                 (List.take 6 immunizationValues)
--                 (List.drop 6 immunizationValues)
--             , viewTableRow language
--                 (Translate.NCDAUniversalInterventionsItemLabel Pages.WellChild.ProgressReport.Model.VitaminA)
--                 pregnancyValues
--                 (List.take 6 vitaminAValues)
--                 (List.drop 6 vitaminAValues)
--             , viewTableRow language
--                 (Translate.NCDAUniversalInterventionsItemLabel Deworming)
--                 pregnancyValues
--                 (List.take 6 dewormerValues)
--                 (List.drop 6 dewormerValues)
--             , viewTableRow language
--                 (Translate.NCDAUniversalInterventionsItemLabel OngeraMNP)
--                 pregnancyValues
--                 (List.take 6 ongeraMNPValues)
--                 (List.drop 6 ongeraMNPValues)
--             , viewTableRow language
--                 (Translate.NCDAUniversalInterventionsItemLabel ECDServices)
--                 pregnancyValues
--                 (List.take 6 ecdValues)
--                 (List.drop 6 ecdValues)
--             ]
--         ]
--
--
-- viewFillTheBlanksPane :
--     Language
--     -> NominalDate
--     -> ZScore.Model.Model
--     -> Person
--     -> ModelIndexedDb
--     -> ChildMeasurementList
--     -> List ( NominalDate, ( NutritionEncounterId, NutritionMeasurements ) )
--     -> List ( NominalDate, ( WellChildEncounterId, WellChildMeasurements ) )
--     -> Html any
-- viewFillTheBlanksPane language currentDate zscores child db groupNutritionMeasurements individualNutritionMeasurementsWithDates individualWellChildMeasurementsWithDates =
--     let
--         pregnancyValues =
--             List.repeat 9 NCDACellValueDash
--
--         maybeAgeInDays =
--             Maybe.map
--                 (\birthDate -> diffDays birthDate currentDate)
--                 child.birthDate
--
--         heightsValues =
--             generateFillTheBlanksValues heightsByAgeInMonths
--
--         weightsValues =
--             generateFillTheBlanksValues weightsByAgeInMonths
--
--         muacsValues =
--             generateFillTheBlanksValues muacsByAgeInMonths
--
--         nutritionsValues =
--             generateFillTheBlanksValues nutritionsByAgeInMonths
--
--         generateFillTheBlanksValues valuesByAgeInMonths =
--             Maybe.map2
--                 (\values ageMonths ->
--                     List.indexedMap
--                         (\month _ ->
--                             if ageMonths < month then
--                                 NCDACellValueEmpty
--
--                             else
--                                 Dict.get month values
--                                     |> Maybe.withDefault NCDACellValueDash
--                         )
--                         emptyNCDAValuesForChild
--                 )
--                 valuesByAgeInMonths
--                 (ageInMonths currentDate child)
--                 |> Maybe.withDefault emptyNCDAValuesForChild
--
--         heightsByAgeInMonths =
--             Maybe.map
--                 (\ageInDays ->
--                     List.filterMap
--                         (\( date, set ) ->
--                             Maybe.andThen
--                                 (\(HeightInCm height) ->
--                                     zScoreLengthHeightForAge zscores ageInDays child.gender (Centimetres height)
--                                         |> Maybe.map (\zscore -> ( date, cellValueByZscore zscore ))
--                                 )
--                                 set.height
--                         )
--                         allValuesSets
--                 )
--                 maybeAgeInDays
--                 |> Maybe.withDefault []
--                 |> distributeByAgeInMonths child
--
--         weightsByAgeInMonths =
--             Maybe.map
--                 (\ageInDays ->
--                     List.filterMap
--                         (\( date, set ) ->
--                             Maybe.andThen
--                                 (\(WeightInKg weight) ->
--                                     zScoreWeightForAge zscores ageInDays child.gender (Kilograms weight)
--                                         |> Maybe.map (\zscore -> ( date, cellValueByZscore zscore ))
--                                 )
--                                 set.weight
--                         )
--                         allValuesSets
--                 )
--                 maybeAgeInDays
--                 |> Maybe.withDefault []
--                 |> distributeByAgeInMonths child
--
--         cellValueByZscore zscore =
--             if zscore < -3 then
--                 NCDACellValueT
--
--             else if zscore < -2 then
--                 NCDACellValueH
--
--             else
--                 NCDACellValueC
--
--         muacsByAgeInMonths =
--             List.filterMap
--                 (\( date, set ) ->
--                     Maybe.map
--                         (\value ->
--                             let
--                                 cellValue =
--                                     case muacIndication value of
--                                         ColorAlertRed ->
--                                             NCDACellValueT
--
--                                         ColorAlertYellow ->
--                                             NCDACellValueH
--
--                                         ColorAlertGreen ->
--                                             NCDACellValueC
--                             in
--                             ( date, cellValue )
--                         )
--                         set.muac
--                 )
--                 allValuesSets
--                 |> distributeByAgeInMonths child
--
--         nutritionsByAgeInMonths =
--             List.filterMap
--                 (\( date, set ) ->
--                     Maybe.map
--                         (\value ->
--                             let
--                                 cellValue =
--                                     if EverySet.member Edema value.signs then
--                                         NCDACellValueT
--
--                                     else
--                                         NCDACellValueC
--                             in
--                             ( date, cellValue )
--                         )
--                         set.nutrition
--                 )
--                 allValuesSets
--                 |> distributeByAgeInMonths child
--
--         allValuesSets =
--             nutritionValuesSets ++ wellChildValuesSets ++ groupsValuesSets
--
--         nutritionValuesSets =
--             List.map
--                 (\( date, ( _, measurements ) ) ->
--                     generateIndividualValuesSet date measurements
--                 )
--                 individualNutritionMeasurementsWithDates
--
--         wellChildValuesSets =
--             List.map
--                 (\( date, ( _, measurements ) ) ->
--                     generateIndividualValuesSet date measurements
--                 )
--                 individualWellChildMeasurementsWithDates
--
--         generateIndividualValuesSet date measurements =
--             ( date
--             , { height = getMeasurementValueFunc measurements.height
--               , weight = getMeasurementValueFunc measurements.weight
--               , muac = getMeasurementValueFunc measurements.muac
--               , nutrition = getMeasurementValueFunc measurements.nutrition
--               }
--             )
--
--         groupsValuesSets =
--             List.map
--                 (\date ->
--                     ( date
--                     , { height = Dict.get date groupHeightsByDate
--                       , weight = Dict.get date groupWeightsByDate
--                       , muac = Dict.get date groupMuacsByDate
--                       , nutrition = Dict.get date groupNutritionsByDate
--                       }
--                     )
--                 )
--                 groupEncounterDates
--
--         groupEncounterDates =
--             Dict.keys groupHeightsByDate
--                 ++ Dict.keys groupWeightsByDate
--                 ++ Dict.keys groupMuacsByDate
--                 ++ Dict.keys groupNutritionsByDate
--                 |> EverySet.fromList
--                 |> EverySet.toList
--
--         groupHeightsByDate =
--             Dict.values groupNutritionMeasurements.heights
--                 |> List.map (\height -> ( height.dateMeasured, height.value ))
--                 |> Dict.fromList
--
--         groupWeightsByDate =
--             Dict.values groupNutritionMeasurements.weights
--                 |> List.map (\weight -> ( weight.dateMeasured, weight.value ))
--                 |> Dict.fromList
--
--         groupMuacsByDate =
--             Dict.values groupNutritionMeasurements.muacs
--                 |> List.map (\muac -> ( muac.dateMeasured, muac.value ))
--                 |> Dict.fromList
--
--         groupNutritionsByDate =
--             Dict.values groupNutritionMeasurements.nutritions
--                 |> List.map (\nutrition -> ( nutrition.dateMeasured, nutrition.value ))
--                 |> Dict.fromList
--     in
--     div [ class "pane fill-the-blanks" ]
--         [ viewPaneHeading language Translate.FillTheBlanks
--         , div [ class "pane-content" ]
--             [ viewTableHeader
--             , viewTableRow language
--                 (Translate.NCDAFillTheBlanksItemLabel HeightToAge)
--                 pregnancyValues
--                 (List.take 6 heightsValues)
--                 (List.drop 6 heightsValues)
--             , viewTableRow language
--                 (Translate.NCDAFillTheBlanksItemLabel WeightToAge)
--                 pregnancyValues
--                 (List.take 6 weightsValues)
--                 (List.drop 6 weightsValues)
--             , viewTableRow language
--                 (Translate.NCDAFillTheBlanksItemLabel MuacValue)
--                 pregnancyValues
--                 (List.take 6 muacsValues)
--                 (List.drop 6 muacsValues)
--             , viewTableRow language
--                 (Translate.NCDAFillTheBlanksItemLabel EdemaPresent)
--                 pregnancyValues
--                 (List.take 6 nutritionsValues)
--                 (List.drop 6 nutritionsValues)
--             ]
--         ]


viewPaneHeading : Language -> TranslationId -> Html any
viewPaneHeading language label =
    div [ class <| "pane-heading" ]
        [ text <| translate language label ]


viewTableHeader : Language -> Html any
viewTableHeader language =
    let
        statusCell =
            div [ class "cell activity" ] [ text <| translate language Translate.Status ]

        monthCells =
            List.map
                (\month ->
                    div [ class "cell" ] [ text <| translate language <| Translate.Month month ]
                )
                [ Jan, Feb, Mar, Apr, May, Jun, Jul, Aug, Sep, Oct, Nov, Dec ]
    in
    div [ class "table-header" ] <|
        statusCell
            :: monthCells


viewTableRow : Language -> TranslationId -> List Int -> Html any
viewTableRow language itemTransId values =
    let
        activityCell =
            div [ class "cell activity" ] [ text <| translate language itemTransId ]

        valueCells =
            List.map
                (\value ->
                    div [ class "cell value" ]
                        [ text <| String.fromInt value ]
                )
                values
    in
    div [ class "table-row" ] <|
        activityCell
            :: valueCells
