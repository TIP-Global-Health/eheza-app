module Pages.ProgressReport.View exposing (view)

import Activity.Model exposing (ActivityType(..), ChildActivityType(..))
import Backend.Child.Model exposing (Child, Gender(..))
import Backend.Entities exposing (..)
import Backend.Measurement.Model exposing (Height, Weight, HeightInCm(..), WeightInKg(..), MuacInCm(..), MuacIndication(..))
import Backend.Measurement.Utils exposing (mapMeasurementData, currentValue, currentValueWithId, muacIndication)
import Backend.Session.Model exposing (EditableSession)
import Backend.Session.Utils exposing (getChildHistoricalMeasurements, getChildMeasurementData, getChild, getMother)
import EveryDict
import EveryDictList
import EverySet
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import List.Extra exposing (greedyGroupsOf)
import Pages.Model exposing (MsgSession(..))
import Pages.Page exposing (Page(..), SessionPage(..))
import Pages.PageNotFound.View
import Translate exposing (Language(..), translate)
import Utils.Html exposing (thumbnailImage)
import Utils.NominalDate exposing (Days(..), Months(..), diffDays, diffMonths, renderDate, renderAgeMonthsDays, renderAgeMonthsDaysAbbrev, renderAgeMonthsDaysHtml)
import ZScore.Model exposing (Centimetres(..), Kilograms(..))
import ZScore.View


view : Language -> ZScore.Model.Model -> ChildId -> ( SessionId, EditableSession ) -> Html MsgSession
view language zscores childId ( sessionId, session ) =
    case getChild childId session.offlineSession of
        Just child ->
            viewFoundChild language zscores ( childId, child ) ( sessionId, session )

        Nothing ->
            ProgressReportPage childId
                |> SessionPage
                |> Pages.PageNotFound.View.viewPage language (SetActivePage LoginPage)


{-| It would probably be beneficial to break up this function into parts.
-}
viewFoundChild : Language -> ZScore.Model.Model -> ( ChildId, Child ) -> ( SessionId, EditableSession ) -> Html MsgSession
viewFoundChild language zscores ( childId, child ) ( sessionId, session ) =
    let
        backIcon =
            a
                [ class "icon-back"
                , ChildPage childId
                    |> SessionPage
                    |> SetActivePage
                    |> onClick
                ]
                []

        title =
            h1
                [ class "ui report header" ]
                [ text <| translate language Translate.ParticipantSummary ]

        subtitle =
            p
                [ class "date" ]
                [ text <| translate language Translate.DateOfLastAssessment
                , text ": "
                , text <| renderDate language session.offlineSession.session.scheduledDate.start
                ]

        childInfo =
            div
                [ class "ui report unstackable items" ]
                [ div
                    [ class "item" ]
                    [ div
                        [ class "ui image" ]
                        [ thumbnailImage "child" child.avatarUrl child.name 152 152
                        ]
                    , div
                        [ class "content" ]
                        [ h2
                            [ class "ui header" ]
                            [ text child.name ]
                        , p []
                            [ diffMonths child.birthDate session.offlineSession.session.scheduledDate.start
                                |> (\(Months months) -> [ text <| toString months ])
                                |> strong []
                            , text " "
                            , text <| translate language Translate.MonthsOld
                            , text " "
                            , strong [] [ text <| translate language (Translate.Gender child.gender) ]
                            ]
                        , p []
                            [ text <| translate language Translate.Born
                            , text " "
                            , strong [] [ text <| renderDate language child.birthDate ]
                            , br [] []
                            , text <| translate language Translate.ChildOf
                            , text " "
                            , strong []
                                [ child.motherId
                                    |> Maybe.andThen (\motherId -> getMother motherId session.offlineSession)
                                    |> Maybe.map .name
                                    |> Maybe.withDefault "Unknown"
                                    |> text
                                ]
                            ]
                        ]
                    ]
                ]

        -- We're using the current value from the current session here, at
        -- least for now. So, we're ignoring any later sessions (normally,
        -- there wouldn't be any), and we're just leaving it blank if it wasn't
        -- entered in this session (rather than looking back to a previous
        -- session when it was entered).
        nutritionSigns =
            table
                [ class "ui celled table" ]
                [ thead []
                    [ tr []
                        [ th
                            [ class "uppercase" ]
                            [ text <| translate language Translate.AgeWord ]
                        , th
                            [ class "last" ]
                            [ text <| renderAgeMonthsDaysAbbrev language child.birthDate session.offlineSession.session.scheduledDate.start ]
                        ]
                    ]
                , tbody []
                    [ tr []
                        [ td
                            [ class "first" ]
                            [ ChildActivity NutritionSigns
                                |> Translate.ActivityProgressReport
                                |> translate language
                                |> text
                            ]
                        , current
                            |> mapMeasurementData .nutrition .nutrition
                            |> currentValue
                            |> Maybe.map .value
                            |> Maybe.withDefault EverySet.empty
                            |> EverySet.toList
                            |> List.map (translate language << Translate.ChildNutritionSignReport)
                            |> String.join ", "
                            |> text
                            |> List.singleton
                            |> td []
                        ]
                    ]
                ]

        -- This is probably a bit too obscure.
        floats =
            EveryDictList.keys session.offlineSession.allSessions
                |> List.Extra.dropWhile
                    (\id ->
                        not <|
                            EveryDict.member id heightValuesBySession
                                || EveryDict.member id muacValuesBySession
                                || EveryDict.member id weightValuesBySession
                    )
                |> List.Extra.takeWhile (\id -> id /= sessionId)
                |> (\ids -> ids ++ [ sessionId ])
                |> greedyGroupsOf 12
                |> List.map
                    (\groupOfTwelve ->
                        let
                            ages =
                                groupOfTwelve
                                    |> List.map
                                        (\id ->
                                            EveryDictList.get id session.offlineSession.allSessions
                                                |> Maybe.map (\columnSession -> renderAgeMonthsDaysHtml language child.birthDate columnSession.scheduledDate.start)
                                                |> Maybe.withDefault []
                                                |> th
                                                    [ classList
                                                        [ ( "center", True )
                                                        , ( "bottom", True )
                                                        , ( "aligned", True )
                                                        , ( "last", id == sessionId )
                                                        , ( "date-header", True )
                                                        ]
                                                    ]
                                        )
                                    |> (::) ageCell
                                    |> tr []

                            heights =
                                groupOfTwelve
                                    |> List.map
                                        (\id ->
                                            EveryDict.get id heightValuesBySession
                                                |> Maybe.map .value
                                                |> Maybe.map (\(HeightInCm cm) -> toString cm ++ translate language Translate.CentimeterShorthand)
                                                |> Maybe.withDefault "--"
                                                |> text
                                                |> List.singleton
                                                |> td [ class "center aligned" ]
                                        )
                                    |> (::) heightCell
                                    |> tr []

                            muacs =
                                groupOfTwelve
                                    |> List.map
                                        (\id ->
                                            EveryDict.get id muacValuesBySession
                                                |> Maybe.map .value
                                                |> Maybe.map
                                                    (\((MuacInCm cm) as muac) ->
                                                        let
                                                            indication =
                                                                case muacIndication muac of
                                                                    MuacRed ->
                                                                        "negative"

                                                                    MuacYellow ->
                                                                        "warning"

                                                                    MuacGreen ->
                                                                        "positive"

                                                            value =
                                                                toString cm ++ translate language Translate.CentimeterShorthand
                                                        in
                                                            span
                                                                [ class indication ]
                                                                [ text value ]
                                                    )
                                                |> Maybe.withDefault (text "--")
                                                |> List.singleton
                                                |> td [ class "center aligned" ]
                                        )
                                    |> (::) muacCell
                                    |> tr []

                            weights =
                                groupOfTwelve
                                    |> List.map
                                        (\id ->
                                            EveryDict.get id weightValuesBySession
                                                |> Maybe.map .value
                                                |> Maybe.map (\(WeightInKg kg) -> toString kg ++ translate language Translate.KilogramShorthand)
                                                |> Maybe.withDefault "--"
                                                |> text
                                                |> List.singleton
                                                |> td [ class "center aligned" ]
                                        )
                                    |> (::) weightCell
                                    |> tr []
                        in
                            [ ages
                            , heights
                            , weights
                            , muacs
                            ]
                    )
                |> List.concat
                |> tbody []
                |> List.singleton
                |> table [ class "ui collapsing celled table" ]

        ageCell =
            th
                [ class "uppercase" ]
                [ text <| translate language Translate.AgeWord ]

        heightCell =
            td
                [ class "first" ]
                [ text <| translate language (Translate.ActivityProgressReport (ChildActivity Height)) ]

        weightCell =
            td
                [ class "first" ]
                [ text <| translate language (Translate.ActivityProgressReport (ChildActivity Weight)) ]

        muacCell =
            td
                [ class "first" ]
                [ text <| translate language (Translate.ActivityProgressReport (ChildActivity Muac)) ]

        photos =
            photoValues
                |> List.map
                    (\photo ->
                        div
                            [ class "report card" ]
                            [ div
                                [ class "content" ]
                                [ text <| renderAgeMonthsDays language child.birthDate photo.dateMeasured ]
                            , div
                                [ class "image" ]
                                [ img [ src photo.value.url ] [] ]
                            ]
                    )
                |> div [ class "ui five report cards" ]

        ( heightForAge, weightForAge, weightForHeight ) =
            case child.gender of
                Male ->
                    ( ZScore.View.viewHeightForAgeBoys
                    , ZScore.View.viewWeightForAgeBoys
                    , ZScore.View.viewWeightForHeightBoys
                    )

                Female ->
                    ( ZScore.View.viewHeightForAgeGirls
                    , ZScore.View.viewWeightForAgeGirls
                    , ZScore.View.viewWeightForHeightGirls
                    )

        historical =
            getChildHistoricalMeasurements childId session.offlineSession

        current =
            getChildMeasurementData childId session

        -- This includes any edits that have been saved locally, but not as-you=type
        -- in the UI before you hit "Save" or "Update".
        getValues func1 func2 func3 =
            let
                currentValue =
                    current
                        |> mapMeasurementData func1 func2
                        |> currentValueWithId

                historicalValues =
                    func3 historical
            in
                case currentValue of
                    Nothing ->
                        -- No current value, so just use historical
                        List.map Tuple.second historicalValues

                    Just ( Nothing, currentValue ) ->
                        -- We have a new current value, so use it
                        currentValue :: List.map Tuple.second historicalValues

                    Just ( Just currentId, currentValue ) ->
                        -- We've edited an old value, so use the edited version
                        -- and leave out the old one.
                        historicalValues
                            |> List.filter (\( id, _ ) -> id /= currentId)
                            |> List.map Tuple.second
                            |> List.append [ currentValue ]

        heightValues =
            getValues .height .height .heights

        weightValues =
            getValues .weight .weight .weights

        muacValues =
            getValues .muac .muac .muacs

        photoValues =
            getValues .photo .photo .photos

        indexBySession values =
            values
                |> List.filterMap
                    (\value ->
                        case value.sessionId of
                            Just id ->
                                Just ( id, value )

                            Nothing ->
                                Nothing
                    )
                |> EveryDict.fromList

        heightValuesBySession =
            indexBySession heightValues

        muacValuesBySession =
            indexBySession muacValues

        weightValuesBySession =
            indexBySession weightValues

        heightForAgeData =
            List.map (chartHeightForAge child) heightValues

        weightForAgeData =
            List.map (chartWeightForAge child) weightValues

        weightForHeightData =
            List.filterMap (chartWeightForHeight heightValues) weightValues

        charts =
            div
                [ class "image-report" ]
                [ ZScore.View.viewMarkers
                , heightForAge language zscores heightForAgeData
                , weightForAge language zscores weightForAgeData
                , weightForHeight language zscores weightForHeightData
                ]
    in
        div [ class "page-report" ]
            [ div
                [ class "wrap-report" ]
                [ backIcon
                , title
                , subtitle
                , childInfo
                , nutritionSigns
                , floats
                , photos
                , charts
                ]
            ]


chartHeightForAge : Child -> Height -> ( Days, Centimetres )
chartHeightForAge child height =
    ( diffDays child.birthDate height.dateMeasured
      -- I suppose one could avoid this little transformation
      -- by unifiying the two tags.
    , case height.value of
        HeightInCm cm ->
            Centimetres cm
    )


chartWeightForAge : Child -> Weight -> ( Days, Kilograms )
chartWeightForAge child weight =
    ( diffDays child.birthDate weight.dateMeasured
      -- I suppose one could avoid this little transformation
      -- by unifiying the two tags.
    , case weight.value of
        WeightInKg cm ->
            Kilograms cm
    )


chartWeightForHeight : List Height -> Weight -> Maybe ( Centimetres, Kilograms )
chartWeightForHeight heights weight =
    -- For each weight, we try to find a height with a matching sessionID
    heights
        |> List.Extra.find (\height -> height.sessionId == weight.sessionId)
        |> Maybe.map
            (\height ->
                ( case height.value of
                    HeightInCm cm ->
                        Centimetres cm
                , case weight.value of
                    WeightInKg kg ->
                        Kilograms kg
                )
            )
