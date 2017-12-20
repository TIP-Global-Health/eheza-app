module Pages.ProgressReport.View exposing (view)

import Activity.Model exposing (ActivityType(..), ChildActivityType(..))
import Backend.Child.Model exposing (Child, Gender(..))
import Backend.Entities exposing (..)
import Backend.Measurement.Model exposing (Height, Weight, HeightInCm(..), WeightInKg(..))
import Backend.Measurement.Utils exposing (mapMeasurementData, currentValueWithId)
import Backend.Session.Model exposing (EditableSession)
import Backend.Session.Utils exposing (getChildHistoricalMeasurements, getChildMeasurementData, getChild, getMother)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import List.Extra
import Pages.Model exposing (MsgSession(..))
import Pages.Page exposing (Page(..), SessionPage(..))
import Pages.PageNotFound.View
import Translate exposing (Language(..), translate)
import Utils.Html exposing (thumbnailImage)
import Utils.NominalDate exposing (Days(..), diffDays, renderDateOfBirth, renderAgeMonthsDays)
import ZScore.Model exposing (Centimetres(..), Kilograms(..))
import ZScore.View


view : Language -> ZScore.Model.Model -> ChildId -> EditableSession -> Html MsgSession
view language zscores childId session =
    case getChild childId session.offlineSession of
        Just child ->
            viewFoundChild language zscores ( childId, child ) session

        Nothing ->
            ProgressReportPage childId
                |> SessionPage
                |> Pages.PageNotFound.View.viewPage language (SetActivePage LoginPage)


viewFoundChild : Language -> ZScore.Model.Model -> ( ChildId, Child ) -> EditableSession -> Html MsgSession
viewFoundChild language zscores ( childId, child ) session =
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

                -- TODO: Use real date of last assessment
                , text "15 July 2017"
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
                            -- TODO: Use real months old
                            [ strong [] [ text "7" ]
                            , text " months old "
                            , strong [] [ text <| translate language (Translate.Gender child.gender) ]
                            ]
                        , p []
                            [ text <| translate language Translate.Born
                            , text " "
                            , strong [] [ text <| renderDateOfBirth language child.birthDate ]
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

        -- TODO: Use real nutrition signs of last assessment.
        nutritionSigns =
            table
                [ class "ui celled table" ]
                [ thead []
                    [ tr []
                        [ th [ class "uppercase" ] [ text <| translate language Translate.AgeWord ]
                        , th [ class "last" ] [ text "6 mo 7 days" ]
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
                        , td [] [ text "Edema, Abdominal Distention, Dry Skin, Apathy, Poor Appetite, Brittle Hair" ]
                        ]
                    ]
                ]

        -- TODO: Use real measurmenets
        floats =
            table
                [ class "ui collapsing celled table" ]
                [ thead []
                    [ tr []
                        [ th [ class "uppercase" ] [ text <| translate language Translate.AgeWord ]
                        , th [ class "center bottom aligned" ] [ text "23 days" ]
                        , th
                            [ class "center bottom aligned" ]
                            [ text "1 mo"
                            , br [] []
                            , text "15 days"
                            ]
                        , th
                            [ class "center bottom aligned" ]
                            [ text "2 mo"
                            , br [] []
                            , text "11 days"
                            ]
                        , th [ class "center bottom aligned" ]
                            [ text "3 mo"
                            , br [] []
                            , text "12 days"
                            ]
                        , th [ class "center bottom aligned" ]
                            [ text "4 mo"
                            , br [] []
                            , text "11 days"
                            ]
                        , th [ class "center bottom aligned" ]
                            [ text "5 mo"
                            , br [] []
                            , text "12 days"
                            ]
                        , th [ class "center bottom aligned last" ]
                            [ text "6 mo"
                            , br [] []
                            , text "7 days"
                            ]
                        ]
                    ]
                , tbody []
                    [ tr []
                        [ td [ class "first" ] [ text <| translate language (Translate.ActivityProgressReport (ChildActivity Height)) ]
                        , td [ class "center aligned negative" ] [ text "52cm" ]
                        , td [ class "center aligned negative" ] [ text "57cm" ]
                        , td [ class "center aligned" ] [ text "--" ]
                        , td [ class "center aligned warning" ] [ text "58cm" ]
                        , td [ class "center aligned positive" ] [ text "70cm" ]
                        , td [ class "center aligned positive" ] [ text "71cm" ]
                        , td [ class "center aligned positive" ] [ text "71cm" ]
                        ]
                    , tr []
                        [ td [ class "first" ] [ text <| translate language (Translate.ActivityProgressReport (ChildActivity Weight)) ]
                        , td [ class "center aligned negative" ] [ text "4.2kg" ]
                        , td [ class "center aligned negative" ] [ text "5.0kg" ]
                        , td [ class "center aligned" ] [ text "--" ]
                        , td [ class "center aligned warning" ] [ text "6.0kg" ]
                        , td [ class "center aligned positive" ] [ text "8.2kg" ]
                        , td [ class "center aligned positive" ] [ text "8.3kg" ]
                        , td [ class "center aligned positive" ] [ text "8.4kg" ]
                        ]
                    , tr []
                        [ td [ class "first" ] [ text <| translate language (Translate.ActivityProgressReport (ChildActivity Muac)) ]
                        , td [ class "center aligned" ] []
                        , td [ class "center aligned" ] []
                        , td [ class "center aligned" ] []
                        , td [ class "center aligned" ] []
                        , td [ class "center aligned" ] []
                        , td [ class "center aligned" ] []
                        , td [ class "center aligned positive" ] [ text "13cm" ]
                        ]
                    ]
                ]

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

        photoValues =
            getValues .photo .photo .photos

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
