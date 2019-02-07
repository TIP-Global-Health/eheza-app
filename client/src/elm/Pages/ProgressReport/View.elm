module Pages.ProgressReport.View exposing (view)

import Activity.Model exposing (Activity(..), ChildActivity(..))
import Backend.Child.Model exposing (Child, Gender(..))
import Backend.Entities exposing (..)
import Backend.Measurement.Model exposing (ChildMeasurementList, Height, HeightInCm(..), MuacInCm(..), MuacIndication(..), Weight, WeightInKg(..))
import Backend.Measurement.Utils exposing (currentValue, currentValueWithId, mapMeasurementData, muacIndication)
import Backend.Model exposing (ModelIndexedDb)
import Backend.Mother.Model exposing (ChildrenRelationType(..))
import Backend.Session.Model exposing (EditableSession, Session)
import Backend.Session.Utils exposing (getChild, getChildHistoricalMeasurements, getChildMeasurementData, getMother)
import EveryDict
import EveryDictList exposing (EveryDictList)
import EverySet
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import List.Extra exposing (greedyGroupsOf)
import Maybe.Extra
import Pages.Page exposing (Page(..), SessionPage(..), UserPage(..))
import Pages.PageNotFound.View
import Pages.Session.Model
import RemoteData exposing (RemoteData(..))
import Translate exposing (Language, translate)
import Utils.Html exposing (thumbnailImage)
import Utils.NominalDate exposing (Days(..), Months(..), diffDays, diffMonths, renderAgeMonthsDays, renderAgeMonthsDaysAbbrev, renderAgeMonthsDaysHtml, renderDate)
import Utils.WebData exposing (viewWebData)
import ZScore.Model exposing (Centimetres(..), Kilograms(..), Length(..), ZScore)
import ZScore.Utils exposing (zScoreLengthHeightForAge, zScoreWeightForAge)
import ZScore.View


view : Language -> ZScore.Model.Model -> ChildId -> ( SessionId, EditableSession ) -> ModelIndexedDb -> Html Pages.Session.Model.Msg
view language zscores childId ( sessionId, session ) db =
    case getChild childId session.offlineSession of
        Just child ->
            let
                childMeasurements =
                    EveryDict.get childId db.childMeasurements
                        |> Maybe.withDefault NotAsked

                expectedSessions =
                    EveryDict.get childId db.expectedSessions
                        |> Maybe.withDefault NotAsked
            in
            div []
                (viewWebData language
                    (viewFoundChild language zscores ( childId, child ) ( sessionId, session ))
                    identity
                    (RemoteData.append expectedSessions childMeasurements)
                )

        Nothing ->
            let
                participantsPage =
                    ParticipantsPage
                        |> SessionPage sessionId
                        |> UserPage
            in
            ProgressReportPage childId
                |> SessionPage sessionId
                |> UserPage
                |> Pages.PageNotFound.View.viewPage language (Pages.Session.Model.SetActivePage participantsPage)


{-| This function is more complex than one would like ... when reviewing the
data model in future, it might be nice to take this function into account.
-}
viewFoundChild : Language -> ZScore.Model.Model -> ( ChildId, Child ) -> ( SessionId, EditableSession ) -> ( EveryDictList SessionId Session, ChildMeasurementList ) -> List (Html Pages.Session.Model.Msg)
viewFoundChild language zscores ( childId, child ) ( sessionId, session ) ( expectedSessions, historical ) =
    let
        backIcon =
            a
                [ class "icon-back"
                , ChildPage childId
                    |> SessionPage sessionId
                    |> UserPage
                    |> Pages.Session.Model.SetActivePage
                    |> onClick
                ]
                []

        title =
            h1
                [ class "ui report header" ]
                [ text <| translate language Translate.ParticipantSummary ]

        -- We use the date of the last session that actually had a measurement.
        -- If there are no measurements, we use the date for the current
        -- session.
        dateOfLastAssessment =
            lastSessionWithMeasurement
                |> Maybe.map (\( _, last ) -> last.scheduledDate.start)
                |> Maybe.withDefault session.offlineSession.session.scheduledDate.start

        subtitle =
            p
                [ class "date" ]
                [ text <| translate language Translate.DateOfLastAssessment
                , text ": "
                , text <| renderDate language dateOfLastAssessment
                ]

        maybeMother =
            child.motherId
                |> Maybe.andThen (\motherId -> getMother motherId session.offlineSession)

        relationText =
            maybeMother
                |> Maybe.map .relation
                -- In case if mother is Nothing, we will show `Child of`.
                |> Maybe.withDefault MotherRelation
                |> (\relation ->
                        case relation of
                            MotherRelation ->
                                Translate.ChildOf

                            CaregiverRelation ->
                                Translate.TakenCareOfBy
                   )

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
                            [ text <| renderAgeMonthsDays language child.birthDate dateOfLastAssessment
                            , text " "
                            , text <| translate language Translate.Old
                            , text " "
                            , strong [] [ text <| translate language (Translate.Gender child.gender) ]
                            ]
                        , p []
                            [ text <| translate language Translate.Born
                            , text " "
                            , strong [] [ text <| renderDate language child.birthDate ]
                            , br [] []
                            , text <| translate language relationText
                            , text " "
                            , strong []
                                [ maybeMother
                                    |> Maybe.map .name
                                    |> Maybe.withDefault (translate language Translate.Unknown)
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
        --
        -- See <https://github.com/Gizra/ihangane/issues/382#issuecomment-353273873>
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

        -- Do we have any kind of measurement for the child for the specified session?
        hasMeasurement ( id, _ ) =
            EveryDict.member id heightValuesBySession
                || EveryDict.member id muacValuesBySession
                || EveryDict.member id weightValuesBySession
                || EveryDict.member id nutritionValuesBySession
                || EveryDict.member id photoValuesBySession

        -- What's the last session for which we have some measurement?
        lastSessionWithMeasurement =
            expectedSessions
                |> EveryDictList.toList
                |> List.reverse
                |> List.Extra.find hasMeasurement

        heightWeightMuacTable =
            expectedSessions
                |> EveryDictList.toList
                |> greedyGroupsOf 12
                |> List.map
                    (\groupOfTwelve ->
                        let
                            ages =
                                groupOfTwelve
                                    |> List.map
                                        (\( id, columnSession ) ->
                                            renderAgeMonthsDaysHtml language child.birthDate columnSession.scheduledDate.start
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
                                        (\( id, _ ) ->
                                            EveryDict.get id heightValuesBySession
                                                |> Maybe.map viewHeightWithIndication
                                                |> Maybe.withDefault (text "--")
                                                |> List.singleton
                                                |> td [ class "center aligned" ]
                                        )
                                    |> (::) heightCell
                                    |> tr []

                            muacs =
                                groupOfTwelve
                                    |> List.map
                                        (\( id, _ ) ->
                                            EveryDict.get id muacValuesBySession
                                                |> Maybe.map .value
                                                |> Maybe.map
                                                    (\((MuacInCm cm) as muac) ->
                                                        span
                                                            [ class <| classForIndication <| muacIndicationToIndication <| muacIndication muac ]
                                                            [ text <| toString cm ++ translate language Translate.CentimeterShorthand ]
                                                    )
                                                |> Maybe.withDefault (text "--")
                                                |> List.singleton
                                                |> td [ class "center aligned" ]
                                        )
                                    |> (::) muacCell
                                    |> tr []

                            viewHeightWithIndication height =
                                let
                                    cm =
                                        case height.value of
                                            HeightInCm cms ->
                                                cms

                                    ageInDays =
                                        diffDays child.birthDate height.dateMeasured

                                    indication =
                                        zScoreLengthHeightForAge zscores ageInDays child.gender (Centimetres cm)
                                            |> Maybe.map (class << classForIndication << zScoreToIndication)
                                            |> Maybe.Extra.toList

                                    value =
                                        toString cm ++ translate language Translate.CentimeterShorthand
                                in
                                span indication [ text value ]

                            viewWeightWithIndication weight =
                                let
                                    kg =
                                        case weight.value of
                                            WeightInKg kilos ->
                                                kilos

                                    ageInDays =
                                        diffDays child.birthDate weight.dateMeasured

                                    indication =
                                        zScoreWeightForAge zscores ageInDays child.gender (Kilograms kg)
                                            |> Maybe.map (class << classForIndication << zScoreToIndication)
                                            |> Maybe.Extra.toList

                                    value =
                                        toString kg ++ translate language Translate.KilogramShorthand
                                in
                                span indication [ text value ]

                            weights =
                                groupOfTwelve
                                    |> List.map
                                        (\( id, _ ) ->
                                            EveryDict.get id weightValuesBySession
                                                |> Maybe.map viewWeightWithIndication
                                                |> Maybe.withDefault (text "--")
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

        nutritionValues =
            getValues .nutrition .nutrition .nutritions

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

        nutritionValuesBySession =
            indexBySession nutritionValues

        photoValuesBySession =
            indexBySession photoValues

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
    [ div [ class "page-report" ]
        [ div
            [ class "wrap-report" ]
            [ backIcon
            , title
            , subtitle
            , childInfo
            , nutritionSigns
            , heightWeightMuacTable
            , photos
            , charts
            ]
        ]
    ]


type Indication
    = Negative
    | Warning
    | Positive


classForIndication : Indication -> String
classForIndication indication =
    case indication of
        Negative ->
            "negative"

        Warning ->
            "warning"

        Positive ->
            "positive"


muacIndicationToIndication : MuacIndication -> Indication
muacIndicationToIndication muacIndication =
    case muacIndication of
        MuacRed ->
            Negative

        MuacYellow ->
            Warning

        MuacGreen ->
            Positive


zScoreToIndication : ZScore -> Indication
zScoreToIndication zScore =
    if zScore <= -3 then
        Negative

    else if zScore <= -2 then
        Warning

    else
        Positive


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


chartWeightForHeight : List Height -> Weight -> Maybe ( Length, Kilograms )
chartWeightForHeight heights weight =
    -- For each weight, we try to find a height with a matching sessionID.
    -- Eventually, we shouild take age into account to distingiush height
    -- and length.
    heights
        |> List.Extra.find (\height -> height.sessionId == weight.sessionId)
        |> Maybe.map
            (\height ->
                ( case height.value of
                    HeightInCm cm ->
                        Length cm
                , case weight.value of
                    WeightInKg kg ->
                        Kilograms kg
                )
            )
