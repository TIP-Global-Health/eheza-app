module Pages.ProgressReport.View exposing (view)

import Activity.Model exposing (Activity(..), ChildActivity(..))
import AssocList as Dict exposing (Dict)
import Backend.Entities exposing (..)
import Backend.Measurement.Model exposing (ChildMeasurementList, Height, HeightInCm(..), MuacInCm(..), MuacIndication(..), PhotoUrl(..), Weight, WeightInKg(..))
import Backend.Measurement.Utils exposing (currentValue, currentValueWithId, mapMeasurementData, muacIndication)
import Backend.Model exposing (ModelIndexedDb)
import Backend.Person.Model exposing (Gender(..), Person)
import Backend.PmtctParticipant.Model exposing (AdultActivities(..))
import Backend.Session.Model exposing (EditableSession, Session)
import Backend.Session.Utils exposing (getChild, getChildHistoricalMeasurements, getChildMeasurementData, getMother, getMyMother)
import Date
import EverySet
import Gizra.Html exposing (emptyNode)
import Gizra.NominalDate exposing (NominalDate, diffMonths)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import List.Extra exposing (greedyGroupsOf)
import LocalData
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


view : Language -> NominalDate -> ZScore.Model.Model -> PersonId -> ( SessionId, EditableSession ) -> ModelIndexedDb -> Html Pages.Session.Model.Msg
view language currentDate zscores childId ( sessionId, session ) db =
    case getChild childId session.offlineSession of
        Just child ->
            let
                childMeasurements =
                    Dict.get childId db.childMeasurements
                        |> Maybe.withDefault NotAsked

                expectedSessions =
                    Dict.get childId db.expectedSessions
                        |> Maybe.withDefault NotAsked
            in
            viewWebData language
                (viewFoundChild language currentDate zscores ( childId, child ) ( sessionId, session ))
                identity
                (RemoteData.append expectedSessions childMeasurements)

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
viewFoundChild : Language -> NominalDate -> ZScore.Model.Model -> ( PersonId, Person ) -> ( SessionId, EditableSession ) -> ( Dict SessionId Session, ChildMeasurementList ) -> Html Pages.Session.Model.Msg
viewFoundChild language currentDate zscores ( childId, child ) ( sessionId, session ) ( expectedSessions, historical ) =
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
                |> Maybe.map (\( _, last ) -> last.startDate)
                |> Maybe.withDefault session.offlineSession.session.startDate

        subtitle =
            p
                [ class "date" ]
                [ text <| translate language Translate.DateOfLastAssessment
                , text ": "
                , text <| renderDate language dateOfLastAssessment
                ]

        maybeMother =
            getMyMother childId session.offlineSession
                |> Maybe.map Tuple.second

        relationText =
            Dict.get childId session.offlineSession.participants.byChildId
                |> Maybe.withDefault []
                |> List.head
                |> Maybe.map
                    (\participant ->
                        case participant.adultActivities of
                            MotherActivities ->
                                Translate.ChildOf

                            CaregiverActivities ->
                                Translate.TakenCareOfBy
                    )
                |> Maybe.withDefault Translate.ChildOf

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
                            [ child.birthDate
                                |> Maybe.map
                                    (\birthDate ->
                                        [ text <| renderAgeMonthsDays language birthDate dateOfLastAssessment
                                        , text " "
                                        , text <| translate language Translate.Old
                                        , text " "
                                        ]
                                    )
                                |> Maybe.withDefault []
                                |> span []
                            , strong [] [ text <| translate language (Translate.Gender child.gender) ]
                            ]
                        , p []
                            [ text <| translate language Translate.Born
                            , text " "
                            , strong []
                                [ child.birthDate
                                    |> Maybe.map (renderDate language)
                                    |> Maybe.withDefault (translate language Translate.NotAvailable)
                                    |> text
                                ]
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
                            [ child.birthDate
                                |> Maybe.map (\birthDate -> renderAgeMonthsDaysAbbrev language birthDate dateOfLastAssessment)
                                |> Maybe.withDefault ""
                                |> text
                            ]
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
                            |> LocalData.unwrap
                                []
                                (mapMeasurementData .nutrition
                                    >> currentValue
                                    >> Maybe.map .value
                                    >> Maybe.withDefault EverySet.empty
                                    >> EverySet.toList
                                    >> List.map (translate language << Translate.ChildNutritionSignReport)
                                    >> String.join ", "
                                    >> text
                                    >> List.singleton
                                )
                            |> td []
                        ]
                    ]
                ]

        -- Do we have any kind of measurement for the child for the specified session?
        hasMeasurement ( id, _ ) =
            Dict.member id heightValuesBySession
                || Dict.member id muacValuesBySession
                || Dict.member id weightValuesBySession
                || Dict.member id nutritionValuesBySession
                || Dict.member id photoValuesBySession

        -- What's the last session for which we have some measurement?
        lastSessionWithMeasurement =
            expectedSessions
                |> Dict.toList
                |> List.sortWith sessionsSortFunc
                |> List.reverse
                |> List.Extra.find hasMeasurement

        sessionsSortFunc ( k1, v1 ) ( k2, v2 ) =
            Date.compare v1.startDate v2.startDate

        heightWeightMuacTable =
            expectedSessions
                |> Dict.toList
                |> List.sortWith sessionsSortFunc
                |> List.reverse
                -- Filter out sessions that occured before child was born.
                |> List.filter
                    (\( id, columnSession ) ->
                        child.birthDate
                            |> Maybe.map
                                (\birthDate ->
                                    case Gizra.NominalDate.compare birthDate columnSession.startDate of
                                        LT ->
                                            True

                                        _ ->
                                            False
                                )
                            |> Maybe.withDefault False
                    )
                |> greedyGroupsOf 6
                |> List.map
                    (\groupOfSix ->
                        let
                            ages =
                                groupOfSix
                                    |> List.map
                                        (\( id, columnSession ) ->
                                            child.birthDate
                                                |> Maybe.map (\birthDate -> renderAgeMonthsDaysHtml language birthDate columnSession.startDate)
                                                |> Maybe.withDefault []
                                                |> th
                                                    [ classList
                                                        [ ( "center", True )
                                                        , ( "bottom", True )
                                                        , ( "aligned", True )
                                                        , ( "last", columnSession.startDate == dateOfLastAssessment )
                                                        , ( "date-header", True )
                                                        ]
                                                    ]
                                        )
                                    |> (::) ageCell
                                    |> tr []

                            heights =
                                groupOfSix
                                    |> List.map
                                        (\( id, _ ) ->
                                            Dict.get id heightValuesBySession
                                                |> Maybe.map viewHeightWithIndication
                                                |> Maybe.withDefault (text "--")
                                                |> List.singleton
                                                |> td [ class "center aligned" ]
                                        )
                                    |> (::) heightCell
                                    |> tr []

                            muacs =
                                groupOfSix
                                    |> List.map
                                        (\( id, _ ) ->
                                            Dict.get id muacValuesBySession
                                                |> Maybe.map .value
                                                |> Maybe.map
                                                    (\((MuacInCm cm) as muac) ->
                                                        span
                                                            [ class <| classForIndication <| muacIndicationToIndication <| muacIndication muac ]
                                                            [ text <| Debug.toString cm ++ translate language Translate.CentimeterShorthand ]
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

                                    maybeAgeInDays =
                                        Maybe.map (\birthDate -> diffDays birthDate height.dateMeasured) child.birthDate

                                    indication =
                                        maybeAgeInDays
                                            |> Maybe.andThen
                                                (\ageInDays ->
                                                    zScoreLengthHeightForAge zscores ageInDays child.gender (Centimetres cm)
                                                        |> Maybe.map (class << classForIndication << zScoreToIndication)
                                                )
                                            |> Maybe.Extra.toList

                                    value =
                                        Debug.toString cm ++ translate language Translate.CentimeterShorthand
                                in
                                span indication [ text value ]

                            viewWeightWithIndication weight =
                                let
                                    kg =
                                        case weight.value of
                                            WeightInKg kilos ->
                                                kilos

                                    maybeAgeInDays =
                                        Maybe.map (\birthDate -> diffDays birthDate weight.dateMeasured) child.birthDate

                                    indication =
                                        maybeAgeInDays
                                            |> Maybe.andThen
                                                (\ageInDays ->
                                                    zScoreWeightForAge zscores ageInDays child.gender (Kilograms kg)
                                                        |> Maybe.map (class << classForIndication << zScoreToIndication)
                                                )
                                            |> Maybe.Extra.toList

                                    value =
                                        Debug.toString kg ++ translate language Translate.KilogramShorthand
                                in
                                span indication [ text value ]

                            weights =
                                groupOfSix
                                    |> List.map
                                        (\( id, _ ) ->
                                            Dict.get id weightValuesBySession
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

        viewPhotoUrl (PhotoUrl url) =
            div
                [ classList
                    [ ( "image", True )
                    , ( "cache-upload", String.contains "cache-upload/images" url )
                    ]
                ]
                [ img [ src url, class "rotate-90" ] [] ]

        photos =
            photoValues
                |> List.map
                    (\photo ->
                        div
                            [ class "report card" ]
                            [ div
                                [ class "content" ]
                                [ child.birthDate
                                    |> Maybe.map (\birthDate -> text <| renderAgeMonthsDays language birthDate photo.dateMeasured)
                                    |> Maybe.withDefault emptyNode
                                ]
                            , viewPhotoUrl photo.value
                            ]
                    )
                |> div [ class "ui five report cards" ]

        zScoreViewCharts =
            case child.gender of
                Male ->
                    { heightForAge = ZScore.View.viewHeightForAgeBoys
                    , heightForAge2To5 = ZScore.View.viewHeightForAgeBoys2To5
                    , heightForAge5To19 = ZScore.View.viewHeightForAgeBoys5To19
                    , weightForAge = ZScore.View.viewWeightForAgeBoys
                    , weightForAge2To5 = ZScore.View.viewWeightForAgeBoys2To5
                    , weightForHeight = ZScore.View.viewWeightForHeightBoys
                    , weightForHeight2To5 = ZScore.View.viewWeightForHeight2To5Boys
                    }

                Female ->
                    { heightForAge = ZScore.View.viewHeightForAgeGirls
                    , heightForAge2To5 = ZScore.View.viewHeightForAgeGirls2To5
                    , heightForAge5To19 = ZScore.View.viewHeightForAgeGirls5To19
                    , weightForAge = ZScore.View.viewWeightForAgeGirls
                    , weightForAge2To5 = ZScore.View.viewWeightForAgeGirls2To5
                    , weightForHeight = ZScore.View.viewWeightForHeightGirls
                    , weightForHeight2To5 = ZScore.View.viewWeightForHeight2To5Girls
                    }

        current =
            getChildMeasurementData childId session

        -- This includes any edits that have been saved locally, but not as-you-type
        -- in the UI before you hit "Save" or "Update".
        getValues func =
            Dict.values (func historical)

        heightValues =
            getValues .heights

        weightValues =
            getValues .weights

        muacValues =
            getValues .muacs

        photoValues =
            getValues .photos

        nutritionValues =
            getValues .nutritions

        indexBySession values =
            values
                |> List.filterMap
                    (\value ->
                        case value.encounterId of
                            Just id ->
                                Just ( id, value )

                            Nothing ->
                                Nothing
                    )
                |> Dict.fromList

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
            List.filterMap (chartHeightForAge child) heightValues

        heightForAgeDataMonths =
            List.filterMap (chartHeightForAgeMonths child) heightValues

        weightForAgeData =
            List.filterMap (chartWeightForAge child) weightValues

        weightForLengthData =
            List.filterMap (chartWeightForLength heightValues) weightValues

        weightForHeightData =
            List.filterMap (chartWeightForHeight heightValues) weightValues

        birthDiff =
            case child.birthDate of
                Just birthDate ->
                    Gizra.NominalDate.diffMonths birthDate currentDate

                Nothing ->
                    0

        charts =
            div
                [ class "image-report" ]
                [ ZScore.View.viewMarkers
                , zScoreViewCharts.heightForAge language zscores heightForAgeData
                , zScoreViewCharts.heightForAge2To5 language zscores heightForAgeData
                , zScoreViewCharts.heightForAge5To19 language zscores heightForAgeDataMonths
                , zScoreViewCharts.weightForAge language zscores weightForAgeData
                , zScoreViewCharts.weightForAge2To5 language zscores weightForAgeData
                , zScoreViewCharts.weightForHeight language zscores weightForLengthData
                , zScoreViewCharts.weightForHeight2To5 language zscores weightForHeightData
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
            , heightWeightMuacTable
            , photos
            , charts
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


chartHeightForAge : Person -> Height -> Maybe ( Days, Centimetres )
chartHeightForAge child height =
    child.birthDate
        |> Maybe.map
            (\birthDate ->
                ( diffDays birthDate height.dateMeasured
                  -- I suppose one could avoid this little transformation
                  -- by unifiying the two tags.
                , case height.value of
                    HeightInCm cm ->
                        Centimetres cm
                )
            )


chartHeightForAgeMonths : Person -> Height -> Maybe ( Months, Centimetres )
chartHeightForAgeMonths child height =
    child.birthDate
        |> Maybe.map
            (\birthDate ->
                ( Utils.NominalDate.diffMonths birthDate height.dateMeasured
                  -- I suppose one could avoid this little transformation
                  -- by unifiying the two tags.
                , case height.value of
                    HeightInCm cm ->
                        Centimetres cm
                )
            )


chartWeightForAge : Person -> Weight -> Maybe ( Days, Kilograms )
chartWeightForAge child weight =
    child.birthDate
        |> Maybe.map
            (\birthDate ->
                ( diffDays birthDate weight.dateMeasured
                  -- I suppose one could avoid this little transformation
                  -- by unifiying the two tags.
                , case weight.value of
                    WeightInKg cm ->
                        Kilograms cm
                )
            )


chartWeightForLength : List Height -> Weight -> Maybe ( Length, Kilograms )
chartWeightForLength heights weight =
    -- For each weight, we try to find a height with a matching sessionID.
    -- Eventually, we shouild take age into account to distingiush height
    -- and length.
    heights
        |> List.Extra.find (\height -> height.encounterId == weight.encounterId)
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


chartWeightForHeight : List Height -> Weight -> Maybe ( ZScore.Model.Height, Kilograms )
chartWeightForHeight heights weight =
    -- For each weight, we try to find a height with a matching sessionID.
    -- Eventually, we shouild take age into account to distingiush height
    -- and length.
    heights
        |> List.Extra.find (\height -> height.encounterId == weight.encounterId)
        |> Maybe.map
            (\height ->
                ( case height.value of
                    HeightInCm cm ->
                        ZScore.Model.Height cm
                , case weight.value of
                    WeightInKg kg ->
                        Kilograms kg
                )
            )
