module Pages.ProgressReport.View exposing (view, viewFoundChild)

import Activity.Model exposing (Activity(..), ChildActivity(..))
import AssocList as Dict exposing (Dict)
import Backend.Entities exposing (..)
import Backend.Measurement.Model exposing (..)
import Backend.Measurement.Utils exposing (currentValue, currentValueWithId, mapMeasurementData, muacIndication)
import Backend.Model exposing (ModelIndexedDb)
import Backend.NutritionEncounter.Utils exposing (generatePreviousMeasurementsForChild)
import Backend.Person.Model exposing (Gender(..), Person)
import Backend.PmtctParticipant.Model exposing (AdultActivities(..))
import Backend.Session.Model exposing (EditableSession, Session)
import Backend.Session.Utils exposing (getChild, getChildMeasurementData, getMyMother)
import Date
import EverySet exposing (EverySet)
import Gizra.Html exposing (emptyNode)
import Gizra.NominalDate exposing (NominalDate)
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
import Restful.Endpoint exposing (fromEntityUuid)
import Translate exposing (Language, TranslationId, translate)
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

                individualChildMeasurements =
                    generatePreviousMeasurementsForChild childId db

                mother =
                    getMyMother childId session.offlineSession
                        |> Maybe.map Tuple.second

                relation =
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

                -- We're using the current value from the current session here, at
                -- least for now. So, we're ignoring any later sessions (normally,
                -- there wouldn't be any), and we're just leaving it blank if it wasn't
                -- entered in this session (rather than looking back to a previous
                -- session when it was entered).
                --
                -- See <https://github.com/Gizra/ihangane/issues/382#issuecomment-353273873>
                currentNutritionSigns =
                    getChildMeasurementData childId session
                        |> LocalData.map
                            (mapMeasurementData .nutrition
                                >> currentValue
                                >> Maybe.map .value
                                >> Maybe.withDefault EverySet.empty
                            )
                        |> LocalData.withDefault EverySet.empty

                defaultLastAssessmentDate =
                    session.offlineSession.session.startDate

                goBackAction =
                    ChildPage childId
                        |> SessionPage sessionId
                        |> UserPage
                        |> Pages.Session.Model.SetActivePage
            in
            viewWebData language
                (viewFoundChild language currentDate zscores ( childId, child ) individualChildMeasurements mother relation currentNutritionSigns defaultLastAssessmentDate goBackAction)
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


viewFoundChild :
    Language
    -> NominalDate
    -> ZScore.Model.Model
    -> ( PersonId, Person )
    -> List ( NominalDate, ( NutritionEncounterId, NutritionMeasurements ) )
    -> Maybe Person
    -> TranslationId
    -> EverySet ChildNutritionSign
    -> NominalDate
    -> msg
    -> ( Dict SessionId Session, ChildMeasurementList )
    -> Html msg
viewFoundChild language currentDate zscores ( childId, child ) individualChildMeasurements maybeMother relation signs defaultLastAssessmentDate goBackAction ( expected, historical ) =
    let
        -- GROUP CONTEXT.
        expectedSessions =
            expected
                |> Dict.toList
                |> List.map (\( uuid, expectedSession ) -> ( fromEntityUuid uuid, expectedSession.startDate ))
                |> List.filter hasGroupMeasurement

        -- Do we have any kind of measurement for the child for the specified session?
        hasGroupMeasurement ( id, _ ) =
            Dict.member id heightValuesBySession
                || Dict.member id muacValuesBySession
                || Dict.member id weightValuesBySession
                || Dict.member id nutritionValuesBySession
                || Dict.member id photoValuesBySession

        -- This includes any edits that have been saved locally, but not as-you-type
        -- in the UI before you hit "Save" or "Update".
        valuesIndexedBySession func =
            Dict.values (func historical)
                |> List.filterMap
                    (\measurement ->
                        measurement.encounterId
                            |> Maybe.map
                                (\encounterId ->
                                    ( fromEntityUuid encounterId
                                    , { dateMeasured = measurement.dateMeasured
                                      , encounterId = fromEntityUuid encounterId
                                      , value = measurement.value
                                      }
                                    )
                                )
                    )
                |> Dict.fromList

        heightValuesBySession =
            valuesIndexedBySession .heights

        weightValuesBySession =
            valuesIndexedBySession .weights

        muacValuesBySession =
            valuesIndexedBySession .muacs

        photoValuesBySession =
            valuesIndexedBySession .photos

        nutritionValuesBySession =
            valuesIndexedBySession .nutritions

        -- INDIVIDUAL CONTEXT.
        expectedlEncounters =
            individualChildMeasurements
                |> List.map (\( startDate, ( uuid, _ ) ) -> ( fromEntityUuid uuid, startDate ))
                |> List.filter hasEncounterMeasurement

        -- Do we have any kind of measurement for the child for the specified encounter?
        hasEncounterMeasurement ( id, _ ) =
            Dict.member id heightValuesByEncounter
                || Dict.member id muacValuesByEncounter
                || Dict.member id weightValuesByEncounter
                || Dict.member id nutritionValuesByEncounter
                || Dict.member id photoValuesByEncounter

        valuesIndexedByEncounter func =
            individualChildMeasurements
                |> List.filterMap
                    (\( startDate, ( uuid, measurements ) ) ->
                        func measurements
                            |> Maybe.andThen
                                (Tuple.second
                                    >> (\measurement ->
                                            measurement.encounterId
                                                |> Maybe.map
                                                    (\encounterId ->
                                                        ( fromEntityUuid encounterId
                                                        , { dateMeasured = measurement.dateMeasured
                                                          , encounterId = fromEntityUuid encounterId
                                                          , value = measurement.value
                                                          }
                                                        )
                                                    )
                                       )
                                )
                    )
                |> Dict.fromList

        heightValuesByEncounter =
            valuesIndexedByEncounter .height

        weightValuesByEncounter =
            valuesIndexedByEncounter .weight

        muacValuesByEncounter =
            valuesIndexedByEncounter .muac

        photoValuesByEncounter =
            valuesIndexedByEncounter .photo

        nutritionValuesByEncounter =
            valuesIndexedByEncounter .nutrition

        -- COMMON CONTEXT --
        sessionsAndEncounters =
            expectedSessions
                ++ expectedlEncounters
                |> List.sortWith (\s1 s2 -> Gizra.NominalDate.compare (Tuple.second s1) (Tuple.second s2))
                |> List.reverse

        heightValuesIndexed =
            Dict.union heightValuesBySession heightValuesByEncounter

        muacValuesIndexed =
            Dict.union muacValuesBySession muacValuesByEncounter

        weightValuesIndexed =
            Dict.union weightValuesBySession weightValuesByEncounter

        heightValues =
            Dict.values heightValuesIndexed

        muacValues =
            Dict.values muacValuesIndexed

        weightValues =
            Dict.values weightValuesIndexed

        photoValues =
            Dict.values photoValuesBySession ++ Dict.values photoValuesByEncounter

        -- We use the date of the last session that actually had a measurement.
        -- If there are no measurements, we use the date for the current
        -- session.
        dateOfLastAssessment =
            List.head sessionsAndEncounters
                |> Maybe.map Tuple.second
                |> Maybe.withDefault defaultLastAssessmentDate

        backIcon =
            a
                [ class "icon-back"
                , onClick goBackAction
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
                , text <| renderDate language dateOfLastAssessment
                ]

        childInfo =
            viewChildInfo language child maybeMother relation dateOfLastAssessment

        nutritionSigns =
            viewNutritionSigns language child dateOfLastAssessment signs

        heightWeightMuacTable =
            sessionsAndEncounters
                |> greedyGroupsOf 6
                |> List.map
                    (\groupOfSix ->
                        let
                            ages =
                                groupOfSix
                                    |> List.map
                                        (\( id, startDate ) ->
                                            child.birthDate
                                                |> Maybe.map (\birthDate -> renderAgeMonthsDaysHtml language birthDate startDate)
                                                |> Maybe.withDefault []
                                                |> th
                                                    [ classList
                                                        [ ( "center", True )
                                                        , ( "bottom", True )
                                                        , ( "aligned", True )
                                                        , ( "last", startDate == dateOfLastAssessment )
                                                        , ( "date-header", True )
                                                        ]
                                                    ]
                                        )
                                    |> (::) (viewAgeCell language)
                                    |> tr []

                            heights =
                                groupOfSix
                                    |> List.map
                                        (\( id, _ ) ->
                                            Dict.get id heightValuesIndexed
                                                |> Maybe.map (viewHeightWithIndication language child zscores)
                                                |> withDefaultTextInCell
                                        )
                                    |> (::) (viewHeightCell language)
                                    |> tr []

                            muacs =
                                groupOfSix
                                    |> List.map
                                        (\( id, _ ) ->
                                            Dict.get id muacValuesIndexed
                                                |> Maybe.map (viewMuactWithIndication language)
                                                |> withDefaultTextInCell
                                        )
                                    |> (::) (viewMuacCell language)
                                    |> tr []

                            weights =
                                groupOfSix
                                    |> List.map
                                        (\( id, _ ) ->
                                            Dict.get id weightValuesIndexed
                                                |> Maybe.map (viewWeightWithIndication language child zscores)
                                                |> withDefaultTextInCell
                                        )
                                    |> (::) (viewWeightCell language)
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

        photos =
            viewPhotos language child photoValues

        zScoreViewCharts =
            case child.gender of
                Male ->
                    { heightForAge = ZScore.View.viewHeightForAgeBoys
                    , heightForAge2To5 = ZScore.View.viewHeightForAgeBoys2To5
                    , heightForAge5To19 = ZScore.View.viewHeightForAgeBoys5To19
                    , weightForAge = ZScore.View.viewWeightForAgeBoys
                    , weightForAge2To5 = ZScore.View.viewWeightForAgeBoys2To5
                    , weightForAge5To10 = ZScore.View.viewWeightForAgeBoys5To10
                    , weightForHeight = ZScore.View.viewWeightForHeightBoys
                    , weightForHeight2To5 = ZScore.View.viewWeightForHeight2To5Boys
                    }

                Female ->
                    { heightForAge = ZScore.View.viewHeightForAgeGirls
                    , heightForAge2To5 = ZScore.View.viewHeightForAgeGirls2To5
                    , heightForAge5To19 = ZScore.View.viewHeightForAgeGirls5To19
                    , weightForAge = ZScore.View.viewWeightForAgeGirls
                    , weightForAge2To5 = ZScore.View.viewWeightForAgeGirls2To5
                    , weightForAge5To10 = ZScore.View.viewWeightForAgeGirls5To10
                    , weightForHeight = ZScore.View.viewWeightForHeightGirls
                    , weightForHeight2To5 = ZScore.View.viewWeightForHeight2To5Girls
                    }

        heightForAgeData =
            List.filterMap (chartHeightForAge child) heightValues

        heightForAgeDaysData =
            heightForAgeData
                |> List.map (\( days, month, height ) -> ( days, height ))

        heightForAgeMonthsData =
            heightForAgeData
                |> List.map (\( days, month, height ) -> ( month, height ))

        weightForAgeData =
            List.filterMap (chartWeightForAge child) weightValues

        weightForAgeDaysData =
            weightForAgeData
                |> List.map (\( days, month, weight ) -> ( days, weight ))

        weightForAgeMonthsData =
            weightForAgeData
                |> List.map (\( days, month, weight ) -> ( month, weight ))

        weightForLengtAndHeighthData =
            List.filterMap (chartWeightForLengthAndHeight heightValues) weightValues

        weightForLengthData =
            weightForLengtAndHeighthData
                |> List.map (\( length, height, weight ) -> ( length, weight ))

        weightForHeightData =
            weightForLengtAndHeighthData
                |> List.map (\( length, height, weight ) -> ( height, weight ))

        childAgeInMonths =
            case child.birthDate of
                Just birthDate ->
                    Gizra.NominalDate.diffMonths birthDate currentDate

                Nothing ->
                    0

        charts =
            if childAgeInMonths <= 24 then
                div
                    [ class "image-report" ]
                    [ ZScore.View.viewMarkers
                    , zScoreViewCharts.heightForAge language zscores heightForAgeDaysData
                    , zScoreViewCharts.weightForAge language zscores weightForAgeDaysData
                    , zScoreViewCharts.weightForHeight language zscores weightForLengthData
                    ]

            else if childAgeInMonths <= 60 then
                div
                    [ class "image-report" ]
                    [ ZScore.View.viewMarkers
                    , zScoreViewCharts.heightForAge2To5 language zscores heightForAgeDaysData
                    , zScoreViewCharts.weightForAge2To5 language zscores weightForAgeDaysData
                    , zScoreViewCharts.weightForHeight2To5 language zscores weightForHeightData
                    ]

            else
                -- Child is older than 5 years.
                div
                    [ class "image-report" ]
                    [ ZScore.View.viewMarkers
                    , zScoreViewCharts.heightForAge5To19 language zscores heightForAgeMonthsData
                    , zScoreViewCharts.weightForAge5To10 language zscores weightForAgeMonthsData
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


viewChildInfo : Language -> Person -> Maybe Person -> TranslationId -> NominalDate -> Html any
viewChildInfo language child maybeMother relationText dateOfLastAssessment =
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


viewNutritionSigns : Language -> Person -> NominalDate -> EverySet ChildNutritionSign -> Html any
viewNutritionSigns language child dateOfLastAssessment signs =
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
                , (signs
                    |> EverySet.toList
                    |> List.map (translate language << Translate.ChildNutritionSignReport)
                    |> String.join ", "
                    |> text
                    |> List.singleton
                  )
                    |> td []
                ]
            ]
        ]


viewAgeCell : Language -> Html any
viewAgeCell language =
    th
        [ class "uppercase" ]
        [ text <| translate language Translate.AgeWord ]


viewHeightCell : Language -> Html any
viewHeightCell language =
    td
        [ class "first" ]
        [ text <| translate language (Translate.ActivityProgressReport (ChildActivity Height)) ]


viewWeightCell : Language -> Html any
viewWeightCell language =
    td
        [ class "first" ]
        [ text <| translate language (Translate.ActivityProgressReport (ChildActivity Weight)) ]


viewMuacCell : Language -> Html any
viewMuacCell language =
    td
        [ class "first" ]
        [ text <| translate language (Translate.ActivityProgressReport (ChildActivity Muac)) ]


withDefaultTextInCell : Maybe (Html any) -> Html any
withDefaultTextInCell maybeHtml =
    maybeHtml
        |> Maybe.withDefault (text "--")
        |> List.singleton
        |> td [ class "center aligned" ]


viewHeightWithIndication language child zscores height =
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


viewMuactWithIndication language muac =
    muac.value
        |> (\((MuacInCm cm) as muac_) ->
                span
                    [ class <| classForIndication <| muacIndicationToIndication <| muacIndication muac_ ]
                    [ text <| Debug.toString cm ++ translate language Translate.CentimeterShorthand ]
           )


viewWeightWithIndication language child zscores weight =
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


viewPhotos language child photos =
    let
        viewPhotoUrl (PhotoUrl url) =
            div
                [ classList
                    [ ( "image", True )
                    , ( "cache-upload", String.contains "cache-upload/images" url )
                    ]
                ]
                [ img [ src url, class "rotate-90" ] [] ]
    in
    photos
        |> List.sortWith (\m1 m2 -> Gizra.NominalDate.compare m1.dateMeasured m2.dateMeasured)
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


chartHeightForAge : Person -> { dateMeasured : NominalDate, encounterId : String, value : HeightInCm } -> Maybe ( Days, Months, Centimetres )
chartHeightForAge child height =
    child.birthDate
        |> Maybe.map
            (\birthDate ->
                ( diffDays birthDate height.dateMeasured
                , diffMonths birthDate height.dateMeasured
                , case height.value of
                    HeightInCm cm ->
                        Centimetres cm
                )
            )


chartWeightForAge : Person -> { dateMeasured : NominalDate, encounterId : String, value : WeightInKg } -> Maybe ( Days, Months, Kilograms )
chartWeightForAge child weight =
    child.birthDate
        |> Maybe.map
            (\birthDate ->
                ( diffDays birthDate weight.dateMeasured
                , diffMonths birthDate weight.dateMeasured
                , case weight.value of
                    WeightInKg kg ->
                        Kilograms kg
                )
            )


chartWeightForLengthAndHeight :
    List { dateMeasured : NominalDate, encounterId : String, value : HeightInCm }
    -> { dateMeasured : NominalDate, encounterId : String, value : WeightInKg }
    -> Maybe ( Length, ZScore.Model.Height, Kilograms )
chartWeightForLengthAndHeight heights weight =
    -- For each weight, we try to find a height with a matching sessionID.
    heights
        |> List.Extra.find (\height -> height.encounterId == weight.encounterId)
        |> Maybe.map
            (\height ->
                let
                    cm =
                        case height.value of
                            HeightInCm val ->
                                val
                in
                ( Length cm
                , ZScore.Model.Height cm
                , case weight.value of
                    WeightInKg kg ->
                        Kilograms kg
                )
            )
