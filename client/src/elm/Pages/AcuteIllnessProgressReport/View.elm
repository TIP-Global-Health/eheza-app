module Pages.AcuteIllnessProgressReport.View exposing (view)

import Activity.Model exposing (Activity(..), ChildActivity(..))
import AssocList as Dict exposing (Dict)
import Backend.AcuteIllnessEncounter.Model exposing (AcuteIllnessDiagnosis(..))
import Backend.Entities exposing (..)
import Backend.Measurement.Model exposing (..)
import Backend.Measurement.Utils exposing (muacIndication)
import Backend.Model exposing (ModelIndexedDb)
import Backend.Person.Model exposing (Gender(..), Person)
import Backend.Person.Utils exposing (ageInYears, isChildUnderAgeOf5, isPersonAnAdult)
import Date
import EverySet exposing (EverySet)
import Gizra.Html exposing (emptyNode)
import Gizra.NominalDate exposing (NominalDate, diffMonths, formatDDMMYY)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import List.Extra exposing (greedyGroupsOf)
import Maybe.Extra exposing (isNothing)
import Pages.AcuteIllnessActivity.Model exposing (NextStepsTask(..))
import Pages.AcuteIllnessActivity.Utils exposing (resolveAmoxicillinDosage, resolveCoartemDosage, resolveORSDosage, resolveZincDosage)
import Pages.AcuteIllnessActivity.View exposing (viewAdministeredMedicationLabel, viewHCRecommendation, viewOralSolutionPrescription, viewSendToHCActionLabel, viewTabletsPrescription)
import Pages.AcuteIllnessEncounter.Model exposing (AssembledData)
import Pages.AcuteIllnessEncounter.Utils
    exposing
        ( acuteIllnessDiagnosisToMaybe
        , dangerSignPresentOnSubsequentVisit
        , generateAssembledData
        , muacRedOnSubsequentVisit
        , noImprovementOnSubsequentVisit
        , resolveNextStepFirstEncounter
        , respiratoryRateElevated
        , sendToHCOnSubsequentVisit
        )
import Pages.AcuteIllnessEncounter.View exposing (splitActivities, viewEndEncounterButton)
import Pages.AcuteIllnessProgressReport.Model exposing (..)
import Pages.DemographicsReport.View exposing (viewItemHeading)
import Pages.Page exposing (Page(..), UserPage(..))
import Pages.Utils exposing (viewEndEncounterDialog)
import RemoteData exposing (RemoteData(..))
import Restful.Endpoint exposing (fromEntityUuid)
import Translate exposing (Language, TranslationId, translate)
import Translate.Model exposing (Language(..))
import Utils.Html exposing (thumbnailImage, viewModal)
import Utils.NominalDate exposing (renderAgeMonthsDays, renderDate)
import Utils.WebData exposing (viewWebData)


thumbnailDimensions : { width : Int, height : Int }
thumbnailDimensions =
    { width = 180
    , height = 180
    }


view : Language -> NominalDate -> AcuteIllnessEncounterId -> ModelIndexedDb -> Model -> Html Msg
view language currentDate id db model =
    let
        data =
            generateAssembledData id db
    in
    viewWebData language (viewContent language currentDate id model) identity data


viewContent : Language -> NominalDate -> AcuteIllnessEncounterId -> Model -> AssembledData -> Html Msg
viewContent language currentDate id model data =
    let
        isFirstEncounter =
            List.isEmpty data.previousMeasurementsWithDates

        firstEncounterData =
            if isFirstEncounter then
                Just ( currentDate, data.measurements )

            else
                List.head data.previousMeasurementsWithDates

        subsequentEncountersData =
            if isFirstEncounter then
                []

            else
                firstEncounterData
                    |> Maybe.map
                        (\( firstEncounterDate, _ ) ->
                            let
                                previousEncountersData =
                                    data.previousMeasurementsWithDates
                                        |> List.filter (\( date, _ ) -> date /= firstEncounterDate)
                            in
                            previousEncountersData ++ [ ( currentDate, data.measurements ) ]
                        )
                    |> Maybe.withDefault []

        ( _, pendingActivities ) =
            splitActivities currentDate isFirstEncounter data

        endEncounterDialog =
            if model.showEndEncounetrDialog then
                Just <|
                    viewEndEncounterDialog language
                        Translate.EndEncounterQuestion
                        Translate.OnceYouEndTheEncounter
                        (CloseEncounter id)
                        (SetEndEncounterDialogState False)

            else
                Nothing
    in
    div [ class "page-report acute-illness" ]
        [ div
            [ class "ui report unstackable items" ]
            [ viewHeader language currentDate id
            , viewPersonInfo language currentDate data.person data.measurements
            , viewAssessmentPane language currentDate isFirstEncounter firstEncounterData subsequentEncountersData data
            , viewSymptomsPane language currentDate isFirstEncounter firstEncounterData
            , viewPhysicalExamPane language currentDate firstEncounterData subsequentEncountersData data
            , viewActionsTakenPane language currentDate firstEncounterData subsequentEncountersData data
            , viewEndEncounterButton language isFirstEncounter data.measurements pendingActivities data.diagnosis SetEndEncounterDialogState
            ]
        , viewModal endEncounterDialog
        ]


viewHeader : Language -> NominalDate -> AcuteIllnessEncounterId -> Html Msg
viewHeader language currentDate id =
    div [ class "report-header" ]
        [ a
            [ class "icon-back"
            , onClick <| SetActivePage (UserPage (AcuteIllnessEncounterPage id))
            ]
            []
        , h1 [ class "ui report header" ]
            [ text <| translate language Translate.ProgressReport ]
        , p [ class "date" ]
            [ text <| translate language Translate.CurrentIllnessBegan
            , text " - "
            , text <| renderDate language currentDate
            ]
        ]


viewPersonInfo : Language -> NominalDate -> Person -> AcuteIllnessMeasurements -> Html Msg
viewPersonInfo language currentDate person measurements =
    let
        isAdult =
            isPersonAnAdult currentDate person
                |> Maybe.withDefault True

        ( thumbnailClass, maybeAge ) =
            if isAdult then
                ( "mother"
                , ageInYears currentDate person
                    |> Maybe.map (\age -> translate language <| Translate.YearsOld age)
                )

            else
                ( "child"
                , person.birthDate
                    |> Maybe.map
                        (\birthDate -> renderAgeMonthsDays language birthDate currentDate)
                )

        viewAge =
            maybeAge
                |> Maybe.map
                    (\age ->
                        p []
                            [ span [ class "label" ] [ text <| translate language Translate.AgeWord ++ ": " ]
                            , span [] [ text age ]
                            ]
                    )
                |> Maybe.withDefault emptyNode

        viewVillage =
            person.village
                |> Maybe.map
                    (\village ->
                        p []
                            [ span [ class "label" ] [ text <| translate language Translate.Village ++ ": " ]
                            , span [] [ text village ]
                            ]
                    )
                |> Maybe.withDefault emptyNode
    in
    div
        [ class "item person-details" ]
        [ div [ class "ui image" ]
            [ thumbnailImage thumbnailClass person.avatarUrl person.name thumbnailDimensions.height thumbnailDimensions.width
            ]
        , div [ class "content" ]
            [ h2 [ class "ui header" ]
                [ text person.name ]
            , viewAge
            , viewVillage
            ]
        ]


viewAssessmentPane :
    Language
    -> NominalDate
    -> Bool
    -> Maybe ( NominalDate, AcuteIllnessMeasurements )
    -> List ( NominalDate, AcuteIllnessMeasurements )
    -> AssembledData
    -> Html Msg
viewAssessmentPane language currentDate isFirstEncounter firstEncounterData subsequentEncountersData data =
    let
        assessment =
            data.diagnosis
                |> Maybe.map
                    (\diagnosis ->
                        let
                            diagnosisText =
                                text <| translate language <| Translate.AcuteIllnessDiagnosisWarning diagnosis

                            ( diagnosisSuffix, additionalObservations ) =
                                if isFirstEncounter then
                                    ( [], [] )

                                else
                                    let
                                        isImproving =
                                            not <| noImprovementOnSubsequentVisit currentDate data.person data.measurements

                                        respiratoryDistress =
                                            if respiratoryRateElevated currentDate data.person data.measurements then
                                                p [] [ text <| translate language Translate.RespiratoryDistress ]

                                            else
                                                emptyNode

                                        severeAcuteMalnutrition =
                                            if muacRedOnSubsequentVisit data.measurements then
                                                p [] [ text <| translate language Translate.SevereAcuteMalnutrition ]

                                            else
                                                emptyNode

                                        malnutritionWithComplications =
                                            if dangerSignPresentOnSubsequentVisit data.measurements then
                                                p [] [ text <| translate language Translate.MalnutritionWithComplications ]

                                            else
                                                emptyNode
                                    in
                                    ( [ text " - ["
                                      , text <| translate language <| Translate.ConditionImproving isImproving
                                      , text "]"
                                      ]
                                    , [ respiratoryDistress
                                      , severeAcuteMalnutrition
                                      , malnutritionWithComplications
                                      ]
                                    )
                        in
                        ((p [] <| diagnosisText :: diagnosisSuffix) :: additionalObservations)
                            |> div [ class "diagnosis" ]
                    )
                |> Maybe.withDefault emptyNode
    in
    div [ class "pane assessment" ]
        [ viewItemHeading language Translate.Assessment "blue"
        , assessment
            :: viewTreatmentSigns language currentDate isFirstEncounter firstEncounterData subsequentEncountersData
            |> div [ class "pane-content" ]
        ]


viewTreatmentSigns :
    Language
    -> NominalDate
    -> Bool
    -> Maybe ( NominalDate, AcuteIllnessMeasurements )
    -> List ( NominalDate, AcuteIllnessMeasurements )
    -> List (Html Msg)
viewTreatmentSigns language currentDate isFirstEncounter firstEncounterData subsequentEncountersData =
    firstEncounterData
        |> Maybe.map
            (\( _, firstEncounterMeasurements ) ->
                if isFirstEncounter then
                    let
                        treatmentReview =
                            firstEncounterMeasurements.treatmentReview
                                |> Maybe.map (Tuple.second >> .value)

                        viewTreatmentSignInfo sign signHelped signTransId =
                            treatmentReview
                                |> Maybe.map
                                    (\signs ->
                                        if EverySet.member sign signs then
                                            let
                                                medicationHelpedEnding =
                                                    EverySet.member signHelped signs
                                                        |> Translate.MedicationHelpedEnding
                                                        |> translate language
                                            in
                                            div [ class "treatment-comment" ]
                                                [ text <| translate language signTransId
                                                , text ","
                                                , b [] [ text medicationHelpedEnding ]
                                                , text "."
                                                ]

                                        else
                                            emptyNode
                                    )
                                |> Maybe.withDefault emptyNode
                    in
                    [ viewTreatmentSignInfo FeverPast6Hours FeverPast6HoursHelped Translate.MedicationForFeverPast6Hours
                    , viewTreatmentSignInfo MalariaToday MalariaTodayHelped Translate.MedicationForMalariaToday
                    , viewTreatmentSignInfo MalariaWithinPastMonth MalariaWithinPastMonthHelped Translate.MedicationForMalariaPastMonth
                    ]

                else
                    let
                        prescribedMedication =
                            firstEncounterMeasurements.medicationDistribution
                                |> Maybe.map
                                    (Tuple.second
                                        >> .value
                                        >> .distributionSigns
                                        >> EverySet.toList
                                        >> List.filter (\sign -> not <| List.member sign [ LemonJuiceOrHoney, NoMedicationDistributionSigns ])
                                    )
                                |> Maybe.withDefault []

                        viewTreatmentOngoing treatmentOngoing =
                            let
                                viewTakenAsPrescribed =
                                    if EverySet.member TakenAsPrescribed treatmentOngoing.signs then
                                        div [ class "treatment-comment" ]
                                            [ text "- "
                                            , text <| translate language <| Translate.TakingMedicationAsPrescribed True
                                            , text "."
                                            ]

                                    else
                                        div [ class "treatment-comment" ]
                                            [ text "- "
                                            , text <| translate language <| Translate.TakingMedicationAsPrescribed False
                                            , text " "
                                            , text <| translate language <| Translate.ReasonForNotTaking treatmentOngoing.reasonForNotTaking
                                            , text "."
                                            ]

                                missedDoses =
                                    if EverySet.member MissedDoses treatmentOngoing.signs then
                                        treatmentOngoing.missedDoses

                                    else
                                        0

                                viewMissedDoses =
                                    div [ class "treatment-comment" ]
                                        [ text "- "
                                        , text <| translate language <| Translate.MissedDosesOfMedicatgion missedDoses
                                        , text "."
                                        ]

                                viewAdverseEvents =
                                    if EverySet.member SideEffects treatmentOngoing.signs then
                                        let
                                            medications =
                                                prescribedMedication
                                                    |> List.map (Translate.MedicationDistributionSign >> translate language)
                                                    |> String.join ", "

                                            events =
                                                EverySet.toList treatmentOngoing.adverseEvents
                                                    |> List.map (Translate.AcuteIllnessAdverseEvent >> translate language)
                                        in
                                        [ div [ class "treatment-comment" ]
                                            [ text "- "
                                            , text <| translate language Translate.MedicationTaken
                                            , text ": "
                                            , text medications
                                            , text "."
                                            ]
                                        , div [ class "treatment-comment" ]
                                            [ text "- "
                                            , text <| translate language <| Translate.AdverseEventSinglePlural <| List.length events
                                            , text ": "
                                            , text <| String.join ", " events
                                            , text "."
                                            ]
                                        ]

                                    else
                                        []
                            in
                            [ viewTakenAsPrescribed
                            , viewMissedDoses
                            ]
                                ++ viewAdverseEvents
                    in
                    if List.isEmpty prescribedMedication then
                        []

                    else
                        subsequentEncountersData
                            |> List.reverse
                            |> List.map
                                (\( date, subsequentEncounterMeasurements ) ->
                                    subsequentEncounterMeasurements.treatmentOngoing
                                        |> Maybe.map
                                            (Tuple.second
                                                >> .value
                                                >> viewTreatmentOngoing
                                                >> List.append
                                                    [ div [ class "visit-date" ]
                                                        [ text <| translate language <| Translate.On
                                                        , text " "
                                                        , text <| formatDDMMYY date
                                                        , text " :"
                                                        ]
                                                    ]
                                            )
                                        |> Maybe.withDefault []
                                )
                            |> List.concat
            )
        |> Maybe.withDefault []


viewSymptomsPane : Language -> NominalDate -> Bool -> Maybe ( NominalDate, AcuteIllnessMeasurements ) -> Html Msg
viewSymptomsPane language currentDate isFirstEncounter firstEncounterData =
    let
        headingTransId =
            if isFirstEncounter then
                Translate.Symptoms

            else
                Translate.SymptomsAtFirstEncounter

        symptomsTable =
            firstEncounterData
                |> Maybe.map
                    (\( firstEncounterDate, measurements ) ->
                        let
                            symptomsMaxDuration getFunc measurement =
                                measurement
                                    |> Maybe.andThen (Tuple.second >> getFunc >> Dict.values >> List.maximum)
                                    |> Maybe.withDefault 1

                            maxDuration =
                                List.maximum
                                    [ symptomsMaxDuration .value measurements.symptomsGeneral
                                    , symptomsMaxDuration .value measurements.symptomsRespiratory
                                    , symptomsMaxDuration (.value >> .signs) measurements.symptomsGI
                                    ]
                                    |> Maybe.withDefault 1

                            filterSymptoms symptomDuration exclusion dict =
                                Dict.toList dict
                                    |> List.filterMap
                                        (\( symptom, count ) ->
                                            if symptom /= exclusion && count > symptomDuration then
                                                Just symptom

                                            else
                                                Nothing
                                        )

                            symptomsGeneral duration =
                                measurements.symptomsGeneral
                                    |> Maybe.map
                                        (Tuple.second
                                            >> .value
                                            >> filterSymptoms duration NoSymptomsGeneral
                                            >> List.map (\symptom -> li [ class "general" ] [ text <| translate language (Translate.SymptomsGeneralSign symptom) ])
                                        )
                                    |> Maybe.withDefault []

                            symptomsRespiratory duration =
                                measurements.symptomsRespiratory
                                    |> Maybe.map
                                        (Tuple.second
                                            >> .value
                                            >> filterSymptoms duration NoSymptomsRespiratory
                                            >> List.map (\symptom -> li [ class "respiratory" ] [ text <| translate language (Translate.SymptomsRespiratorySign symptom) ])
                                        )
                                    |> Maybe.withDefault []

                            symptomsGI duration =
                                measurements.symptomsGI
                                    |> Maybe.map
                                        (\measurement ->
                                            Tuple.second measurement
                                                |> .value
                                                |> .signs
                                                |> filterSymptoms duration NoSymptomsGI
                                                |> List.map
                                                    (\symptom ->
                                                        let
                                                            translation =
                                                                if symptom == Vomiting then
                                                                    Tuple.second measurement
                                                                        |> .value
                                                                        |> .derivedSigns
                                                                        |> EverySet.member IntractableVomiting
                                                                        |> Translate.IntractableVomiting

                                                                else
                                                                    Translate.SymptomsGISignAbbrev symptom
                                                        in
                                                        li [ class "gi" ] [ text <| translate language translation ]
                                                    )
                                        )
                                    |> Maybe.withDefault []

                            values =
                                List.repeat maxDuration firstEncounterDate
                                    |> List.indexedMap
                                        (\index date ->
                                            ( Date.add Date.Days (-1 * index) date |> formatDDMMYY
                                            , symptomsGeneral index ++ symptomsRespiratory index ++ symptomsGI index
                                            )
                                        )
                                    |> List.filter (Tuple.second >> List.isEmpty >> not)

                            totalValues =
                                List.length values
                        in
                        values
                            |> List.indexedMap
                                (\index ( date, symptoms ) ->
                                    let
                                        timeline =
                                            if index == 0 then
                                                viewTimeLineTop (totalValues == 1)

                                            else if index == totalValues - 1 then
                                                viewTimeLineBottom

                                            else
                                                viewTimeLineMiddle
                                    in
                                    div [ class "symptoms-table-row" ]
                                        [ div [ class "date" ] [ text date ]
                                        , div [ class "timeline" ] timeline
                                        , ul [] symptoms
                                        ]
                                )
                            |> div [ class "symptoms-table" ]
                    )
                |> Maybe.withDefault emptyNode
    in
    div [ class "pane symptoms" ]
        [ viewItemHeading language headingTransId "blue"
        , symptomsTable
        ]


viewTimeLineTop : Bool -> List (Html any)
viewTimeLineTop isSingle =
    [ div [ class "line half" ] []
    , div
        [ classList
            [ ( "line half", True )
            , ( "blue", not isSingle )
            ]
        ]
        [ img [ src "assets/images/icon-blue-ball.svg" ]
            []
        ]
    ]


viewTimeLineMiddle : List (Html any)
viewTimeLineMiddle =
    [ div [ class "line blue" ]
        [ img [ src "assets/images/icon-blue-circle.png" ]
            []
        ]
    ]


viewTimeLineBottom : List (Html any)
viewTimeLineBottom =
    [ div [ class "line half blue" ] []
    , div [ class "line half" ]
        [ img [ src "assets/images/icon-blue-circle.png" ]
            []
        ]
    ]


viewPhysicalExamPane :
    Language
    -> NominalDate
    -> Maybe ( NominalDate, AcuteIllnessMeasurements )
    -> List ( NominalDate, AcuteIllnessMeasurements )
    -> AssembledData
    -> Html Msg
viewPhysicalExamPane language currentDate firstEncounterData subsequentEncountersData data =
    let
        viewDateCell date =
            th [] [ text <| formatDDMMYY date ]

        viewBodyTemperatureCell maybeBodyTemperature =
            maybeBodyTemperature
                |> Maybe.map
                    (\bodyTemperature_ ->
                        if bodyTemperature_ < 37.5 then
                            td [] [ text <| "(" ++ (String.toLower <| translate language Translate.Normal) ++ ")" ]

                        else
                            td [ class "red" ] [ text <| String.fromFloat bodyTemperature_ ++ " " ++ translate language Translate.CelsiusAbbrev ]
                    )
                |> Maybe.withDefault (td [] [])

        viewRespiratoryRateCell maybeRespiratoryRate =
            maybeRespiratoryRate
                |> Maybe.map
                    (\respiratoryRate_ ->
                        if respiratoryRate_ < 20 then
                            td [] [ text <| "(" ++ (String.toLower <| translate language Translate.Normal) ++ ")" ]

                        else
                            td [ class "red" ] [ text <| translate language <| Translate.BpmUnit respiratoryRate_ ]
                    )
                |> Maybe.withDefault (td [] [])

        viewMuacCell maybeMuac =
            maybeMuac
                |> Maybe.map
                    (\(MuacInCm muac_) ->
                        let
                            muacColor =
                                case muacIndication (MuacInCm muac_) of
                                    MuacRed ->
                                        "red"

                                    MuacYellow ->
                                        "yellow"

                                    MuacGreen ->
                                        "green"
                        in
                        td [ class muacColor ] [ text <| String.fromFloat muac_ ]
                    )
                |> Maybe.withDefault (td [] [])

        allEncountersData =
            firstEncounterData
                |> Maybe.map
                    (\( date, measurements ) ->
                        ( date, measurements ) :: subsequentEncountersData
                    )
                |> Maybe.withDefault []

        tables =
            allEncountersData
                |> greedyGroupsOf 4
                |> List.map
                    (\groupOfFour ->
                        let
                            dates =
                                groupOfFour
                                    |> List.map Tuple.first

                            bodyTemperatures =
                                groupOfFour
                                    |> List.map
                                        (Tuple.second
                                            >> .vitals
                                            >> Maybe.map (Tuple.second >> .value >> .bodyTemperature)
                                        )

                            respiratoryRates =
                                groupOfFour
                                    |> List.map
                                        (Tuple.second
                                            >> .vitals
                                            >> Maybe.map (Tuple.second >> .value >> .respiratoryRate)
                                        )

                            muacs =
                                groupOfFour
                                    |> List.map
                                        (Tuple.second
                                            >> .muac
                                            >> Maybe.map (Tuple.second >> .value)
                                        )

                            tableHead =
                                th [ class "first" ] []
                                    :: List.map viewDateCell dates
                                    |> tr []
                                    |> List.singleton

                            feverRow =
                                td [ class "first" ] [ text <| translate language Translate.Fever ]
                                    :: List.map viewBodyTemperatureCell bodyTemperatures
                                    |> tr []

                            tachypneaRow =
                                td [ class "first" ] [ text <| translate language Translate.Tachypnea ]
                                    :: List.map viewRespiratoryRateCell respiratoryRates
                                    |> tr []

                            muacRow =
                                if isChildUnderAgeOf5 currentDate data.person then
                                    td [ class "first" ] [ text <| translate language Translate.MUAC ]
                                        :: List.map viewMuacCell muacs
                                        |> tr []

                                else
                                    emptyNode

                            tableBody =
                                [ feverRow
                                , tachypneaRow
                                , muacRow
                                ]
                        in
                        table
                            [ class "ui collapsing celled table" ]
                            [ thead [] tableHead
                            , tbody [] tableBody
                            ]
                    )

        heading =
            viewItemHeading language Translate.PhysicalExam "blue"

        nutrition =
            -- We show nutrition data of current encounter.
            data.measurements.nutrition
                |> Maybe.map (Tuple.second >> .value)

        nutritionSignsTable =
            nutrition
                |> Maybe.map
                    (viewNutritionSigns language currentDate)
                |> Maybe.withDefault emptyNode
    in
    (heading :: tables)
        ++ [ nutritionSignsTable ]
        |> div [ class "pane physical-exam" ]


viewNutritionSigns : Language -> NominalDate -> EverySet ChildNutritionSign -> Html any
viewNutritionSigns language dateOfLastAssessment signs =
    table
        [ class "ui celled table nutrition-signs" ]
        [ tbody []
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


viewActionsTakenPane :
    Language
    -> NominalDate
    -> Maybe ( NominalDate, AcuteIllnessMeasurements )
    -> List ( NominalDate, AcuteIllnessMeasurements )
    -> AssembledData
    -> Html Msg
viewActionsTakenPane language currentDate firstEncounterData subsequentEncountersData data =
    let
        actionsTakenFirstEncounter =
            firstEncounterData
                |> Maybe.map
                    (\( date, measurements ) ->
                        case resolveNextStepFirstEncounter date data.person data.diagnosis data.measurements of
                            -- This is COVID19 case
                            Just NextStepsIsolation ->
                                viewActionsTakenIsolationAndContactHC language date measurements

                            Just NextStepsMedicationDistribution ->
                                viewActionsTakenMedicationDistribution language date data.person data.diagnosis measurements

                            Just NextStepsSendToHC ->
                                viewActionsTakenSendToHC language date measurements

                            _ ->
                                emptyNode
                    )
                |> Maybe.withDefault emptyNode

        -- @todo: expand for other options
        actionsTakenSubsequentEncounters =
            subsequentEncountersData
                |> List.map
                    (\( date, measurements ) -> viewActionsTakenSendToHC language date measurements)
    in
    div [ class "pane actions-taken" ]
        [ viewItemHeading language Translate.ActionsTaken "blue"
        , actionsTakenFirstEncounter :: actionsTakenSubsequentEncounters |> List.reverse |> div [ class "instructions" ]
        ]


viewActionsTakenIsolationAndContactHC : Language -> NominalDate -> AcuteIllnessMeasurements -> Html Msg
viewActionsTakenIsolationAndContactHC language currentDate measurements =
    let
        called114Action =
            measurements.call114
                |> Maybe.map
                    (Tuple.second
                        >> .value
                        >> (\value ->
                                let
                                    viewRecommendation recommenation =
                                        div [ class "recommendation" ]
                                            [ text <| "- " ++ translate language recommenation ++ "."
                                            ]

                                    recommenationOf114 =
                                        value.recommendations114
                                            |> EverySet.toList
                                            -- There can be only one recommendation.
                                            |> List.head
                                            |> Maybe.map (Translate.ResultOfContacting114 >> viewRecommendation)
                                            |> Maybe.withDefault emptyNode

                                    recommenationOfSite =
                                        value.recommendationsSite
                                            |> EverySet.toList
                                            |> List.filter ((/=) RecommendationSiteNotApplicable)
                                            -- There can be only one recommendation.
                                            |> List.head
                                            |> Maybe.map (Translate.ResultOfContactingRecommendedSite >> viewRecommendation)
                                            |> Maybe.withDefault emptyNode
                                in
                                [ viewSendToHCActionLabel language Translate.Contacted114 "icon-phone" (Just currentDate)
                                , recommenationOf114
                                , recommenationOfSite
                                ]
                           )
                    )
                |> Maybe.withDefault []

        contacedHCValue =
            measurements.hcContact
                |> Maybe.map (Tuple.second >> .value)

        contacedHC =
            contacedHCValue
                |> Maybe.map
                    (.signs
                        >> EverySet.member ContactedHealthCenter
                    )
                |> Maybe.withDefault False

        contacedHCAction =
            contacedHCValue
                |> Maybe.map
                    (\value ->
                        if EverySet.member ContactedHealthCenter value.signs then
                            let
                                recommendation =
                                    value.recommendations
                                        |> EverySet.toList
                                        |> List.head
                                        |> Maybe.withDefault HCRecommendationNotApplicable
                            in
                            [ viewSendToHCActionLabel language Translate.ContactedHC "icon-phone" (Just currentDate)
                            , viewHCRecommendationActionTaken language recommendation
                            ]

                        else
                            []
                    )
                |> Maybe.withDefault []

        patientIsolated =
            measurements.isolation
                |> Maybe.map
                    (Tuple.second
                        >> .value
                        >> .signs
                        >> EverySet.member PatientIsolated
                    )
                |> Maybe.withDefault False

        patientIsolatedAction =
            if patientIsolated then
                [ viewSendToHCActionLabel language Translate.IsolatedAtHome "icon-patient-in-bed" (Just currentDate) ]

            else
                []
    in
    called114Action
        ++ contacedHCAction
        ++ patientIsolatedAction
        |> div [ class "isolate-and-contact-hc" ]


viewActionsTakenMedicationDistribution : Language -> NominalDate -> Person -> Maybe AcuteIllnessDiagnosis -> AcuteIllnessMeasurements -> Html Msg
viewActionsTakenMedicationDistribution language currentDate person diagnosis measurements =
    let
        medicationSigns =
            Maybe.map (Tuple.second >> .value >> .distributionSigns) measurements.medicationDistribution
    in
    case diagnosis of
        Just DiagnosisMalariaUncomplicated ->
            let
                coartemPrescribed =
                    Maybe.map (EverySet.member Coartem) medicationSigns
                        |> Maybe.withDefault False
            in
            if coartemPrescribed then
                resolveCoartemDosage currentDate person
                    |> Maybe.map
                        (\dosage ->
                            div [ class "malaria-uncomplicated" ]
                                [ viewAdministeredMedicationLabel language Translate.Administered (Translate.MedicationDistributionSign Coartem) "icon-pills" (Just currentDate)
                                , viewTabletsPrescription language dosage (Translate.ByMouthTwiceADayForXDays 3)
                                ]
                        )
                    |> Maybe.withDefault emptyNode

            else
                emptyNode

        Just DiagnosisGastrointestinalInfectionUncomplicated ->
            let
                orsPrescribed =
                    Maybe.map (EverySet.member ORS) medicationSigns
                        |> Maybe.withDefault False

                orsAction =
                    if orsPrescribed then
                        Maybe.map
                            (\dosage ->
                                [ viewAdministeredMedicationLabel language Translate.Administered (Translate.MedicationDistributionSign ORS) "icon-oral-solution" (Just currentDate)
                                , viewOralSolutionPrescription language dosage
                                ]
                            )
                            (resolveORSDosage currentDate person)
                            |> Maybe.withDefault []

                    else
                        []

                zincPrescribed =
                    Maybe.map (EverySet.member Zinc) medicationSigns
                        |> Maybe.withDefault False

                zincAction =
                    if zincPrescribed then
                        Maybe.map
                            (\dosage ->
                                [ viewAdministeredMedicationLabel language Translate.Administered (Translate.MedicationDistributionSign Zinc) "icon-pills" (Just currentDate)
                                , viewTabletsPrescription language dosage (Translate.ByMouthDaylyForXDays 10)
                                ]
                            )
                            (resolveZincDosage currentDate person)
                            |> Maybe.withDefault []

                    else
                        []
            in
            if orsPrescribed || zincPrescribed then
                div [ class "gastrointestinal-uncomplicated" ] <|
                    (orsAction ++ zincAction)

            else
                emptyNode

        Just DiagnosisSimpleColdAndCough ->
            div [ class "simple-cough-and-cold" ]
                [ viewAdministeredMedicationLabel language Translate.Administered (Translate.MedicationDistributionSign LemonJuiceOrHoney) "icon-pills" (Just currentDate) ]

        Just DiagnosisRespiratoryInfectionUncomplicated ->
            let
                amoxicillinPrescribed =
                    Maybe.map (EverySet.member Amoxicillin) medicationSigns
                        |> Maybe.withDefault False
            in
            if amoxicillinPrescribed then
                resolveAmoxicillinDosage currentDate person
                    |> Maybe.map
                        (\dosage ->
                            div [ class "respiratory-infection-uncomplicated" ]
                                [ viewAdministeredMedicationLabel language Translate.Administered (Translate.MedicationDistributionSign Amoxicillin) "icon-pills" (Just currentDate)
                                , viewTabletsPrescription language dosage (Translate.ByMouthTwiceADayForXDays 5)
                                ]
                        )
                    |> Maybe.withDefault emptyNode

            else
                emptyNode

        _ ->
            emptyNode


viewActionsTakenSendToHC : Language -> NominalDate -> AcuteIllnessMeasurements -> Html Msg
viewActionsTakenSendToHC language currentDate measurements =
    let
        sendToHCSigns =
            Maybe.map (Tuple.second >> .value) measurements.sendToHC

        completedForm =
            Maybe.map (EverySet.member HandReferrerForm) sendToHCSigns
                |> Maybe.withDefault False

        completedFormAction =
            if completedForm then
                [ viewSendToHCActionLabel language Translate.CompletedHCReferralForm "icon-forms" (Just currentDate) ]

            else
                []

        sentToHC =
            Maybe.map (EverySet.member ReferToHealthCenter) sendToHCSigns
                |> Maybe.withDefault False

        sentToHCAction =
            if sentToHC then
                [ viewSendToHCActionLabel language Translate.SentPatientToHC "icon-shuttle" (Just currentDate) ]

            else
                []
    in
    if completedForm || sentToHC then
        div [ class "send-to-hc" ] <|
            (completedFormAction ++ sentToHCAction)

    else
        emptyNode


viewHCRecommendationActionTaken : Language -> HCRecommendation -> Html any
viewHCRecommendationActionTaken language recommendation =
    if recommendation == HCRecommendationNotApplicable then
        emptyNode

    else
        div [ class "recommendation" ]
            [ viewHCRecommendation language recommendation
            , span [] [ text "." ]
            ]
