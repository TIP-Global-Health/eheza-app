module Pages.AcuteIllnessProgressReport.View exposing (view)

import Activity.Model exposing (Activity(..), ChildActivity(..))
import AssocList as Dict exposing (Dict)
import Backend.AcuteIllnessEncounter.Model exposing (AcuteIllnessDiagnosis(..), AcuteIllnessProgressReportInitiator(..))
import Backend.Entities exposing (..)
import Backend.Measurement.Model exposing (..)
import Backend.Measurement.Utils exposing (getMeasurementValueFunc, muacIndication)
import Backend.Model exposing (ModelIndexedDb)
import Backend.Person.Model exposing (Gender(..), Person)
import Backend.Person.Utils exposing (ageInMonths, ageInYears, isChildUnderAgeOf5, isPersonAnAdult)
import Date
import EverySet exposing (EverySet)
import Gizra.Html exposing (emptyNode)
import Gizra.NominalDate exposing (NominalDate, diffDays, diffMonths, formatDDMMYY)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import List.Extra exposing (greedyGroupsOf)
import Maybe.Extra exposing (isJust, isNothing)
import Measurement.View exposing (renderDatePart, viewActionTakenLabel)
import Pages.AcuteIllnessActivity.Model exposing (NextStepsTask(..))
import Pages.AcuteIllnessActivity.Utils
    exposing
        ( muacRedOnSubsequentVisit
        , resolveAcuteIllnessDiagnosis
        , resolveAmoxicillinDosage
        , resolveCoartemDosage
        , resolveMedicationsNonAdministrationReasons
        , resolveORSDosage
        , resolveZincDosage
        , respiratoryRateAbnormalForAge
        , viewAmoxicillinAdministrationInstructions
        )
import Pages.AcuteIllnessActivity.View exposing (viewAdministeredMedicationLabel, viewHCRecommendation, viewOralSolutionPrescription, viewTabletsPrescription)
import Pages.AcuteIllnessEncounter.Model exposing (AcuteIllnessEncounterData, AssembledData)
import Pages.AcuteIllnessEncounter.Utils exposing (generateAssembledData)
import Pages.AcuteIllnessEncounter.View exposing (splitActivities, viewEndEncounterButton)
import Pages.AcuteIllnessProgressReport.Model exposing (..)
import Pages.GlobalCaseManagement.Utils exposing (calculateDueDate)
import Pages.Page exposing (Page(..), SessionPage(..), UserPage(..))
import Pages.Utils exposing (viewEndEncounterDialog)
import Pages.WellChildProgressReport.View exposing (viewNutritionSigns, viewPaneHeading, viewPersonInfoPane)
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


view : Language -> NominalDate -> AcuteIllnessEncounterId -> Bool -> AcuteIllnessProgressReportInitiator -> ModelIndexedDb -> Model -> Html Msg
view language currentDate id isChw initiator db model =
    let
        data =
            generateAssembledData currentDate id isChw db
    in
    viewWebData language (viewContent language currentDate id isChw initiator model) identity data


viewContent : Language -> NominalDate -> AcuteIllnessEncounterId -> Bool -> AcuteIllnessProgressReportInitiator -> Model -> AssembledData -> Html Msg
viewContent language currentDate id isChw initiator model data =
    let
        isFirstEncounter =
            List.isEmpty data.previousEncountersData

        diagnosisByCurrentEncounterMeasurements =
            resolveAcuteIllnessDiagnosis currentDate isChw data
                |> Maybe.withDefault NoAcuteIllnessDiagnosis

        currentEncounterData =
            AcuteIllnessEncounterData id
                data.encounter.startDate
                data.encounter.sequenceNumber
                diagnosisByCurrentEncounterMeasurements
                data.measurements

        firstEncounterData =
            if isFirstEncounter then
                Just currentEncounterData

            else
                List.head data.previousEncountersData

        subsequentEncountersData =
            if isFirstEncounter then
                []

            else
                firstEncounterData
                    |> Maybe.map
                        (\dataFirst ->
                            let
                                previousEncountersData =
                                    data.previousEncountersData
                                        |> List.filter (.id >> (/=) dataFirst.id)
                            in
                            previousEncountersData ++ [ currentEncounterData ]
                        )
                    |> Maybe.withDefault []

        ( _, pendingActivities ) =
            splitActivities currentDate isChw isFirstEncounter data

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

        diagnosis =
            Maybe.map Tuple.second data.diagnosis

        endEncounterButton =
            case initiator of
                InitiatorEncounterPage ->
                    viewEndEncounterButton language isFirstEncounter data.measurements pendingActivities diagnosis SetEndEncounterDialogState

                _ ->
                    emptyNode
    in
    div [ class "page-report acute-illness" ]
        [ viewHeader language id initiator
        , div
            [ class "ui report unstackable items" ]
            [ viewPersonInfoPane language currentDate data.person
            , viewAssessmentPane language currentDate isFirstEncounter firstEncounterData subsequentEncountersData data
            , viewSymptomsPane language currentDate isFirstEncounter firstEncounterData
            , viewPhysicalExamPane language currentDate firstEncounterData subsequentEncountersData data
            , viewNutritionSignsPane language currentDate firstEncounterData subsequentEncountersData data
            , viewTreatmentPane language currentDate isFirstEncounter firstEncounterData subsequentEncountersData
            , viewActionsTakenPane language currentDate firstEncounterData subsequentEncountersData data
            , viewNextStepsPane language currentDate data
            , endEncounterButton
            ]
        , viewModal endEncounterDialog
        ]


viewHeader : Language -> AcuteIllnessEncounterId -> AcuteIllnessProgressReportInitiator -> Html Msg
viewHeader language id initiator =
    let
        label =
            case initiator of
                InitiatorEncounterPage ->
                    Translate.ProgressReport

                _ ->
                    Translate.AcuteIllnessHistory

        goBackPage =
            case initiator of
                InitiatorEncounterPage ->
                    AcuteIllnessEncounterPage id

                InitiatorIndividualNutritionProgressReport nutritionEncounterId ->
                    NutritionProgressReportPage nutritionEncounterId

                InitiatorWellChildProgressReport wellChildEncounterId ->
                    WellChildProgressReportPage wellChildEncounterId

                InitiatorGroupNutritionProgressReport sessionId personId ->
                    SessionPage sessionId (ProgressReportPage personId)
    in
    div [ class "ui basic segment head" ]
        [ h1 [ class "ui header" ]
            [ text <| translate language label
            ]
        , span
            [ class "link-back"
            , onClick <| SetActivePage (UserPage goBackPage)
            ]
            [ span [ class "icon-back" ] []
            , span [] []
            ]
        ]


viewAssessmentPane :
    Language
    -> NominalDate
    -> Bool
    -> Maybe AcuteIllnessEncounterData
    -> List AcuteIllnessEncounterData
    -> AssembledData
    -> Html Msg
viewAssessmentPane language currentDate isFirstEncounter firstEncounterData subsequentEncountersData data =
    if isNothing data.diagnosis && isNothing data.previousDiagnosis then
        emptyNode

    else
        let
            viewAssessmentEntry date diagnosis status background =
                div [ class <| "entry " ++ background ]
                    [ div [ class "title" ] [ text <| translate language Translate.Assessment ++ ":" ]
                    , div [ class "assessment" ] [ text <| translate language <| Translate.AcuteIllnessDiagnosisWarning diagnosis ]
                    , div [ class "date" ] [ text <| (translate language <| Translate.AcuteIllnessStatus status) ++ ": " ++ formatDDMMYY date ]
                    ]

            currentAssessment =
                Maybe.map
                    (\( date, diagnosis ) ->
                        if isJust data.participant.outcome then
                            let
                                endDate =
                                    -- Illness resolution date is stored at data.participant.endDate.
                                    Maybe.withDefault date data.participant.endDate
                            in
                            viewAssessmentEntry endDate diagnosis AcuteIllnessResolved "green"

                        else
                            let
                                status =
                                    if isNothing data.previousDiagnosis then
                                        AcuteIllnessBegan

                                    else
                                        AcuteIllnessUpdated
                            in
                            viewAssessmentEntry date diagnosis status "red"
                    )
                    data.diagnosis
                    |> Maybe.withDefault emptyNode

            previousAssessment =
                Maybe.map
                    (\( date, diagnosis ) ->
                        if isJust data.participant.outcome then
                            emptyNode

                        else
                            viewAssessmentEntry date diagnosis AcuteIllnessBegan "orange"
                    )
                    data.previousDiagnosis
                    |> Maybe.withDefault emptyNode
        in
        div [ class "pane assessment" ]
            [ currentAssessment
            , previousAssessment
            ]


viewSymptomsPane : Language -> NominalDate -> Bool -> Maybe AcuteIllnessEncounterData -> Html Msg
viewSymptomsPane language currentDate isFirstEncounter firstEncounterData =
    let
        symptomsTable =
            firstEncounterData
                |> Maybe.map
                    (\dataFirst ->
                        let
                            symptomsMaxDuration getFunc measurement =
                                measurement
                                    |> Maybe.andThen (Tuple.second >> getFunc >> Dict.values >> List.maximum)
                                    |> Maybe.withDefault 1

                            maxDuration =
                                List.maximum
                                    [ symptomsMaxDuration .value dataFirst.measurements.symptomsGeneral
                                    , symptomsMaxDuration .value dataFirst.measurements.symptomsRespiratory
                                    , symptomsMaxDuration (.value >> .signs) dataFirst.measurements.symptomsGI
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
                                dataFirst.measurements.symptomsGeneral
                                    |> Maybe.map
                                        (Tuple.second
                                            >> .value
                                            >> filterSymptoms duration NoSymptomsGeneral
                                            >> List.map (\symptom -> li [ class "general" ] [ text <| translate language (Translate.SymptomsGeneralSign symptom) ])
                                        )
                                    |> Maybe.withDefault []

                            symptomsRespiratory duration =
                                dataFirst.measurements.symptomsRespiratory
                                    |> Maybe.map
                                        (Tuple.second
                                            >> .value
                                            >> filterSymptoms duration NoSymptomsRespiratory
                                            >> List.map (\symptom -> li [ class "respiratory" ] [ text <| translate language (Translate.SymptomsRespiratorySign symptom) ])
                                        )
                                    |> Maybe.withDefault []

                            symptomsGI duration =
                                dataFirst.measurements.symptomsGI
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
                                List.repeat maxDuration dataFirst.startDate
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
        [ viewPaneHeading language Translate.Symptoms
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
    -> Maybe AcuteIllnessEncounterData
    -> List AcuteIllnessEncounterData
    -> AssembledData
    -> Html Msg
viewPhysicalExamPane language currentDate firstEncounterData subsequentEncountersData data =
    let
        allEncountersData =
            firstEncounterData
                |> Maybe.map (\dataFirst -> dataFirst :: subsequentEncountersData)
                |> Maybe.withDefault []

        maybeAgeMonths =
            ageInMonths currentDate data.person

        showMuac =
            isChildUnderAgeOf5 currentDate data.person

        muacHeader =
            if showMuac then
                th [ class "muac" ] [ text <| translate language Translate.MUAC ]

            else
                emptyNode

        header =
            [ tr []
                [ th [ class "date" ] [ text <| translate language Translate.Date ]
                , th [ class "body-temperature" ] [ text <| translate language Translate.BodyTemperature ]
                , th [ class "respiratory-rate" ] [ text <| translate language Translate.RespiratoryRate ]
                , muacHeader
                ]
            ]

        rows =
            List.map
                (\encounterData ->
                    let
                        bodyTemperature =
                            encounterData.measurements.vitals
                                |> getMeasurementValueFunc
                                |> Maybe.map .bodyTemperature

                        bodyTemperatureValue =
                            Maybe.map
                                (\value ->
                                    String.fromFloat value ++ " " ++ translate language Translate.CelsiusAbbrev
                                )
                                bodyTemperature

                        bodyTemperatureWarning =
                            Maybe.andThen
                                (\bodyTemperature_ ->
                                    if bodyTemperature_ < 35 || bodyTemperature_ >= 37.5 then
                                        Just "red"

                                    else
                                        Nothing
                                )
                                bodyTemperature

                        respiratoryRate =
                            encounterData.measurements.vitals
                                |> getMeasurementValueFunc
                                |> Maybe.map .respiratoryRate

                        respiratoryRateValue =
                            Maybe.map
                                (\value ->
                                    translate language <| Translate.BpmUnit value
                                )
                                respiratoryRate

                        respiratoryRateWarning =
                            Maybe.andThen
                                (\respiratoryRate_ ->
                                    if respiratoryRateAbnormalForAge maybeAgeMonths respiratoryRate_ then
                                        Just "red"

                                    else
                                        Nothing
                                )
                                respiratoryRate

                        muac =
                            encounterData.measurements
                                |> .muac
                                |> getMeasurementValueFunc
                                |> Maybe.map (\(MuacInCm muac_) -> muac_)

                        muacValue =
                            Maybe.map String.fromFloat muac

                        muacWarning =
                            Maybe.map
                                (\muac_ ->
                                    case muacIndication (MuacInCm muac_) of
                                        ColorAlertRed ->
                                            "red"

                                        ColorAlertYellow ->
                                            "orange"

                                        ColorAlertGreen ->
                                            "green"
                                )
                                muac

                        muacCell =
                            if not showMuac then
                                emptyNode

                            else if isNothing muac then
                                viewNotTaken

                            else if muacWarning == Just "green" then
                                td [ class "muac" ] [ text <| "(" ++ (String.toLower <| translate language Translate.Normal) ++ ")" ]

                            else
                                viewValueWithAlert muacValue muacWarning "muac"
                    in
                    tr []
                        [ td [ class "date" ] [ text <| formatDDMMYY encounterData.startDate ]
                        , viewValueWithAlert bodyTemperatureValue bodyTemperatureWarning "body-temperature"
                        , viewValueWithAlert respiratoryRateValue respiratoryRateWarning "respiratory-rate"
                        , muacCell
                        ]
                )
                allEncountersData

        viewNotTaken =
            td [] [ text <| translate language Translate.NotTaken ]

        viewValueWithAlert maybeValue maybeAlert class_ =
            Maybe.map
                (\value ->
                    let
                        alert =
                            Maybe.map
                                (\color_ -> span [ class <| "alert " ++ color_ ] [])
                                maybeAlert
                                |> Maybe.withDefault emptyNode
                    in
                    td [ class class_ ]
                        [ span [] [ text value ]
                        , alert
                        ]
                )
                maybeValue
                |> Maybe.withDefault viewNotTaken
    in
    div [ class "pane physical-exam" ]
        [ viewPaneHeading language Translate.PhysicalExam
        , table [ class "ui collapsing celled table" ]
            [ thead [] header
            , tbody [] rows
            ]
        ]


viewNutritionSignsPane :
    Language
    -> NominalDate
    -> Maybe AcuteIllnessEncounterData
    -> List AcuteIllnessEncounterData
    -> AssembledData
    -> Html Msg
viewNutritionSignsPane language currentDate firstEncounterData subsequentEncountersData data =
    let
        nutritions =
            firstEncounterData
                |> Maybe.map (\dataFirst -> dataFirst :: subsequentEncountersData)
                |> Maybe.withDefault []
                |> List.filterMap
                    (.measurements
                        >> .nutrition
                        >> Maybe.map (\( _, measurement ) -> ( measurement.dateMeasured, measurement.value ))
                    )
    in
    if List.isEmpty nutritions then
        -- If there is no signs to display, we do not show the pane.
        -- Note that we do not record nutrition signs for adults.
        emptyNode

    else
        div [ class "pane nutrition-signs" ]
            [ viewPaneHeading language Translate.NitritionSigns
            , div [ class "pane-content" ] <|
                viewNutritionSigns language data.person nutritions
            ]


viewTreatmentPane :
    Language
    -> NominalDate
    -> Bool
    -> Maybe AcuteIllnessEncounterData
    -> List AcuteIllnessEncounterData
    -> Html Msg
viewTreatmentPane language currentDate isFirstEncounter firstEncounterData subsequentEncountersData =
    div [ class "pane treatment" ]
        [ viewPaneHeading language Translate.Treatment
        , div [ class "pane-content" ] <|
            viewTreatmentSigns language currentDate isFirstEncounter firstEncounterData subsequentEncountersData
        ]


viewTreatmentSigns :
    Language
    -> NominalDate
    -> Bool
    -> Maybe AcuteIllnessEncounterData
    -> List AcuteIllnessEncounterData
    -> List (Html Msg)
viewTreatmentSigns language currentDate isFirstEncounter firstEncounterData subsequentEncountersData =
    firstEncounterData
        |> Maybe.map
            (\dataFirst ->
                if isFirstEncounter then
                    let
                        treatmentReview =
                            dataFirst.measurements.treatmentReview
                                |> getMeasurementValueFunc

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
                                                , text ", "
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
                            dataFirst.measurements.medicationDistribution
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
                                            , text " "
                                            , text <| translate language <| Translate.To
                                            , text " "
                                            , text medications
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
                                (\dataSubsequent ->
                                    dataSubsequent.measurements.treatmentOngoing
                                        |> Maybe.map
                                            (Tuple.second
                                                >> .value
                                                >> viewTreatmentOngoing
                                                >> List.append
                                                    [ div [ class "visit-date" ]
                                                        [ text <| translate language <| Translate.On
                                                        , text " "
                                                        , text <| formatDDMMYY dataSubsequent.startDate
                                                        , text " :"
                                                        ]
                                                    ]
                                            )
                                        |> Maybe.withDefault []
                                )
                            |> List.concat
            )
        |> Maybe.withDefault []


viewActionsTakenPane :
    Language
    -> NominalDate
    -> Maybe AcuteIllnessEncounterData
    -> List AcuteIllnessEncounterData
    -> AssembledData
    -> Html Msg
viewActionsTakenPane language currentDate firstEncounterData subsequentEncountersData data =
    let
        actionsTakenFirstEncounter =
            firstEncounterData
                |> Maybe.map
                    (\dataFirst ->
                        if dataFirst.diagnosis == DiagnosisCovid19Suspect then
                            viewActionsTakenCovid19Suspect language dataFirst.startDate dataFirst.measurements

                        else if List.member dataFirst.diagnosis [ DiagnosisCovid19Suspect, DiagnosisPneuminialCovid19, DiagnosisLowRiskCovid19 ] then
                            viewActionsTakenCovid19Confirmed language dataFirst.startDate data.person dataFirst.diagnosis dataFirst.measurements

                        else
                            viewActionsTakenNonCovid19 language dataFirst.startDate data.person dataFirst.diagnosis dataFirst.measurements
                    )
                |> Maybe.withDefault emptyNode

        actionsTakenSubsequentEncounters =
            subsequentEncountersData
                |> List.map
                    (\dataSubsequent ->
                        viewActionsTakenNonCovid19 language dataSubsequent.startDate data.person dataSubsequent.diagnosis dataSubsequent.measurements
                    )
                |> List.reverse

        content =
            actionsTakenSubsequentEncounters
                ++ [ actionsTakenFirstEncounter ]
                |> div [ class "instructions" ]
    in
    div [ class "pane actions-taken" ]
        [ viewPaneHeading language Translate.ActionsTaken
        , content
        ]


viewActionsTakenCovid19Suspect : Language -> NominalDate -> AcuteIllnessMeasurements -> Html Msg
viewActionsTakenCovid19Suspect language date measurements =
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
                                [ viewActionTakenLabel language Translate.Contacted114 "icon-phone" (Just date)
                                , recommenationOf114
                                , recommenationOfSite
                                ]
                           )
                    )
                |> Maybe.withDefault []
    in
    called114Action
        ++ viewContacedHCAction language date measurements
        ++ viewPatientIsolatedAction language date measurements
        |> div [ class "encounter-actions" ]


viewActionsTakenCovid19Confirmed : Language -> NominalDate -> Person -> AcuteIllnessDiagnosis -> AcuteIllnessMeasurements -> Html Msg
viewActionsTakenCovid19Confirmed language date person diagnosis measurements =
    viewPatientIsolatedAction language date measurements
        ++ viewActionsTakenMedicationDistribution language date person diagnosis measurements
        ++ viewActionsTakenSendToHC language date measurements
        |> div [ class "encounter-actions" ]


viewActionsTakenNonCovid19 : Language -> NominalDate -> Person -> AcuteIllnessDiagnosis -> AcuteIllnessMeasurements -> Html Msg
viewActionsTakenNonCovid19 language date person diagnosis measurements =
    viewActionsTakenMedicationDistribution language date person diagnosis measurements
        ++ viewContacedHCAction language date measurements
        ++ viewActionsTakenSendToHC language date measurements
        ++ viewActionsTakenHealthEducation language date measurements
        |> div [ class "encounter-actions" ]


viewPatientIsolatedAction : Language -> NominalDate -> AcuteIllnessMeasurements -> List (Html Msg)
viewPatientIsolatedAction language date measurements =
    getMeasurementValueFunc measurements.isolation
        |> Maybe.map
            (\value ->
                if EverySet.member PatientIsolated value.signs then
                    [ viewActionTakenLabel language Translate.IsolatedAtHome "icon-patient-in-bed" (Just date) ]

                else
                    []
            )
        |> Maybe.withDefault []


viewContacedHCAction : Language -> NominalDate -> AcuteIllnessMeasurements -> List (Html Msg)
viewContacedHCAction language date measurements =
    getMeasurementValueFunc measurements.hcContact
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
                    [ viewActionTakenLabel language Translate.ContactedHC "icon-phone" (Just date)
                    , viewHCRecommendationActionTaken language recommendation
                    ]

                else
                    []
            )
        |> Maybe.withDefault []


viewActionsTakenMedicationDistribution : Language -> NominalDate -> Person -> AcuteIllnessDiagnosis -> AcuteIllnessMeasurements -> List (Html Msg)
viewActionsTakenMedicationDistribution language date person diagnosis measurements =
    let
        distributionSigns =
            Maybe.map (Tuple.second >> .value >> .distributionSigns) measurements.medicationDistribution

        nonAdministrationReasons =
            resolveMedicationsNonAdministrationReasons measurements

        resolveNonAdministrationReason medicine_ =
            nonAdministrationReasons
                |> List.filterMap
                    (\( medicine, reason ) ->
                        if medicine == medicine_ then
                            Just reason

                        else
                            Nothing
                    )
                |> List.head

        uncomplicatedPneumoniaActions =
            let
                amoxicillinPrescribed =
                    Maybe.map (EverySet.member Amoxicillin) distributionSigns
                        |> Maybe.withDefault False
            in
            if amoxicillinPrescribed then
                resolveAmoxicillinDosage date person
                    |> Maybe.map
                        (\( numberOfPills, pillMass, duration ) ->
                            viewAmoxicillinAdministrationInstructions language numberOfPills pillMass duration (Just date)
                        )
                    |> Maybe.withDefault []

            else
                resolveNonAdministrationReason Amoxicillin
                    |> Maybe.map
                        (\reason ->
                            [ viewNonAdministrationReason language (Translate.MedicationDistributionSign Amoxicillin) "icon-pills" (Just date) reason ]
                        )
                    |> Maybe.withDefault []
    in
    case diagnosis of
        DiagnosisMalariaUncomplicated ->
            let
                coartemPrescribed =
                    Maybe.map (EverySet.member Coartem) distributionSigns
                        |> Maybe.withDefault False
            in
            if coartemPrescribed then
                resolveCoartemDosage date person
                    |> Maybe.map
                        (\dosage ->
                            [ viewAdministeredMedicationLabel language Translate.Administered (Translate.MedicationDistributionSign Coartem) "icon-pills" (Just date)
                            , viewTabletsPrescription language dosage (Translate.ByMouthTwiceADayForXDays 3)
                            ]
                        )
                    |> Maybe.withDefault []

            else
                resolveNonAdministrationReason Coartem
                    |> Maybe.map
                        (\reason ->
                            [ viewNonAdministrationReason language (Translate.MedicationDistributionSign Coartem) "icon-pills" (Just date) reason ]
                        )
                    |> Maybe.withDefault []

        DiagnosisGastrointestinalInfectionUncomplicated ->
            let
                orsPrescribed =
                    Maybe.map (EverySet.member ORS) distributionSigns
                        |> Maybe.withDefault False

                orsAction =
                    if orsPrescribed then
                        Maybe.map
                            (\dosage ->
                                [ viewAdministeredMedicationLabel language Translate.Administered (Translate.MedicationDistributionSign ORS) "icon-oral-solution" (Just date)
                                , viewOralSolutionPrescription language dosage
                                ]
                            )
                            (resolveORSDosage date person)
                            |> Maybe.withDefault []

                    else
                        resolveNonAdministrationReason ORS
                            |> Maybe.map
                                (\reason ->
                                    [ viewNonAdministrationReason language (Translate.MedicationDistributionSign ORS) "icon-oral-solution" (Just date) reason ]
                                )
                            |> Maybe.withDefault []

                zincPrescribed =
                    Maybe.map (EverySet.member Zinc) distributionSigns
                        |> Maybe.withDefault False

                zincAction =
                    if zincPrescribed then
                        Maybe.map
                            (\dosage ->
                                [ viewAdministeredMedicationLabel language Translate.Administered (Translate.MedicationDistributionSign Zinc) "icon-pills" (Just date)
                                , viewTabletsPrescription language dosage (Translate.ByMouthDaylyForXDays 10)
                                ]
                            )
                            (resolveZincDosage date person)
                            |> Maybe.withDefault []

                    else
                        resolveNonAdministrationReason Zinc
                            |> Maybe.map
                                (\reason ->
                                    [ viewNonAdministrationReason language (Translate.MedicationDistributionSign Zinc) "icon-pills" (Just date) reason ]
                                )
                            |> Maybe.withDefault []
            in
            orsAction ++ zincAction

        DiagnosisSimpleColdAndCough ->
            [ viewAdministeredMedicationLabel language Translate.Administered (Translate.MedicationDistributionSign LemonJuiceOrHoney) "icon-pills" (Just date) ]

        DiagnosisRespiratoryInfectionUncomplicated ->
            uncomplicatedPneumoniaActions

        DiagnosisPneuminialCovid19 ->
            uncomplicatedPneumoniaActions

        _ ->
            []


viewNonAdministrationReason : Language -> TranslationId -> String -> Maybe NominalDate -> AdministrationNote -> Html any
viewNonAdministrationReason language medicineTranslationId iconClass maybeDate reason =
    let
        message =
            div [] <|
                [ span [ class "medicine" ] [ text <| translate language medicineTranslationId ]
                , text " "
                , text <| translate language <| Translate.RecommendedButNotGivenDueTo
                , text ": "
                , text <| translate language <| Translate.AdministrationNote reason
                ]
                    ++ renderDatePart language maybeDate
    in
    div [ class "header icon-label" ] <|
        [ i [ class iconClass ] []
        , message
        ]


viewActionsTakenSendToHC : Language -> NominalDate -> AcuteIllnessMeasurements -> List (Html Msg)
viewActionsTakenSendToHC language date measurements =
    let
        sendToHCSigns =
            getMeasurementValueFunc measurements.sendToHC

        completedForm =
            Maybe.map (.signs >> EverySet.member HandReferrerForm) sendToHCSigns
                |> Maybe.withDefault False

        completedFormAction =
            if completedForm then
                [ viewActionTakenLabel language Translate.CompletedHCReferralForm "icon-forms" (Just date) ]

            else
                []

        sentToHC =
            Maybe.map (.signs >> EverySet.member ReferToHealthCenter) sendToHCSigns
                |> Maybe.withDefault False

        sentToHCAction =
            if sentToHC then
                [ viewActionTakenLabel language Translate.SentPatientToHC "icon-shuttle" (Just date) ]

            else
                []
    in
    completedFormAction ++ sentToHCAction


viewHCRecommendationActionTaken : Language -> HCRecommendation -> Html any
viewHCRecommendationActionTaken language recommendation =
    if recommendation == HCRecommendationNotApplicable then
        emptyNode

    else
        div [ class "recommendation" ]
            [ viewHCRecommendation language recommendation
            , span [] [ text "." ]
            ]


viewActionsTakenHealthEducation : Language -> NominalDate -> AcuteIllnessMeasurements -> List (Html Msg)
viewActionsTakenHealthEducation language date measurements =
    let
        healthEducationProvided =
            Maybe.map
                (Tuple.second
                    >> .value
                    >> .signs
                    >> EverySet.toList
                    >> (\signs ->
                            case signs of
                                [] ->
                                    False

                                [ NoHealthEducationSigns ] ->
                                    False

                                _ ->
                                    True
                       )
                )
                measurements.healthEducation
                |> Maybe.withDefault False
    in
    if healthEducationProvided then
        [ viewActionTakenLabel language Translate.ProvidedHealthEducationAction "icon-open-book" (Just date) ]

    else
        []


viewNextStepsPane :
    Language
    -> NominalDate
    -> AssembledData
    -> Html Msg
viewNextStepsPane language currentDate data =
    if isJust data.participant.outcome then
        -- Illness resolved, therefore, we do not show
        -- Next Steps pane.
        emptyNode

    else
        let
            instructions =
                data.measurements.followUp
                    |> getMeasurementValueFunc
                    |> Maybe.andThen (EverySet.toList >> List.head)
                    |> Maybe.map
                        (\followUp ->
                            let
                                followUpDate =
                                    calculateDueDate data.encounter.startDate followUp

                                diff =
                                    diffDays currentDate followUpDate
                            in
                            if diff > 0 then
                                [ text <| translate language Translate.FollowUpWithPatientIn
                                , text " "
                                , span [ class "in-days" ] [ text <| String.toLower <| translate language <| Translate.DaySinglePlural diff ]
                                , text " "
                                , text <| String.toLower <| translate language Translate.On
                                , text " "
                                , text <| formatDDMMYY followUpDate
                                , text "."
                                ]

                            else
                                [ text <| translate language Translate.FollowUpWithPatientOn
                                , text " "
                                , text <| formatDDMMYY followUpDate
                                , text "."
                                ]
                        )
                    |> Maybe.withDefault []
        in
        div [ class "pane next-steps" ]
            [ viewPaneHeading language Translate.NextSteps
            , div [ class "instructions" ]
                instructions
            ]
