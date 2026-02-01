module Pages.AcuteIllness.ProgressReport.View exposing (view)

import AssocList as Dict
import Backend.AcuteIllnessEncounter.Types exposing (AcuteIllnessDiagnosis(..), AcuteIllnessEncounterType(..), AcuteIllnessProgressReportInitiator(..))
import Backend.Entities exposing (..)
import Backend.Measurement.Model exposing (..)
import Backend.Measurement.Utils exposing (getMeasurementValueFunc, muacIndication)
import Backend.Model exposing (ModelIndexedDb)
import Backend.Person.Model exposing (Person)
import Backend.Person.Utils exposing (ageInMonths, isChildUnderAgeOf5, isPersonAnAdult)
import Components.ReportToWhatsAppDialog.Model
import Components.ReportToWhatsAppDialog.View
import Date
import EverySet exposing (EverySet)
import Gizra.Html exposing (emptyNode, showIf)
import Gizra.NominalDate exposing (NominalDate, diffDays, formatDDMMYYYY)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Maybe.Extra exposing (isJust, isNothing)
import Measurement.Utils exposing (renderDatePart)
import Measurement.View exposing (viewActionTakenLabel)
import Pages.AcuteIllness.Activity.Utils
    exposing
        ( resolveAmoxicillinDosage
        , resolveCoartemDosage
        , resolveMedicationsNonAdministrationReasons
        , resolveORSDosage
        , resolveZincDosage
        , respiratoryRateAbnormalForAge
        , viewAdministeredMedicationLabel
        , viewAmoxicillinAdministrationInstructions
        , viewHCRecommendation
        , viewOralSolutionPrescription
        , viewParacetamolAdministrationInstructions
        , viewTabletsPrescription
        )
import Pages.AcuteIllness.Encounter.Model exposing (AcuteIllnessEncounterData, AssembledData)
import Pages.AcuteIllness.Encounter.Utils exposing (generateAssembledData)
import Pages.AcuteIllness.Encounter.View exposing (allowEndingEncounter, partitionActivities)
import Pages.AcuteIllness.ProgressReport.Model exposing (AcuteIllnessStatus(..), Model, Msg(..))
import Pages.GlobalCaseManagement.Utils exposing (calculateDueDate)
import Pages.Page exposing (Page(..), SessionPage(..), UserPage(..))
import Pages.Utils exposing (viewConfirmationDialog, viewEndEncounterMenuForProgressReport)
import Pages.WellChild.ProgressReport.View exposing (viewNutritionSigns, viewPaneHeading, viewPersonInfoPane)
import SyncManager.Model exposing (Site, SiteFeature)
import Translate exposing (TranslationId, translate)
import Translate.Model exposing (Language)
import Utils.Html exposing (viewModal)
import Utils.WebData exposing (viewWebData)


view :
    Language
    -> NominalDate
    -> Site
    -> EverySet SiteFeature
    -> AcuteIllnessEncounterId
    -> Bool
    -> AcuteIllnessProgressReportInitiator
    -> ModelIndexedDb
    -> Model
    -> Html Msg
view language currentDate site features id isChw initiator db model =
    let
        assembled =
            generateAssembledData currentDate features id isChw db
    in
    viewWebData language (viewContent language currentDate site features id isChw initiator model) identity assembled


viewContent :
    Language
    -> NominalDate
    -> Site
    -> EverySet SiteFeature
    -> AcuteIllnessEncounterId
    -> Bool
    -> AcuteIllnessProgressReportInitiator
    -> Model
    -> AssembledData
    -> Html Msg
viewContent language currentDate site features id isChw initiator model assembled =
    let
        endEncounterDialog =
            if model.showEndEncounterDialog then
                Just <|
                    viewConfirmationDialog language
                        Translate.EndEncounterQuestion
                        Translate.OnceYouEndTheEncounter
                        (CloseEncounter id)
                        (SetEndEncounterDialogState False)

            else
                Nothing

        endEncounterMenu =
            case initiator of
                InitiatorEncounterPage ->
                    let
                        ( _, pendingActivities ) =
                            partitionActivities currentDate isChw assembled

                        allowEndEncounter =
                            allowEndingEncounter currentDate isChw assembled pendingActivities
                    in
                    viewEndEncounterMenuForProgressReport language
                        features
                        allowEndEncounter
                        SetEndEncounterDialogState
                        (MsgReportToWhatsAppDialog <|
                            Components.ReportToWhatsAppDialog.Model.SetState <|
                                Just Components.ReportToWhatsAppDialog.Model.Consent
                        )

                _ ->
                    emptyNode
    in
    div [ class "page-report acute-illness" ]
        [ viewHeader language id initiator
        , div
            [ class "ui report unstackable items"
            , Html.Attributes.id "report-content"
            ]
            [ viewPersonInfoPane language currentDate assembled.person
            , viewAssessmentPane language assembled.firstInitialWithSubsequent assembled.secondInitialWithSubsequent assembled
            , viewSymptomsPane language assembled.firstInitialWithSubsequent assembled.secondInitialWithSubsequent
            , viewPhysicalExamPane language currentDate assembled.firstInitialWithSubsequent assembled.secondInitialWithSubsequent assembled
            , viewNutritionSignsPane language assembled.firstInitialWithSubsequent assembled.secondInitialWithSubsequent
            , viewTreatmentPane language assembled.firstInitialWithSubsequent assembled.secondInitialWithSubsequent assembled
            , viewActionsTakenPane language assembled.firstInitialWithSubsequent assembled.secondInitialWithSubsequent assembled
            , viewNextStepsPane language currentDate assembled
            , -- Actions are hidden when 'Share via WhatsApp' dialog is open,
              -- so they do not appear on generated screenshot.
              showIf (isNothing model.reportToWhatsAppDialog.state) endEncounterMenu
            ]
        , viewModal endEncounterDialog
        , Html.map MsgReportToWhatsAppDialog
            (Components.ReportToWhatsAppDialog.View.view
                language
                site
                ( assembled.participant.person, assembled.person )
                Components.ReportToWhatsAppDialog.Model.ReportAcuteIllness
                Nothing
                model.reportToWhatsAppDialog
            )
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

                InitiatorPatientRecord patientRecordInitiator personId ->
                    PatientRecordPage patientRecordInitiator personId

                InitiatorNCDProgressReport ncdProgressReportInitiator ->
                    NCDProgressReportPage ncdProgressReportInitiator

                InitiatorChildScoreboardProgressReport childScoreboardEncounterId ->
                    ChildScoreboardProgressReportPage childScoreboardEncounterId
    in
    div [ class "ui basic segment head" ]
        [ h1 [ class "ui header" ]
            [ text <| translate language label ]
        , span
            [ class "link-back"
            , onClick <| SetActivePage (UserPage goBackPage)
            ]
            [ span [ class "icon-back" ] [] ]
        ]


viewAssessmentPane :
    Language
    -> List AcuteIllnessEncounterData
    -> List AcuteIllnessEncounterData
    -> AssembledData
    -> Html Msg
viewAssessmentPane language firstInitialWithSubsequent secondInitialWithSubsequent assembled =
    let
        encountersWithDiagnosis =
            firstInitialWithSubsequent
                ++ secondInitialWithSubsequent
                |> List.filter (.diagnosis >> (/=) NoAcuteIllnessDiagnosis)
                |> List.reverse
    in
    case encountersWithDiagnosis of
        [] ->
            emptyNode

        current :: previous ->
            let
                viewAssessmentEntry date diagnosis status background =
                    div [ class <| "entry " ++ background ]
                        [ div [ class "title" ] [ text <| translate language Translate.Assessment ++ ":" ]
                        , div [ class "assessment" ] [ text <| translate language <| Translate.AcuteIllnessDiagnosisWarning diagnosis ]
                        , div [ class "date" ] [ text <| (translate language <| Translate.AcuteIllnessStatus status) ++ ": " ++ formatDDMMYYYY date ]
                        ]

                ( currentAssessment, previousAssessments ) =
                    if isJust assembled.participant.outcome then
                        let
                            endDate =
                                -- Illness resolution date is stored at assembled.participant.endDate.
                                Maybe.withDefault current.startDate assembled.participant.endDate
                        in
                        ( viewAssessmentEntry endDate current.diagnosis AcuteIllnessResolved "green"
                        , []
                        )

                    else
                        let
                            status =
                                if List.isEmpty previous then
                                    AcuteIllnessBegan

                                else
                                    AcuteIllnessUpdated
                        in
                        ( viewAssessmentEntry current.startDate current.diagnosis status "red"
                        , List.indexedMap
                            (\index encounter ->
                                let
                                    diagnosisStatus =
                                        if (index + 1) == List.length previous then
                                            AcuteIllnessBegan

                                        else
                                            AcuteIllnessUpdated
                                in
                                viewAssessmentEntry encounter.startDate encounter.diagnosis diagnosisStatus "orange"
                            )
                            previous
                        )
            in
            div [ class "pane assessment" ] <|
                currentAssessment
                    :: previousAssessments


viewSymptomsPane : Language -> List AcuteIllnessEncounterData -> List AcuteIllnessEncounterData -> Html Msg
viewSymptomsPane language firstInitialWithSubsequent secondInitialWithSubsequent =
    let
        initialWithSubsequent =
            if List.isEmpty secondInitialWithSubsequent then
                firstInitialWithSubsequent

            else
                secondInitialWithSubsequent

        symptomsTable =
            List.head initialWithSubsequent
                |> Maybe.map
                    (\encounterData ->
                        let
                            symptomsMaxDuration getFunc measurement =
                                measurement
                                    |> Maybe.andThen (Tuple.second >> getFunc >> Dict.values >> List.maximum)
                                    |> Maybe.withDefault 1

                            maxDuration =
                                List.maximum
                                    [ symptomsMaxDuration .value encounterData.measurements.symptomsGeneral
                                    , symptomsMaxDuration .value encounterData.measurements.symptomsRespiratory
                                    , symptomsMaxDuration (.value >> .signs) encounterData.measurements.symptomsGI
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
                                encounterData.measurements.symptomsGeneral
                                    |> Maybe.map
                                        (Tuple.second
                                            >> .value
                                            >> filterSymptoms duration NoSymptomsGeneral
                                            >> List.map (\symptom -> li [ class "general" ] [ text <| translate language (Translate.SymptomsGeneralSign symptom) ])
                                        )
                                    |> Maybe.withDefault []

                            symptomsRespiratory duration =
                                encounterData.measurements.symptomsRespiratory
                                    |> Maybe.map
                                        (Tuple.second
                                            >> .value
                                            >> filterSymptoms duration NoSymptomsRespiratory
                                            >> List.map (\symptom -> li [ class "respiratory" ] [ text <| translate language (Translate.SymptomsRespiratorySign symptom) ])
                                        )
                                    |> Maybe.withDefault []

                            symptomsGI duration =
                                encounterData.measurements.symptomsGI
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
                                List.repeat maxDuration encounterData.startDate
                                    |> List.indexedMap
                                        (\index date ->
                                            ( Date.add Date.Days (-1 * index) date |> formatDDMMYYYY
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
    -> List AcuteIllnessEncounterData
    -> List AcuteIllnessEncounterData
    -> AssembledData
    -> Html Msg
viewPhysicalExamPane language currentDate firstInitialWithSubsequent secondInitialWithSubsequent assembled =
    let
        maybeAgeMonths =
            ageInMonths currentDate assembled.person

        showMuac =
            isChildUnderAgeOf5 currentDate assembled.person

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
            firstInitialWithSubsequent
                ++ secondInitialWithSubsequent
                |> List.map
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

                            muacCell =
                                if not showMuac then
                                    emptyNode

                                else
                                    let
                                        muac =
                                            encounterData.measurements
                                                |> .muac
                                                |> getMeasurementValueFunc
                                                |> Maybe.map (\(MuacInCm muac_) -> muac_)
                                    in
                                    if isNothing muac then
                                        viewNotTaken

                                    else
                                        let
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
                                        in
                                        if muacWarning == Just "green" then
                                            td [ class "muac" ] [ text <| "(" ++ (String.toLower <| translate language Translate.Normal) ++ ")" ]

                                        else
                                            let
                                                muacValue =
                                                    Maybe.map String.fromFloat muac
                                            in
                                            viewValueWithAlert muacValue muacWarning "muac"
                        in
                        tr []
                            [ td [ class "date" ] [ text <| formatDDMMYYYY encounterData.startDate ]
                            , viewValueWithAlert bodyTemperatureValue bodyTemperatureWarning "body-temperature"
                            , viewValueWithAlert respiratoryRateValue respiratoryRateWarning "respiratory-rate"
                            , muacCell
                            ]
                    )
                |> List.reverse

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
    -> List AcuteIllnessEncounterData
    -> List AcuteIllnessEncounterData
    -> Html Msg
viewNutritionSignsPane language firstInitialWithSubsequent secondInitialWithSubsequent =
    let
        nutritions =
            firstInitialWithSubsequent
                ++ secondInitialWithSubsequent
                |> List.filterMap
                    (.measurements
                        >> .nutrition
                        >> Maybe.map (\( _, measurement ) -> ( measurement.dateMeasured, measurement.value ))
                    )
    in
    if List.isEmpty nutritions then
        -- If there are no signs to display, we do not show the pane.
        -- Note that we do not record nutrition signs for adults.
        emptyNode

    else
        div [ class "pane nutrition-signs" ]
            [ viewPaneHeading language Translate.NitritionSigns
            , div [ class "pane-content" ] <|
                viewNutritionSigns language nutritions
            ]


viewTreatmentPane :
    Language
    -> List AcuteIllnessEncounterData
    -> List AcuteIllnessEncounterData
    -> AssembledData
    -> Html Msg
viewTreatmentPane language firstInitialWithSubsequent secondInitialWithSubsequent assembled =
    div [ class "pane treatment" ]
        [ viewPaneHeading language Translate.Treatment
        , div [ class "pane-content" ] <|
            viewTreatmentSigns language assembled.initialEncounter firstInitialWithSubsequent secondInitialWithSubsequent
        ]


viewTreatmentSigns :
    Language
    -> Bool
    -> List AcuteIllnessEncounterData
    -> List AcuteIllnessEncounterData
    -> List (Html Msg)
viewTreatmentSigns language initialEncounter firstInitialWithSubsequent secondInitialWithSubsequent =
    let
        initialWithSubsequent =
            if List.isEmpty secondInitialWithSubsequent then
                firstInitialWithSubsequent

            else
                secondInitialWithSubsequent
    in
    List.head initialWithSubsequent
        |> Maybe.map
            (\dataInitial ->
                if initialEncounter then
                    let
                        treatmentReview =
                            dataInitial.measurements.treatmentReview
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
                        prescribedMedications =
                            List.filterMap
                                (\data ->
                                    let
                                        medicationList =
                                            Maybe.map
                                                (Tuple.second
                                                    >> .value
                                                    >> .distributionSigns
                                                    >> EverySet.toList
                                                    >> List.filter (\sign -> not <| List.member sign [ LemonJuiceOrHoney, NoMedicationDistributionSigns ])
                                                )
                                                data.measurements.medicationDistribution
                                    in
                                    Maybe.andThen
                                        (\list ->
                                            if List.isEmpty list then
                                                Nothing

                                            else
                                                Just ( data.startDate, list )
                                        )
                                        medicationList
                                )
                                initialWithSubsequent

                        viewTreatmentOngoing encounterDate treatmentOngoing =
                            List.filter (\( date, _ ) -> Date.compare date encounterDate == LT)
                                prescribedMedications
                                |> List.reverse
                                |> List.head
                                |> Maybe.map
                                    (\( _, prescribedMedication ) ->
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
                                                    , text <| translate language <| Translate.MissedDosesOfMedication missedDoses
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
                                                                |> List.map (Translate.AdverseEvent >> translate language)
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
                                                        , text <| translate language Translate.To
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
                                    )
                                |> Maybe.withDefault []
                    in
                    List.tail initialWithSubsequent
                        |> Maybe.withDefault []
                        |> List.reverse
                        |> List.concatMap
                            (\dataSubsequent ->
                                dataSubsequent.measurements.treatmentOngoing
                                    |> Maybe.map
                                        (Tuple.second
                                            >> .value
                                            >> viewTreatmentOngoing dataSubsequent.startDate
                                            >> List.append
                                                [ div [ class "visit-date" ]
                                                    [ text <| translate language Translate.On
                                                    , text " "
                                                    , text <| formatDDMMYYYY dataSubsequent.startDate
                                                    , text " :"
                                                    ]
                                                ]
                                        )
                                    |> Maybe.withDefault []
                            )
            )
        |> Maybe.withDefault []


viewActionsTakenPane :
    Language
    -> List AcuteIllnessEncounterData
    -> List AcuteIllnessEncounterData
    -> AssembledData
    -> Html Msg
viewActionsTakenPane language firstInitialWithSubsequent secondInitialWithSubsequent assembled =
    let
        content =
            firstInitialWithSubsequent
                ++ secondInitialWithSubsequent
                |> List.map
                    (\encounterData ->
                        if encounterData.diagnosis == DiagnosisCovid19Suspect then
                            viewActionsTakenCovid19Suspect language encounterData.startDate encounterData.measurements

                        else if List.member encounterData.diagnosis [ DiagnosisSevereCovid19, DiagnosisPneuminialCovid19, DiagnosisLowRiskCovid19 ] then
                            viewActionsTakenCovid19Confirmed
                                language
                                encounterData.startDate
                                assembled.person
                                encounterData.encounterType
                                encounterData.diagnosis
                                encounterData.measurements

                        else
                            viewActionsTakenNonCovid19 language
                                encounterData.startDate
                                assembled.person
                                encounterData.encounterType
                                encounterData.diagnosis
                                encounterData.measurements
                    )
                |> List.reverse
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


viewActionsTakenCovid19Confirmed :
    Language
    -> NominalDate
    -> Person
    -> AcuteIllnessEncounterType
    -> AcuteIllnessDiagnosis
    -> AcuteIllnessMeasurements
    -> Html Msg
viewActionsTakenCovid19Confirmed language date person encounterType diagnosis measurements =
    viewPatientIsolatedAction language date measurements
        ++ viewActionsTakenMedicationDistribution language date person diagnosis measurements
        ++ viewActionsTakenSendToHC language date encounterType measurements
        |> div [ class "encounter-actions" ]


viewActionsTakenNonCovid19 :
    Language
    -> NominalDate
    -> Person
    -> AcuteIllnessEncounterType
    -> AcuteIllnessDiagnosis
    -> AcuteIllnessMeasurements
    -> Html Msg
viewActionsTakenNonCovid19 language date person encounterType diagnosis measurements =
    viewActionsTakenMedicationDistribution language date person diagnosis measurements
        ++ viewContacedHCAction language date measurements
        ++ viewActionsTakenSendToHC language date encounterType measurements
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
            getMeasurementValueFunc measurements.medicationDistribution
                |> Maybe.map .distributionSigns

        nonAdministrationReasons =
            resolveMedicationsNonAdministrationReasons measurements

        resolveNonAdministrationReason medicine_ =
            Dict.get medicine_ nonAdministrationReasons

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
                                , viewTabletsPrescription language dosage (Translate.ByMouthDailyForXDays 10)
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

        DiagnosisLowRiskCovid19 ->
            let
                paracetamolPrescribed =
                    Maybe.map (EverySet.member Paracetamol) distributionSigns
                        |> Maybe.withDefault False
            in
            if paracetamolPrescribed then
                isPersonAnAdult date person
                    |> Maybe.map (viewParacetamolAdministrationInstructions language (Just date))
                    |> Maybe.withDefault []

            else
                resolveNonAdministrationReason Paracetamol
                    |> Maybe.map
                        (\reason ->
                            [ viewNonAdministrationReason language (Translate.MedicationDistributionSign Paracetamol) "icon-pills" (Just date) reason ]
                        )
                    |> Maybe.withDefault []

        _ ->
            []


viewNonAdministrationReason : Language -> TranslationId -> String -> Maybe NominalDate -> AdministrationNote -> Html any
viewNonAdministrationReason language medicineTranslationId iconClass maybeDate reason =
    let
        message =
            div [] <|
                [ span [ class "medicine" ] [ text <| translate language medicineTranslationId ]
                , text " "
                , text <| translate language Translate.RecommendedButNotGivenDueTo
                , text ": "
                , text <| translate language <| Translate.AdministrationNote reason
                ]
                    ++ renderDatePart language maybeDate
    in
    div [ class "header icon-label" ]
        [ i [ class iconClass ] []
        , message
        ]


viewActionsTakenSendToHC : Language -> NominalDate -> AcuteIllnessEncounterType -> AcuteIllnessMeasurements -> List (Html Msg)
viewActionsTakenSendToHC language date encounterType measurements =
    let
        sendToHCSigns =
            getMeasurementValueFunc measurements.sendToHC

        facility =
            if encounterType == AcuteIllnessEncounterCHW then
                FacilityHealthCenter

            else
                FacilityHospital

        completedForm =
            Maybe.map (.signs >> EverySet.member HandReferrerForm) sendToHCSigns
                |> Maybe.withDefault False

        completedFormAction =
            if completedForm then
                [ viewActionTakenLabel language (Translate.CompleteFacilityReferralForm facility) "icon-forms" (Just date) ]

            else
                []

        sentToHC =
            Maybe.map (.signs >> EverySet.member ReferToHealthCenter) sendToHCSigns
                |> Maybe.withDefault False

        sentToHCAction =
            if sentToHC then
                [ viewActionTakenLabel language (Translate.SendPatientToFacility facility) "icon-shuttle" (Just date) ]

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
viewNextStepsPane language currentDate assembled =
    if isJust assembled.participant.outcome then
        -- Illness resolved, therefore, we do not show
        -- Next Steps pane.
        emptyNode

    else
        let
            instructions =
                getMeasurementValueFunc assembled.measurements.followUp
                    |> Maybe.andThen (.options >> EverySet.toList >> List.head)
                    |> Maybe.map
                        (\option ->
                            if option == FollowUpNotNeeded then
                                [ text <| translate language Translate.FollowUpWithPatientNotNeeded ]

                            else
                                let
                                    followUpDate =
                                        calculateDueDate assembled.encounter.startDate option

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
                                    , text <| formatDDMMYYYY followUpDate
                                    , text "."
                                    ]

                                else
                                    [ text <| translate language Translate.FollowUpWithPatientOn
                                    , text " "
                                    , text <| formatDDMMYYYY followUpDate
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
