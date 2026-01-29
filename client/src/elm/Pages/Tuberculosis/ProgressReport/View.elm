module Pages.Tuberculosis.ProgressReport.View exposing (view)

import Backend.Entities exposing (..)
import Backend.Measurement.Model
    exposing
        ( FollowUpOption(..)
        , ReferralFacility(..)
        , SendToHCSign(..)
        , TreatmentOngoingSign(..)
        , TuberculosisDOTSign(..)
        , TuberculosisHealthEducationSign(..)
        , TuberculosisPrescribedMedication(..)
        )
import Backend.Measurement.Utils exposing (getMeasurementValueFunc)
import Backend.Model exposing (ModelIndexedDb)
import Components.ReportToWhatsAppDialog.Model
import Components.ReportToWhatsAppDialog.View
import Date exposing (Unit(..))
import EverySet exposing (EverySet)
import Gizra.Html exposing (emptyNode, showIf)
import Gizra.NominalDate exposing (NominalDate, formatDDMMYYYY)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Maybe.Extra exposing (isJust, isNothing)
import Pages.Page exposing (Page(..), UserPage(..))
import Pages.Tuberculosis.Activity.Utils exposing (generateAllEncountersData, resolvePrescribedMedicationSets)
import Pages.Tuberculosis.Encounter.Model exposing (AssembledData, EncounterData)
import Pages.Tuberculosis.Encounter.Utils exposing (generateAssembledData, partitionActivities)
import Pages.Tuberculosis.Encounter.View exposing (allowEndingEncounter)
import Pages.Tuberculosis.ProgressReport.Model exposing (Model, Msg(..), ViewMode(..))
import Pages.Utils
    exposing
        ( viewConfirmationDialog
        , viewEndEncounterMenuForProgressReport
        )
import Pages.WellChild.ProgressReport.View exposing (viewPaneHeading, viewPersonInfoPane)
import SyncManager.Model exposing (Site, SiteFeature)
import Translate exposing (Language, translate)
import Utils.Html exposing (viewModal)
import Utils.WebData exposing (viewWebData)


view :
    Language
    -> NominalDate
    -> Site
    -> EverySet SiteFeature
    -> TuberculosisEncounterId
    -> ModelIndexedDb
    -> Model
    -> Html Msg
view language currentDate site features id db model =
    let
        assembled =
            generateAssembledData id db

        header =
            viewHeader language id model

        content =
            viewWebData language (viewContent language currentDate site features model) identity assembled

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
    in
    div [ class "page-report tuberculosis" ] <|
        [ header
        , content
        , viewModal endEncounterDialog
        ]


viewHeader : Language -> TuberculosisEncounterId -> Model -> Html Msg
viewHeader language id model =
    let
        action =
            case model.viewMode of
                ViewModeGlobal ->
                    SetActivePage (UserPage <| TuberculosisEncounterPage id)

                ViewModeEncounter _ ->
                    SetViewMode ViewModeGlobal
    in
    div [ class "ui basic segment head" ]
        [ h1 [ class "ui header" ]
            [ text <| translate language Translate.ProgressReport ]
        , span
            [ class "link-back"
            , onClick action
            ]
            [ span [ class "icon-back" ] [] ]
        ]


viewContent :
    Language
    -> NominalDate
    -> Site
    -> EverySet SiteFeature
    -> Model
    -> AssembledData
    -> Html Msg
viewContent language currentDate site features model assembled =
    let
        allEncountersData =
            generateAllEncountersData assembled

        panes =
            case model.viewMode of
                ViewModeGlobal ->
                    viewGlobalContent language
                        currentDate
                        allEncountersData
                        assembled

                ViewModeEncounter encounterId ->
                    viewEncounterDetailsContent language
                        encounterId
                        allEncountersData

        endEncounterMenu =
            let
                ( _, pendingActivities ) =
                    partitionActivities currentDate assembled

                allowEndEncounter =
                    allowEndingEncounter pendingActivities
            in
            viewEndEncounterMenuForProgressReport language
                features
                allowEndEncounter
                SetEndEncounterDialogState
                (MsgReportToWhatsAppDialog <|
                    Components.ReportToWhatsAppDialog.Model.SetState <|
                        Just Components.ReportToWhatsAppDialog.Model.Consent
                )
    in
    div
        [ class "ui report unstackable items"
        , Html.Attributes.id "report-content"
        ]
    <|
        panes
            ++ [ -- Actions are hidden when 'Share via WhatsApp' dialog is open,
                 -- so they do not appear on generated screenshot.
                 showIf (isNothing model.reportToWhatsAppDialog.state) endEncounterMenu
               , Html.map MsgReportToWhatsAppDialog
                    (Components.ReportToWhatsAppDialog.View.view
                        language
                        site
                        ( assembled.participant.person, assembled.person )
                        Components.ReportToWhatsAppDialog.Model.ReportTuberculosis
                        Nothing
                        model.reportToWhatsAppDialog
                    )
               ]


viewGlobalContent :
    Language
    -> NominalDate
    -> List EncounterData
    -> AssembledData
    -> List (Html Msg)
viewGlobalContent language currentDate allEncountersData assembled =
    let
        firstEncounterData =
            List.filter (.measurements >> .diagnostics >> isJust)
                allEncountersData
                |> List.reverse
                |> List.head

        initiationDate =
            Maybe.map .startDate firstEncounterData
    in
    [ viewPersonInfoPane language currentDate assembled.person
    , viewSummaryPane language allEncountersData firstEncounterData initiationDate
    , viewTreatmentTimelinePane language currentDate initiationDate
    , viewAdverseEventsPane language allEncountersData
    , viewEncountersPane language allEncountersData
    ]


viewSummaryPane :
    Language
    -> List EncounterData
    -> Maybe EncounterData
    -> Maybe NominalDate
    -> Html any
viewSummaryPane language allEncountersData firstEncounterData initiationDate =
    let
        completionDate =
            Maybe.map (Date.add Months 6)
                initiationDate

        diagnosis =
            Maybe.andThen (.measurements >> .diagnostics >> getMeasurementValueFunc)
                firstEncounterData

        missedDoses =
            List.filterMap
                (\data ->
                    getMeasurementValueFunc data.measurements.treatmentReview
                        |> Maybe.map .missedDoses
                )
                allEncountersData
                |> List.sum

        ( currentMedications, previousMedications ) =
            resolvePrescribedMedicationSets TuberculosisMedicationsNotChanged allEncountersData

        diagnosisForView =
            Maybe.map (Translate.TuberculosisDiagnosis >> translate language) diagnosis
                |> Maybe.withDefault ""

        initiationDateForView =
            Maybe.map formatDDMMYYYY initiationDate
                |> Maybe.withDefault ""

        completionDateForView =
            Maybe.map formatDDMMYYYY completionDate
                |> Maybe.withDefault ""

        viewMedicationsEntry label =
            Maybe.map
                (EverySet.toList
                    >> List.map (Translate.TuberculosisPrescribedMedication >> translate language)
                    >> String.join ", "
                    >> viewEntry label
                )
                >> Maybe.withDefault emptyNode

        viewEntry label value =
            div [ class "entry" ]
                [ div [ class "label" ] [ text <| translate language label ]
                , div [ class "value" ] [ text value ]
                ]
    in
    div [ class "pane summary" ]
        [ viewPaneHeading language Translate.Summary
        , div [ class "pane-content" ]
            [ viewEntry Translate.Diagnosis diagnosisForView
            , viewEntry Translate.InitiationDate initiationDateForView
            , viewEntry Translate.MissedDoses (String.fromInt missedDoses)
            , viewEntry Translate.CompletionDate completionDateForView
            , viewMedicationsEntry Translate.CurrentMedication currentMedications
            , viewMedicationsEntry Translate.PreviousMedication previousMedications
            ]
        ]


viewTreatmentTimelinePane : Language -> NominalDate -> Maybe NominalDate -> Html any
viewTreatmentTimelinePane language currentDate initiationDate =
    let
        content =
            Maybe.map
                (\initDate ->
                    List.range 1 6
                        |> List.map
                            (\index ->
                                div [ classList [ ( "green", Date.diff Months initDate currentDate >= index ) ] ]
                                    [ text <| translate language Translate.Month ++ " " ++ String.fromInt index ]
                            )
                        |> div [ class "timeline" ]
                )
                initiationDate
                |> Maybe.withDefault emptyNode
    in
    div [ class "pane treatment-timeline" ]
        [ viewPaneHeading language Translate.TreatmentTimeline
        , div [ class "pane-content" ] [ content ]
        ]


viewAdverseEventsPane : Language -> List EncounterData -> Html any
viewAdverseEventsPane language allEncountersData =
    let
        adverseEventsWithDates =
            List.filterMap
                (\data ->
                    getMeasurementValueFunc data.measurements.treatmentReview
                        |> Maybe.map
                            (\value ->
                                ( data.startDate, value.adverseEvents )
                            )
                )
                allEncountersData

        entries =
            List.map
                (\( date, events ) ->
                    let
                        eventsForView =
                            EverySet.toList events
                                |> List.map (Translate.AdverseEvent >> translate language)
                                |> String.join ", "
                    in
                    viewEntry eventsForView (formatDDMMYYYY date)
                )
                adverseEventsWithDates

        viewEntry label value =
            div [ class "entry" ]
                [ div [ class "label" ] [ text label ]
                , div [ class "value" ] [ text value ]
                ]
    in
    div [ class "pane adverse-events" ]
        [ viewPaneHeading language Translate.AdverseEvents
        , div [ class "pane-content" ] entries
        ]


viewEncountersPane : Language -> List EncounterData -> Html Msg
viewEncountersPane language allEncountersData =
    let
        heading =
            div [ class "heading" ]
                [ div [ class "date" ] [ text <| translate language Translate.EncounterDate ]
                , div [ class "icon" ] [ text <| translate language Translate.SeeMore ]
                ]

        entries =
            List.map
                (\data ->
                    viewEntry (formatDDMMYYYY data.startDate) data.id
                )
                allEncountersData

        viewEntry label encounterId =
            div [ class "entry" ]
                [ div [ class "label" ] [ text label ]
                , div [ class "icon" ]
                    [ div
                        [ class "icon-forward"
                        , onClick <| SetViewMode <| ViewModeEncounter encounterId
                        ]
                        []
                    ]
                ]
    in
    div [ class "pane encounters" ]
        [ viewPaneHeading language Translate.Encounters
        , div [ class "pane-content" ] <|
            heading
                :: entries
        ]


viewEncounterDetailsContent :
    Language
    -> TuberculosisEncounterId
    -> List EncounterData
    -> List (Html Msg)
viewEncounterDetailsContent language encounterId allEncountersData =
    List.filter (.id >> (==) encounterId) allEncountersData
        |> List.head
        |> Maybe.map
            (\data ->
                let
                    currentMedications =
                        List.filterMap
                            (\encounterData ->
                                if not <| Date.compare encounterData.startDate data.startDate == GT then
                                    getMeasurementValueFunc encounterData.measurements.medication

                                else
                                    Nothing
                            )
                            allEncountersData
                            |> List.filter (not << EverySet.member TuberculosisMedicationsNotChanged)
                            |> List.head

                    currentMedicationsForView =
                        Maybe.map
                            (EverySet.toList
                                >> List.map (Translate.TuberculosisPrescribedMedication >> translate language)
                                >> String.join ", "
                            )
                            currentMedications
                            |> Maybe.withDefault ""

                    medicationDistributed =
                        getMeasurementValueFunc data.measurements.dot
                            |> Maybe.map (.medicationDistributionSign >> (==) DOTPositive)

                    treatmentReviewEntriesData =
                        getMeasurementValueFunc data.measurements.treatmentReview
                            |> Maybe.map
                                (\value ->
                                    let
                                        takenAsPrescribed =
                                            EverySet.member TakenAsPrescribed value.signs

                                        missedDoses =
                                            EverySet.member MissedDoses value.signs

                                        adverseEvents =
                                            EverySet.member SideEffects value.signs
                                    in
                                    { takenAsPrescribedData =
                                        ( Just takenAsPrescribed
                                        , if takenAsPrescribed then
                                            ""

                                          else
                                            translate language <| Translate.ReasonForNotTaking value.reasonForNotTaking
                                        )
                                    , feelingBetterData = ( Just <| EverySet.member FeelingBetter value.signs, "" )
                                    , missedDosesData =
                                        ( Just missedDoses
                                        , if missedDoses then
                                            String.fromInt value.missedDoses

                                          else
                                            ""
                                        )
                                    , adverseEventsData =
                                        ( Just adverseEvents
                                        , if adverseEvents then
                                            EverySet.toList value.adverseEvents
                                                |> List.map (Translate.AdverseEvent >> translate language)
                                                |> String.join ", "

                                          else
                                            ""
                                        )
                                    }
                                )

                    symptomsEntry =
                        getMeasurementValueFunc data.measurements.symptomReview
                            |> Maybe.map
                                (\symptoms ->
                                    let
                                        symptomsForView =
                                            EverySet.toList symptoms
                                                |> List.map (Translate.TuberculosisSymptom >> translate language)
                                                |> String.join ", "
                                    in
                                    div [ class "entry" ]
                                        [ div [ class "label" ] [ text <| translate language Translate.SymptomReview ]
                                        , div [ class "value long" ] [ text symptomsForView ]
                                        ]
                                )
                            |> Maybe.withDefault emptyNode

                    actionsTakenEntry =
                        let
                            sendToHCSignsSection =
                                getMeasurementValueFunc data.measurements.referral
                                    |> Maybe.map
                                        (\value ->
                                            let
                                                completedForm =
                                                    EverySet.member HandReferrerForm value.signs

                                                sentToHC =
                                                    EverySet.member ReferToHealthCenter value.signs
                                            in
                                            [ if completedForm then
                                                li [] [ text <| translate language (Translate.CompleteFacilityReferralForm FacilityHealthCenter) ]

                                              else
                                                emptyNode
                                            , if sentToHC then
                                                li [] [ text <| translate language (Translate.SendPatientToFacility FacilityHealthCenter) ]

                                              else
                                                emptyNode
                                            ]
                                        )
                                    |> Maybe.withDefault []

                            healthEducationSection =
                                getMeasurementValueFunc data.measurements.healthEducation
                                    |> Maybe.map
                                        (\value ->
                                            if EverySet.member EducationFollowUpTesting value then
                                                [ li [] [ text <| translate language Translate.ProvidedHealthEducationAction ] ]

                                            else
                                                []
                                        )
                                    |> Maybe.withDefault []

                            followUpSection =
                                getMeasurementValueFunc data.measurements.followUp
                                    |> Maybe.map
                                        (\value ->
                                            let
                                                filtered =
                                                    EverySet.toList value.options
                                                        |> List.filter ((/=) FollowUpNotNeeded)
                                            in
                                            if not <| List.isEmpty filtered then
                                                [ li [] [ text <| translate language Translate.ScheduleFollowUp ] ]

                                            else
                                                []
                                        )
                                    |> Maybe.withDefault []
                        in
                        div [ class "entry" ]
                            [ div [ class "label" ] [ text <| translate language Translate.ActionsTaken ]
                            , ul [ class "value long" ] <|
                                sendToHCSignsSection
                                    ++ healthEducationSection
                                    ++ followUpSection
                            ]

                    viewTreatmentReviewEntry label mValue =
                        let
                            ( confirmed, value ) =
                                Maybe.withDefault ( Nothing, "" ) mValue
                        in
                        viewEntry label confirmed value

                    viewEntry label confirmation value =
                        let
                            confirmationForView =
                                Maybe.map
                                    (\confirmed ->
                                        let
                                            transId =
                                                if confirmed then
                                                    Translate.Yes

                                                else
                                                    Translate.No
                                        in
                                        translate language transId
                                    )
                                    confirmation
                                    |> Maybe.withDefault ""
                        in
                        div [ class "entry" ]
                            [ div [ class "label" ] [ text <| translate language label ]
                            , div [ class "confirmation" ] [ text confirmationForView ]
                            , div [ class "value" ] [ text value ]
                            ]
                in
                [ div [ class "pane encounter-details" ]
                    [ div [ class <| "pane-heading" ]
                        [ text <| translate language Translate.EncounterDate ++ ": " ++ formatDDMMYYYY data.startDate ]
                    , div [ class <| "pane-content" ]
                        [ viewEntry Translate.Medication medicationDistributed currentMedicationsForView
                        , Maybe.map .takenAsPrescribedData treatmentReviewEntriesData
                            |> viewTreatmentReviewEntry Translate.TakenAsPrescribed
                        , Maybe.map .feelingBetterData treatmentReviewEntriesData
                            |> viewTreatmentReviewEntry Translate.FeelingBetter
                        , Maybe.map .missedDosesData treatmentReviewEntriesData
                            |> viewTreatmentReviewEntry Translate.MissedDoses
                        , Maybe.map .adverseEventsData treatmentReviewEntriesData
                            |> viewTreatmentReviewEntry Translate.AdverseEvents
                        , symptomsEntry
                        , actionsTakenEntry
                        ]
                    ]
                ]
            )
        |> Maybe.withDefault []
