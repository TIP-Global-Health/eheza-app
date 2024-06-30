module Pages.Tuberculosis.ProgressReport.View exposing (view)

import AssocList as Dict
import Backend.Entities exposing (..)
import Backend.IndividualEncounterParticipant.Model exposing (IndividualEncounterParticipant)
import Backend.Measurement.Model exposing (TreatmentOngoingSign(..), TuberculosisDOTSign(..))
import Backend.Measurement.Utils exposing (getMeasurementValueFunc)
import Backend.Model exposing (ModelIndexedDb)
import Backend.Person.Model exposing (Person)
import Backend.TuberculosisActivity.Utils exposing (allActivities)
import Components.ReportToWhatsAppDialog.Model
import Components.ReportToWhatsAppDialog.Utils
import Components.ReportToWhatsAppDialog.View
import Date exposing (Unit(..))
import EverySet exposing (EverySet)
import Gizra.Html exposing (emptyNode, showIf)
import Gizra.NominalDate exposing (NominalDate, formatDDMMYYYY)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Maybe.Extra exposing (isJust, isNothing)
import Measurement.Model exposing (LaboratoryTask(..))
import Pages.Page exposing (Page(..), UserPage(..))
import Pages.Tuberculosis.Encounter.Model exposing (AssembledData, EncounterData)
import Pages.Tuberculosis.Encounter.Utils exposing (generateAssembledData)
import Pages.Tuberculosis.ProgressReport.Model exposing (..)
import Pages.Utils
    exposing
        ( viewEndEncounterDialog
        , viewEndEncounterMenuForProgressReport
        , viewPersonDetailsExtended
        )
import Pages.WellChild.ProgressReport.View exposing (viewPaneHeading, viewPersonInfoPane)
import RemoteData
import SyncManager.Model exposing (Site, SiteFeature)
import Translate exposing (Language, TranslationId, translate)
import Utils.Html exposing (viewModal)
import Utils.NominalDate exposing (sortTuplesByDateDesc)
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

        -- endEncounterDialog =
        --     if model.showEndEncounterDialog then
        --         Just <|
        --             viewEndEncounterDialog language
        --                 Translate.EndEncounterQuestion
        --                 Translate.OnceYouEndTheEncounter
        --                 (CloseEncounter id)
        --                 (SetEndEncounterDialogState False)
        --
        --     else
        --         Nothing
    in
    div [ class "page-report tuberculosis" ] <|
        [ header
        , content

        -- , viewModal endEncounterDialog
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
            { id = assembled.id
            , startDate = assembled.encounter.startDate
            , measurements = assembled.measurements
            }
                :: assembled.previousEncountersData
    in
    div
        [ class "ui report unstackable items"
        , Html.Attributes.id "report-content"
        ]
    <|
        case model.viewMode of
            ViewModeGlobal ->
                viewGlobalContent language
                    currentDate
                    model
                    allEncountersData
                    assembled

            ViewModeEncounter encounterId ->
                viewEncounterDetailsContent language
                    currentDate
                    encounterId
                    model
                    allEncountersData


viewGlobalContent :
    Language
    -> NominalDate
    -> Model
    -> List EncounterData
    -> AssembledData
    -> List (Html Msg)
viewGlobalContent language currentDate model allEncountersData assembled =
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
    , viewSummaryPane language currentDate allEncountersData firstEncounterData initiationDate
    , viewTreatmentTimelinePane language currentDate initiationDate
    , viewAdverseEventsPane language currentDate allEncountersData
    , viewEncountersPane language currentDate allEncountersData
    ]


viewSummaryPane :
    Language
    -> NominalDate
    -> List EncounterData
    -> Maybe EncounterData
    -> Maybe NominalDate
    -> Html any
viewSummaryPane language currentDate allEncountersData firstEncounterData initiationDate =
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

        currentMedications =
            List.filter (.measurements >> .medication >> isJust) allEncountersData
                |> List.head
                |> Maybe.andThen (.measurements >> .medication >> getMeasurementValueFunc)

        diagnosisForView =
            Maybe.map (Translate.TuberculosisDiagnosis >> translate language) diagnosis
                |> Maybe.withDefault ""

        initiationDateForView =
            Maybe.map formatDDMMYYYY initiationDate
                |> Maybe.withDefault ""

        completionDateForView =
            Maybe.map formatDDMMYYYY completionDate
                |> Maybe.withDefault ""

        currentMedicationsForView =
            Maybe.map
                (EverySet.toList
                    >> List.map (Translate.TuberculosisPrescribedMedication >> translate language)
                    >> String.join ", "
                )
                currentMedications
                |> Maybe.withDefault ""

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
            , viewEntry Translate.CurrentMedication currentMedicationsForView
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


viewAdverseEventsPane : Language -> NominalDate -> List EncounterData -> Html any
viewAdverseEventsPane language currentDate allEncountersData =
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


viewEncountersPane : Language -> NominalDate -> List EncounterData -> Html Msg
viewEncountersPane language currentDate allEncountersData =
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
    -> NominalDate
    -> TuberculosisEncounterId
    -> Model
    -> List EncounterData
    -> List (Html Msg)
viewEncounterDetailsContent language currentDate encounterId model allEncountersData =
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
                        ]
                    ]
                ]
            )
        >> Maybe.withDefault []
