module Pages.Tuberculosis.ProgressReport.View exposing (view)

import AssocList as Dict
import Backend.Entities exposing (..)
import Backend.IndividualEncounterParticipant.Model exposing (IndividualEncounterParticipant)
import Backend.Measurement.Model
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
            viewHeader language id

        content =
            viewWebData language (viewContent language currentDate site features model) identity assembled

        endEncounterDialog =
            if model.showEndEncounterDialog then
                Just <|
                    viewEndEncounterDialog language
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


viewHeader : Language -> TuberculosisEncounterId -> Html Msg
viewHeader language id =
    div [ class "ui basic segment head" ]
        [ h1 [ class "ui header" ]
            [ text <| translate language Translate.ProgressReport ]
        , span
            [ class "link-back"
            , onClick <| SetActivePage (UserPage <| TuberculosisEncounterPage id)
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

        firstEncounterData =
            List.filter (.measurements >> .diagnostics >> isJust)
                allEncountersData
                |> List.reverse
                |> List.head

        initiationDate =
            Maybe.map .startDate firstEncounterData
    in
    div
        [ class "ui report unstackable items"
        , Html.Attributes.id "report-content"
        ]
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


viewEncountersPane : Language -> NominalDate -> List EncounterData -> Html any
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
                    [ div [ class "icon-forward" ] [] ]
                ]
    in
    div [ class "pane encounters" ]
        [ viewPaneHeading language Translate.Encounters
        , div [ class "pane-content" ] <|
            heading
                :: entries
        ]
