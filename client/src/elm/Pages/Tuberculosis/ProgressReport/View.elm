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
import Pages.Tuberculosis.Encounter.Model exposing (AssembledData)
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
    div
        [ class "ui report unstackable items"
        , Html.Attributes.id "report-content"
        ]
        [ viewPersonInfoPane language currentDate assembled.person
        , viewSummaryPane language currentDate assembled
        , viewTreatmentTimelinePane language currentDate assembled
        , viewAdverseEventsPane language currentDate assembled
        , viewEncountersPane language currentDate assembled
        ]


viewSummaryPane : Language -> NominalDate -> AssembledData -> Html any
viewSummaryPane language currentDate assembled =
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

        completionDate =
            Maybe.map (Date.add Months 6)
                initiationDate

        diagnosis =
            Maybe.andThen (.measurements >> .diagnostics >> getMeasurementValueFunc)
                firstEncounterData

        currentMedications =
            List.filter (.measurements >> .medication >> isJust) allEncountersData
                |> List.head
                |> Maybe.andThen (.measurements >> .medication >> getMeasurementValueFunc)

        viewEntry label value =
            div [ class "entry" ]
                [ div [ class "label" ] [ text <| translate language label ]
                , div [ class "value" ] [ text value ]
                ]

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
    in
    div [ class "pane summary" ]
        [ viewPaneHeading language Translate.Summary
        , div [ class "pane-content" ]
            [ viewEntry Translate.Diagnosis diagnosisForView
            , viewEntry Translate.InitiationDate initiationDateForView
            , viewEntry Translate.CompletionDate completionDateForView
            , viewEntry Translate.CurrentMedication currentMedicationsForView
            ]
        ]


viewTreatmentTimelinePane : Language -> NominalDate -> AssembledData -> Html any
viewTreatmentTimelinePane language currentDate assembled =
    let
        content =
            []
    in
    div [ class "pane treatment-timeline" ]
        [ viewPaneHeading language Translate.TreatmentTimeline
        , div [ class "pane-content" ] content
        ]


viewAdverseEventsPane : Language -> NominalDate -> AssembledData -> Html any
viewAdverseEventsPane language currentDate assembled =
    let
        content =
            []
    in
    div [ class "pane adverse-events" ]
        [ viewPaneHeading language Translate.AdverseEvents
        , div [ class "pane-content" ] content
        ]


viewEncountersPane : Language -> NominalDate -> AssembledData -> Html any
viewEncountersPane language currentDate assembled =
    let
        content =
            []
    in
    div [ class "pane encounters" ]
        [ viewPaneHeading language Translate.Encounters
        , div [ class "pane-content" ] content
        ]
