module Pages.Tuberculosis.ProgressReport.Test exposing (all)

import Backend.Measurement.Model exposing (AdverseEvent(..), ReasonForNotTaking(..), TreatmentOngoingSign(..), TuberculosisMeasurements)
import Date
import EverySet
import Expect
import Gizra.NominalDate exposing (NominalDate)
import Pages.Tuberculosis.Encounter.Model exposing (EncounterData)
import Pages.Tuberculosis.ProgressReport.View exposing (viewAdverseEventsPane)
import Restful.Endpoint exposing (toEntityUuid)
import Test exposing (Test, describe, test)
import Test.Html.Query as Query
import Test.Html.Selector exposing (class, text)
import TestFixtures exposing (wrapMeasurement)
import Time
import Translate.Model exposing (Language(..))


all : Test
all =
    describe "Pages.Tuberculosis.ProgressReport"
        [ adverseEventsPaneTests
        ]


{-| A visit where side effects were answered "No" stores `NoAdverseEvent`,
and lists nothing in the pane. A visit with side effects lists them.
-}
adverseEventsPaneTests : Test
adverseEventsPaneTests =
    let
        entries encounters =
            viewAdverseEventsPane English encounters
                |> Query.fromHtml
                |> Query.findAll [ class "entry" ]
    in
    describe "adverse events pane"
        [ test "a visit without side effects adds no entry" <|
            \_ ->
                entries [ encounterWithoutSideEffects ]
                    |> Query.count (Expect.equal 0)
        , test "a visit with side effects adds an entry naming them" <|
            \_ ->
                entries [ encounterWithSideEffects ]
                    |> Expect.all
                        [ Query.count (Expect.equal 1)
                        , Query.first >> Query.has [ text "Fever" ]
                        ]
        , test "only visits with side effects are listed" <|
            \_ ->
                entries [ encounterWithoutSideEffects, encounterWithSideEffects, encounterWithoutSideEffects ]
                    |> Query.count (Expect.equal 1)
        ]



-- FIXTURES


currentDate : NominalDate
currentDate =
    Date.fromCalendarDate 2026 Time.Oct 1


encounterWithoutSideEffects : EncounterData
encounterWithoutSideEffects =
    encounterWithTreatmentReview [ TakenAsPrescribed ] [ NoAdverseEvent ]


encounterWithSideEffects : EncounterData
encounterWithSideEffects =
    encounterWithTreatmentReview [ TakenAsPrescribed, SideEffects ] [ AdverseEventFever ]


encounterWithTreatmentReview : List TreatmentOngoingSign -> List AdverseEvent -> EncounterData
encounterWithTreatmentReview signs adverseEvents =
    { id = toEntityUuid "encounter"
    , startDate = currentDate
    , measurements =
        { emptyMeasurements
            | treatmentReview =
                wrapMeasurement currentDate
                    { signs = EverySet.fromList signs
                    , reasonForNotTaking = NoReasonForNotTakingSign
                    , missedDoses = 0
                    , adverseEvents = EverySet.fromList adverseEvents
                    }
        }
    }


emptyMeasurements : TuberculosisMeasurements
emptyMeasurements =
    { diagnostics = Nothing
    , dot = Nothing
    , followUp = Nothing
    , healthEducation = Nothing
    , medication = Nothing
    , referral = Nothing
    , symptomReview = Nothing
    , treatmentReview = Nothing
    }
