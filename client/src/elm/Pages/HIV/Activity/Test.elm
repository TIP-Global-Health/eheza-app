module Pages.HIV.Activity.Test exposing (all)

import App.Model
import AssocList as Dict
import Backend.HIVEncounter.Model
import Backend.IndividualEncounterParticipant.Model exposing (HIVOutcome(..))
import Backend.Measurement.Model exposing (HIVDiagnosisSign(..), HIVMeasurements, TestResult(..))
import Backend.Model exposing (emptyModelIndexedDb)
import Date
import EverySet
import Expect
import Gizra.NominalDate exposing (NominalDate)
import Pages.HIV.Activity.Model exposing (Msg(..), emptyModel)
import Pages.HIV.Activity.Update exposing (update)
import Pages.HIV.Activity.Utils exposing (diagnosticsFormWithDefault, setResultPositive, setRunHIVTest, toDiagnosticsValue)
import Pages.Page exposing (Page(..), UserPage(..))
import RemoteData
import Restful.Endpoint exposing (toEntityUuid)
import Test exposing (Test, describe, test)
import TestFixtures
import Time


all : Test
all =
    describe "HIV Activity tests"
        [ diagnosticsCorrectionTest
        ]


currentDate : NominalDate
currentDate =
    Date.fromCalendarDate 2026 Time.Sep 22


emptyHIVMeasurements : HIVMeasurements
emptyHIVMeasurements =
    { diagnostics = Nothing
    , followUp = Nothing
    , healthEducation = Nothing
    , medication = Nothing
    , referral = Nothing
    , symptomReview = Nothing
    , treatmentReview = Nothing
    }


{-| Diagnostics is edited on a record that is already saved, so the form in
the page starts empty and every answer shown comes from the record. Each case
sends the messages the page sends: an answer, then Save, which asks to end the
encounter first when the patient is not diagnosed, then the confirmed save.
-}
diagnosticsCorrectionTest : Test
diagnosticsCorrectionTest =
    let
        encounterId =
            toEntityUuid "encounter"

        personId =
            toEntityUuid "person"

        participantId =
            toEntityUuid "participant"

        withSaved value =
            let
                measurement =
                    TestFixtures.wrapMeasurement currentDate value
            in
            ( { emptyModelIndexedDb
                | hivMeasurements =
                    Dict.singleton encounterId
                        (RemoteData.Success { emptyHIVMeasurements | diagnostics = measurement })
              }
            , measurement
            )

        -- The page's messages in order, the last being the confirmed save;
        -- what it sends to the app is returned.
        runOn value msgs =
            let
                ( db, measurement ) =
                    withSaved value

                step msg ( model, _ ) =
                    let
                        ( updated, _, appMsgs ) =
                            update currentDate encounterId db msg model
                    in
                    ( updated, appMsgs )
            in
            List.foldl step ( emptyModel, [] ) (msgs ++ [ SaveDiagnostics personId participantId False measurement ])
                |> Tuple.second

        -- The form the page shows after the given messages.
        shownAfter value msgs =
            let
                ( db, measurement ) =
                    withSaved value

                step msg model =
                    update currentDate encounterId db msg model
                        |> (\( updated, _, _ ) -> updated)
            in
            List.foldl step emptyModel msgs
                |> .diagnosticsData
                |> .form
                |> (\form -> diagnosticsFormWithDefault form (Maybe.map (Tuple.second >> .value) measurement))

        saved value =
            Backend.HIVEncounter.Model.SaveDiagnostics personId (Just (toEntityUuid "dummy-id")) value
                |> Backend.Model.MsgHIVEncounter encounterId
                |> App.Model.MsgIndexedDb

        closed =
            [ Backend.IndividualEncounterParticipant.Model.CloseHIVSession HIVOutcomeNotDiagnosed
                |> Backend.Model.MsgIndividualEncounterParticipant participantId
                |> App.Model.MsgIndexedDb
            , App.Model.SetActivePage PinCodePage
            ]

        stayOpen =
            [ App.Model.SetActivePage <| UserPage <| HIVEncounterPage encounterId ]

        notReported signs testResult =
            { signs = EverySet.fromList signs
            , positiveResultDate = Nothing
            , testResult = testResult
            }

        testRunPositive =
            { signs = EverySet.singleton HIVTestRun
            , positiveResultDate = Just currentDate
            , testResult = Just TestPositive
            }

        askToEnd =
            SetEndEncounterDialogState True Nothing
    in
    describe "HIV Diagnostics: correcting a saved answer"
        [ test "a Positive corrected to test not run is saved without a result, and the participant is closed" <|
            \_ ->
                runOn testRunPositive [ SetDiagnosticsBoolInput setRunHIVTest False, askToEnd ]
                    |> Expect.equal (saved (notReported [] Nothing) :: closed)
        , test "a record with a result but no test run saves no result, and the participant is closed" <|
            \_ ->
                runOn (notReported [] (Just TestPositive)) [ askToEnd ]
                    |> Expect.equal (saved (notReported [] Nothing) :: closed)
        , test "repeating Yes on test run keeps the saved Positive, and the encounter stays open" <|
            \_ ->
                runOn testRunPositive [ SetDiagnosticsBoolInput setRunHIVTest True ]
                    |> Expect.equal (saved testRunPositive :: stayOpen)
        , test "repeating No on diagnosed positive keeps the saved Positive, and the encounter stays open" <|
            \_ ->
                runOn testRunPositive [ SetDiagnosticsBoolInput setResultPositive False ]
                    |> Expect.equal (saved testRunPositive :: stayOpen)
        , test "a saved Negative is kept when saved again, and the participant is closed" <|
            \_ ->
                runOn (notReported [ HIVTestRun ] (Just TestNegative)) [ askToEnd ]
                    |> Expect.equal (saved (notReported [ HIVTestRun ] (Just TestNegative)) :: closed)
        , -- A later message must not bring the withdrawn result back.
          test "answering test run No and then Yes asks for the result again" <|
            \_ ->
                shownAfter testRunPositive
                    [ SetDiagnosticsBoolInput setRunHIVTest False
                    , SetDiagnosticsBoolInput setRunHIVTest True
                    , SetDateSelectorState Nothing
                    ]
                    |> .testResult
                    |> Expect.equal Nothing
        , test "a new diagnosis of positive is saved without a test result" <|
            \_ ->
                let
                    form =
                        emptyModel.diagnosticsData.form
                in
                toDiagnosticsValue False
                    { form
                        | resultPositive = Just True
                        , positiveResultDate = Just currentDate
                        , testResult = Just TestNegative
                    }
                    |> Maybe.map .testResult
                    |> Expect.equal (Just Nothing)
        ]
