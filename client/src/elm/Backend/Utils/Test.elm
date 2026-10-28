module Backend.Utils.Test exposing (all)

import AssocList as Dict exposing (Dict)
import Backend.NCDEncounter.Decoder exposing (decodeNCDEncounter)
import Backend.NCDEncounter.Types exposing (NCDDiagnosis(..))
import Backend.PrenatalEncounter.Decoder exposing (decodePrenatalEncounter)
import Backend.PrenatalEncounter.Types exposing (PrenatalDiagnosis(..))
import Backend.Utils exposing (isPostInFlight, storedDiagnosesEqual)
import EverySet
import Expect
import Http
import Json.Decode exposing (decodeString)
import RemoteData exposing (RemoteData(..), WebData)
import Test exposing (Test, describe, test)


all : Test
all =
    describe "Backend.Utils"
        [ isPostInFlightTests
        , storedDiagnosesEqualTests
        ]


{-| Entities are created optimistically: the slot goes `Loading`, we POST, and
the page only moves on once the response lands. The button that started the
create stays live for that whole round-trip, so a second tap would create a
second entity - a duplicate encounter, or worse, a duplicate patient.

`isPostInFlight` is what every create handler in Backend.Update asks before it
posts. These pin the answers that matter: block while Loading, allow otherwise,
and never let one key's create block another's.

-}
isPostInFlightTests : Test
isPostInFlightTests =
    describe "isPostInFlight"
        [ test "a create already in flight for this key blocks the duplicate" <|
            \_ ->
                Dict.fromList [ ( "child", Loading ) ]
                    |> isPostInFlight "child"
                    |> Expect.equal True
        , test "a key we have never posted for is free to create" <|
            \_ ->
                Dict.fromList [ ( "child", NotAsked ) ]
                    |> isPostInFlight "child"
                    |> Expect.equal False
        , test "a key absent from the dict is free to create" <|
            \_ ->
                Dict.empty
                    |> isPostInFlight "child"
                    |> Expect.equal False
        , test "an earlier successful create does not block a later one" <|
            \_ ->
                -- A participant legitimately gets another encounter another day.
                Dict.fromList [ ( "child", Success () ) ]
                    |> isPostInFlight "child"
                    |> Expect.equal False
        , test "a failed create can be retried" <|
            \_ ->
                Dict.fromList [ ( "child", Failure Http.NetworkError ) ]
                    |> isPostInFlight "child"
                    |> Expect.equal False
        , test "a create in flight for one key does not block a different key" <|
            \_ ->
                let
                    dict : Dict String (WebData ())
                    dict =
                        Dict.fromList [ ( "sibling", Loading ) ]
                in
                ( isPostInFlight "sibling" dict, isPostInFlight "child" dict )
                    |> Expect.equal ( True, False )
        ]


{-| Diagnoses are re-generated on every measurement save, and the encounter is
saved only when they changed. An encounter stored with no diagnoses must not
count as changed.
-}
storedDiagnosesEqualTests : Test
storedDiagnosesEqualTests =
    let
        encounterJson fields =
            """{"individual_participant":"p1","scheduled_date":{"value":"2026-10-01"},"deleted":false""" ++ fields ++ "}"

        -- Nothing leaves the diagnoses field out of the stored encounter.
        diagnosesField name =
            Maybe.map (\diagnoses -> ",\"" ++ name ++ "\":" ++ diagnoses)
                >> Maybe.withDefault ""

        prenatalEqual stored generated =
            decodeString decodePrenatalEncounter
                (encounterJson (",\"prenatal_encounter_type\":\"nurse\"" ++ diagnosesField "prenatal_diagnoses" stored))
                |> Result.map (\encounter -> storedDiagnosesEqual NoPrenatalDiagnosis encounter.diagnoses generated)

        ncdEqual stored generated =
            decodeString decodeNCDEncounter (encounterJson (diagnosesField "ncd_diagnoses" stored))
                |> Result.map (\encounter -> storedDiagnosesEqual NoNCDDiagnosis encounter.diagnoses generated)
    in
    describe "storedDiagnosesEqual"
        [ test "prenatal: stored none equals no generated diagnoses" <|
            \_ ->
                prenatalEqual (Just "[\"none\"]") EverySet.empty
                    |> Expect.equal (Ok True)
        , test "prenatal: stored empty list equals no generated diagnoses" <|
            \_ ->
                prenatalEqual (Just "[]") EverySet.empty
                    |> Expect.equal (Ok True)
        , test "prenatal: stored without the field equals no generated diagnoses" <|
            \_ ->
                prenatalEqual Nothing EverySet.empty
                    |> Expect.equal (Ok True)
        , test "prenatal: stored none differs from a generated diagnosis" <|
            \_ ->
                prenatalEqual (Just "[\"none\"]") (EverySet.singleton DiagnosisChronicHypertensionImmediate)
                    |> Expect.equal (Ok False)
        , test "prenatal: a stored diagnosis differs from no generated diagnoses" <|
            \_ ->
                prenatalEqual (Just "[\"chronic-hypertension-immediate\"]") EverySet.empty
                    |> Expect.equal (Ok False)
        , test "the order of the two sets does not matter" <|
            \_ ->
                storedDiagnosesEqual NoPrenatalDiagnosis EverySet.empty (EverySet.singleton NoPrenatalDiagnosis)
                    |> Expect.equal True
        , test "NCD: stored none equals no generated diagnoses" <|
            \_ ->
                ncdEqual (Just "[\"none\"]") EverySet.empty
                    |> Expect.equal (Ok True)
        , test "NCD: a stored diagnosis differs from no generated diagnoses" <|
            \_ ->
                ncdEqual (Just "[\"renal-complications\"]") EverySet.empty
                    |> Expect.equal (Ok False)
        ]
