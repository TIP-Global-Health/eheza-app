module B499Replay.Test exposing (suite)

import AssocList as Dict exposing (Dict)
import B499Replay.Payload exposing (payload)
import Backend.AcuteIllnessEncounter.Decoder exposing (decodeAcuteIllnessEncounter)
import Backend.AcuteIllnessEncounter.Utils exposing (acuteIllnessDiagnosisToString)
import Backend.IndividualEncounterParticipant.Decoder exposing (decodeIndividualEncounterParticipant)
import Backend.Measurement.Decoder exposing (decodeAcuteIllnessMeasurements, decodePrenatalMeasurements)
import Backend.Model exposing (ModelIndexedDb, emptyModelIndexedDb)
import Backend.Person.Decoder exposing (decodePerson)
import Backend.PrenatalEncounter.Decoder exposing (decodePrenatalEncounter)
import Backend.PrenatalEncounter.Encoder exposing (encodePrenatalDiagnosis)
import EverySet
import Expect
import Gizra.NominalDate exposing (NominalDate)
import Json.Decode as D
import Json.Encode as E
import Pages.AcuteIllness.Encounter.Utils
import Pages.Prenatal.Activity.Utils
import Pages.Prenatal.Encounter.Utils
import RemoteData exposing (RemoteData(..))
import Restful.Endpoint exposing (toEntityUuid)
import SyncManager.Model exposing (Site(..), SiteFeature(..))
import Test exposing (Test, describe, test)


type alias Item =
    { type_ : String
    , uuid : String
    , id : String
    , value : D.Value
    }


items : List Item
items =
    D.decodeString
        (D.list
            (D.map4 Item
                (D.field "type" D.string)
                (D.field "uuid" D.string)
                (D.field "id" D.string)
                D.value
            )
        )
        payload
        |> Result.withDefault []


{-| Ihangane live site features on 2026-11-25.
-}
features : EverySet.EverySet SiteFeature
features =
    EverySet.fromList
        [ FeatureGroupEducation
        , FeatureHealthyStart
        , FeatureHIVManagement
        , FeatureNCDA
        , FeatureReportToWhatsApp
        , FeatureTuberculosisManagement
        ]


ofType : String -> List Item
ofType t =
    List.filter (\i -> i.type_ == t) items


decodeAll : String -> D.Decoder a -> List ( String, a )
decodeAll t decoder =
    ofType t
        |> List.filterMap
            (\i ->
                D.decodeValue decoder i.value
                    |> Result.toMaybe
                    |> Maybe.map (\v -> ( i.uuid, v ))
            )


decodeErrors : List String
decodeErrors =
    List.filterMap
        (\( t, decoder ) ->
            ofType t
                |> List.filterMap
                    (\i ->
                        case D.decodeValue decoder i.value of
                            Ok _ ->
                                Nothing

                            Err e ->
                                Just (t ++ " " ++ i.id ++ ": " ++ D.errorToString e)
                    )
                |> List.head
        )
        [ ( "acute_illness_encounter", D.map (always ()) decodeAcuteIllnessEncounter )
        , ( "prenatal_encounter", D.map (always ()) decodePrenatalEncounter )
        , ( "individual_participant", D.map (always ()) decodeIndividualEncounterParticipant )
        , ( "person", D.map (always ()) decodePerson )
        ]


{-| The measurement set of one encounter, shaped as the service worker serves it:
an object keyed by type, each holding a list of that type's nodes.
-}
measurementsJson : String -> String -> D.Value
measurementsJson encounterField encounterUuid =
    items
        |> List.filter
            (\i ->
                D.decodeValue (D.field encounterField D.string) i.value == Ok encounterUuid
            )
        |> List.foldl
            (\i acc -> Dict.update i.type_ (Maybe.withDefault [] >> (::) i.value >> Just) acc)
            Dict.empty
        |> Dict.toList
        |> List.map (\( t, vs ) -> ( t, E.list identity vs ))
        |> E.object


db : ModelIndexedDb
db =
    let
        aiEncounters =
            decodeAll "acute_illness_encounter" decodeAcuteIllnessEncounter

        pnEncounters =
            decodeAll "prenatal_encounter" decodePrenatalEncounter

        byParticipant encounters =
            List.foldl
                (\( uuid, e ) acc ->
                    Dict.update e.participant
                        (Maybe.withDefault Dict.empty >> Dict.insert (toEntityUuid uuid) e >> Just)
                        acc
                )
                Dict.empty
                encounters
                |> Dict.map (\_ v -> Success v)
    in
    { emptyModelIndexedDb
        | people =
            decodeAll "person" decodePerson
                |> List.map (\( u, v ) -> ( toEntityUuid u, Success v ))
                |> Dict.fromList
        , individualParticipants =
            decodeAll "individual_participant" decodeIndividualEncounterParticipant
                |> List.map (\( u, v ) -> ( toEntityUuid u, Success v ))
                |> Dict.fromList
        , acuteIllnessEncounters =
            List.map (\( u, v ) -> ( toEntityUuid u, Success v )) aiEncounters |> Dict.fromList
        , prenatalEncounters =
            List.map (\( u, v ) -> ( toEntityUuid u, Success v )) pnEncounters |> Dict.fromList
        , acuteIllnessEncountersByParticipant = byParticipant aiEncounters
        , prenatalEncountersByParticipant = byParticipant pnEncounters
        , acuteIllnessMeasurements =
            aiEncounters
                |> List.map
                    (\( u, _ ) ->
                        ( toEntityUuid u
                        , measurementsJson "acute_illness_encounter" u
                            |> D.decodeValue decodeAcuteIllnessMeasurements
                            |> Result.map Success
                            |> Result.withDefault NotAsked
                        )
                    )
                |> Dict.fromList
        , prenatalMeasurements =
            pnEncounters
                |> List.map
                    (\( u, _ ) ->
                        ( toEntityUuid u
                        , measurementsJson "prenatal_encounter" u
                            |> D.decodeValue decodePrenatalMeasurements
                            |> Result.map Success
                            |> Result.withDefault NotAsked
                        )
                    )
                |> Dict.fromList
    }


idOf : String -> String
idOf uuid =
    List.filter (\i -> i.uuid == uuid) items
        |> List.head
        |> Maybe.map .id
        |> Maybe.withDefault uuid


acuteLines : List String
acuteLines =
    decodeAll "acute_illness_encounter" decodeAcuteIllnessEncounter
        |> List.map
            (\( uuid, encounter ) ->
                let
                    currentDate =
                        encounter.startDate
                in
                case Pages.AcuteIllness.Encounter.Utils.generateAssembledData currentDate features (toEntityUuid uuid) False db of
                    Success assembled ->
                        String.join "\t"
                            [ "AI"
                            , idOf uuid
                            , assembled.diagnosis
                                |> Maybe.map (Tuple.second >> acuteIllnessDiagnosisToString)
                                |> Maybe.withDefault "none"
                            , acuteIllnessDiagnosisToString encounter.diagnosis
                            , "initial=" ++ boolText assembled.initialEncounter
                            ]

                    other ->
                        "AI\t" ++ idOf uuid ++ "\tNOT ASSEMBLED " ++ Debug.toString (RemoteData.isNotAsked other)
            )


prenatalLines : List String
prenatalLines =
    decodeAll "prenatal_encounter" decodePrenatalEncounter
        |> List.map
            (\( uuid, encounter ) ->
                let
                    currentDate =
                        encounter.startDate

                    diagnosesText set =
                        EverySet.toList set
                            |> List.map (encodePrenatalDiagnosis >> E.encode 0 >> String.replace "\"" "")
                            |> List.sort
                            |> String.join ","
                            |> (\s ->
                                    if s == "" then
                                        "none"

                                    else
                                        s
                               )
                in
                case Pages.Prenatal.Encounter.Utils.generateAssembledData (toEntityUuid uuid) db of
                    Success assembled ->
                        String.join "\t"
                            [ "PN"
                            , idOf uuid
                            , Pages.Prenatal.Activity.Utils.generatePrenatalDiagnosesForNurse currentDate assembled
                                |> diagnosesText
                            , diagnosesText encounter.diagnoses
                            , "mandatory="
                                ++ boolText (Pages.Prenatal.Activity.Utils.mandatoryActivitiesForAssessmentCompleted currentDate SiteRwanda features assembled)
                            ]

                    _ ->
                        "PN\t" ++ idOf uuid ++ "\tNOT ASSEMBLED"
            )


boolText : Bool -> String
boolText b =
    if b then
        "yes"

    else
        "no"


suite : Test
suite =
    describe "B-499 replay"
        [ test "report" <|
            \_ ->
                Expect.fail
                    ("\nRESULTS (type, nid, engine, stored, gate)\n"
                        ++ String.join "\n" (acuteLines ++ prenatalLines)
                        ++ "\nDECODE ERRORS\n"
                        ++ String.join "\n" decodeErrors
                        ++ "\nITEMS "
                        ++ String.fromInt (List.length items)
                    )
        ]
