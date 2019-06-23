module Backend.Measurement.Decoder exposing (decodeAttendance, decodeChildMeasurement, decodeChildMeasurementList, decodeChildNutritionSign, decodeCounselingSession, decodeFamilyPlanning, decodeFamilyPlanningSign, decodeHeight, decodeHistoricalMeasurements, decodeMeasurement, decodeMotherMeasurement, decodeMotherMeasurementList, decodeMuac, decodeNutrition, decodeParticipantConsent, decodeParticipantConsentValue, decodePhoto, decodeSavedMeasurement, decodeWeight, decodeWithEntityUuid, toEntityUuidDict)

import AssocList as Dict
import Backend.Counseling.Decoder exposing (decodeCounselingTiming)
import Backend.Entities exposing (..)
import Backend.Measurement.Model exposing (..)
import Dict exposing (Dict)
import Gizra.Json exposing (decodeEmptyArrayAs, decodeFloat, decodeInt, decodeIntDict)
import Gizra.NominalDate
import Json.Decode exposing (..)
import Json.Decode.Pipeline exposing (custom, decode, hardcoded, optional, optionalAt, required, requiredAt)
import Restful.Endpoint exposing (EntityUuid, decodeEntityUuid, toEntityUuid)
import Translate.Utils exposing (decodeLanguage)
import Utils.EntityUuidDict as EntityUuidDict exposing (EntityUuidDict)
import Utils.EntityUuidDictList as EntityUuidDictList exposing (EntityUuidDictList)
import Utils.Json exposing (decodeEverySet)


{-| Given a decoder for a value, produces a decoder for our `Measurement` type.
-}
decodeChildMeasurement : Decoder value -> Decoder (Measurement PersonId value)
decodeChildMeasurement =
    decodeMeasurement (field "person" decodeEntityUuid)


{-| Given a decoder for a value, produces a decoder for our `Measurement` type.
-}
decodeMotherMeasurement : Decoder value -> Decoder (Measurement PersonId value)
decodeMotherMeasurement =
    decodeMeasurement (field "person" decodeEntityUuid)


decodeMeasurement : Decoder participantId -> Decoder value -> Decoder (Measurement participantId value)
decodeMeasurement participantDecoder valueDecoder =
    decode Measurement
        |> required "date_measured" Gizra.NominalDate.decodeYYYYMMDD
        |> required "nurse" (nullable decodeEntityUuid)
        |> custom participantDecoder
        |> required "session" (nullable decodeEntityUuid)
        |> custom valueDecoder


{-| Decodes a measurement that has an ID ... that is, a saved measurement.

Tye `type` field controls which decoder we apply.

-}
decodeSavedMeasurement : Decoder SavedMeasurement
decodeSavedMeasurement =
    field "type" string
        |> andThen
            (\s ->
                case s of
                    "attendance" ->
                        decodeWithEntityUuid decodeAttendance
                            |> map (\( a, b ) -> SavedAttendance a b)

                    "family_planning" ->
                        decodeWithEntityUuid decodeFamilyPlanning
                            |> map (\( a, b ) -> SavedFamilyPlanning a b)

                    "participant_consent" ->
                        decodeWithEntityUuid decodeParticipantConsent
                            |> map (\( a, b ) -> SavedParticipantConsent a b)

                    "height" ->
                        decodeWithEntityUuid decodeHeight
                            |> map (\( a, b ) -> SavedHeight a b)

                    "muac" ->
                        decodeWithEntityUuid decodeMuac
                            |> map (\( a, b ) -> SavedMuac a b)

                    "nutrition" ->
                        decodeWithEntityUuid decodeNutrition
                            |> map (\( a, b ) -> SavedChildNutrition a b)

                    "photo" ->
                        decodeWithEntityUuid decodePhoto
                            |> map (\( a, b ) -> SavedPhoto a b)

                    "weight" ->
                        decodeWithEntityUuid decodeWeight
                            |> map (\( a, b ) -> SavedWeight a b)

                    "counseling_session" ->
                        decodeWithEntityUuid decodeCounselingSession
                            |> map (\( a, b ) -> SavedCounselingSession a b)

                    _ ->
                        fail <|
                            s
                                ++ " is not a recognized measurement type"
            )


{-| Decodes `HistoricalMeasurements` as sent by `/api/offline_sessions/`
-}
decodeHistoricalMeasurements : Decoder HistoricalMeasurements
decodeHistoricalMeasurements =
    decode HistoricalMeasurements
        |> requiredAt [ "participants", "mother_activity" ]
            (oneOf
                [ decodeEmptyArrayAs EntityUuidDict.empty
                , map toEntityUuidDict (dict decodeMotherMeasurementList)
                ]
            )
        |> requiredAt [ "participants", "child_activity" ]
            (oneOf
                [ decodeEmptyArrayAs EntityUuidDict.empty
                , map toEntityUuidDict (dict decodeChildMeasurementList)
                ]
            )


{-| TODO: Put in elm-essentials.
-}
toEntityUuidDict : Dict String v -> EntityUuidDict (EntityUuid k) v
toEntityUuidDict =
    Dict.foldl (\key value acc -> Dict.insert (toEntityUuid key) value acc) EntityUuidDict.empty


decodeWithEntityUuid : Decoder a -> Decoder ( EntityUuid b, a )
decodeWithEntityUuid decoder =
    map2 (\a b -> ( a, b ))
        (field "uuid" decodeEntityUuid)
        decoder


decodeMotherMeasurementList : Decoder MotherMeasurementList
decodeMotherMeasurementList =
    decode MotherMeasurementList
        |> optional "attendance" (map EntityUuidDictList.fromList <| list (decodeWithEntityUuid decodeAttendance)) EntityUuidDictList.empty
        |> optional "family_planning" (map EntityUuidDictList.fromList <| list (decodeWithEntityUuid decodeFamilyPlanning)) EntityUuidDictList.empty
        |> optional "participant_consent" (map EntityUuidDictList.fromList <| list (decodeWithEntityUuid decodeParticipantConsent)) EntityUuidDictList.empty


decodeChildMeasurementList : Decoder ChildMeasurementList
decodeChildMeasurementList =
    decode ChildMeasurementList
        |> optional "height" (map EntityUuidDictList.fromList <| list (decodeWithEntityUuid decodeHeight)) EntityUuidDictList.empty
        |> optional "muac" (map EntityUuidDictList.fromList <| list (decodeWithEntityUuid decodeMuac)) EntityUuidDictList.empty
        |> optional "nutrition" (map EntityUuidDictList.fromList <| list (decodeWithEntityUuid decodeNutrition)) EntityUuidDictList.empty
        |> optional "photo" (map EntityUuidDictList.fromList <| list (decodeWithEntityUuid decodePhoto)) EntityUuidDictList.empty
        |> optional "weight" (map EntityUuidDictList.fromList <| list (decodeWithEntityUuid decodeWeight)) EntityUuidDictList.empty
        |> optional "counseling_session" (map EntityUuidDictList.fromList <| list (decodeWithEntityUuid decodeCounselingSession)) EntityUuidDictList.empty


decodePhoto : Decoder Photo
decodePhoto =
    field "photo" string
        |> map PhotoUrl
        |> decodeChildMeasurement


decodeHeight : Decoder Height
decodeHeight =
    field "height" decodeFloat
        |> map HeightInCm
        |> decodeChildMeasurement


decodeWeight : Decoder Weight
decodeWeight =
    field "weight" decodeFloat
        |> map WeightInKg
        |> decodeChildMeasurement


decodeMuac : Decoder Muac
decodeMuac =
    field "muac" decodeFloat
        |> map MuacInCm
        |> decodeChildMeasurement


decodeFamilyPlanning : Decoder FamilyPlanning
decodeFamilyPlanning =
    decodeEverySet decodeFamilyPlanningSign
        |> field "family_planning_signs"
        |> decodeMotherMeasurement


decodeAttendance : Decoder Attendance
decodeAttendance =
    field "attended" bool
        |> decodeMotherMeasurement


decodeParticipantConsent : Decoder ParticipantConsent
decodeParticipantConsent =
    decodeMotherMeasurement decodeParticipantConsentValue


decodeParticipantConsentValue : Decoder ParticipantConsentValue
decodeParticipantConsentValue =
    decode ParticipantConsentValue
        |> required "language" decodeLanguage
        |> required "participant_form" decodeEntityUuid


decodeNutrition : Decoder ChildNutrition
decodeNutrition =
    decodeEverySet decodeChildNutritionSign
        |> field "nutrition_signs"
        |> decodeChildMeasurement


decodeCounselingSession : Decoder CounselingSession
decodeCounselingSession =
    decodeChildMeasurement <|
        map2 (\a b -> ( a, b ))
            (field "timing" decodeCounselingTiming)
            (field "topics" (decodeEverySet decodeEntityUuid))


decodeChildNutritionSign : Decoder ChildNutritionSign
decodeChildNutritionSign =
    string
        |> andThen
            (\sign ->
                case sign of
                    -- We're keeping this one for back-compat, as
                    -- this value still may exists in the browser
                    -- local storage. Should be removed a little bit
                    -- later.
                    "abdominal-disortion" ->
                        succeed AbdominalDistension

                    -- We briefly used this typo as well, so also
                    -- keeping for back-compat.
                    "abdominal-distention" ->
                        succeed AbdominalDistension

                    "abdominal-distension" ->
                        succeed AbdominalDistension

                    "apathy" ->
                        succeed Apathy

                    "brittle-hair" ->
                        succeed BrittleHair

                    "dry-skin" ->
                        succeed DrySkin

                    "edema" ->
                        succeed Edema

                    "none" ->
                        succeed None

                    "poor-appetite" ->
                        succeed PoorAppetite

                    _ ->
                        fail <|
                            sign
                                ++ " is not a recognized ChildNutritionSign"
            )


decodeFamilyPlanningSign : Decoder FamilyPlanningSign
decodeFamilyPlanningSign =
    string
        |> andThen
            (\sign ->
                case sign of
                    "pill" ->
                        succeed Pill

                    "condoms" ->
                        succeed Condoms

                    "iud" ->
                        succeed IUD

                    "implant" ->
                        succeed Implant

                    "injection" ->
                        succeed Injection

                    "necklace" ->
                        succeed Necklace

                    "none" ->
                        succeed NoFamilyPlanning

                    _ ->
                        fail <|
                            sign
                                ++ " is not a recognized FamilyPlanningSign"
            )
