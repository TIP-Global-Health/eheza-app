module Backend.Measurement.Decoder exposing (decodeChildEdits, decodeChildMeasurement, decodeChildMeasurementList, decodeChildNutritionSign, decodeCounselingSession, decodeEdit, decodeFamilyPlanning, decodeFamilyPlanningSign, decodeHeight, decodeHistoricalMeasurements, decodeMeasurement, decodeMeasurementEdits, decodeMotherEdits, decodeMotherMeasurement, decodeMotherMeasurementList, decodeMuac, decodeNutrition, decodeParticipantConsent, decodeParticipantConsentValue, decodePhoto, decodeWeight, decodeWithEntityUuid, toEveryDict)

import Backend.Counseling.Decoder exposing (decodeCounselingTiming)
import Backend.Entities exposing (..)
import Backend.Measurement.Model exposing (..)
import Dict exposing (Dict)
import EveryDict exposing (EveryDict)
import Gizra.Json exposing (decodeEmptyArrayAs, decodeFloat, decodeInt, decodeIntDict)
import Gizra.NominalDate
import Json.Decode exposing (..)
import Json.Decode.Pipeline exposing (custom, decode, hardcoded, optional, optionalAt, required, requiredAt)
import Restful.Endpoint exposing (EntityUuid, decodeEntityUuid, toEntityUuid)
import Translate.Utils exposing (decodeLanguage)
import Utils.Json exposing (decodeEverySet)


{-| Given a decoder for a value, produces a decoder for our `Measurement` type.
-}
decodeChildMeasurement : Decoder value -> Decoder (Measurement ChildId value)
decodeChildMeasurement =
    decodeMeasurement (field "child" decodeEntityUuid)


{-| Given a decoder for a value, produces a decoder for our `Measurement` type.
-}
decodeMotherMeasurement : Decoder value -> Decoder (Measurement MotherId value)
decodeMotherMeasurement =
    decodeMeasurement (field "mother" decodeEntityUuid)


decodeMeasurement : Decoder participantId -> Decoder value -> Decoder (Measurement participantId value)
decodeMeasurement participantDecoder valueDecoder =
    decode Measurement
        |> custom participantDecoder
        |> required "session" (nullable decodeEntityUuid)
        |> required "date_measured" Gizra.NominalDate.decodeYYYYMMDD
        |> custom valueDecoder


{-| Decodes `HistoricalMeasurements` as sent by `/api/offline_sessions/`
-}
decodeHistoricalMeasurements : Decoder HistoricalMeasurements
decodeHistoricalMeasurements =
    decode HistoricalMeasurements
        |> requiredAt [ "participants", "mother_activity" ]
            (oneOf
                [ decodeEmptyArrayAs EveryDict.empty
                , map (toEveryDict toEntityUuid) (dict decodeMotherMeasurementList)
                ]
            )
        |> requiredAt [ "participants", "child_activity" ]
            (oneOf
                [ decodeEmptyArrayAs EveryDict.empty
                , map (toEveryDict toEntityUuid) (dict decodeChildMeasurementList)
                ]
            )


{-| TODO: Put in elm-essentials.
-}
toEveryDict : (comparable -> a) -> Dict comparable b -> EveryDict a b
toEveryDict func =
    Dict.foldl (\key value acc -> EveryDict.insert (func key) value acc) EveryDict.empty


decodeWithEntityUuid : Decoder a -> Decoder ( EntityUuid b, a )
decodeWithEntityUuid decoder =
    map2 (,)
        (field "id" decodeEntityUuid)
        decoder


decodeMotherMeasurementList : Decoder MotherMeasurementList
decodeMotherMeasurementList =
    decode MotherMeasurementList
        |> optional "family_planning" (list (decodeWithEntityUuid decodeFamilyPlanning)) []
        |> optional "participant_consent" (list (decodeWithEntityUuid decodeParticipantConsent)) []


decodeChildMeasurementList : Decoder ChildMeasurementList
decodeChildMeasurementList =
    decode ChildMeasurementList
        |> optional "height" (list (decodeWithEntityUuid decodeHeight)) []
        |> optional "muac" (list (decodeWithEntityUuid decodeMuac)) []
        |> optional "nutrition" (list (decodeWithEntityUuid decodeNutrition)) []
        |> optional "photo" (list (decodeWithEntityUuid decodePhoto)) []
        |> optional "weight" (list (decodeWithEntityUuid decodeWeight)) []
        |> optional "counseling_session" (list (decodeWithEntityUuid decodeCounselingSession)) []


{-| The `oneOf` provides some back-compat for locally stored values.
-}
decodePhoto : Decoder Photo
decodePhoto =
    decode PhotoValue
        |> custom
            (oneOf
                [ at [ "photo", "styles", "patient-photo" ] string
                , at [ "photo", "styles", "thumbnail" ] string
                ]
            )
        |> optionalAt [ "photo", "id" ] (map Just decodeInt) Nothing
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
        map2 (,)
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


{-| Decodes what `encodeMeasurementEdits` produces.
-}
decodeMeasurementEdits : Decoder MeasurementEdits
decodeMeasurementEdits =
    decode MeasurementEdits
        |> optional "closed" bool False
        |> required "mothers" (map (toEveryDict toEntityUuid) (dict decodeMotherEdits))
        |> required "children" (map (toEveryDict toEntityUuid) (dict decodeChildEdits))


{-| Decodes what `encodeChildEdits` produces.

The keys should match the machine name of the entity on the backend, in order
for the upload mechanism to work as expected.

-}
decodeChildEdits : Decoder ChildEdits
decodeChildEdits =
    decode ChildEdits
        |> required "height" (decodeEdit decodeHeight)
        |> required "muac" (decodeEdit decodeMuac)
        |> required "nutrition" (decodeEdit decodeNutrition)
        |> required "photo" (decodeEdit decodePhoto)
        |> required "weight" (decodeEdit decodeWeight)
        |> optional "counseling_session" (decodeEdit decodeCounselingSession) Unedited


{-| Decodes what `encodeChildEdits` produces.
-}
decodeMotherEdits : Decoder MotherEdits
decodeMotherEdits =
    decode MotherEdits
        |> required "family_planning" (decodeEdit decodeFamilyPlanning)
        |> optional "participant_consent" (list (decodeEdit decodeParticipantConsent)) []
        |> optional "checked_in" bool False


{-| The opposite of `encodeEdit`
-}
decodeEdit : Decoder value -> Decoder (Edit (EntityUuid id) value)
decodeEdit valueDecoder =
    field "tag" string
        |> andThen
            (\tag ->
                case tag of
                    "unedited" ->
                        succeed Unedited

                    "created" ->
                        field "value" valueDecoder
                            |> map Created

                    "edited" ->
                        map3
                            (\backend edited id ->
                                Edited
                                    { backend = backend
                                    , edited = edited
                                    , id = id
                                    }
                            )
                            (field "backend" valueDecoder)
                            (field "edited" valueDecoder)
                            -- The `maybe` below is transitional ... for back-compat with
                            -- existing cached sessions.
                            (field "id" decodeEntityUuid
                                |> maybe
                                |> map (Maybe.withDefault (toEntityUuid ""))
                            )

                    "deleted" ->
                        map2 Deleted
                            (field "id" decodeEntityUuid)
                            (field "value" valueDecoder)

                    _ ->
                        fail <|
                            "tag '"
                                ++ tag
                                ++ "' is not a valid tag for an Edit"
            )
