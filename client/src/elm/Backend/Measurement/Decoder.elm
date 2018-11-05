module Backend.Measurement.Decoder exposing (decodeChildEdits, decodeChildMeasurement, decodeChildMeasurementList, decodeChildNutritionSign, decodeCounselingSession, decodeEdit, decodeFamilyPlanning, decodeFamilyPlanningSign, decodeHeight, decodeHistoricalMeasurements, decodeMeasurement, decodeMeasurementEdits, decodeMotherEdits, decodeMotherMeasurement, decodeMotherMeasurementList, decodeMuac, decodeNutrition, decodeParticipantConsent, decodeParticipantConsentValue, decodePhoto, decodeWeight, decodeWithEntityId, toEveryDict)

import Backend.Counseling.Decoder exposing (decodeCounselingTiming)
import Backend.Entities exposing (..)
import Backend.Measurement.Model exposing (..)
import Dict exposing (Dict)
import EveryDict exposing (EveryDict)
import Gizra.Json exposing (decodeEmptyArrayAs, decodeFloat, decodeInt, decodeIntDict)
import Gizra.NominalDate
import Json.Decode exposing (..)
import Json.Decode.Pipeline exposing (custom, decode, hardcoded, optional, optionalAt, required, requiredAt)
import Restful.Endpoint exposing (EntityId, decodeEntityId, toEntityId)
import Translate.Utils exposing (decodeLanguage)
import Utils.Json exposing (decodeEverySet)


{-| Given a decoder for a value, produces a decoder for our `Measurement` type.
-}
decodeChildMeasurement : Decoder value -> Decoder (Measurement ChildId value)
decodeChildMeasurement =
    decodeMeasurement (field "child" decodeEntityId)


{-| Given a decoder for a value, produces a decoder for our `Measurement` type.
-}
decodeMotherMeasurement : Decoder value -> Decoder (Measurement MotherId value)
decodeMotherMeasurement =
    decodeMeasurement (field "mother" decodeEntityId)


decodeMeasurement : Decoder participantId -> Decoder value -> Decoder (Measurement participantId value)
decodeMeasurement participantDecoder valueDecoder =
    decode Measurement
        |> custom participantDecoder
        |> required "session" (nullable decodeEntityId)
        |> required "date_measured" Gizra.NominalDate.decodeYYYYMMDD
        |> custom valueDecoder


{-| Decodes `HistoricalMeasurements` as sent by /api/offline\_sessions/
-}
decodeHistoricalMeasurements : Decoder HistoricalMeasurements
decodeHistoricalMeasurements =
    decode HistoricalMeasurements
        |> requiredAt [ "participants", "mother_activity" ]
            (oneOf
                [ decodeEmptyArrayAs EveryDict.empty
                , map (toEveryDict toEntityId) (decodeIntDict decodeMotherMeasurementList)
                ]
            )
        |> requiredAt [ "participants", "child_activity" ]
            (oneOf
                [ decodeEmptyArrayAs EveryDict.empty
                , map (toEveryDict toEntityId) (decodeIntDict decodeChildMeasurementList)
                ]
            )


{-| TODO: Put in elm-essentials.
-}
toEveryDict : (comparable -> a) -> Dict comparable b -> EveryDict a b
toEveryDict func =
    Dict.foldl (\key value acc -> EveryDict.insert (func key) value acc) EveryDict.empty


decodeWithEntityId : Decoder a -> Decoder ( EntityId b, a )
decodeWithEntityId decoder =
    map2 (,)
        (field "id" decodeEntityId)
        decoder


decodeMotherMeasurementList : Decoder MotherMeasurementList
decodeMotherMeasurementList =
    decode MotherMeasurementList
        |> optional "family_planning" (list (decodeWithEntityId decodeFamilyPlanning)) []
        |> optional "participant_consent" (list (decodeWithEntityId decodeParticipantConsent)) []


decodeChildMeasurementList : Decoder ChildMeasurementList
decodeChildMeasurementList =
    decode ChildMeasurementList
        |> optional "height" (list (decodeWithEntityId decodeHeight)) []
        |> optional "muac" (list (decodeWithEntityId decodeMuac)) []
        |> optional "nutrition" (list (decodeWithEntityId decodeNutrition)) []
        |> optional "photo" (list (decodeWithEntityId decodePhoto)) []
        |> optional "weight" (list (decodeWithEntityId decodeWeight)) []
        |> optional "counseling_session" (list (decodeWithEntityId decodeCounselingSession)) []


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
        |> required "witness" decodeEntityId
        |> required "language" decodeLanguage
        |> required "participant_form" decodeEntityId


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
            (field "topics" (decodeEverySet decodeEntityId))


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
        |> required "mothers" (map (toEveryDict toEntityId) (decodeIntDict decodeMotherEdits))
        |> required "children" (map (toEveryDict toEntityId) (decodeIntDict decodeChildEdits))


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
        |> required "counseling_session" (decodeEdit decodeCounselingSession)


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
decodeEdit : Decoder value -> Decoder (Edit (EntityId id) value)
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
                            (field "id" decodeEntityId
                                |> maybe
                                |> map (Maybe.withDefault (toEntityId 0))
                            )

                    "deleted" ->
                        map2 Deleted
                            (field "id" decodeEntityId)
                            (field "value" valueDecoder)

                    _ ->
                        fail <|
                            "tag '"
                                ++ tag
                                ++ "' is not a valid tag for an Edit"
            )
