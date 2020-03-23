module Backend.Measurement.Decoder exposing
    ( decodeAttendance
    , decodeChildMeasurementList
    , decodeChildNutritionSign
    , decodeCounselingSession
    , decodeFamilyPlanning
    , decodeFamilyPlanningSign
    , decodeFbf
    , decodeHeight
    , decodeHistoricalMeasurements
    , decodeLactation
    , decodeMeasurement
    , decodeMotherMeasurementList
    , decodeMuac
    , decodeNutrition
    , decodeParticipantConsent
    , decodeParticipantConsentValue
    , decodePhoto
    , decodeWeight
    , decodeWithEntityUuid
    , toDict
    )

import AssocList as Dict exposing (Dict)
import Backend.Counseling.Decoder exposing (decodeCounselingTiming)
import Backend.Measurement.Model exposing (..)
import Gizra.Json exposing (decodeEmptyArrayAs, decodeFloat, decodeInt, decodeIntDict)
import Gizra.NominalDate
import Json.Decode exposing (..)
import Json.Decode.Pipeline exposing (custom, hardcoded, optional, optionalAt, required, requiredAt)
import Restful.Endpoint exposing (EntityUuid, decodeEntityUuid, toEntityUuid)
import Translate.Utils exposing (decodeLanguage)
import Utils.Json exposing (decodeEverySet)


decodeMeasurement : Decoder value -> Decoder (Measurement value)
decodeMeasurement valueDecoder =
    succeed Measurement
        |> required "date_measured" Gizra.NominalDate.decodeYYYYMMDD
        |> required "nurse" (nullable decodeEntityUuid)
        |> required "person" decodeEntityUuid
        |> required "session" (nullable decodeEntityUuid)
        |> custom valueDecoder


{-| Decodes `HistoricalMeasurements` as sent by `/api/offline_sessions/`
-}
decodeHistoricalMeasurements : Decoder HistoricalMeasurements
decodeHistoricalMeasurements =
    succeed HistoricalMeasurements
        |> requiredAt [ "participants", "mother_activity" ]
            (oneOf
                [ decodeEmptyArrayAs Dict.empty
                , map toDict (Gizra.Json.dict decodeMotherMeasurementList)
                ]
            )
        |> requiredAt [ "participants", "child_activity" ]
            (oneOf
                [ decodeEmptyArrayAs Dict.empty
                , map toDict (Gizra.Json.dict decodeChildMeasurementList)
                ]
            )


{-| TODO: Put in elm-essentials.
-}
toDict : Dict String v -> Dict (EntityUuid k) v
toDict =
    Dict.foldl (\key value acc -> Dict.insert (toEntityUuid key) value acc) Dict.empty


decodeWithEntityUuid : Decoder a -> Decoder ( EntityUuid b, a )
decodeWithEntityUuid decoder =
    map2 (\a b -> ( a, b ))
        (field "uuid" decodeEntityUuid)
        decoder


decodeMotherMeasurementList : Decoder MotherMeasurementList
decodeMotherMeasurementList =
    succeed MotherMeasurementList
        |> optional "attendance" (map Dict.fromList <| list (decodeWithEntityUuid decodeAttendance)) Dict.empty
        |> optional "family_planning" (map Dict.fromList <| list (decodeWithEntityUuid decodeFamilyPlanning)) Dict.empty
        |> optional "participant_consent" (map Dict.fromList <| list (decodeWithEntityUuid decodeParticipantConsent)) Dict.empty
        |> optional "lactation" (map Dict.fromList <| list (decodeWithEntityUuid decodeLactation)) Dict.empty
        |> optional "mother_fbf" (map Dict.fromList <| list (decodeWithEntityUuid decodeFbf)) Dict.empty


decodeChildMeasurementList : Decoder ChildMeasurementList
decodeChildMeasurementList =
    succeed ChildMeasurementList
        |> optional "height" (map Dict.fromList <| list (decodeWithEntityUuid decodeHeight)) Dict.empty
        |> optional "muac" (map Dict.fromList <| list (decodeWithEntityUuid decodeMuac)) Dict.empty
        |> optional "nutrition" (map Dict.fromList <| list (decodeWithEntityUuid decodeNutrition)) Dict.empty
        |> optional "photo" (map Dict.fromList <| list (decodeWithEntityUuid decodePhoto)) Dict.empty
        |> optional "weight" (map Dict.fromList <| list (decodeWithEntityUuid decodeWeight)) Dict.empty
        |> optional "counseling_session" (map Dict.fromList <| list (decodeWithEntityUuid decodeCounselingSession)) Dict.empty
        |> optional "child_fbf" (map Dict.fromList <| list (decodeWithEntityUuid decodeFbf)) Dict.empty


decodePhoto : Decoder Photo
decodePhoto =
    field "photo" string
        |> map PhotoUrl
        |> decodeMeasurement


decodeHeight : Decoder Height
decodeHeight =
    field "height" decodeFloat
        |> map HeightInCm
        |> decodeMeasurement


decodeWeight : Decoder Weight
decodeWeight =
    field "weight" decodeFloat
        |> map WeightInKg
        |> decodeMeasurement


decodeMuac : Decoder Muac
decodeMuac =
    field "muac" decodeFloat
        |> map MuacInCm
        |> decodeMeasurement


decodeFamilyPlanning : Decoder FamilyPlanning
decodeFamilyPlanning =
    decodeEverySet decodeFamilyPlanningSign
        |> field "family_planning_signs"
        |> decodeMeasurement


decodeLactation : Decoder Lactation
decodeLactation =
    decodeEverySet decodeLactationSign
        |> field "lactation_signs"
        |> decodeMeasurement


decodeFbf : Decoder Fbf
decodeFbf =
    decodeMeasurement decodeFbfValue


decodeAttendance : Decoder Attendance
decodeAttendance =
    field "attended" bool
        |> decodeMeasurement


decodeParticipantConsent : Decoder ParticipantConsent
decodeParticipantConsent =
    decodeMeasurement decodeParticipantConsentValue


decodeParticipantConsentValue : Decoder ParticipantConsentValue
decodeParticipantConsentValue =
    succeed ParticipantConsentValue
        |> required "language" decodeLanguage
        |> required "participant_form" decodeEntityUuid


decodeNutrition : Decoder ChildNutrition
decodeNutrition =
    decodeEverySet decodeChildNutritionSign
        |> field "nutrition_signs"
        |> decodeMeasurement


decodeCounselingSession : Decoder CounselingSession
decodeCounselingSession =
    decodeMeasurement <|
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
                    "auto-observation" ->
                        succeed AutoObservation

                    "condoms" ->
                        succeed Condoms

                    "cycle-counting" ->
                        succeed CycleCounting

                    "hysterectomy" ->
                        succeed Hysterectomy

                    "implant" ->
                        succeed Implants

                    "injection" ->
                        succeed Injectables

                    "iud" ->
                        succeed IUD

                    "lactation-amenorrhea" ->
                        succeed LactationAmenorrhea

                    "none" ->
                        succeed NoFamilyPlanning

                    "spermicide" ->
                        succeed Spermicide

                    "tubal-ligatures" ->
                        succeed TubalLigatures

                    "vasectomy" ->
                        succeed Vasectomy

                    "pill" ->
                        succeed OralContraceptives

                    "necklace" ->
                        succeed CycleBeads

                    _ ->
                        fail <|
                            sign
                                ++ " is not a recognized FamilyPlanningSign"
            )


decodeLactationSign : Decoder LactationSign
decodeLactationSign =
    string
        |> andThen
            (\sign ->
                case sign of
                    "breastfeeding" ->
                        succeed Breastfeeding

                    "none" ->
                        succeed NoLactationSigns

                    _ ->
                        fail <|
                            sign
                                ++ " is not a recognized LactationSign"
            )


decodeFbfValue : Decoder FbfValue
decodeFbfValue =
    succeed FbfValue
        |> required "distributed_amount" float
        |> required "distribution_notice" decodeDistributionNotice


decodeDistributionNotice : Decoder DistributionNotice
decodeDistributionNotice =
    string
        |> andThen
            (\notice ->
                case notice of
                    "complete" ->
                        succeed DistributedFully

                    "lack-of-stock" ->
                        succeed DistributedPartiallyLackOfStock

                    "other" ->
                        succeed DistributedPartiallyOther

                    _ ->
                        fail <|
                            notice
                                ++ " is not a recognized DistributionNotice"
            )
