module Backend.Scoreboard.Decoder exposing (decodeScoreboardData)

import AssocList as Dict exposing (Dict)
import Backend.Scoreboard.Model exposing (..)
import Backend.Scoreboard.Utils exposing (..)
import Date
import EverySet exposing (EverySet)
import Gizra.NominalDate exposing (NominalDate, decodeYYYYMMDD, diffMonths)
import Json.Decode exposing (Decoder, andThen, bool, fail, float, int, list, map, maybe, string, succeed)
import Json.Decode.Pipeline exposing (hardcoded, optional, required)
import Maybe.Extra exposing (isNothing)


decodeScoreboardData : NominalDate -> Decoder ScoreboardData
decodeScoreboardData currentDate =
    succeed ScoreboardData
        |> required "entity_name" string
        |> required "entity_type" decodeSelectedEntity
        |> required "results" (list (decodePatientData currentDate))


decodeSelectedEntity : Decoder SelectedEntity
decodeSelectedEntity =
    string
        |> andThen
            (\entityType ->
                case entityType of
                    "district" ->
                        succeed EntityDistrict

                    "sector" ->
                        succeed EntitySector

                    "cell" ->
                        succeed EntityCell

                    "village" ->
                        succeed EntityVillage

                    _ ->
                        fail <| entityType ++ " is unknown SelectedEntity type"
            )


decodePatientData : NominalDate -> Decoder PatientData
decodePatientData currentDate =
    succeed PatientData
        |> required "birth_date" decodeYYYYMMDD
        |> optional "low_birth_weight" (maybe bool) Nothing
        |> optional "nutrition" (decodeNutritionCriterionsData currentDate) emptyNutritionCriterionsData
        |> optional "ncda" (decodeNCDAData currentDate) emptyNCDAData


decodeNutritionCriterionsData : NominalDate -> Decoder NutritionCriterionsData
decodeNutritionCriterionsData currentDate =
    succeed NutritionCriterionsData
        |> required "stunting" (decodeCriterionBySeverities currentDate)
        |> required "underweight" (decodeCriterionBySeverities currentDate)
        |> required "wasting" (decodeCriterionBySeverities currentDate)
        |> required "muac" (decodeCriterionBySeverities currentDate)


decodeCriterionBySeverities : NominalDate -> Decoder CriterionBySeverities
decodeCriterionBySeverities currentDate =
    succeed CriterionBySeverities
        |> optional "severe" (list decodeYYYYMMDD) []
        |> optional "moderate" (list decodeYYYYMMDD) []
        |> optional "normal" (list decodeYYYYMMDD) []
        |> map (sainitzeCriterionBySeverities currentDate)


{-| Guiding rule is that patient should have only one severity value
during calendar month.
In case there are multuple severities, most severe one needs to be selected.
-}
sainitzeCriterionBySeverities : NominalDate -> CriterionBySeverities -> CriterionBySeverities
sainitzeCriterionBySeverities currentDate data =
    let
        -- Transfering from list to dict with diff months as key makes
        -- sure we get only single value for given months.
        severeDict =
            List.map (\date -> ( diffMonths date currentDate, date ))
                data.severe
                |> Dict.fromList

        -- Transfering from list to dict with diff months as key makes
        -- sure we get only single value for given months.
        moderateDict =
            List.map (\date -> ( diffMonths date currentDate, date ))
                data.moderate
                |> Dict.fromList

        -- Transfering from list to dict with diff months as key makes
        -- sure we get only single value for given months.
        normalDict =
            List.map (\date -> ( diffMonths date currentDate, date ))
                data.normal
                |> Dict.fromList

        -- Filtering out moderate value, in case severe value was taken
        -- during same month.
        sanitizedModerate =
            Dict.toList moderateDict
                |> List.filterMap
                    (\( months, date ) ->
                        if isNothing <| Dict.get months severeDict then
                            Just date

                        else
                            Nothing
                    )

        -- Filtering out normal value, in case severe or moderate values
        -- wewre taken during same month.
        sanitizedNormal =
            Dict.toList normalDict
                |> List.filterMap
                    (\( months, date ) ->
                        if
                            (isNothing <| Dict.get months severeDict)
                                && (isNothing <| Dict.get months moderateDict)
                        then
                            Just date

                        else
                            Nothing
                    )
    in
    { data
        | severe = Dict.values severeDict
        , moderate = sanitizedModerate
        , normal = sanitizedNormal
    }


decodeNCDAData : NominalDate -> Decoder NCDAData
decodeNCDAData currentDate =
    succeed NCDAData
        |> optional "pane1" decodeANCNewbornData emptyANCNewbornData
        |> optional "pane2" (decodeUniversalInterventionData currentDate) emptyUniversalInterventionData
        |> optional "pane3" (decodeNutritionBehaviorData currentDate) emptyNutritionBehaviorData
        |> optional "pane4" (decodeTargetedInterventionsData currentDate) emptyTargetedInterventionsData
        |> optional "pane5" (decodeInfrastructureEnvironmentWashData currentDate) emptyInfrastructureEnvironmentWashData


decodeANCNewbornData : Decoder ANCNewbornData
decodeANCNewbornData =
    succeed ANCNewbornData
        |> optional "row1" bool False
        |> optional "row2" bool False


decodeUniversalInterventionData : NominalDate -> Decoder UniversalInterventionData
decodeUniversalInterventionData currentDate =
    succeed UniversalInterventionData
        |> optional "row1" decodeVaccinationProgressDict Dict.empty
        |> optional "row2" (decodeMonthlyValues currentDate) []
        |> optional "row3" (decodeMonthlyValues currentDate) []
        |> optional "row4" (decodeMonthlyValues currentDate) []
        |> optional "row5" (list decodeECDEncounterData |> map toInitialUniversalInterventionECDData) emptyUniversalInterventionECDData


decodeVaccinationProgressDict : Decoder VaccinationProgressDict
decodeVaccinationProgressDict =
    succeed RawVaccinationData
        |> required "bcg" decodeUniqueDates
        |> required "opv" decodeUniqueDates
        |> required "dtp" decodeUniqueDates
        |> required "pcv13" decodeUniqueDates
        |> required "rotarix" decodeUniqueDates
        |> required "ipv" decodeUniqueDates
        |> required "mr" decodeUniqueDates
        |> map rawVaccinationDataToVaccinationProgressDict


toInitialUniversalInterventionECDData : List ECDEncounterData -> UniversalInterventionECDData
toInitialUniversalInterventionECDData encountersData =
    UniversalInterventionECDData encountersData []


decodeECDEncounterData : Decoder ECDEncounterData
decodeECDEncounterData =
    succeed ECDEncounterData
        |> required "date" decodeYYYYMMDD
        |> optional "warning" decodeECDWarning NoECDMilstoneWarning
        |> optional "signs" (list decodeECDSign) [ NoECDSigns ]


decodeECDWarning : Decoder ECDWarning
decodeECDWarning =
    string
        |> andThen
            (\warning ->
                case warning of
                    "warning-ecd-milestone-behind" ->
                        succeed WarningECDMilestoneBehind

                    "warning-ecd-milestone-refer-to-specialist" ->
                        succeed WarningECDMilestoneReferToSpecialist

                    "no-ecd-milstone-warning" ->
                        succeed NoECDMilstoneWarning

                    _ ->
                        fail <|
                            warning
                                ++ " is not a recognized ECDWarning"
            )


decodeECDSign : Decoder ECDSign
decodeECDSign =
    string
        |> andThen
            (\sign ->
                case sign of
                    "follow-mothers-eyes" ->
                        succeed FollowMothersEyes

                    "move-arms-and-legs" ->
                        succeed MoveArmsAndLegs

                    "raise-hands-up" ->
                        succeed RaiseHandsUp

                    "smile" ->
                        succeed Smile

                    "roll-sideways" ->
                        succeed RollSideways

                    "bring-hands-to-mouth" ->
                        succeed BringHandsToMouth

                    "hold-head-without-support" ->
                        succeed HoldHeadWithoutSupport

                    "hold-and-shake-toys" ->
                        succeed HoldAndShakeToys

                    "react-to-sudden-sounds" ->
                        succeed ReactToSuddenSounds

                    "use-consonant-sounds" ->
                        succeed UseConsonantSounds

                    "respond-to-sound-with-sound" ->
                        succeed RespondToSoundWithSound

                    "turn-head-when-called" ->
                        succeed TurnHeadWhenCalled

                    "sit-without-support" ->
                        succeed SitWithoutSupport

                    "smile-back" ->
                        succeed SmileBack

                    "roll-tummy-to-back" ->
                        succeed RollTummyToBack

                    "reach-for-toys" ->
                        succeed ReachForToys

                    "use-simple-gestures" ->
                        succeed UseSimpleGestures

                    "stand-on-their-own" ->
                        succeed StandOnTheirOwn

                    "copy-during-play" ->
                        succeed CopyDuringPlay

                    "say-mama-dada" ->
                        succeed SayMamaDada

                    "can-hold-small-objects" ->
                        succeed CanHoldSmallObjects

                    "looks-when-pointed-at" ->
                        succeed LooksWhenPointedAt

                    "use-single-words" ->
                        succeed UseSingleWords

                    "walk-without-help" ->
                        succeed WalkWithoutHelp

                    "play-pretend" ->
                        succeed PlayPretend

                    "point-to-things-of-interest" ->
                        succeed PointToThingsOfInterest

                    "use-short-phrases" ->
                        succeed UseShortPhrases

                    "interested-in-other-children" ->
                        succeed InterestedInOtherChildren

                    "follow-simple-instructions" ->
                        succeed FollowSimpleInstructions

                    "kick-ball" ->
                        succeed KickBall

                    "point-at-named-objects" ->
                        succeed PointAtNamedObjects

                    "dress-themselves" ->
                        succeed DressThemselves

                    "wash-hands-go-to-toiled" ->
                        succeed WashHandsGoToToiled

                    "knows-colors-and-numbers" ->
                        succeed KnowsColorsAndNumbers

                    "use-medium-phrases" ->
                        succeed UseMediumPhrases

                    "play-make-believe" ->
                        succeed PlayMakeBelieve

                    "follow-three-step-instructions" ->
                        succeed FollowThreeStepInstructions

                    "stand-on-one-foot-five-seconds" ->
                        succeed StandOnOneFootFiveSeconds

                    "use-long-phrases" ->
                        succeed UseLongPhrases

                    "share-with-other-children" ->
                        succeed ShareWithOtherChildren

                    "count-to-ten" ->
                        succeed CountToTen

                    "none" ->
                        succeed NoECDSigns

                    _ ->
                        fail <|
                            sign
                                ++ " is not a recognized ECDSign"
            )


decodeUniqueDates : Decoder (EverySet NominalDate)
decodeUniqueDates =
    list decodeYYYYMMDD
        |> map EverySet.fromList


rawVaccinationDataToVaccinationProgressDict : RawVaccinationData -> VaccinationProgressDict
rawVaccinationDataToVaccinationProgressDict data =
    let
        generateVaccinationProgressForVaccine dates =
            EverySet.toList dates
                |> List.sortWith Date.compare
                |> List.indexedMap
                    (\index date ->
                        vaccineDoseFromOrder index
                            |> Maybe.map (\dose -> ( dose, date ))
                    )
                |> Maybe.Extra.values
                |> Dict.fromList
    in
    [ ( VaccineBCG, generateVaccinationProgressForVaccine data.bcg )
    , ( VaccineOPV, generateVaccinationProgressForVaccine data.opv )
    , ( VaccineDTP, generateVaccinationProgressForVaccine data.dtp )
    , ( VaccinePCV13, generateVaccinationProgressForVaccine data.pcv13 )
    , ( VaccineRotarix, generateVaccinationProgressForVaccine data.rotarix )
    , ( VaccineIPV, generateVaccinationProgressForVaccine data.ipv )
    , ( VaccineMR, generateVaccinationProgressForVaccine data.mr )
    ]
        |> Dict.fromList


decodeNutritionBehaviorData : NominalDate -> Decoder NutritionBehaviorData
decodeNutritionBehaviorData currentDate =
    succeed NutritionBehaviorData
        |> optional "row1" (decodeMonthlyValues currentDate) []
        |> optional "row2" (decodeMonthlyValues currentDate) []
        |> optional "row3" (decodeMonthlyValues currentDate) []
        |> optional "row4" (decodeMonthlyValues currentDate) []


decodeTargetedInterventionsData : NominalDate -> Decoder TargetedInterventionsData
decodeTargetedInterventionsData currentDate =
    succeed TargetedInterventionsData
        |> optional "row1" (decodeMonthlyValues currentDate) []
        |> optional "row2" (decodeMonthlyValues currentDate) []
        |> optional "row3" (decodeMonthlyValues currentDate) []
        |> optional "row4" (decodeMonthlyValues currentDate) []
        |> optional "row5" (decodeMonthlyValues currentDate) []
        |> optional "row6" (decodeMonthlyValues currentDate) []


decodeInfrastructureEnvironmentWashData : NominalDate -> Decoder InfrastructureEnvironmentWashData
decodeInfrastructureEnvironmentWashData currentDate =
    succeed InfrastructureEnvironmentWashData
        |> optional "row1" (decodeMonthlyValues currentDate) []
        |> optional "row2" (decodeMonthlyValues currentDate) []
        |> optional "row3" (decodeMonthlyValues currentDate) []
        |> optional "row4" bool False
        |> optional "row5" (decodeMonthlyValues currentDate) []


decodeMonthlyValues : NominalDate -> Decoder (List NominalDate)
decodeMonthlyValues currentDate =
    list decodeYYYYMMDD
        |> map (sanitizeSingleValuePerMonth currentDate)


sanitizeSingleValuePerMonth : NominalDate -> List NominalDate -> List NominalDate
sanitizeSingleValuePerMonth currentDate dates =
    -- Transfering from list to dict with diff months as key makes
    -- sure we get only single value for given months.
    List.map (\date -> ( diffMonths date currentDate, date ))
        dates
        |> Dict.fromList
        |> Dict.values
