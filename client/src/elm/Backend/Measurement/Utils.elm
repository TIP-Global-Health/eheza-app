module Backend.Measurement.Utils exposing (..)

import AssocList as Dict exposing (Dict)
import Backend.Entities exposing (..)
import Backend.Measurement.Model exposing (..)
import Backend.Model exposing (ModelIndexedDb)
import Backend.Person.Model exposing (Person)
import Backend.Person.Utils exposing (ageInMonths)
import Backend.Session.Model exposing (OfflineSession)
import Backend.Utils exposing (ncdaEnabled)
import Date
import EverySet exposing (EverySet)
import Gizra.NominalDate exposing (NominalDate)
import LocalData
import Maybe.Extra exposing (isJust)
import RemoteData exposing (RemoteData(..), WebData)
import Restful.Endpoint exposing (EntityUuid)
import SyncManager.Model exposing (SiteFeature)
import Utils.NominalDate exposing (sortTuplesByDateDesc)


generatePreviousMeasurements :
    (ModelIndexedDb -> IndividualEncounterParticipantId -> List ( encounterId, { encounter | startDate : NominalDate } ))
    -> (ModelIndexedDb -> Dict encounterId (WebData measurements))
    -> Maybe encounterId
    -> IndividualEncounterParticipantId
    -> ModelIndexedDb
    -> List ( NominalDate, ( encounterId, measurements ) )
generatePreviousMeasurements encountersByParticipantFunc measurementsByEncouterFunc currentEncounterId participantId db =
    encountersByParticipantFunc db participantId
        |> List.filterMap
            (\( encounterId, encounter ) ->
                -- We do not want to get data of current encounter.
                if currentEncounterId == Just encounterId then
                    Nothing

                else
                    case Dict.get encounterId (measurementsByEncouterFunc db) of
                        Just (Success data) ->
                            Just ( encounter.startDate, ( encounterId, data ) )

                        _ ->
                            Nothing
            )
        -- Most recent date to least recent date.
        |> List.sortWith sortTuplesByDateDesc


{-| Given a MUAC in cm, classify according to the measurement tool shown
at <https://github.com/Gizra/ihangane/issues/282>
-}
muacIndication : MuacInCm -> ColorAlertIndication
muacIndication (MuacInCm value) =
    if value <= 11.5 then
        ColorAlertRed

    else if value <= 12.5 then
        ColorAlertYellow

    else
        ColorAlertGreen


headCircumferenceIndication : HeadCircumferenceInCm -> ColorAlertIndication
headCircumferenceIndication (HeadCircumferenceInCm value) =
    if value <= -3 || value >= 3 then
        ColorAlertRed

    else
        ColorAlertGreen


{-| Given the data, do we have a current value? May be the value
stored in the backend, or an edited value.
-}
currentValue : MeasurementData (Maybe ( id, value )) -> Maybe value
currentValue data =
    Maybe.map Tuple.second data.current


{-| Like `currentValue`, but for cases where we have a list of values.
-}
currentValues : MeasurementData (Dict id value) -> List ( Maybe id, value )
currentValues data =
    data.current
        |> Dict.map (\k v -> ( Just k, v ))
        |> Dict.values


mapMeasurementData : (a -> b) -> MeasurementData a -> MeasurementData b
mapMeasurementData dataFunc measurements =
    { previous = dataFunc measurements.previous
    , current = dataFunc measurements.current
    , update = measurements.update
    }


splitMotherMeasurements : SessionId -> Dict PersonId MotherMeasurementList -> Dict PersonId { current : MotherMeasurements, previous : MotherMeasurements }
splitMotherMeasurements sessionId =
    Dict.map
        (\_ list ->
            let
                attendance =
                    getCurrentAndPrevious sessionId list.attendances

                familyPlanning =
                    getCurrentAndPrevious sessionId list.familyPlannings

                consent =
                    getCurrentAndPrevious sessionId list.consents
                        |> .current

                lactation =
                    getCurrentAndPrevious sessionId list.lactations

                fbf =
                    getCurrentAndPrevious sessionId list.fbfs
            in
            { current =
                { attendance =
                    attendance.current
                        |> Dict.toList
                        |> List.head
                , familyPlanning =
                    familyPlanning.current
                        |> Dict.toList
                        |> List.head
                , consent = consent
                , lactation =
                    lactation.current
                        |> Dict.toList
                        |> List.head
                , fbf =
                    fbf.current
                        |> Dict.toList
                        |> List.head
                }
            , previous =
                -- We don't "compare" consents, so previous doesn't mean
                -- anything for it.
                { attendance = attendance.previous
                , familyPlanning = familyPlanning.previous
                , consent = Dict.empty
                , lactation = lactation.previous
                , fbf = fbf.previous
                }
            }
        )


splitChildMeasurements : SessionId -> Dict PersonId ChildMeasurementList -> Dict PersonId { current : ChildMeasurements, previous : ChildMeasurements }
splitChildMeasurements sessionId =
    Dict.map
        (\_ list ->
            let
                height =
                    getCurrentAndPrevious sessionId list.heights

                weight =
                    getCurrentAndPrevious sessionId list.weights

                muac =
                    getCurrentAndPrevious sessionId list.muacs

                nutrition =
                    getCurrentAndPrevious sessionId list.nutritions

                photo =
                    getCurrentAndPrevious sessionId list.photos

                counselingSession =
                    getCurrentAndPrevious sessionId list.counselingSessions

                fbf =
                    getCurrentAndPrevious sessionId list.fbfs

                contributingFactors =
                    getCurrentAndPrevious sessionId list.contributingFactors

                followUp =
                    getCurrentAndPrevious sessionId list.followUp

                healthEducation =
                    getCurrentAndPrevious sessionId list.healthEducation

                sendToHC =
                    getCurrentAndPrevious sessionId list.sendToHC

                ncda =
                    getCurrentAndPrevious sessionId list.ncda
            in
            { current =
                -- We can only have one per session ... we enforce that here.
                { height =
                    height.current
                        |> Dict.toList
                        |> List.head
                , weight =
                    weight.current
                        |> Dict.toList
                        |> List.head
                , muac =
                    muac.current
                        |> Dict.toList
                        |> List.head
                , nutrition =
                    nutrition.current
                        |> Dict.toList
                        |> List.head
                , photo =
                    photo.current
                        |> Dict.toList
                        |> List.head
                , counselingSession =
                    counselingSession.current
                        |> Dict.toList
                        |> List.head
                , fbf =
                    fbf.current
                        |> Dict.toList
                        |> List.head
                , contributingFactors =
                    contributingFactors.current
                        |> Dict.toList
                        |> List.head
                , followUp =
                    followUp.current
                        |> Dict.toList
                        |> List.head
                , healthEducation =
                    healthEducation.current
                        |> Dict.toList
                        |> List.head
                , sendToHC =
                    sendToHC.current
                        |> Dict.toList
                        |> List.head
                , ncda =
                    ncda.current
                        |> Dict.toList
                        |> List.head
                }
            , previous =
                { height = height.previous
                , weight = weight.previous
                , muac = muac.previous
                , nutrition = nutrition.previous
                , photo = photo.previous
                , counselingSession = counselingSession.previous
                , fbf = fbf.previous
                , contributingFactors = contributingFactors.previous
                , followUp = followUp.previous
                , healthEducation = healthEducation.previous
                , sendToHC = sendToHC.previous
                , ncda = ncda.previous
                }
            }
        )


{-| Picks out current and previous values from a list of measurements.
-}
getCurrentAndPrevious : SessionId -> Dict (EntityUuid id) (GroupMeasurement b) -> { current : Dict (EntityUuid id) (GroupMeasurement b), previous : Maybe ( EntityUuid id, GroupMeasurement b ) }
getCurrentAndPrevious sessionId =
    let
        -- This is designed to iterate through each list only once, to get both
        -- the current and previous value
        go id value acc =
            if value.encounterId == Just sessionId then
                let
                    current_ =
                        Dict.toList acc.current
                            |> (::) ( id, value )
                            |> Dict.fromList
                in
                -- If it's got our session ID, then it's current
                { acc | current = current_ }

            else
                case acc.previous of
                    -- Otherwise, it might be previous
                    Nothing ->
                        { acc | previous = Just ( id, value ) }

                    Just ( _, previousValue ) ->
                        if Date.compare value.dateMeasured previousValue.dateMeasured == GT then
                            { acc | previous = Just ( id, value ) }

                        else
                            acc
    in
    Dict.foldl go
        { current = Dict.empty
        , previous = Nothing
        }


administrationNoteFromString : String -> Maybe AdministrationNote
administrationNoteFromString reason =
    case reason of
        "lack-of-stock" ->
            Just NonAdministrationLackOfStock

        "known-allergy" ->
            Just NonAdministrationKnownAllergy

        "patient-declined" ->
            Just NonAdministrationPatientDeclined

        "patient-unable-to-afford" ->
            Just NonAdministrationPatientUnableToAfford

        "home-birth" ->
            Just NonAdministrationHomeBirth

        "childs-condition" ->
            Just NonAdministrationTooIll

        "other" ->
            Just NonAdministrationOther

        "administered-today" ->
            Just AdministeredToday

        "administered-previously" ->
            Just AdministeredPreviously

        _ ->
            Nothing


administrationNoteToString : AdministrationNote -> String
administrationNoteToString reason =
    case reason of
        NonAdministrationLackOfStock ->
            "lack-of-stock"

        NonAdministrationKnownAllergy ->
            "known-allergy"

        NonAdministrationPatientDeclined ->
            "patient-declined"

        NonAdministrationPatientUnableToAfford ->
            "patient-unable-to-afford"

        NonAdministrationHomeBirth ->
            "home-birth"

        NonAdministrationTooIll ->
            "childs-condition"

        NonAdministrationOther ->
            "other"

        AdministeredToday ->
            "administered-today"

        AdministeredPreviously ->
            "administered-previously"


mapChildMeasurementsAtOfflineSession : PersonId -> (ChildMeasurements -> ChildMeasurements) -> OfflineSession -> OfflineSession
mapChildMeasurementsAtOfflineSession childId func offlineSession =
    let
        mapped =
            LocalData.map
                (\measurements ->
                    let
                        updatedChildMeasurements =
                            Dict.get childId measurements.current.children
                                |> Maybe.map (\childMeasurements -> func childMeasurements)

                        childrenUpdated =
                            updatedChildMeasurements
                                |> Maybe.map (\updated -> Dict.insert childId updated measurements.current.children)
                                |> Maybe.withDefault measurements.current.children

                        currentUpdated =
                            measurements.current
                                |> (\current -> { current | children = childrenUpdated })
                    in
                    { measurements | current = currentUpdated }
                )
                offlineSession.measurements
    in
    { offlineSession | measurements = mapped }


nutritionAssessmentToString : NutritionAssessment -> String
nutritionAssessmentToString assesment =
    case assesment of
        AssesmentAcuteMalnutritionModerate ->
            "malnutrition-moderate"

        AssesmentAcuteMalnutritionSevere ->
            "malnutrition-severe"

        AssesmentUnderweightModerate ->
            "underweight-moderate"

        AssesmentUnderweightSevere ->
            "underweight-severe"

        AssesmentDangerSignsNotPresent ->
            "danger-signs-not-present"

        AssesmentDangerSignsPresent ->
            "danger-signs-present"

        AssesmentMalnutritionSigns _ ->
            "malnutrition-signs"

        AssesmentConsecutiveWeightLoss ->
            "consecutive-weight-loss"

        NoNutritionAssessment ->
            "none"


nutritionAssessmentFromString : String -> Maybe NutritionAssessment
nutritionAssessmentFromString assesment =
    case assesment of
        "malnutrition-moderate" ->
            Just AssesmentAcuteMalnutritionModerate

        "malnutrition-severe" ->
            Just AssesmentAcuteMalnutritionSevere

        "underweight-moderate" ->
            Just AssesmentUnderweightModerate

        "underweight-severe" ->
            Just AssesmentUnderweightSevere

        "danger-signs-not-present" ->
            Just AssesmentDangerSignsNotPresent

        "danger-signs-present" ->
            Just AssesmentDangerSignsPresent

        "malnutrition-signs" ->
            -- We don't actually know which malnutrition signs we discovered.
            -- We will be able to determine this by looking at the Nutrition activity.
            Just (AssesmentMalnutritionSigns [])

        "consecutive-weight-loss" ->
            Just AssesmentConsecutiveWeightLoss

        "none" ->
            Just NoNutritionAssessment

        _ ->
            Nothing


nutritionAssessmentToComparable : NutritionAssessment -> Int
nutritionAssessmentToComparable assesment =
    case assesment of
        AssesmentAcuteMalnutritionModerate ->
            1

        AssesmentAcuteMalnutritionSevere ->
            1

        AssesmentUnderweightModerate ->
            1

        AssesmentUnderweightSevere ->
            1

        AssesmentMalnutritionSigns _ ->
            1

        AssesmentConsecutiveWeightLoss ->
            1

        NoNutritionAssessment ->
            1

        AssesmentDangerSignsNotPresent ->
            2

        AssesmentDangerSignsPresent ->
            2


postpartumMotherDangerSignToString : PostpartumMotherDangerSign -> String
postpartumMotherDangerSignToString sign =
    case sign of
        PostpartumMotheUterineBleeding ->
            "uterine-bleeding"

        PostpartumMotherFever ->
            "fever"

        PostpartumMotherMigraine ->
            "migraine"

        PostpartumMotherParalysis ->
            "paralysis"

        PostpartumMotherAcuteAbdominalPain ->
            "abdominal-pain"

        PostpartumMotherLabouredBreathing ->
            "laboured-breathing"

        NoPostpartumMotherDangerSigns ->
            "none"


postpartumMotherDangerSignFromString : String -> Maybe PostpartumMotherDangerSign
postpartumMotherDangerSignFromString sign =
    case sign of
        "uterine-bleeding" ->
            Just PostpartumMotheUterineBleeding

        "fever" ->
            Just PostpartumMotherFever

        "migraine" ->
            Just PostpartumMotherMigraine

        "paralysis" ->
            Just PostpartumMotherParalysis

        "abdominal-pain" ->
            Just PostpartumMotherAcuteAbdominalPain

        "laboured-breathing" ->
            Just PostpartumMotherLabouredBreathing

        "none" ->
            Just NoPostpartumMotherDangerSigns

        _ ->
            Nothing


postpartumChildDangerSignToString : PostpartumChildDangerSign -> String
postpartumChildDangerSignToString sign =
    case sign of
        PostpartumChildInabilityToSuckle ->
            "inability-to-suckle"

        PostpartumChildParalysis ->
            "paralysis"

        PostpartumChildLabouredBreathing ->
            "laboured-breathing"

        PostpartumChildAbnormalTemperature ->
            "abnormal-temperature"

        PostpartumChildInactiveNoMovement ->
            "inactive-or-no-movement"

        PostpartumChildBodyTurnedYellow ->
            "body-turned-yellow"

        NoPostpartumChildDangerSigns ->
            "none"


postpartumChildDangerSignFromString : String -> Maybe PostpartumChildDangerSign
postpartumChildDangerSignFromString sign =
    case sign of
        "inability-to-suckle" ->
            Just PostpartumChildInabilityToSuckle

        "paralysis" ->
            Just PostpartumChildParalysis

        "laboured-breathing" ->
            Just PostpartumChildLabouredBreathing

        "abnormal-temperature" ->
            Just PostpartumChildAbnormalTemperature

        "inactive-or-no-movement" ->
            Just PostpartumChildInactiveNoMovement

        "body-turned-yellow" ->
            Just PostpartumChildBodyTurnedYellow

        "none" ->
            Just NoPostpartumChildDangerSigns

        _ ->
            Nothing


getHeightValue : HeightInCm -> Float
getHeightValue =
    \(HeightInCm cm) -> cm


muacValueFunc : MuacInCm -> Float
muacValueFunc =
    \(MuacInCm cm) -> cm


weightValueFunc : WeightInKg -> Float
weightValueFunc =
    \(WeightInKg kg) -> kg


headCircumferenceValueFunc : HeadCircumferenceInCm -> Float
headCircumferenceValueFunc =
    \(HeadCircumferenceInCm cm) -> cm


vaccineDoseFromString : String -> Maybe VaccineDose
vaccineDoseFromString dose =
    case dose of
        "dose-1" ->
            Just VaccineDoseFirst

        "dose-2" ->
            Just VaccineDoseSecond

        "dose-3" ->
            Just VaccineDoseThird

        "dose-4" ->
            Just VaccineDoseFourth

        "dose-5" ->
            Just VaccineDoseFifth

        _ ->
            Nothing


vaccineDoseToString : VaccineDose -> String
vaccineDoseToString dose =
    case dose of
        VaccineDoseFirst ->
            "dose-1"

        VaccineDoseSecond ->
            "dose-2"

        VaccineDoseThird ->
            "dose-3"

        VaccineDoseFourth ->
            "dose-4"

        VaccineDoseFifth ->
            "dose-5"


getMeasurementValueFunc : Maybe ( id, { measurement | value : v } ) -> Maybe v
getMeasurementValueFunc =
    Maybe.map (Tuple.second >> .value)


getMeasurementDateMeasuredFunc : Maybe ( id, { measurement | dateMeasured : NominalDate } ) -> Maybe NominalDate
getMeasurementDateMeasuredFunc =
    Maybe.map (Tuple.second >> .dateMeasured)


nutritionSignToString : ChildNutritionSign -> String
nutritionSignToString sign =
    case sign of
        AbdominalDistension ->
            "abdominal-distension"

        Apathy ->
            "apathy"

        BrittleHair ->
            "brittle-hair"

        DrySkin ->
            "dry-skin"

        Edema ->
            "edema"

        NormalChildNutrition ->
            "none"

        PoorAppetite ->
            "poor-appetite"


symptomsGeneralSignToString : SymptomsGeneralSign -> String
symptomsGeneralSignToString sign =
    case sign of
        BodyAches ->
            "body-aches"

        Chills ->
            "chills"

        SymptomGeneralFever ->
            "fever"

        Headache ->
            "headache"

        NightSweats ->
            "night-sweats"

        Lethargy ->
            "lethargy"

        PoorSuck ->
            "poor-suck"

        UnableToDrink ->
            "unable-to-drink"

        UnableToEat ->
            "unable-to-eat"

        IncreasedThirst ->
            "increased-thirst"

        DryMouth ->
            "dry-mouth"

        SevereWeakness ->
            "severe-weakness"

        YellowEyes ->
            "yellow-eyes"

        CokeColoredUrine ->
            "coke-colored-urine"

        SymptomsGeneralConvulsions ->
            "convulsions"

        SpontaneousBleeding ->
            "spontaneos-bleeding"

        NoSymptomsGeneral ->
            "none"


symptomsGeneralSignFromString : String -> Maybe SymptomsGeneralSign
symptomsGeneralSignFromString s =
    case s of
        "body-aches" ->
            Just BodyAches

        "chills" ->
            Just Chills

        "fever" ->
            Just SymptomGeneralFever

        "headache" ->
            Just Headache

        "night-sweats" ->
            Just NightSweats

        "lethargy" ->
            Just Lethargy

        "poor-suck" ->
            Just PoorSuck

        "unable-to-drink" ->
            Just UnableToDrink

        "unable-to-eat" ->
            Just UnableToEat

        "increased-thirst" ->
            Just IncreasedThirst

        "dry-mouth" ->
            Just DryMouth

        "severe-weakness" ->
            Just SevereWeakness

        "yellow-eyes" ->
            Just YellowEyes

        "coke-colored-urine" ->
            Just CokeColoredUrine

        "convulsions" ->
            Just SymptomsGeneralConvulsions

        "spontaneos-bleeding" ->
            Just SpontaneousBleeding

        "none" ->
            Just NoSymptomsGeneral

        _ ->
            Nothing


symptomsRespiratorySignToString : SymptomsRespiratorySign -> String
symptomsRespiratorySignToString sign =
    case sign of
        BloodInSputum ->
            "blood-in-sputum"

        Cough ->
            "cough"

        NasalCongestion ->
            "nasal-congestion"

        ShortnessOfBreath ->
            "shortness-of-breath"

        SoreThroat ->
            "sore-throat"

        LossOfSmell ->
            "loss-of-smell"

        StabbingChestPain ->
            "stabbing-chest-pain"

        NoSymptomsRespiratory ->
            "none"


symptomsRespiratorySignFromString : String -> Maybe SymptomsRespiratorySign
symptomsRespiratorySignFromString s =
    case s of
        "blood-in-sputum" ->
            Just BloodInSputum

        "cough" ->
            Just Cough

        "nasal-congestion" ->
            Just NasalCongestion

        "shortness-of-breath" ->
            Just ShortnessOfBreath

        "sore-throat" ->
            Just SoreThroat

        "loss-of-smell" ->
            Just LossOfSmell

        "stabbing-chest-pain" ->
            Just StabbingChestPain

        "none" ->
            Just NoSymptomsRespiratory

        _ ->
            Nothing


symptomsGISignToString : SymptomsGISign -> String
symptomsGISignToString sign =
    case sign of
        SymptomGIAbdominalPain ->
            "abdominal-pain"

        BloodyDiarrhea ->
            "bloody-diarrhea"

        Nausea ->
            "nausea"

        NonBloodyDiarrhea ->
            "non-bloody-diarrhea"

        Vomiting ->
            "vomiting"

        NoSymptomsGI ->
            "none"


symptomsGISignFromString : String -> Maybe SymptomsGISign
symptomsGISignFromString s =
    case s of
        "abdominal-pain" ->
            Just SymptomGIAbdominalPain

        "bloody-diarrhea" ->
            Just BloodyDiarrhea

        "nausea" ->
            Just Nausea

        "non-bloody-diarrhea" ->
            Just NonBloodyDiarrhea

        "vomiting" ->
            Just Vomiting

        "none" ->
            Just NoSymptomsGI

        _ ->
            Nothing


covidIsolationPeriod : Int
covidIsolationPeriod =
    10


testResultToString : TestResult -> String
testResultToString value =
    case value of
        TestPositive ->
            "positive"

        TestNegative ->
            "negative"

        TestIndeterminate ->
            "indeterminate"


testResultFromString : String -> Maybe TestResult
testResultFromString value =
    case value of
        "positive" ->
            Just TestPositive

        "negative" ->
            Just TestNegative

        "indeterminate" ->
            Just TestIndeterminate

        _ ->
            Nothing


bloodGroupToString : BloodGroup -> String
bloodGroupToString value =
    case value of
        BloodGroupA ->
            "a"

        BloodGroupB ->
            "b"

        BloodGroupAB ->
            "ab"

        BloodGroupO ->
            "o"


bloodGroupFromString : String -> Maybe BloodGroup
bloodGroupFromString value =
    case value of
        "a" ->
            Just BloodGroupA

        "b" ->
            Just BloodGroupB

        "ab" ->
            Just BloodGroupAB

        "o" ->
            Just BloodGroupO

        _ ->
            Nothing


rhesusToString : Rhesus -> String
rhesusToString value =
    case value of
        RhesusPositive ->
            "positive"

        RhesusNegative ->
            "negative"


rhesusFromString : String -> Maybe Rhesus
rhesusFromString value =
    case value of
        "positive" ->
            Just RhesusPositive

        "negative" ->
            Just RhesusNegative

        _ ->
            Nothing


proteinValueToString : ProteinValue -> String
proteinValueToString value =
    case value of
        Protein0 ->
            "0"

        ProteinPlus1 ->
            "+1"

        ProteinPlus2 ->
            "+2"

        ProteinPlus3 ->
            "+3"

        ProteinPlus4 ->
            "+4"


proteinValueFromString : String -> Maybe ProteinValue
proteinValueFromString value =
    case value of
        "0" ->
            Just Protein0

        "+1" ->
            Just ProteinPlus1

        "+2" ->
            Just ProteinPlus2

        "+3" ->
            Just ProteinPlus3

        "+4" ->
            Just ProteinPlus4

        _ ->
            Nothing


phValueToString : PHValue -> String
phValueToString value =
    case value of
        Ph40 ->
            "4.0"

        Ph45 ->
            "4.5"

        Ph50 ->
            "5.0"

        Ph60 ->
            "6.0"

        Ph65 ->
            "6.5"

        Ph70 ->
            "7.0"

        Ph75 ->
            "7.5"

        Ph80 ->
            "8.0"

        Ph85 ->
            "8.5"


phValueFromString : String -> Maybe PHValue
phValueFromString value =
    case value of
        "4.0" ->
            Just Ph40

        "4.5" ->
            Just Ph45

        "5.0" ->
            Just Ph50

        "6.0" ->
            Just Ph60

        "6.5" ->
            Just Ph65

        "7.0" ->
            Just Ph70

        "7.5" ->
            Just Ph75

        "8.0" ->
            Just Ph80

        "8.5" ->
            Just Ph85

        _ ->
            Nothing


glucoseValueToString : GlucoseValue -> String
glucoseValueToString value =
    case value of
        Glucose0 ->
            "0"

        GlucosePlus1 ->
            "+1"

        GlucosePlus2 ->
            "+2"

        GlucosePlus3 ->
            "+3"

        GlucosePlus4 ->
            "+4"


glucoseValueFromString : String -> Maybe GlucoseValue
glucoseValueFromString value =
    case value of
        "0" ->
            Just Glucose0

        "+1" ->
            Just GlucosePlus1

        "+2" ->
            Just GlucosePlus2

        "+3" ->
            Just GlucosePlus3

        "+4" ->
            Just GlucosePlus4

        _ ->
            Nothing


leukocytesValueToString : LeukocytesValue -> String
leukocytesValueToString value =
    case value of
        LeukocytesNegative ->
            "negative"

        LeukocytesSmall ->
            "small"

        LeukocytesMedium ->
            "medium"

        LeukocytesLarge ->
            "large"


leukocytesValueFromString : String -> Maybe LeukocytesValue
leukocytesValueFromString value =
    case value of
        "negative" ->
            Just LeukocytesNegative

        "small" ->
            Just LeukocytesSmall

        "medium" ->
            Just LeukocytesMedium

        "large" ->
            Just LeukocytesLarge

        _ ->
            Nothing


nitriteValueToString : NitriteValue -> String
nitriteValueToString value =
    case value of
        NitriteNegative ->
            "negative"

        NitritePlus ->
            "+"

        NitritePlusPlus ->
            "++"


nitriteValueFromString : String -> Maybe NitriteValue
nitriteValueFromString value =
    case value of
        "negative" ->
            Just NitriteNegative

        "+" ->
            Just NitritePlus

        "++" ->
            Just NitritePlusPlus

        _ ->
            Nothing


urobilinogenValueToString : UrobilinogenValue -> String
urobilinogenValueToString value =
    case value of
        Urobilinogen002 ->
            "0-0.2"

        Urobilinogen10 ->
            "1"

        Urobilinogen20 ->
            "2"

        Urobilinogen40 ->
            "4"

        Urobilinogen80 ->
            "8"


urobilinogenValueFromString : String -> Maybe UrobilinogenValue
urobilinogenValueFromString value =
    case value of
        "0-0.2" ->
            Just Urobilinogen002

        "1" ->
            Just Urobilinogen10

        "2" ->
            Just Urobilinogen20

        "4" ->
            Just Urobilinogen40

        "8" ->
            Just Urobilinogen80

        _ ->
            Nothing


haemoglobinValueToString : HaemoglobinValue -> String
haemoglobinValueToString value =
    case value of
        HaemoglobinNegative ->
            "negative"

        HaemoglobinNonHemolyzedTrace ->
            "non-hemolyzed-trace"

        HaemoglobinNonHemolyzedModerate ->
            "non-hemolyzed-moderate"

        HaemoglobinHemolyzedTrace ->
            "hemolyzed-trace"

        HaemoglobinSmall ->
            "small"

        HaemoglobinModerate ->
            "moderate"

        HaemoglobinLarge ->
            "large"


haemoglobinValueFromString : String -> Maybe HaemoglobinValue
haemoglobinValueFromString value =
    case value of
        "negative" ->
            Just HaemoglobinNegative

        "non-hemolyzed-trace" ->
            Just HaemoglobinNonHemolyzedTrace

        "non-hemolyzed-moderate" ->
            Just HaemoglobinNonHemolyzedModerate

        "hemolyzed-trace" ->
            Just HaemoglobinHemolyzedTrace

        "small" ->
            Just HaemoglobinSmall

        "moderate" ->
            Just HaemoglobinModerate

        "large" ->
            Just HaemoglobinLarge

        _ ->
            Nothing


ketoneValueToString : KetoneValue -> String
ketoneValueToString value =
    case value of
        KetoneNegative ->
            "negative"

        Ketone5 ->
            "5"

        Ketone10 ->
            "10"

        Ketone15 ->
            "15"

        Ketone40 ->
            "40"

        Ketone80 ->
            "80"

        Ketone100 ->
            "100"


ketoneValueFromString : String -> Maybe KetoneValue
ketoneValueFromString value =
    case value of
        "negative" ->
            Just KetoneNegative

        "5" ->
            Just Ketone5

        "10" ->
            Just Ketone10

        "15" ->
            Just Ketone15

        "40" ->
            Just Ketone40

        "80" ->
            Just Ketone80

        "100" ->
            Just Ketone100

        _ ->
            Nothing


bilirubinValueToString : BilirubinValue -> String
bilirubinValueToString value =
    case value of
        BilirubinNegative ->
            "negative"

        BilirubinSmall ->
            "small"

        BilirubinMedium ->
            "medium"

        BilirubinLarge ->
            "large"


bilirubinValueFromString : String -> Maybe BilirubinValue
bilirubinValueFromString value =
    case value of
        "negative" ->
            Just BilirubinNegative

        "small" ->
            Just BilirubinSmall

        "medium" ->
            Just BilirubinMedium

        "large" ->
            Just BilirubinLarge

        _ ->
            Nothing


{-| If lab results are not provided within this period, we consider them
expired, and do not provide option of filling the results.
Note: HIV PCR can take up to one month to get a result.
-}
labExpirationPeriod : Int
labExpirationPeriod =
    35


prenatalHIVSignToString : PrenatalHIVSign -> String
prenatalHIVSignToString sign =
    case sign of
        HIVProgramHC ->
            "hiv-program-hc"

        PartnerHIVPositive ->
            "partner-hiv-positive"

        PartnerTakingARV ->
            "partner-taking-arv"

        PartnerSurpressedViralLoad ->
            "partner-surpressed-viral-load"

        PrenatalHIVSignPendingInput ->
            "pending-input"

        NoPrenatalHIVSign ->
            "none"


prenatalHIVSignFromString : String -> Maybe PrenatalHIVSign
prenatalHIVSignFromString sign =
    case sign of
        "hiv-program-hc" ->
            Just HIVProgramHC

        "partner-hiv-positive" ->
            Just PartnerHIVPositive

        "partner-taking-arv" ->
            Just PartnerTakingARV

        "partner-surpressed-viral-load" ->
            Just PartnerSurpressedViralLoad

        "pending-input" ->
            Just PrenatalHIVSignPendingInput

        "none" ->
            Just NoPrenatalHIVSign

        _ ->
            Nothing


recommendedTreatmentSignToString : RecommendedTreatmentSign -> String
recommendedTreatmentSignToString sign =
    case sign of
        TreatmentQuinineSulphate ->
            "quinine-sulphate"

        TreatmentCoartem ->
            "coartem"

        TreatmentWrittenProtocols ->
            "written-protocols"

        TreatmentReferToHospital ->
            "refer-to-hospital"

        NoTreatmentForMalaria ->
            "no-treatment-malaria"

        TreatmentPenecilin1 ->
            "penecilin-1"

        TreatmentPenecilin3 ->
            "penecilin-3"

        TreatmentErythromycin ->
            "erythromycin"

        TreatmentAzithromycin ->
            "azithromycin"

        TreatmentCeftriaxon ->
            "ceftriaxon"

        NoTreatmentForSyphilis ->
            "no-treatment-syphilis"

        TreatmentMethyldopa2 ->
            "methyldopa-2"

        TreatmentMethyldopa3 ->
            "methyldopa-3"

        TreatmentMethyldopa4 ->
            "methyldopa-4"

        TreatmentHypertensionAddCarvedilol ->
            "add-carvedilol"

        TreatmentHypertensionAddAmlodipine ->
            "add-amlodipine"

        TreatmentHydrochlorothiazide ->
            "hydrochlorothiazide"

        TreatmentAmlodipine ->
            "amlodipine"

        TreatmentNifedipine ->
            "nifedipine"

        TreatmentCaptopril ->
            "captopril"

        TreatmentLisinopril ->
            "lisinopril"

        TreatmentAtenlol ->
            "atenlol"

        NoTreatmentForHypertension ->
            "no-treatment-hypertension"

        TreatmentAluminiumHydroxide ->
            "aluminium-hydroxide"

        TreatmentHealthEducationForHeartburn ->
            "education-heartburn"

        TreatmentNitrofurantoin ->
            "nitrofurantoin"

        TreatmentAmoxicillin ->
            "amoxicillin"

        TreatmentClotrimaxazole200 ->
            "clotrimaxazole-200"

        TreatmentClotrimaxazole500 ->
            "clotrimaxazole-500"

        TreatmentCloxacillin ->
            "cloxacillin"

        TreatmentMastitisAmoxicillin ->
            "mastitis-amoxicillin"

        TreatmentPenecilinV ->
            "penecilin-v"

        TreatmentParacetamol ->
            "paracetamol"

        TreatmentIbuprofen ->
            "ibuprofen"

        NoTreatmentForMastitis ->
            "no-treatment-mastitis"

        TreatmentMetformin1m1e ->
            "metformin-1m1e"

        TreatmentGlipenclamide1m1e ->
            "glipenclamide-1m1e"

        TreatmentMetformin2m1e ->
            "metformin-2m1e"

        TreatmentGlipenclamide2m1e ->
            "glipenclamide-2m1e"

        TreatmentMetformin2m2e ->
            "metformin-2m2e"

        TreatmentGlipenclamide2m2e ->
            "glipenclamide-2m2e"

        TreatmentMetformin2m2eGlipenclamide1m1e ->
            "metformin-2m2e-glipenclamide-1m1e"

        TreatmentGlipenclamide2m2eMetformin1m1e ->
            "glipenclamide-2m2e-metformin-1m1e"

        NoTreatmentForDiabetes ->
            "no-treatment-diabetes"


recommendedTreatmentSignFromString : String -> Maybe RecommendedTreatmentSign
recommendedTreatmentSignFromString sign =
    case sign of
        "quinine-sulphate" ->
            Just TreatmentQuinineSulphate

        "coartem" ->
            Just TreatmentCoartem

        "written-protocols" ->
            Just TreatmentWrittenProtocols

        "refer-to-hospital" ->
            Just TreatmentReferToHospital

        "no-treatment-malaria" ->
            Just NoTreatmentForMalaria

        "penecilin-1" ->
            Just TreatmentPenecilin1

        "penecilin-3" ->
            Just TreatmentPenecilin3

        "erythromycin" ->
            Just TreatmentErythromycin

        "azithromycin" ->
            Just TreatmentAzithromycin

        "ceftriaxon" ->
            Just TreatmentCeftriaxon

        "no-treatment-syphilis" ->
            Just NoTreatmentForSyphilis

        "methyldopa-2" ->
            Just TreatmentMethyldopa2

        "methyldopa-3" ->
            Just TreatmentMethyldopa3

        "methyldopa-4" ->
            Just TreatmentMethyldopa4

        "add-carvedilol" ->
            Just TreatmentHypertensionAddCarvedilol

        "add-amlodipine" ->
            Just TreatmentHypertensionAddAmlodipine

        "hydrochlorothiazide" ->
            Just TreatmentHydrochlorothiazide

        "amlodipine" ->
            Just TreatmentAmlodipine

        "nifedipine" ->
            Just TreatmentNifedipine

        "captopril" ->
            Just TreatmentCaptopril

        "lisinopril" ->
            Just TreatmentLisinopril

        "atenlol" ->
            Just TreatmentAtenlol

        "no-treatment-hypertension" ->
            Just NoTreatmentForHypertension

        "aluminium-hydroxide" ->
            Just TreatmentAluminiumHydroxide

        "education-heartburn" ->
            Just TreatmentHealthEducationForHeartburn

        "nitrofurantoin" ->
            Just TreatmentNitrofurantoin

        "amoxicillin" ->
            Just TreatmentAmoxicillin

        "clotrimaxazole-200" ->
            Just TreatmentClotrimaxazole200

        "clotrimaxazole-500" ->
            Just TreatmentClotrimaxazole500

        "cloxacillin" ->
            Just TreatmentCloxacillin

        "mastitis-amoxicillin" ->
            Just TreatmentMastitisAmoxicillin

        "penecilin-v" ->
            Just TreatmentPenecilinV

        "paracetamol" ->
            Just TreatmentParacetamol

        "ibuprofen" ->
            Just TreatmentIbuprofen

        "no-treatment-mastitis" ->
            Just NoTreatmentForMastitis

        "metformin-1m1e" ->
            Just TreatmentMetformin1m1e

        "glipenclamide-1m1e" ->
            Just TreatmentGlipenclamide1m1e

        "metformin-2m1e" ->
            Just TreatmentMetformin2m1e

        "glipenclamide-2m1e" ->
            Just TreatmentGlipenclamide2m1e

        "metformin-2m2e" ->
            Just TreatmentMetformin2m2e

        "glipenclamide-2m2e" ->
            Just TreatmentGlipenclamide2m2e

        "metformin-2m2e-glipenclamide-1m1e" ->
            Just TreatmentMetformin2m2eGlipenclamide1m1e

        "glipenclamide-2m2e-metformin-1m1e" ->
            Just TreatmentGlipenclamide2m2eMetformin1m1e

        "no-treatment-diabetes" ->
            Just NoTreatmentForDiabetes

        _ ->
            Nothing


avoidingGuidanceReasonFromString : String -> Maybe AvoidingGuidanceReason
avoidingGuidanceReasonFromString value =
    case value of
        "hypertension-lack-of-stock" ->
            Just AvoidingGuidanceHypertensionLackOfStock

        "hypertension-known-allergy" ->
            Just AvoidingGuidanceHypertensionKnownAllergy

        "hypertension-patient-declined" ->
            Just AvoidingGuidanceHypertensionPatientDeclined

        "hypertension-patient-unable-to-afford" ->
            Just AvoidingGuidanceHypertensionPatientUnableToAfford

        "hypertension-reinforce-adherence" ->
            Just AvoidingGuidanceHypertensionReinforceAdherence

        "hypertension-other" ->
            Just AvoidingGuidanceHypertensionOther

        _ ->
            Nothing


avoidingGuidanceReasonToString : AvoidingGuidanceReason -> String
avoidingGuidanceReasonToString value =
    case value of
        AvoidingGuidanceHypertensionLackOfStock ->
            "hypertension-lack-of-stock"

        AvoidingGuidanceHypertensionKnownAllergy ->
            "hypertension-known-allergy"

        AvoidingGuidanceHypertensionPatientDeclined ->
            "hypertension-patient-declined"

        AvoidingGuidanceHypertensionPatientUnableToAfford ->
            "hypertension-patient-unable-to-afford"

        AvoidingGuidanceHypertensionReinforceAdherence ->
            "hypertension-reinforce-adherence"

        AvoidingGuidanceHypertensionOther ->
            "hypertension-other"


illnessSymptomToString : IllnessSymptom -> String
illnessSymptomToString symptom =
    case symptom of
        IllnessSymptomHeadache ->
            "headache"

        IllnessSymptomVisionChanges ->
            "vision-changes"

        IllnessSymptomRash ->
            "rash"

        IllnessSymptomPainlessUlcerMouth ->
            "painless-ulcer-mouth"

        IllnessSymptomPainlessUlcerGenitals ->
            "painless-ulcer-genitals"

        IllnessSymptomPendingInput ->
            "pending-input"

        NoIllnessSymptoms ->
            "none"


illnessSymptomFromString : String -> Maybe IllnessSymptom
illnessSymptomFromString symptom =
    case symptom of
        "headache" ->
            Just IllnessSymptomHeadache

        "vision-changes" ->
            Just IllnessSymptomVisionChanges

        "rash" ->
            Just IllnessSymptomRash

        "painless-ulcer-mouth" ->
            Just IllnessSymptomPainlessUlcerMouth

        "painless-ulcer-genitals" ->
            Just IllnessSymptomPainlessUlcerGenitals

        "pending-input" ->
            Just IllnessSymptomPendingInput

        "none" ->
            Just NoIllnessSymptoms

        _ ->
            Nothing


prenatalSymptomToString : PrenatalSymptom -> String
prenatalSymptomToString value =
    case value of
        BurningWithUrination ->
            "burning-with-urination"

        AbnormalVaginalDischarge ->
            "abnormal-vaginal-discharge"

        NauseaAndVomiting ->
            "nausea-and-vomiting"

        Heartburn ->
            "heartburn"

        LegCramps ->
            "leg-cramps"

        LowBackPain ->
            "low-back-pain"

        CoughContinuous ->
            "cough-continuous"

        PelvicPain ->
            "pelvic-pain"

        Constipation ->
            "constipation"

        VaricoseVeins ->
            "varicose-veins"

        LegPainRedness ->
            "leg-pain-redness"

        PostpartumAbdominalPain ->
            "abdominal-pain"

        PostpartumUrinaryIncontinence ->
            "urinary-incontinence"

        PostpartumHeadache ->
            "headache"

        PostpartumFatigue ->
            "fatigue"

        PostpartumFever ->
            "fever"

        PostpartumPerinealPainOrDischarge ->
            "perineal-pain-discharge"

        NoPrenatalSymptoms ->
            "none"


prenatalSymptomFromString : String -> Maybe PrenatalSymptom
prenatalSymptomFromString value =
    case value of
        "burning-with-urination" ->
            Just BurningWithUrination

        "abnormal-vaginal-discharge" ->
            Just AbnormalVaginalDischarge

        "nausea-and-vomiting" ->
            Just NauseaAndVomiting

        "heartburn" ->
            Just Heartburn

        "leg-cramps" ->
            Just LegCramps

        "low-back-pain" ->
            Just LowBackPain

        "cough-continuous" ->
            Just CoughContinuous

        "pelvic-pain" ->
            Just PelvicPain

        "constipation" ->
            Just Constipation

        "varicose-veins" ->
            Just VaricoseVeins

        "leg-pain-redness" ->
            Just LegPainRedness

        "abdominal-pain" ->
            Just PostpartumAbdominalPain

        "urinary-incontinence" ->
            Just PostpartumUrinaryIncontinence

        "headache" ->
            Just PostpartumHeadache

        "fatigue" ->
            Just PostpartumFatigue

        "fever" ->
            Just PostpartumFever

        "perineal-pain-discharge" ->
            Just PostpartumPerinealPainOrDischarge

        "none" ->
            Just NoPrenatalSymptoms

        _ ->
            Nothing


prenatalSymptomQuestionToString : PrenatalSymptomQuestion -> String
prenatalSymptomQuestionToString value =
    case value of
        SymptomQuestionDizziness ->
            "dizziness"

        SymptomQuestionLowUrineOutput ->
            "low-urine-output"

        SymptomQuestionDarkUrine ->
            "dark-urine"

        SymptomQuestionPelvicPainHospitalization ->
            "pelvic-pain-hospitalization"

        SymptomQuestionLegPainRednessLeft ->
            "leg-pain-redness-left"

        SymptomQuestionLegSwollen ->
            "leg-swollen"

        SymptomQuestionLegPainful ->
            "leg-painful"

        SymptomQuestionLegWarm ->
            "leg-warm"

        SymptomQuestionNightSweats ->
            "night-sweats"

        SymptomQuestionBloodInSputum ->
            "blood-in-sputum"

        SymptomQuestionWeightLoss ->
            "weight-loss"

        SymptomQuestionSevereFatigue ->
            "severe-fatigue"

        SymptomQuestionVaginalItching ->
            "vaginal-itching"

        SymptomQuestionPartnerUrethralDischarge ->
            "partner-urethral-discharge"

        SymptomQuestionVaginalDischarge ->
            "vaginal-discharge"

        SymptomQuestionFrequentUrination ->
            "frequent-urination"

        SymptomQuestionFlankPain ->
            "flank-pain"

        NoSymptomQuestions ->
            "none"


prenatalSymptomQuestionFromString : String -> Maybe PrenatalSymptomQuestion
prenatalSymptomQuestionFromString value =
    case value of
        "dizziness" ->
            Just SymptomQuestionDizziness

        "low-urine-output" ->
            Just SymptomQuestionLowUrineOutput

        "dark-urine" ->
            Just SymptomQuestionDarkUrine

        "pelvic-pain-hospitalization" ->
            Just SymptomQuestionPelvicPainHospitalization

        "leg-pain-redness-left" ->
            Just SymptomQuestionLegPainRednessLeft

        "leg-painful" ->
            Just SymptomQuestionLegPainful

        "leg-swollen" ->
            Just SymptomQuestionLegSwollen

        "leg-warm" ->
            Just SymptomQuestionLegWarm

        "night-sweats" ->
            Just SymptomQuestionNightSweats

        "blood-in-sputum" ->
            Just SymptomQuestionBloodInSputum

        "weight-loss" ->
            Just SymptomQuestionWeightLoss

        "severe-fatigue" ->
            Just SymptomQuestionSevereFatigue

        "vaginal-itching" ->
            Just SymptomQuestionVaginalItching

        "partner-urethral-discharge" ->
            Just SymptomQuestionPartnerUrethralDischarge

        "vaginal-discharge" ->
            Just SymptomQuestionVaginalDischarge

        "frequent-urination" ->
            Just SymptomQuestionFrequentUrination

        "flank-pain" ->
            Just SymptomQuestionFlankPain

        "none" ->
            Just NoSymptomQuestions

        _ ->
            Nothing


prenatalFlankPainSignToString : PrenatalFlankPainSign -> String
prenatalFlankPainSignToString value =
    case value of
        FlankPainLeftSide ->
            "left"

        FlankPainRightSide ->
            "right"

        FlankPainBothSides ->
            "both"

        NoFlankPain ->
            "none"


prenatalFlankPainSignFromString : String -> Maybe PrenatalFlankPainSign
prenatalFlankPainSignFromString value =
    case value of
        "left" ->
            Just FlankPainLeftSide

        "right" ->
            Just FlankPainRightSide

        "both" ->
            Just FlankPainBothSides

        "none" ->
            Just NoFlankPain

        _ ->
            Nothing


outsideCareSignToString : OutsideCareSign -> String
outsideCareSignToString value =
    case value of
        SeenAtAnotherFacility ->
            "seen-at-another-facility"

        GivenNewDiagnoses ->
            "given-new-diagnoses"

        GivenMedicine ->
            "given-medicine"

        PlannedFollowUpCareWithSpecialist ->
            "follow-up-with-specialist"

        NoOutsideCareSigns ->
            "none"


outsideCareSignFromString : String -> Maybe OutsideCareSign
outsideCareSignFromString value =
    case value of
        "seen-at-another-facility" ->
            Just SeenAtAnotherFacility

        "given-new-diagnoses" ->
            Just GivenNewDiagnoses

        "given-medicine" ->
            Just GivenMedicine

        "follow-up-with-specialist" ->
            Just PlannedFollowUpCareWithSpecialist

        "none" ->
            Just NoOutsideCareSigns

        _ ->
            Nothing


outsideCareMedicationToString : OutsideCareMedication -> String
outsideCareMedicationToString value =
    case value of
        OutsideCareMedicationQuinineSulphate ->
            "quinine-sulphate"

        OutsideCareMedicationCoartem ->
            "coartem"

        NoOutsideCareMedicationForMalaria ->
            "no-treatment-malaria"

        OutsideCareMedicationPenecilin1 ->
            "penecilin-1"

        OutsideCareMedicationPenecilin3 ->
            "penecilin-3"

        OutsideCareMedicationErythromycin ->
            "erythromycin"

        OutsideCareMedicationAzithromycin ->
            "azithromycin"

        OutsideCareMedicationCeftriaxon ->
            "ceftriaxon"

        NoOutsideCareMedicationForSyphilis ->
            "no-treatment-syphilis"

        OutsideCareMedicationMethyldopa2 ->
            "methyldopa-2"

        OutsideCareMedicationMethyldopa3 ->
            "methyldopa-3"

        OutsideCareMedicationMethyldopa4 ->
            "methyldopa-4"

        NoOutsideCareMedicationForHypertension ->
            "no-treatment-hypertension"

        OutsideCareMedicationCarvedilol ->
            "carvedilol"

        OutsideCareMedicationAmlodipine ->
            "amlodipine"

        OutsideCareMedicationTDF3TC ->
            "tdf3tc"

        OutsideCareMedicationDolutegravir ->
            "dolutegravir"

        NoOutsideCareMedicationForHIV ->
            "no-treatment-hiv"

        OutsideCareMedicationIron1 ->
            "iron1"

        OutsideCareMedicationIron2 ->
            "iron2"

        OutsideCareMedicationFolicAcid ->
            "folic-acid"

        NoOutsideCareMedicationForAnemia ->
            "no-treatment-anemia"

        NoOutsideCareMedications ->
            "none"


outsideCareMedicationFromString : String -> Maybe OutsideCareMedication
outsideCareMedicationFromString value =
    case value of
        "quinine-sulphate" ->
            Just OutsideCareMedicationQuinineSulphate

        "coartem" ->
            Just OutsideCareMedicationCoartem

        "no-treatment-malaria" ->
            Just NoOutsideCareMedicationForMalaria

        "penecilin-1" ->
            Just OutsideCareMedicationPenecilin1

        "penecilin-3" ->
            Just OutsideCareMedicationPenecilin3

        "erythromycin" ->
            Just OutsideCareMedicationErythromycin

        "azithromycin" ->
            Just OutsideCareMedicationAzithromycin

        "ceftriaxon" ->
            Just OutsideCareMedicationCeftriaxon

        "no-treatment-syphilis" ->
            Just NoOutsideCareMedicationForSyphilis

        "methyldopa-2" ->
            Just OutsideCareMedicationMethyldopa2

        "methyldopa-3" ->
            Just OutsideCareMedicationMethyldopa3

        "methyldopa-4" ->
            Just OutsideCareMedicationMethyldopa4

        "carvedilol" ->
            Just OutsideCareMedicationCarvedilol

        "amlodipine" ->
            Just OutsideCareMedicationAmlodipine

        "no-treatment-hypertension" ->
            Just NoOutsideCareMedicationForHypertension

        "tdf3tc" ->
            Just OutsideCareMedicationTDF3TC

        "dolutegravir" ->
            Just OutsideCareMedicationDolutegravir

        "no-treatment-hiv" ->
            Just NoOutsideCareMedicationForHIV

        "iron1" ->
            Just OutsideCareMedicationIron1

        "iron2" ->
            Just OutsideCareMedicationIron2

        "folic-acid" ->
            Just OutsideCareMedicationFolicAcid

        "no-treatment-anemia" ->
            Just NoOutsideCareMedicationForAnemia

        "none" ->
            Just NoOutsideCareMedications

        _ ->
            Nothing


prenatalMentalHealthQuestionToString : PrenatalMentalHealthQuestion -> String
prenatalMentalHealthQuestionToString value =
    case value of
        MentalHealthQuestion1 ->
            "q1"

        MentalHealthQuestion2 ->
            "q2"

        MentalHealthQuestion3 ->
            "q3"

        MentalHealthQuestion4 ->
            "q4"

        MentalHealthQuestion5 ->
            "q5"

        MentalHealthQuestion6 ->
            "q6"

        MentalHealthQuestion7 ->
            "q7"

        MentalHealthQuestion8 ->
            "q8"

        MentalHealthQuestion9 ->
            "q9"

        MentalHealthQuestion10 ->
            "q10"


prenatalMentalHealthQuestionFromString : String -> Maybe PrenatalMentalHealthQuestion
prenatalMentalHealthQuestionFromString value =
    case value of
        "q1" ->
            Just MentalHealthQuestion1

        "q2" ->
            Just MentalHealthQuestion2

        "q3" ->
            Just MentalHealthQuestion3

        "q4" ->
            Just MentalHealthQuestion4

        "q5" ->
            Just MentalHealthQuestion5

        "q6" ->
            Just MentalHealthQuestion6

        "q7" ->
            Just MentalHealthQuestion7

        "q8" ->
            Just MentalHealthQuestion8

        "q9" ->
            Just MentalHealthQuestion9

        "q10" ->
            Just MentalHealthQuestion10

        _ ->
            Nothing


prenatalMentalHealthQuestionOptionToString : PrenatalMentalHealthQuestionOption -> String
prenatalMentalHealthQuestionOptionToString value =
    case value of
        MentalHealthQuestionOption0 ->
            "0"

        MentalHealthQuestionOption1 ->
            "1"

        MentalHealthQuestionOption2 ->
            "2"

        MentalHealthQuestionOption3 ->
            "3"


prenatalMentalHealthQuestionOptionFromString : String -> Maybe PrenatalMentalHealthQuestionOption
prenatalMentalHealthQuestionOptionFromString value =
    case value of
        "0" ->
            Just MentalHealthQuestionOption0

        "1" ->
            Just MentalHealthQuestionOption1

        "2" ->
            Just MentalHealthQuestionOption2

        "3" ->
            Just MentalHealthQuestionOption3

        _ ->
            Nothing


breastfeedingSignToString : BreastfeedingSign -> String
breastfeedingSignToString value =
    case value of
        IsBreastfeeding ->
            "breastfeeding"

        NotBreastfeedingBreastPain ->
            "not-breastfeeding-breast-pain"

        NotBreastfeedingBreastRedness ->
            "not-breastfeeding-breast-redness"

        NotBreastfeedingLowMilkProduction ->
            "not-breastfeeding-low-milk-production"

        NotBreastfeedingProblemsLatching ->
            "not-breastfeeding-problems-latching"

        NotBreastfeedingMedicalProblems ->
            "not-breastfeeding-medical-problems"

        NotBreastfeedingPersonalChoice ->
            "not-breastfeeding-personal-choice"

        NotBreastfeedingOther ->
            "not-breastfeeding-other"

        BreastPain ->
            "breast-pain"

        BreastRedness ->
            "breast-redness"

        EnoughMilk ->
            "enough-milk"

        LatchingWell ->
            "latching-well"

        NoBreastfeedingSigns ->
            "none"


breastfeedingSignFromString : String -> Maybe BreastfeedingSign
breastfeedingSignFromString value =
    case value of
        "breastfeeding" ->
            Just IsBreastfeeding

        "not-breastfeeding-breast-pain" ->
            Just NotBreastfeedingBreastPain

        "not-breastfeeding-breast-redness" ->
            Just NotBreastfeedingBreastRedness

        "not-breastfeeding-low-milk-production" ->
            Just NotBreastfeedingLowMilkProduction

        "not-breastfeeding-problems-latching" ->
            Just NotBreastfeedingProblemsLatching

        "not-breastfeeding-medical-problems" ->
            Just NotBreastfeedingMedicalProblems

        "not-breastfeeding-personal-choice" ->
            Just NotBreastfeedingPersonalChoice

        "not-breastfeeding-other" ->
            Just NotBreastfeedingOther

        "breast-pain" ->
            Just BreastPain

        "breast-redness" ->
            Just BreastRedness

        "enough-milk" ->
            Just EnoughMilk

        "latching-well" ->
            Just LatchingWell

        "none" ->
            Just NoBreastfeedingSigns

        _ ->
            Nothing


reasonForNonReferralFromString : String -> Maybe ReasonForNonReferral
reasonForNonReferralFromString value =
    case value of
        "client-refused" ->
            Just ClientRefused

        "no-ambulance" ->
            Just NoAmbulance

        "unable-to-afford-fee" ->
            Just ClientUnableToAffordFees

        "already-in-care" ->
            Just ClientAlreadyInCare

        "not-indicated" ->
            Just ReasonForNonReferralNotIndicated

        "other" ->
            Just ReasonForNonReferralOther

        "none" ->
            Just NoReasonForNonReferral

        _ ->
            Nothing


vaginalExamSignToString : VaginalExamSign -> String
vaginalExamSignToString value =
    case value of
        FoulSmellingLochia ->
            "foul-smelling-lochia"

        ExcessiveVaginalBleeding ->
            "bleeding"

        NormalVaginalExam ->
            "normal"


vaginalExamSignFromString : String -> Maybe VaginalExamSign
vaginalExamSignFromString value =
    case value of
        "foul-smelling-lochia" ->
            Just FoulSmellingLochia

        "bleeding" ->
            Just ExcessiveVaginalBleeding

        "normal" ->
            Just NormalVaginalExam

        _ ->
            Nothing


guExamSignToString : GUExamSign -> String
guExamSignToString value =
    case value of
        EpisiotomyOrPerinealTear ->
            "episiotomy-perineal-tear"

        RectalHemorrhoids ->
            "rectal-hemorrhoids"

        NoGUExamSigns ->
            "none"


guExamSignFromString : String -> Maybe GUExamSign
guExamSignFromString value =
    case value of
        "episiotomy-perineal-tear" ->
            Just EpisiotomyOrPerinealTear

        "rectal-hemorrhoids" ->
            Just RectalHemorrhoids

        "none" ->
            Just NoGUExamSigns

        _ ->
            Nothing


reasonForNonReferralToString : ReasonForNonReferral -> String
reasonForNonReferralToString value =
    case value of
        ClientRefused ->
            "client-refused"

        NoAmbulance ->
            "no-ambulance"

        ClientUnableToAffordFees ->
            "unable-to-afford-fee"

        ClientAlreadyInCare ->
            "already-in-care"

        ReasonForNonReferralNotIndicated ->
            "not-indicated"

        ReasonForNonReferralOther ->
            "other"

        NoReasonForNonReferral ->
            "none"


postpartumHealingProblemToString : PostpartumHealingProblem -> String
postpartumHealingProblemToString value =
    case value of
        NormalPostpartumHealing ->
            "normal-healing"

        HealingProblemSwelling ->
            "swelling"

        HealingProblemDischarge ->
            "discharge"

        HealingProblemReleaseOfSutures ->
            "release-of-sutures"

        HealingProblemHematoma ->
            "hematoma"

        HealingProblemBruising ->
            "bruising"


postpartumHealingProblemFromString : String -> Maybe PostpartumHealingProblem
postpartumHealingProblemFromString value =
    case value of
        "normal-healing" ->
            Just NormalPostpartumHealing

        "swelling" ->
            Just HealingProblemSwelling

        "discharge" ->
            Just HealingProblemDischarge

        "release-of-sutures" ->
            Just HealingProblemReleaseOfSutures

        "hematoma" ->
            Just HealingProblemHematoma

        "bruising" ->
            Just HealingProblemBruising

        _ ->
            Nothing


pregnancyTestResultFromString : String -> Maybe PregnancyTestResult
pregnancyTestResultFromString result =
    case result of
        "positive" ->
            Just PregnancyTestPositive

        "negative" ->
            Just PregnancyTestNegative

        "indeterminate" ->
            Just PregnancyTestIndeterminate

        "unable-to-conduct" ->
            Just PregnancyTestUnableToConduct

        _ ->
            Nothing


pregnancyTestResultToString : PregnancyTestResult -> String
pregnancyTestResultToString sign =
    case sign of
        PregnancyTestPositive ->
            "positive"

        PregnancyTestNegative ->
            "negative"

        PregnancyTestIndeterminate ->
            "indeterminate"

        PregnancyTestUnableToConduct ->
            "unable-to-conduct"


ncdDangerSignFromString : String -> Maybe NCDDangerSign
ncdDangerSignFromString sign =
    case sign of
        "dyspnea" ->
            Just Dyspnea

        "vision-changes" ->
            Just VisionChanges

        "chest-pain" ->
            Just ChestPain

        "flank-pain" ->
            Just FlankPain

        "hematuria" ->
            Just Hematuria

        "severe-headaches" ->
            Just SevereHeadaches

        "loss-of-conciousness" ->
            Just LossOfConciousness

        "none" ->
            Just NoNCDDangerSigns

        _ ->
            Nothing


ncdDangerSignToString : NCDDangerSign -> String
ncdDangerSignToString sign =
    case sign of
        Dyspnea ->
            "dyspnea"

        VisionChanges ->
            "vision-changes"

        ChestPain ->
            "chest-pain"

        FlankPain ->
            "flank-pain"

        Hematuria ->
            "hematuria"

        SevereHeadaches ->
            "severe-headaches"

        LossOfConciousness ->
            "loss-of-conciousness"

        NoNCDDangerSigns ->
            "none"


ncdGroup1SymptomFromString : String -> Maybe NCDGroup1Symptom
ncdGroup1SymptomFromString sign =
    case sign of
        "swelling-in-legs" ->
            Just SwellingInLegs

        "urinary-frequency" ->
            Just UrinaryFrequency

        "anxiety" ->
            Just Anxiety

        "weight-loss" ->
            Just WeightLoss

        "palpitations" ->
            Just Palpitations

        "tremor" ->
            Just Tremor

        "swelling-in-face" ->
            Just SwellingInFace

        "swelling-in-abdomen" ->
            Just SwellingInAbdomen

        "dizziness-with-changing-position" ->
            Just DizzinessWithChangingPosition

        "mild-headache" ->
            Just MildHeadache

        "none" ->
            Just NoNCDGroup1Symptoms

        _ ->
            Nothing


ncdGroup1SymptomToString : NCDGroup1Symptom -> String
ncdGroup1SymptomToString sign =
    case sign of
        SwellingInLegs ->
            "swelling-in-legs"

        UrinaryFrequency ->
            "urinary-frequency"

        Anxiety ->
            "anxiety"

        WeightLoss ->
            "weight-loss"

        Palpitations ->
            "palpitations"

        Tremor ->
            "tremor"

        SwellingInFace ->
            "swelling-in-face"

        SwellingInAbdomen ->
            "swelling-in-abdomen"

        DizzinessWithChangingPosition ->
            "dizziness-with-changing-position"

        MildHeadache ->
            "mild-headache"

        NoNCDGroup1Symptoms ->
            "none"


ncdGroup2SymptomFromString : String -> Maybe NCDGroup2Symptom
ncdGroup2SymptomFromString sign =
    case sign of
        "weakness-of-one-side-of-the-body" ->
            Just WeaknessOfOneSideOfTheBody

        "problems-with-walking" ->
            Just ProblemsWithWalking

        "problems-with-talking" ->
            Just ProblemsWithTalking

        "decreased-vision" ->
            Just DecreasedVision

        "blurry-vision" ->
            Just BlurryVision

        "increased-fatigue-with-daily-activities" ->
            Just IncreasedFatigueWithDailyActivities

        "short-of-breath-when-laying-down" ->
            Just ShortOfBreathWhenLayingDown

        "short-of-breath-at-night" ->
            Just ShortOfBreathAtNight

        "kidney-problems" ->
            Just KidneyProblems

        "increased-thirst" ->
            Just NCDIncreasedThirst

        "none" ->
            Just NoNCDGroup2Symptoms

        _ ->
            Nothing


ncdGroup2SymptomToString : NCDGroup2Symptom -> String
ncdGroup2SymptomToString sign =
    case sign of
        WeaknessOfOneSideOfTheBody ->
            "weakness-of-one-side-of-the-body"

        ProblemsWithWalking ->
            "problems-with-walking"

        ProblemsWithTalking ->
            "problems-with-talking"

        DecreasedVision ->
            "decreased-vision"

        BlurryVision ->
            "blurry-vision"

        IncreasedFatigueWithDailyActivities ->
            "increased-fatigue-with-daily-activities"

        ShortOfBreathWhenLayingDown ->
            "short-of-breath-when-laying-down"

        ShortOfBreathAtNight ->
            "short-of-breath-at-night"

        KidneyProblems ->
            "kidney-problems"

        NCDIncreasedThirst ->
            "increased-thirst"

        NoNCDGroup2Symptoms ->
            "none"


ncdPainSymptomFromString : String -> Maybe NCDPainSymptom
ncdPainSymptomFromString sign =
    case sign of
        "flank" ->
            Just PainFlank

        "lower-back" ->
            Just PainLowerBack

        "feet" ->
            Just PainFeet

        "neck" ->
            Just PainNeck

        "abdomen" ->
            Just PainAbdomen

        "none" ->
            Just NoNCDPainSymptoms

        _ ->
            Nothing


ncdPainSymptomToString : NCDPainSymptom -> String
ncdPainSymptomToString sign =
    case sign of
        PainFlank ->
            "flank"

        PainLowerBack ->
            "lower-back"

        PainFeet ->
            "feet"

        PainNeck ->
            "neck"

        PainAbdomen ->
            "abdomen"

        NoNCDPainSymptoms ->
            "none"


medicalConditionFromString : String -> Maybe MedicalCondition
medicalConditionFromString value =
    case value of
        "hiv" ->
            Just MedicalConditionHIV

        "diabetes" ->
            Just MedicalConditionDiabetes

        "kidney-disease" ->
            Just MedicalConditionKidneyDisease

        "pregnancy" ->
            Just MedicalConditionPregnancy

        "hypertension" ->
            Just MedicalConditionHypertension

        "gestational-diabetes" ->
            Just MedicalConditionGestationalDiabetes

        "pregnancy-related-hypertension" ->
            Just MedicalConditionPregnancyRelatedHypertension

        "none" ->
            Just NoMedicalConditions

        "neuropathy" ->
            Just MedicalConditionNeuropathy

        "rental-complications" ->
            Just MedicalConditionRentalComplications

        "malaria" ->
            Just MedicalConditionMalaria

        "tuberculosis" ->
            Just MedicalConditionTuberculosis

        "hepatitis-b" ->
            Just MedicalConditionHepatitisB

        "syphilis" ->
            Just MedicalConditionSyphilis

        "eye-complications" ->
            Just MedicalConditionEyeComplications

        "anemia" ->
            Just MedicalConditionAnemia

        "other" ->
            Just MedicalConditionOther

        _ ->
            Nothing


medicalConditionToString : MedicalCondition -> String
medicalConditionToString value =
    case value of
        MedicalConditionHIV ->
            "hiv"

        MedicalConditionDiabetes ->
            "diabetes"

        MedicalConditionKidneyDisease ->
            "kidney-disease"

        MedicalConditionPregnancy ->
            "pregnancy"

        MedicalConditionHypertension ->
            "hypertension"

        MedicalConditionGestationalDiabetes ->
            "gestational-diabetes"

        MedicalConditionPregnancyRelatedHypertension ->
            "pregnancy-related-hypertension"

        MedicalConditionNeuropathy ->
            "neuropathy"

        MedicalConditionRentalComplications ->
            "rental-complications"

        MedicalConditionMalaria ->
            "malaria"

        MedicalConditionTuberculosis ->
            "tuberculosis"

        MedicalConditionHepatitisB ->
            "hepatitis-b"

        MedicalConditionSyphilis ->
            "syphilis"

        MedicalConditionEyeComplications ->
            "eye-complications"

        MedicalConditionAnemia ->
            "anemia"

        MedicalConditionOther ->
            "other"

        NoMedicalConditions ->
            "none"


ncdFamilyHistorySignFromString : String -> Maybe NCDFamilyHistorySign
ncdFamilyHistorySignFromString value =
    case value of
        "hypertension-history" ->
            Just SignHypertensionHistory

        "heart-problem-history" ->
            Just SignHeartProblemHistory

        "diabetes-history" ->
            Just SignDiabetesHistory

        "none" ->
            Just NoNCDFamilyHistorySigns

        _ ->
            Nothing


ncdFamilyHistorySignToString : NCDFamilyHistorySign -> String
ncdFamilyHistorySignToString value =
    case value of
        SignHypertensionHistory ->
            "hypertension-history"

        SignHeartProblemHistory ->
            "heart-problem-history"

        SignDiabetesHistory ->
            "diabetes-history"

        NoNCDFamilyHistorySigns ->
            "none"


predecessorFromString : String -> Maybe Predecessor
predecessorFromString value =
    case value of
        "father" ->
            Just PredecessorFather

        "mother" ->
            Just PredecessorMother

        "grand-father" ->
            Just PredecessorGrandFather

        "grand-mother" ->
            Just PredecessorGrandMother

        "none" ->
            Just NoPredecessors

        _ ->
            Nothing


predecessorToString : Predecessor -> String
predecessorToString value =
    case value of
        PredecessorFather ->
            "father"

        PredecessorMother ->
            "mother"

        PredecessorGrandFather ->
            "grand-father"

        PredecessorGrandMother ->
            "grand-mother"

        NoPredecessors ->
            "none"


medicationCausingHypertensionFromString : String -> Maybe MedicationCausingHypertension
medicationCausingHypertensionFromString value =
    case value of
        "oestrogens" ->
            Just MedicationOestrogens

        "steroids" ->
            Just MedicationSteroids

        "amitriptyline" ->
            Just MedicationAmitriptyline

        "ibuprofen" ->
            Just MedicationIbuprofen

        "none" ->
            Just NoMedicationCausingHypertension

        _ ->
            Nothing


medicationCausingHypertensionToString : MedicationCausingHypertension -> String
medicationCausingHypertensionToString value =
    case value of
        MedicationOestrogens ->
            "oestrogens"

        MedicationSteroids ->
            "steroids"

        MedicationAmitriptyline ->
            "amitriptyline"

        MedicationIbuprofen ->
            "ibuprofen"

        NoMedicationCausingHypertension ->
            "none"


medicationTreatingHypertensionFromString : String -> Maybe MedicationTreatingHypertension
medicationTreatingHypertensionFromString value =
    case value of
        "ace-inhibitors" ->
            Just MedicationAceInhibitors

        "arbs" ->
            Just MedicationARBs

        "hctz" ->
            Just MedicationHCTZ

        "calcium-channel-blockers" ->
            Just MedicationCalciumChannelBlockers

        "methyldopa" ->
            Just MedicationMethyldopa

        "beta-blockers" ->
            Just MedicationBetaBlockers

        "hydralazine" ->
            Just MedicationHydralazine

        "none" ->
            Just NoMedicationTreatingHypertension

        _ ->
            Nothing


medicationTreatingHypertensionToString : MedicationTreatingHypertension -> String
medicationTreatingHypertensionToString value =
    case value of
        MedicationAceInhibitors ->
            "ace-inhibitors"

        MedicationARBs ->
            "arbs"

        MedicationHCTZ ->
            "hctz"

        MedicationCalciumChannelBlockers ->
            "calcium-channel-blockers"

        MedicationMethyldopa ->
            "methyldopa"

        MedicationBetaBlockers ->
            "beta-blockers"

        MedicationHydralazine ->
            "hydralazine"

        NoMedicationTreatingHypertension ->
            "none"


medicationTreatingDiabetesFromString : String -> Maybe MedicationTreatingDiabetes
medicationTreatingDiabetesFromString value =
    case value of
        "metformin" ->
            Just MedicationMetformin

        "glibenclamide" ->
            Just MedicationGlibenclamide

        "insulin" ->
            Just MedicationInsulin

        "none" ->
            Just NoMedicationTreatingDiabetes

        _ ->
            Nothing


medicationTreatingDiabetesToString : MedicationTreatingDiabetes -> String
medicationTreatingDiabetesToString value =
    case value of
        MedicationMetformin ->
            "metformin"

        MedicationGlibenclamide ->
            "glibenclamide"

        MedicationInsulin ->
            "insulin"

        NoMedicationTreatingDiabetes ->
            "none"


ncdSocialHistorySignFromString : String -> Maybe NCDSocialHistorySign
ncdSocialHistorySignFromString value =
    case value of
        "drink-alcohol" ->
            Just SignDrinkAlcohol

        "smoke-cigarettes" ->
            Just SignSmokeCigarettes

        "consume-salt" ->
            Just SignConsumeSalt

        "difficult-4-times-a-year" ->
            Just SignDifficult4TimesAYear

        "help-with-treatment-at-home" ->
            Just SignHelpWithTreatmentAtHome

        "none" ->
            Just NoNCDSocialHistorySigns

        _ ->
            Nothing


ncdSocialHistorySignToString : NCDSocialHistorySign -> String
ncdSocialHistorySignToString value =
    case value of
        SignDrinkAlcohol ->
            "drink-alcohol"

        SignSmokeCigarettes ->
            "smoke-cigarettes"

        SignConsumeSalt ->
            "consume-salt"

        SignDifficult4TimesAYear ->
            "difficult-4-times-a-year"

        SignHelpWithTreatmentAtHome ->
            "help-with-treatment-at-home"

        NoNCDSocialHistorySigns ->
            "none"


foodGroupFromString : String -> Maybe FoodGroup
foodGroupFromString value =
    case value of
        "vegetables" ->
            Just FoodGroupVegetables

        "carbohydrates" ->
            Just FoodGroupCarbohydrates

        "protein" ->
            Just FoodGroupProtein

        _ ->
            Nothing


foodGroupToString : FoodGroup -> String
foodGroupToString value =
    case value of
        FoodGroupVegetables ->
            "vegetables"

        FoodGroupCarbohydrates ->
            "carbohydrates"

        FoodGroupProtein ->
            "protein"


laboratoryTestToString : LaboratoryTest -> String
laboratoryTestToString value =
    case value of
        TestBloodGpRs ->
            "blood-group"

        TestHemoglobin ->
            "hemoglobin"

        TestHepatitisB ->
            "hepatitis-b"

        TestHIV ->
            "hiv"

        TestPartnerHIV ->
            "partner-hiv"

        TestHIVPCR ->
            "hiv-pcr"

        TestMalaria ->
            "malaria"

        TestRandomBloodSugar ->
            "random-blood-sugar"

        TestSyphilis ->
            "syphilis"

        TestUrineDipstick ->
            "urine-dipstick"

        TestVitalsRecheck ->
            "vitals-recheck"

        TestCreatinine ->
            "creatinine"

        TestLiverFunction ->
            "liver-function"

        TestLipidPanel ->
            "lipid-panel"


laboratoryTestFromString : String -> Maybe LaboratoryTest
laboratoryTestFromString value =
    case value of
        "blood-group" ->
            Just TestBloodGpRs

        "hemoglobin" ->
            Just TestHemoglobin

        "hepatitis-b" ->
            Just TestHepatitisB

        "hiv" ->
            Just TestHIV

        "partner-hiv" ->
            Just TestPartnerHIV

        "hiv-pcr" ->
            Just TestHIVPCR

        "malaria" ->
            Just TestMalaria

        "random-blood-sugar" ->
            Just TestRandomBloodSugar

        "syphilis" ->
            Just TestSyphilis

        "urine-dipstick" ->
            Just TestUrineDipstick

        "vitals-recheck" ->
            Just TestVitalsRecheck

        "creatinine" ->
            Just TestCreatinine

        "liver-function" ->
            Just TestLiverFunction

        "lipid-panel" ->
            Just TestLipidPanel

        _ ->
            Nothing


reviewStateToString : LabsResultsReviewState -> String
reviewStateToString state =
    case state of
        LabsResultsReviewRequested ->
            "requested"

        LabsResultsReviewCompleted ->
            "completed"


reviewStateFromString : String -> Maybe LabsResultsReviewState
reviewStateFromString state =
    case state of
        "requested" ->
            Just LabsResultsReviewRequested

        "completed" ->
            Just LabsResultsReviewCompleted

        _ ->
            Nothing


{-| Referal to facility is completed when we mark that facility was referred to,
or, reason was set for not referring to that facility.
-}
referralToFacilityCompleted : EverySet ReferToFacilitySign -> Maybe (EverySet NonReferralSign) -> ReferralFacility -> Bool
referralToFacilityCompleted referralSigns nonReferralReasons facility =
    let
        referralConfig =
            case facility of
                FacilityHospital ->
                    Just ( ReferToHospital, NonReferralReasonHospital )

                FacilityMentalHealthSpecialist ->
                    Just ( ReferToMentalHealthSpecialist, NonReferralReasonMentalHealthSpecialist )

                FacilityARVProgram ->
                    Just ( ReferToARVProgram, NonReferralReasonARVProgram )

                FacilityNCDProgram ->
                    Just ( ReferToNCDProgram, NonReferralReasonNCDProgram )

                FacilityANCServices ->
                    Just ( ReferToANCServices, NonReferralReasonANCServices )

                FacilityUltrasound ->
                    Just ( ReferToUltrasound, NonReferralReasonUltrasound )

                FacilityHealthCenter ->
                    -- We should never get here, as referral to HC
                    -- got special treatement, and not supported here.
                    Nothing
    in
    Maybe.map
        (\( referralSign, nonReferralSign ) ->
            let
                facilityWasReferred =
                    EverySet.member referralSign referralSigns

                facilityNonReferralReasonSet =
                    isJust <| getCurrentReasonForNonReferral nonReferralSign nonReferralReasons
            in
            facilityWasReferred || facilityNonReferralReasonSet
        )
        referralConfig
        |> Maybe.withDefault False


getCurrentReasonForNonReferral :
    (ReasonForNonReferral -> NonReferralSign)
    -> Maybe (EverySet NonReferralSign)
    -> Maybe ReasonForNonReferral
getCurrentReasonForNonReferral reasonToSignFunc nonReferralReasons =
    let
        facilityNonReferralReasons =
            Maybe.withDefault EverySet.empty nonReferralReasons
    in
    List.filterMap
        (\reason ->
            if EverySet.member (reasonToSignFunc reason) facilityNonReferralReasons then
                Just reason

            else
                Nothing
        )
        [ ClientRefused
        , NoAmbulance
        , ClientUnableToAffordFees
        , ClientAlreadyInCare
        , ReasonForNonReferralNotIndicated
        , ReasonForNonReferralOther
        ]
        |> List.head


nonReferralReasonToSign : ReferralFacility -> ReasonForNonReferral -> NonReferralSign
nonReferralReasonToSign facility reason =
    case facility of
        FacilityHospital ->
            NonReferralReasonHospital reason

        FacilityMentalHealthSpecialist ->
            NonReferralReasonMentalHealthSpecialist reason

        FacilityARVProgram ->
            NonReferralReasonARVProgram reason

        FacilityNCDProgram ->
            NonReferralReasonNCDProgram reason

        FacilityANCServices ->
            NonReferralReasonANCServices reason

        FacilityUltrasound ->
            NonReferralReasonUltrasound reason

        FacilityHealthCenter ->
            -- We should never get here, as referral to HC
            -- got special treatement, and not supported here.
            NoNonReferralSigns


{-| Recommended Treatment activity appears on both initial and recurrent encounters.
Each one of them got unique set of signs that can be used, and at least one of
them must be set.
In order to know if activity was completed or not, we check if at least one
of those signs was set.
-}
recommendedTreatmentMeasurementTaken : List RecommendedTreatmentSign -> EverySet RecommendedTreatmentSign -> Bool
recommendedTreatmentMeasurementTaken allowedSigns signs =
    List.any (\sign -> EverySet.member sign signs) allowedSigns


diabetesBySugarCount : RandomBloodSugarTestValue encounterId -> Bool
diabetesBySugarCount value =
    Maybe.map2
        (\testPrerequisites sugarCount ->
            if EverySet.member PrerequisiteFastFor12h testPrerequisites then
                sugarCount > 126

            else
                sugarCount >= 200
        )
        value.testPrerequisites
        value.sugarCount
        |> Maybe.withDefault False


diabetesByUrineGlucose : UrineDipstickTestValue -> Bool
diabetesByUrineGlucose value =
    Maybe.map (\glucose -> List.member glucose [ GlucosePlus2, GlucosePlus3, GlucosePlus4 ]) value.glucose
        |> Maybe.withDefault False


unitOfMeasurementToString : UnitOfMeasurement -> String
unitOfMeasurementToString value =
    case value of
        UnitMmolL ->
            "mmol-L"

        UnitMgdL ->
            "mg-dL"


unitOfMeasurementFromString : String -> Maybe UnitOfMeasurement
unitOfMeasurementFromString value =
    case value of
        "mmol-L" ->
            Just UnitMmolL

        "mg-dL" ->
            Just UnitMgdL

        _ ->
            Nothing


ncdaSignFromString : String -> Maybe NCDASign
ncdaSignFromString value =
    case value of
        "appropriate-complementary-feeding" ->
            Just AppropriateComplementaryFeeding

        "born-with-birth-defect" ->
            Just BornWithBirthDefect

        "breastfed-for-six-months" ->
            Just BreastfedForSixMonths

        "conditional-food-items" ->
            Just ConditionalFoodItems

        "five-food-groups" ->
            Just FiveFoodGroups

        "has-clean-water" ->
            Just HasCleanWater

        "has-handwashing-facility" ->
            Just HasHandwashingFacility

        "has-toilets" ->
            Just HasToilets

        "has-kitchen-garden" ->
            Just HasKitchenGarden

        "beneficiary-cash-transfer" ->
            Just BeneficiaryCashTransfer

        "child-got-diarrhea" ->
            Just ChildGotDiarrhea

        "child-receives-fbf" ->
            Just ChildReceivesFBF

        "child-taking-fbf" ->
            Just ChildTakingFBF

        -- Sign not set on backend anymore.
        "child-receives-vitamin-a" ->
            Just ChildReceivesVitaminA

        "child-receives-dewormer" ->
            Just ChildReceivesDewormer

        "child-receives-ecd" ->
            Just ChildReceivesECD

        "child-with-disability" ->
            Just ChildWithDisability

        "ongera-mnp" ->
            Just OngeraMNP

        "insecticide-treated-bednets" ->
            Just InsecticideTreatedBednets

        "meals-at-recommended-times" ->
            Just MealsAtRecommendedTimes

        "child-behind-on-vaccination" ->
            Just ChildBehindOnVaccination

        "receiving-cash-transfer" ->
            Just ReceivingCashTransfer

        "receiving-support" ->
            Just ReceivingSupport

        "supplements-during-pregnancy" ->
            Just SupplementsDuringPregnancy

        "taken-supplements-per-guidance" ->
            Just TakenSupplementsPerGuidance

        "taking-ongera-mnp" ->
            Just TakingOngeraMNP

        "treated-for-acute-malnutrition" ->
            Just TreatedForAcuteMalnutrition

        "none" ->
            Just NoNCDASigns

        "shows-edema-signs" ->
            Just ShowsEdemaSigns

        _ ->
            Nothing


ncdaSignToString : NCDASign -> String
ncdaSignToString value =
    case value of
        AppropriateComplementaryFeeding ->
            "appropriate-complementary-feeding"

        BornWithBirthDefect ->
            "born-with-birth-defect"

        BreastfedForSixMonths ->
            "breastfed-for-six-months"

        ConditionalFoodItems ->
            "conditional-food-items"

        FiveFoodGroups ->
            "five-food-groups"

        HasCleanWater ->
            "has-clean-water"

        HasHandwashingFacility ->
            "has-handwashing-facility"

        HasToilets ->
            "has-toilets"

        HasKitchenGarden ->
            "has-kitchen-garden"

        BeneficiaryCashTransfer ->
            "beneficiary-cash-transfer"

        ChildGotDiarrhea ->
            "child-got-diarrhea"

        ChildReceivesFBF ->
            "child-receives-fbf"

        ChildTakingFBF ->
            "child-taking-fbf"

        -- Sign not set on backend anymore.
        ChildReceivesVitaminA ->
            "child-receives-vitamin-a"

        ChildReceivesDewormer ->
            "child-receives-dewormer"

        ChildReceivesECD ->
            "child-receives-ecd"

        ChildWithDisability ->
            "child-with-disability"

        OngeraMNP ->
            "ongera-mnp"

        InsecticideTreatedBednets ->
            "insecticide-treated-bednets"

        MealsAtRecommendedTimes ->
            "meals-at-recommended-times"

        ChildBehindOnVaccination ->
            "child-behind-on-vaccination"

        ReceivingCashTransfer ->
            "receiving-cash-transfer"

        ReceivingSupport ->
            "receiving-support"

        SupplementsDuringPregnancy ->
            "supplements-during-pregnancy"

        TakenSupplementsPerGuidance ->
            "taken-supplements-per-guidance"

        TakingOngeraMNP ->
            "taking-ongera-mnp"

        TreatedForAcuteMalnutrition ->
            "treated-for-acute-malnutrition"

        ShowsEdemaSigns ->
            "shows-edema-signs"

        NoNCDASigns ->
            "none"


receiveOptionFromString : String -> Maybe ReceiveOption
receiveOptionFromString value =
    case value of
        "receive" ->
            Just OptionReceive

        "not-receive" ->
            Just OptionNotReceive

        "not-applicable" ->
            Just OptionNotApplicable

        _ ->
            Nothing


receiveOptionToString : ReceiveOption -> String
receiveOptionToString value =
    case value of
        OptionReceive ->
            "receive"

        OptionNotReceive ->
            "not-receive"

        OptionNotApplicable ->
            "not-applicable"


stuntingLevelFromString : String -> Maybe StuntingLevel
stuntingLevelFromString value =
    case value of
        "green" ->
            Just LevelGreen

        "yellow" ->
            Just LevelYellow

        "red" ->
            Just LevelRed

        _ ->
            Nothing


stuntingLevelToString : StuntingLevel -> String
stuntingLevelToString value =
    case value of
        LevelGreen ->
            "green"

        LevelYellow ->
            "yellow"

        LevelRed ->
            "red"


expectNCDAActivity : NominalDate -> EverySet SiteFeature -> Bool -> Person -> Bool
expectNCDAActivity currentDate features isChw person =
    -- NCDA feature enabled.
    ncdaEnabled features
        && -- Show only for nurses.
           not isChw
        && -- Show for children that are younger than 2 years old.
           (ageInMonths currentDate person
                |> Maybe.map (\ageMonths -> ageMonths < 24)
                |> Maybe.withDefault False
           )


lmpDateNotConfidentReasonToString : LmpDateNotConfidentReason -> String
lmpDateNotConfidentReasonToString value =
    case value of
        ReasonIrregularMenses ->
            "irregular-menses"

        ReasonOnFamilyPlanningMethod ->
            "on-family-planning-method"

        ReasonCanNotRememberDates ->
            "can-not-remember-dates"


lmpDateNotConfidentReasonFromString : String -> Maybe LmpDateNotConfidentReason
lmpDateNotConfidentReasonFromString value =
    case value of
        "irregular-menses" ->
            Just ReasonIrregularMenses

        "on-family-planning-method" ->
            Just ReasonOnFamilyPlanningMethod

        "can-not-remember-dates" ->
            Just ReasonCanNotRememberDates

        _ ->
            Nothing


bloodSmearResultToString : BloodSmearResult -> String
bloodSmearResultToString value =
    case value of
        BloodSmearNegative ->
            "negative"

        BloodSmearPlus ->
            "+"

        BloodSmearPlusPlus ->
            "++"

        BloodSmearPlusPlusPlus ->
            "+++"

        BloodSmearNotTaken ->
            "not-taken"

        BloodSmearPendingInput ->
            "pending-input"


bloodSmearResultFromString : String -> Maybe BloodSmearResult
bloodSmearResultFromString value =
    case value of
        "negative" ->
            Just BloodSmearNegative

        "+" ->
            Just BloodSmearPlus

        "++" ->
            Just BloodSmearPlusPlus

        "+++" ->
            Just BloodSmearPlusPlusPlus

        "not-taken" ->
            Just BloodSmearNotTaken

        "pending-input" ->
            Just BloodSmearPendingInput

        _ ->
            Nothing


tuberculosisDiagnosisToString : TuberculosisDiagnosis -> String
tuberculosisDiagnosisToString diagnosis =
    case diagnosis of
        TuberculosisPulmonary ->
            "pulmonary"

        TuberculosisExtrapulmonary ->
            "extrapulmonary"

        NoTuberculosis ->
            "none"


tuberculosisDiagnosisFromString : String -> Maybe TuberculosisDiagnosis
tuberculosisDiagnosisFromString diagnosis =
    case diagnosis of
        "pulmonary" ->
            Just TuberculosisPulmonary

        "extrapulmonary" ->
            Just TuberculosisExtrapulmonary

        "none" ->
            Just NoTuberculosis

        _ ->
            Nothing


tuberculosisSymptomToString : TuberculosisSymptom -> String
tuberculosisSymptomToString symptom =
    case symptom of
        TuberculosisSymptomNightSweats ->
            "night-sweats"

        TuberculosisSymptomBloodInSputum ->
            "blood-in-sputum"

        TuberculosisSymptomWeightLoss ->
            "wight-loss"

        TuberculosisSymptomSevereFatigue ->
            "severe-fatigue"

        NoTuberculosisSymptoms ->
            "none"


tuberculosisSymptomFromString : String -> Maybe TuberculosisSymptom
tuberculosisSymptomFromString symptom =
    case symptom of
        "night-sweats" ->
            Just TuberculosisSymptomNightSweats

        "blood-in-sputum" ->
            Just TuberculosisSymptomBloodInSputum

        "wight-loss" ->
            Just TuberculosisSymptomWeightLoss

        "severe-fatigue" ->
            Just TuberculosisSymptomSevereFatigue

        "none" ->
            Just NoTuberculosisSymptoms

        _ ->
            Nothing


tuberculosisHealthEducationSignToString : TuberculosisHealthEducationSign -> String
tuberculosisHealthEducationSignToString sign =
    case sign of
        EducationFollowUpTesting ->
            "followup-testing"

        NoTuberculosisHealthEducationSigns ->
            "none"


tuberculosisHealthEducationSignFromString : String -> Maybe TuberculosisHealthEducationSign
tuberculosisHealthEducationSignFromString sign =
    case sign of
        "followup-testing" ->
            Just EducationFollowUpTesting

        "none" ->
            Just NoTuberculosisHealthEducationSigns

        _ ->
            Nothing


tuberculosisDOTSignToString : TuberculosisDOTSign -> String
tuberculosisDOTSignToString sign =
    case sign of
        DOTPositive ->
            "positive"

        DOTNegativeTakenToday ->
            "negative-taken-today"

        DOTNegativeUnavailable ->
            "negative-unavailable"

        DOTNegativeSideEffects ->
            "negative-side-effects"

        DOTNegativePatientRefused ->
            "negative-patient-refused"

        DOTNegativeNotIndicated ->
            "negative-not-indicated"


tuberculosisDOTSignFromString : String -> Maybe TuberculosisDOTSign
tuberculosisDOTSignFromString sign =
    case sign of
        "positive" ->
            Just DOTPositive

        "negative-taken-today" ->
            Just DOTNegativeTakenToday

        "negative-unavailable" ->
            Just DOTNegativeUnavailable

        "negative-side-effects" ->
            Just DOTNegativeSideEffects

        "negative-patient-refused" ->
            Just DOTNegativePatientRefused

        "negative-not-indicated" ->
            Just DOTNegativeNotIndicated

        _ ->
            Nothing


tuberculosisPrescribedMedicationToString : TuberculosisPrescribedMedication -> String
tuberculosisPrescribedMedicationToString sign =
    case sign of
        MedicationRHZE ->
            "rhze"

        MedicationRH ->
            "rh"

        MedicationOther ->
            "other"

        TuberculosisMedicationsNotChanged ->
            "not-changed"

        NoTuberculosisPrescribedMedications ->
            "none"


tuberculosisPrescribedMedicationFromString : String -> Maybe TuberculosisPrescribedMedication
tuberculosisPrescribedMedicationFromString sign =
    case sign of
        "rhze" ->
            Just MedicationRHZE

        "rh" ->
            Just MedicationRH

        "other" ->
            Just MedicationOther

        "not-changed" ->
            Just TuberculosisMedicationsNotChanged

        "none" ->
            Just NoTuberculosisPrescribedMedications

        _ ->
            Nothing


hivDiagnosisSignToString : HIVDiagnosisSign -> String
hivDiagnosisSignToString diagnosis =
    case diagnosis of
        HIVResultPositiveReported ->
            "result-positive-reported"

        HIVResultPositiveKnown ->
            "result-positive-known"

        HIVResultDateEstimated ->
            "result-date-estimated"

        HIVTestRun ->
            "test-run"

        NoHIVDiagnosisSigns ->
            "none"


hivDiagnosisSignFromString : String -> Maybe HIVDiagnosisSign
hivDiagnosisSignFromString diagnosis =
    case diagnosis of
        "result-positive-reported" ->
            Just HIVResultPositiveReported

        "result-positive-known" ->
            Just HIVResultPositiveKnown

        "result-date-estimated" ->
            Just HIVResultDateEstimated

        "test-run" ->
            Just HIVTestRun

        "none" ->
            Just NoHIVDiagnosisSigns

        _ ->
            Nothing


hivPrescribedMedicationToString : HIVPrescribedMedication -> String
hivPrescribedMedicationToString sign =
    case sign of
        HIVMedicationDolutegravirLamivudineTenofovir ->
            "dtg-3tc-tdf"

        HIVMedicationAtazanavirRitonavir ->
            "atz-r"

        HIVMedicationDolutegravir ->
            "dtg"

        HIVMedicationAbacavirLamivudine ->
            "abc-3tc"

        HIVMedicationLamivudineTenofovir ->
            "3tc-tdf"

        HIVMedicationZidovudine ->
            "azt"

        HIVMedicationLamivudineZidovudineNevirapine ->
            "3tc-azt-nvp"

        HIVMedicationEfavirenzLamivudineTenofovir ->
            "efv-3tc-tdf"

        HIVMedicationLamivudineZidovudine ->
            "3tc-azt"

        HIVMedicationLopinavirRitonavir ->
            "lvp-r"

        HIVMedicationDarunavirRitonavir ->
            "drv-r"

        HIVMedicationDarunavirCobicistat ->
            "drv-c"

        HIVMedicationRaltegravir ->
            "ral"

        HIVMedicationEfavirenz ->
            "efv"

        HIVMedicationNevirapine ->
            "nvp"

        HIVMedicationEtravirine ->
            "etr"

        HIVMedicationTenofovir ->
            "tdf"

        HIVMedicationLamivudine ->
            "3tc"

        HIVMedicationAbacavir ->
            "abc"

        HIVMedicationBactrim ->
            "bactrim"

        HIVMedicationTrimethoprimSulfamethoxazole ->
            "trimethoprim-sulfamethoxazole"

        HIVMedicationCoTrimoxazoleTablets ->
            "co-trimoxazole-tablets"

        HIVMedicationCoTrimoxazoleOralSuspension ->
            "co-trimoxazole-oral-suspension"

        HIVMedicationsNotChanged ->
            "not-changed"

        NoHIVPrescribedMedications ->
            "none"


hivPrescribedMedicationFromString : String -> Maybe HIVPrescribedMedication
hivPrescribedMedicationFromString sign =
    case sign of
        "dtg-3tc-tdf" ->
            Just HIVMedicationDolutegravirLamivudineTenofovir

        "atz-r" ->
            Just HIVMedicationAtazanavirRitonavir

        "dtg" ->
            Just HIVMedicationDolutegravir

        "abc-3tc" ->
            Just HIVMedicationAbacavirLamivudine

        "3tc-tdf" ->
            Just HIVMedicationLamivudineTenofovir

        "azt" ->
            Just HIVMedicationZidovudine

        "3tc-azt-nvp" ->
            Just HIVMedicationLamivudineZidovudineNevirapine

        "efv-3tc-tdf" ->
            Just HIVMedicationEfavirenzLamivudineTenofovir

        "3tc-azt" ->
            Just HIVMedicationLamivudineZidovudine

        "lvp-r" ->
            Just HIVMedicationLopinavirRitonavir

        "drv-r" ->
            Just HIVMedicationDarunavirRitonavir

        "drv-c" ->
            Just HIVMedicationDarunavirCobicistat

        "ral" ->
            Just HIVMedicationRaltegravir

        "efv" ->
            Just HIVMedicationEfavirenz

        "nvp" ->
            Just HIVMedicationNevirapine

        "etr" ->
            Just HIVMedicationEtravirine

        "tdf" ->
            Just HIVMedicationTenofovir

        "3tc" ->
            Just HIVMedicationLamivudine

        "abc" ->
            Just HIVMedicationAbacavir

        "bactrim" ->
            Just HIVMedicationBactrim

        "trimethoprim-sulfamethoxazole" ->
            Just HIVMedicationTrimethoprimSulfamethoxazole

        "co-trimoxazole-tablets" ->
            Just HIVMedicationCoTrimoxazoleTablets

        "co-trimoxazole-oral-suspension" ->
            Just HIVMedicationCoTrimoxazoleOralSuspension

        "not-changed" ->
            Just HIVMedicationsNotChanged

        "none" ->
            Just NoHIVPrescribedMedications

        _ ->
            Nothing


hivSymptomToString : HIVSymptom -> String
hivSymptomToString symptom =
    case symptom of
        HIVSymptomFever ->
            "fever"

        HIVSymptomFatigue ->
            "fatigue"

        HIVSymptomSwollenLymphNodes ->
            "swollen-lymph-nodes"

        HIVSymptomSoreThroat ->
            "sore-throat"

        HIVSymptomRash ->
            "rash"

        HIVSymptomMuscleJointPain ->
            "muscle-joint-pain"

        HIVSymptomHeadache ->
            "headache"

        HIVSymptomSevereAbdominalPain ->
            "severe-abdominal-pain"

        HIVSymptomNightSweats ->
            "night-sweats"

        HIVSymptomDiarrhea ->
            "diarrhea"

        HIVSymptomWeightLoss ->
            "wight-loss"

        HIVSymptomCoughingUpBlood ->
            "coughing-up-blood"

        HIVSymptomHairLoss ->
            "hair-loss"

        HIVSymptomMouthUlcers ->
            "mouth-ulcers"

        HIVSymptomDifficultyBreathing ->
            "difficulty-breathing"

        HIVSymptomVomiting ->
            "vomiting"

        NoHIVSymptoms ->
            "none"


hivSymptomFromString : String -> Maybe HIVSymptom
hivSymptomFromString symptom =
    case symptom of
        "fever" ->
            Just HIVSymptomFever

        "fatigue" ->
            Just HIVSymptomFatigue

        "swollen-lymph-nodes" ->
            Just HIVSymptomSwollenLymphNodes

        "sore-throat" ->
            Just HIVSymptomSoreThroat

        "rash" ->
            Just HIVSymptomRash

        "muscle-joint-pain" ->
            Just HIVSymptomMuscleJointPain

        "headache" ->
            Just HIVSymptomHeadache

        "severe-abdominal-pain" ->
            Just HIVSymptomSevereAbdominalPain

        "night-sweats" ->
            Just HIVSymptomNightSweats

        "diarrhea" ->
            Just HIVSymptomDiarrhea

        "wight-loss" ->
            Just HIVSymptomWeightLoss

        "coughing-up-blood" ->
            Just HIVSymptomCoughingUpBlood

        "hair-loss" ->
            Just HIVSymptomHairLoss

        "mouth-ulcers" ->
            Just HIVSymptomMouthUlcers

        "difficulty-breathing" ->
            Just HIVSymptomDifficultyBreathing

        "vomiting" ->
            Just HIVSymptomVomiting

        "none" ->
            Just NoHIVSymptoms

        _ ->
            Nothing


hivHealthEducationSignToString : HIVHealthEducationSign -> String
hivHealthEducationSignToString diagnosis =
    case diagnosis of
        EducationPositiveResult ->
            "positive-result"

        EducationSaferSexPractices ->
            "safer-sex-practices"

        EducationEncouragedPartnerTesting ->
            "encouraged-partner-testing"

        EducationFamilyPlanningOptions ->
            "family-planning-options"

        NoHIVHealthEducationSigns ->
            "none"


hivHealthEducationSignFromString : String -> Maybe HIVHealthEducationSign
hivHealthEducationSignFromString diagnosis =
    case diagnosis of
        "positive-result" ->
            Just EducationPositiveResult

        "safer-sex-practices" ->
            Just EducationSaferSexPractices

        "encouraged-partner-testing" ->
            Just EducationEncouragedPartnerTesting

        "family-planning-options" ->
            Just EducationFamilyPlanningOptions

        "none" ->
            Just NoHIVHealthEducationSigns

        _ ->
            Nothing
