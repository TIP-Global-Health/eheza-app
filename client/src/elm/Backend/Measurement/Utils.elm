module Backend.Measurement.Utils exposing (..)

import AssocList as Dict exposing (Dict)
import Backend.Entities exposing (..)
import Backend.Measurement.Model exposing (..)
import Backend.Person.Model exposing (Person, Ubudehe(..))
import Backend.Person.Utils exposing (isAdult)
import Backend.Session.Model exposing (OfflineSession)
import Date
import EverySet exposing (EverySet)
import Gizra.NominalDate exposing (NominalDate, diffMonths)
import LocalData
import Measurement.Model exposing (..)
import Restful.Endpoint exposing (EntityUuid)


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


{-| Like `currentValue`, but also supplies the ID if we have one
(i.e. if we're editing a value saved on the backend).
-}
currentValueWithId : MeasurementData (Maybe ( id, value )) -> Maybe ( Maybe id, value )
currentValueWithId data =
    currentValue data
        |> Maybe.map (\value -> ( Maybe.map Tuple.first data.current, value ))


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


prenatalTestResultToString : PrenatalTestResult -> String
prenatalTestResultToString value =
    case value of
        PrenatalTestPositive ->
            "positive"

        PrenatalTestNegative ->
            "negative"

        PrenatalTestIndeterminate ->
            "indeterminate"


prenatalTestResultFromString : String -> Maybe PrenatalTestResult
prenatalTestResultFromString value =
    case value of
        "positive" ->
            Just PrenatalTestPositive

        "negative" ->
            Just PrenatalTestNegative

        "indeterminate" ->
            Just PrenatalTestIndeterminate

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
prenatalLabExpirationPeriod : Int
prenatalLabExpirationPeriod =
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


prenatalOutsideCareSignToString : PrenatalOutsideCareSign -> String
prenatalOutsideCareSignToString value =
    case value of
        SeenAtAnotherFacility ->
            "seen-at-another-facility"

        GivenNewDiagnoses ->
            "given-new-diagnoses"

        GivenMedicine ->
            "given-medicine"

        PlannedFollowUpCareWithSpecialist ->
            "follow-up-with-specialist"

        NoPrenatalOutsideCareSigns ->
            "none"


prenatalOutsideCareSignFromString : String -> Maybe PrenatalOutsideCareSign
prenatalOutsideCareSignFromString value =
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
            Just NoPrenatalOutsideCareSigns

        _ ->
            Nothing


prenatalOutsideCareMedicationToString : PrenatalOutsideCareMedication -> String
prenatalOutsideCareMedicationToString value =
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

        NoPrenatalOutsideCareMedications ->
            "none"


prenatalOutsideCareMedicationFromString : String -> Maybe PrenatalOutsideCareMedication
prenatalOutsideCareMedicationFromString value =
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
            Just NoPrenatalOutsideCareMedications

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


socialHistoryHivTestingResultFromString : String -> Maybe SocialHistoryHivTestingResult
socialHistoryHivTestingResultFromString result =
    case result of
        "positive" ->
            Just ResultHivPositive

        "negative" ->
            Just ResultHivNegative

        "indeterminate" ->
            Just ResultHivIndeterminate

        "none" ->
            Just NoHivTesting

        _ ->
            Nothing


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


socialHistoryHivTestingResultToString : SocialHistoryHivTestingResult -> String
socialHistoryHivTestingResultToString result =
    case result of
        ResultHivPositive ->
            "positive"

        ResultHivNegative ->
            "negative"

        ResultHivIndeterminate ->
            "indeterminate"

        NoHivTesting ->
            "none"


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
