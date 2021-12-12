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


heightValueFunc : HeightInCm -> Float
heightValueFunc =
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


vaccineTypeFromString : String -> Maybe VaccineType
vaccineTypeFromString type_ =
    case type_ of
        "bcg" ->
            Just VaccineBCG

        "opv" ->
            Just VaccineOPV

        "dtp" ->
            Just VaccineDTP

        "pcv13" ->
            Just VaccinePCV13

        "rotarix" ->
            Just VaccineRotarix

        "ipv" ->
            Just VaccineIPV

        "mr" ->
            Just VaccineMR

        "hpv" ->
            Just VaccineHPV

        _ ->
            Nothing


vaccineTypeToString : VaccineType -> String
vaccineTypeToString type_ =
    case type_ of
        VaccineBCG ->
            "bcg"

        VaccineOPV ->
            "opv"

        VaccineDTP ->
            "dtp"

        VaccinePCV13 ->
            "pcv13"

        VaccineRotarix ->
            "rotarix"

        VaccineIPV ->
            "ipv"

        VaccineMR ->
            "mr"

        VaccineHPV ->
            "hpv"


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
        ProteinNegative ->
            "negative"

        Protein30 ->
            "30"

        Protein100 ->
            "100"

        Protein300 ->
            "300"

        Protein2000 ->
            "2000"


proteinValueFromString : String -> Maybe ProteinValue
proteinValueFromString value =
    case value of
        "negative" ->
            Just ProteinNegative

        "30" ->
            Just Protein30

        "100" ->
            Just Protein100

        "300" ->
            Just Protein300

        "2000" ->
            Just Protein2000

        _ ->
            Nothing


phValueToString : PHValue -> String
phValueToString value =
    case value of
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

        LeukocytesNotApplicable ->
            "n-a"


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

        "n-a" ->
            Just LeukocytesNotApplicable

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

        NitriteNotApplicable ->
            "n-a"


nitriteValueFromString : String -> Maybe NitriteValue
nitriteValueFromString value =
    case value of
        "negative" ->
            Just NitriteNegative

        "+" ->
            Just NitritePlus

        "++" ->
            Just NitritePlusPlus

        "n-a" ->
            Just NitriteNotApplicable

        _ ->
            Nothing


urobilinogenValueToString : UrobilinogenValue -> String
urobilinogenValueToString value =
    case value of
        Urobilinogen02 ->
            "0.2"

        Urobilinogen10 ->
            "1"

        Urobilinogen20 ->
            "2"

        Urobilinogen40 ->
            "4"

        Urobilinogen80 ->
            "8"

        UrobilinogenNotApplicable ->
            "n-a"


urobilinogenValueFromString : String -> Maybe UrobilinogenValue
urobilinogenValueFromString value =
    case value of
        "0.2" ->
            Just Urobilinogen02

        "1" ->
            Just Urobilinogen10

        "2" ->
            Just Urobilinogen20

        "4" ->
            Just Urobilinogen40

        "8" ->
            Just Urobilinogen80

        "n-a" ->
            Just UrobilinogenNotApplicable

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

        HaemoglobinNotApplicable ->
            "n-a"


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

        "n-a" ->
            Just HaemoglobinNotApplicable

        _ ->
            Nothing


specificGravityValueToString : SpecificGravityValue -> String
specificGravityValueToString value =
    case value of
        SpecificGravity1000 ->
            "1.000"

        SpecificGravity1005 ->
            "1.005"

        SpecificGravity1010 ->
            "1.010"

        SpecificGravity1015 ->
            "1.015"

        SpecificGravity1020 ->
            "1.020"

        SpecificGravity1025 ->
            "1.025"

        SpecificGravity1030 ->
            "1.030"

        SpecificGravityNotApplicable ->
            "n-a"


specificGravityValueFromString : String -> Maybe SpecificGravityValue
specificGravityValueFromString value =
    case value of
        "1.000" ->
            Just SpecificGravity1000

        "1.005" ->
            Just SpecificGravity1005

        "1.010" ->
            Just SpecificGravity1010

        "1.015" ->
            Just SpecificGravity1015

        "1.020" ->
            Just SpecificGravity1020

        "1.025" ->
            Just SpecificGravity1025

        "1.030" ->
            Just SpecificGravity1030

        "n-a" ->
            Just SpecificGravityNotApplicable

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

        KetoneNotApplicable ->
            "n-a"


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

        "n-a" ->
            Just KetoneNotApplicable

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

        BilirubinotApplicable ->
            "n-a"


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

        "n-a" ->
            Just BilirubinotApplicable

        _ ->
            Nothing


{-| If lab results are not provided within 14 days, we consider the expired,
and do not provide option of filling the results.
-}
prenatalLabExpirationPeriod : Int
prenatalLabExpirationPeriod =
    14
