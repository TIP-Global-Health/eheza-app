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
