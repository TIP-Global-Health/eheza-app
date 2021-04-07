module Pages.HomeVisitActivity.Utils exposing (..)

import AssocList as Dict exposing (Dict)
import Backend.Entities exposing (HomeVisitEncounterId)
import Backend.HomeVisitActivity.Model exposing (HomeVisitActivity(..))
import Backend.Measurement.Model exposing (..)
import Backend.Model exposing (ModelIndexedDb)
import Backend.Person.Model exposing (Person)
import EverySet exposing (EverySet)
import Gizra.NominalDate exposing (NominalDate)
import List.Extra
import Maybe.Extra exposing (andMap, isJust, isNothing, or, unwrap)
import Measurement.Model exposing (..)
import Pages.HomeVisitActivity.Model exposing (..)
import Pages.HomeVisitEncounter.Model exposing (AssembledData)
import Pages.Utils exposing (ifEverySetEmpty, ifNullableTrue, taskCompleted)
import RemoteData exposing (RemoteData(..))


expectActivity : NominalDate -> Person -> AssembledData -> ModelIndexedDb -> HomeVisitActivity -> Bool
expectActivity currentDate child data db activity =
    -- For now, we show all activities without any conditions.
    case activity of
        _ ->
            True


activityCompleted : NominalDate -> Person -> AssembledData -> ModelIndexedDb -> HomeVisitActivity -> Bool
activityCompleted currentDate child data db activity =
    let
        measurements =
            data.measurements
    in
    case activity of
        Feeding ->
            (not <| expectActivity currentDate child data db Feeding)
                || isJust measurements.feeding

        Caring ->
            -- @todo
            True

        Hygiene ->
            (not <| expectActivity currentDate child data db Hygiene)
                || isJust measurements.hygiene

        FoodSecurity ->
            (not <| expectActivity currentDate child data db FoodSecurity)
                || isJust measurements.foodSecurity


fromNutritionFeedingValue : Maybe NutritionFeedingValue -> NutritionFeedingForm
fromNutritionFeedingValue saved =
    { receiveSupplement = Maybe.map (.signs >> EverySet.member ReceiveSupplement) saved
    , rationPresentAtHome = Maybe.map (.signs >> EverySet.member RationPresentAtHome) saved
    , enoughTillNextSession = Maybe.map (.signs >> EverySet.member EnoughTillNextSession) saved
    , supplementShared = Maybe.map (.signs >> EverySet.member SupplementShared) saved
    , encouragedToEat = Maybe.map (.signs >> EverySet.member EncouragedToEat) saved
    , refusingToEat = Maybe.map (.signs >> EverySet.member RefusingToEat) saved
    , breastfeeding = Maybe.map (.signs >> EverySet.member FeedingSignBreastfeeding) saved
    , cleanWaterAvailable = Maybe.map (.signs >> EverySet.member CleanWaterAvailable) saved
    , eatenWithWater = Maybe.map (.signs >> EverySet.member EatenWithWater) saved
    , supplementType = Maybe.map .supplementType saved
    , sachetsPerDay = Maybe.map .sachetsPerDay saved
    }


nutritionFeedingFormWithDefault : NutritionFeedingForm -> Maybe NutritionFeedingValue -> NutritionFeedingForm
nutritionFeedingFormWithDefault form saved =
    saved
        |> unwrap
            form
            (\value ->
                { receiveSupplement = or form.receiveSupplement (EverySet.member ReceiveSupplement value.signs |> Just)
                , rationPresentAtHome = or form.rationPresentAtHome (EverySet.member RationPresentAtHome value.signs |> Just)
                , enoughTillNextSession = or form.enoughTillNextSession (EverySet.member EnoughTillNextSession value.signs |> Just)
                , supplementShared = or form.supplementShared (EverySet.member SupplementShared value.signs |> Just)
                , encouragedToEat = or form.encouragedToEat (EverySet.member EncouragedToEat value.signs |> Just)
                , refusingToEat = or form.refusingToEat (EverySet.member RefusingToEat value.signs |> Just)
                , breastfeeding = or form.breastfeeding (EverySet.member FeedingSignBreastfeeding value.signs |> Just)
                , cleanWaterAvailable = or form.cleanWaterAvailable (EverySet.member CleanWaterAvailable value.signs |> Just)
                , eatenWithWater = or form.eatenWithWater (EverySet.member EatenWithWater value.signs |> Just)
                , supplementType = or form.supplementType (Just value.supplementType)
                , sachetsPerDay = or form.sachetsPerDay (Just value.sachetsPerDay)
                }
            )


toNutritionFeedingValueWithDefault : Maybe NutritionFeedingValue -> NutritionFeedingForm -> Maybe NutritionFeedingValue
toNutritionFeedingValueWithDefault saved form =
    nutritionFeedingFormWithDefault form saved
        |> toNutritionFeedingValue


toNutritionFeedingValue : NutritionFeedingForm -> Maybe NutritionFeedingValue
toNutritionFeedingValue form =
    let
        signs =
            [ ifNullableTrue ReceiveSupplement form.receiveSupplement
            , ifNullableTrue RationPresentAtHome form.rationPresentAtHome
            , ifNullableTrue EnoughTillNextSession form.enoughTillNextSession
            , ifNullableTrue SupplementShared form.supplementShared
            , ifNullableTrue EncouragedToEat form.encouragedToEat
            , ifNullableTrue RefusingToEat form.refusingToEat
            , ifNullableTrue FeedingSignBreastfeeding form.breastfeeding
            , ifNullableTrue CleanWaterAvailable form.cleanWaterAvailable
            , ifNullableTrue EatenWithWater form.eatenWithWater
            ]
                |> Maybe.Extra.combine
                |> Maybe.map (List.foldl EverySet.union EverySet.empty >> ifEverySetEmpty NoNutritionFeedingSigns)

        supplementType =
            form.supplementType
                |> Maybe.withDefault NoNutritionSupplementType
                |> Just

        sachetsPerDay =
            form.sachetsPerDay
                |> Maybe.withDefault 0
                |> Just
    in
    Maybe.map NutritionFeedingValue signs
        |> andMap supplementType
        |> andMap sachetsPerDay


fromNutritionHygieneValue : Maybe NutritionHygieneValue -> NutritionHygieneForm
fromNutritionHygieneValue saved =
    { soapInTheHouse = Maybe.map (.signs >> EverySet.member SoapInTheHouse) saved
    , washHandsBeforeFeeding = Maybe.map (.signs >> EverySet.member WashHandsBeforeFeeding) saved
    , foodCovered = Maybe.map (.signs >> EverySet.member FoodCovered) saved
    , mainWaterSource = Maybe.map .mainWaterSource saved
    }


nutritionHygieneFormWithDefault : NutritionHygieneForm -> Maybe NutritionHygieneValue -> NutritionHygieneForm
nutritionHygieneFormWithDefault form saved =
    saved
        |> unwrap
            form
            (\value ->
                { soapInTheHouse = or form.soapInTheHouse (EverySet.member SoapInTheHouse value.signs |> Just)
                , washHandsBeforeFeeding = or form.washHandsBeforeFeeding (EverySet.member WashHandsBeforeFeeding value.signs |> Just)
                , foodCovered = or form.foodCovered (EverySet.member FoodCovered value.signs |> Just)
                , mainWaterSource = or form.mainWaterSource (Just value.mainWaterSource)
                }
            )


toNutritionHygieneValueWithDefault : Maybe NutritionHygieneValue -> NutritionHygieneForm -> Maybe NutritionHygieneValue
toNutritionHygieneValueWithDefault saved form =
    nutritionHygieneFormWithDefault form saved
        |> toNutritionHygieneValue


toNutritionHygieneValue : NutritionHygieneForm -> Maybe NutritionHygieneValue
toNutritionHygieneValue form =
    let
        signs =
            [ ifNullableTrue SoapInTheHouse form.soapInTheHouse
            , ifNullableTrue WashHandsBeforeFeeding form.washHandsBeforeFeeding
            , ifNullableTrue FoodCovered form.foodCovered
            ]
                |> Maybe.Extra.combine
                |> Maybe.map (List.foldl EverySet.union EverySet.empty >> ifEverySetEmpty NoNutritionHygieneSigns)
    in
    Maybe.map NutritionHygieneValue signs
        |> andMap form.mainWaterSource


fromNutritionFoodSecurityValue : Maybe NutritionFoodSecurityValue -> NutritionFoodSecurityForm
fromNutritionFoodSecurityValue saved =
    { householdGotFood = Maybe.map (.signs >> EverySet.member HouseholdGotFood) saved
    , mainIncomeSource = Maybe.map .mainIncomeSource saved
    }


nutritionFoodSecurityFormWithDefault : NutritionFoodSecurityForm -> Maybe NutritionFoodSecurityValue -> NutritionFoodSecurityForm
nutritionFoodSecurityFormWithDefault form saved =
    saved
        |> unwrap
            form
            (\value ->
                { householdGotFood = or form.householdGotFood (EverySet.member HouseholdGotFood value.signs |> Just)
                , mainIncomeSource = or form.mainIncomeSource (Just value.mainIncomeSource)
                }
            )


toNutritionFoodSecurityValueWithDefault : Maybe NutritionFoodSecurityValue -> NutritionFoodSecurityForm -> Maybe NutritionFoodSecurityValue
toNutritionFoodSecurityValueWithDefault saved form =
    nutritionFoodSecurityFormWithDefault form saved
        |> toNutritionFoodSecurityValue


toNutritionFoodSecurityValue : NutritionFoodSecurityForm -> Maybe NutritionFoodSecurityValue
toNutritionFoodSecurityValue form =
    let
        signs =
            [ ifNullableTrue HouseholdGotFood form.householdGotFood
            ]
                |> Maybe.Extra.combine
                |> Maybe.map (List.foldl EverySet.union EverySet.empty >> ifEverySetEmpty NoNutritionFoodSecuritySigns)
    in
    Maybe.map NutritionFoodSecurityValue signs
        |> andMap form.mainIncomeSource
