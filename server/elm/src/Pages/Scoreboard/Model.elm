module Pages.Scoreboard.Model exposing (..)

import AssocList
import Dict exposing (Dict)
import Json.Decode exposing (Value)
import Restful.Endpoint exposing (toEntityId)
import Utils.GeoLocation exposing (GeoLocationId)


type alias Model =
    { displayMode : DisplayMode
    , form : ViewSelectionForm
    , yearSelectorGap : Int
    }


emptyModel : Model
emptyModel =
    { displayMode = DisplayViewSelection

    -- displayMode = DisplayResultTable { cell = Nothing, district = toEntityId 2046, province = toEntityId 1990, sector = Nothing, village = Nothing }
    , form = emptyViewSelectionForm
    , yearSelectorGap = 0
    }


type DisplayMode
    = DisplayViewSelection
    | DisplayResultTable ViewSelectionValue


type alias ViewSelectionForm =
    { province : Maybe GeoLocationId
    , district : Maybe GeoLocationId
    , sector : Maybe GeoLocationId
    , cell : Maybe GeoLocationId
    , village : Maybe GeoLocationId
    }


emptyViewSelectionForm : ViewSelectionForm
emptyViewSelectionForm =
    { province = Nothing
    , district = Nothing
    , sector = Nothing
    , cell = Nothing
    , village = Nothing
    }


type alias ViewSelectionValue =
    { province : GeoLocationId
    , district : GeoLocationId
    , sector : Maybe GeoLocationId
    , cell : Maybe GeoLocationId
    , village : Maybe GeoLocationId
    }


type SelectedEntity
    = EntityDistrict
    | EntitySector
    | EntityCell
    | EntityVillage


type Msg
    = SetGeoLocation (String -> ViewSelectionForm -> ViewSelectionForm) String
    | GenerateReport
    | ResetSelection
    | ChaneYearGap Int


type NCDADemographicsItem
    = ChildrenUnder2
    | NewbornsThisMonth
    | LowBirthWeigh


type NCDAAcuteMalnutritionItem
    = SevereAcuteMalnutrition
    | ModerateAcuteMalnutrition
    | GoodNutrition


type NCDAStuntingItem
    = SevereStunting
    | ModerateStunting
    | NoStunting


type NCDAANCNewbornItem
    = RegularCheckups
    | IronDuringPregnancy


type NCDANutritionBehaviorItem
    = BreastfedSixMonths
    | AppropriateComplementaryFeeding
    | DiverseDiet
    | MealsADay


type NCDAInfrastructureEnvironmentWashItem
    = HasToilets
    | HasCleanWater
    | HasHandwashingFacility
    | HasKitchenGarden
    | InsecticideTreatedBedNets


type NCDATargetedInterventionsItem
    = FBFGiven
    | TreatmentForAcuteMalnutrition
    | TreatmentForDiarrhea
    | SupportChildWithDisability
    | ConditionalCashTransfer
    | ConditionalFoodItems


type NCDAUniversalInterventionItem
    = Immunization
    | VitaminA
    | Deworming
    | OngeraMNP
    | ECDServices


type NCDAFillTheBlanksItem
    = HeightToAge
    | WeightToAge
    | MuacValue
    | EdemaPresent
