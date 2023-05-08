module Pages.Scoreboard.Model exposing (..)


type alias Model =
    { yearSelectorGap : Int
    }


emptyModel : Model
emptyModel =
    { yearSelectorGap = 0
    }


type SelectedEntity
    = EntityDistrict
    | EntitySector
    | EntityCell
    | EntityVillage


type Msg
    = ChaneYearGap Int


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
