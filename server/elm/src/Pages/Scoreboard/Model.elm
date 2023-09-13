module Pages.Scoreboard.Model exposing (..)


type alias Model =
    { yearSelectorGap : Int
    , viewMode : ViewMode
    }


emptyModel : Model
emptyModel =
    { yearSelectorGap = 0
    , viewMode = ModeValues
    }


type ViewMode
    = ModeValues
    | ModePercentages


type Msg
    = ChaneYearGap Int
    | SetViewMode ViewMode


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
    = WeightToAge
