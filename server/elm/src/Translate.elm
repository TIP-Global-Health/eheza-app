module Translate exposing
    ( StringIdHttpError(..)
    , TranslationId(..)
    , translate
    )

import App.Types exposing (Language(..))
import Pages.Scoreboard.Model exposing (..)
import Time exposing (Month(..))


{-| Main function to call for translation.
-}
translate : Language -> TranslationId -> String
translate language transId =
    let
        set =
            translationSet transId
    in
    case language of
        English ->
            .english set

        Kinyarwanda ->
            .kinyarwanda set
                |> Maybe.withDefault (.english set)


type alias TranslationSet =
    { english : String
    , kinyarwanda : Maybe String
    }


type StringIdHttpError
    = ErrorBadUrl
    | ErrorBadPayload String
    | ErrorBadStatus String
    | ErrorNetworkError
    | ErrorTimeout


type TranslationId
    = AcuteMalnutrition
    | AggregatedChildScoreboard
    | ANCNewborn
    | Cell
    | Demographics
    | District
    | GenerateReport
    | HttpError StringIdHttpError
    | InfrastructureEnvironmentWash
    | Month Month
    | NCDADemographicsItemLabel NCDADemographicsItem
    | NCDAAcuteMalnutritionItemLabel NCDAAcuteMalnutritionItem
    | NCDAStuntingItemLabel NCDAStuntingItem
    | NCDAANCNewbornItemLabel NCDAANCNewbornItem
    | NCDAInfrastructureEnvironmentWashItemLabel NCDAInfrastructureEnvironmentWashItem
    | NCDANutritionBehaviorItemLabel NCDANutritionBehaviorItem
    | NCDATargetedInterventionsItemLabel NCDATargetedInterventionsItem
    | NCDAUniversalInterventionItemLabel NCDAUniversalInterventionItem
    | NCDAFillTheBlanksItemLabel NCDAFillTheBlanksItem
    | NewSelection
    | NutritionBehavior
    | Province
    | Sector
    | SelectedEntity SelectedEntity
    | Stunting
    | Status
    | TargetedInterventions
    | Village
    | UniversalIntervention


translationSet : TranslationId -> TranslationSet
translationSet transId =
    case transId of
        AcuteMalnutrition ->
            { english = "Acute Malnutrition"
            , kinyarwanda = Nothing
            }

        AggregatedChildScoreboard ->
            { english = "Aggregated Child Scoreboard"
            , kinyarwanda = Nothing
            }

        ANCNewborn ->
            { english = "ANC + Newborn"
            , kinyarwanda = Nothing
            }

        Cell ->
            { english = "Cell"
            , kinyarwanda = Nothing
            }

        District ->
            { english = "District"
            , kinyarwanda = Nothing
            }

        Demographics ->
            { english = "Demographics"
            , kinyarwanda = Nothing
            }

        GenerateReport ->
            { english = "Generate Report"
            , kinyarwanda = Nothing
            }

        HttpError val ->
            translateHttpError val

        InfrastructureEnvironmentWash ->
            { english = "Infrastructure, Environment & Wash"
            , kinyarwanda = Nothing
            }

        Month month ->
            translateMonth month False

        NCDADemographicsItemLabel item ->
            case item of
                ChildrenUnder2 ->
                    { english = "Children under 2"
                    , kinyarwanda = Nothing
                    }

                NewbornsThisMonth ->
                    { english = "Number of Newborns this month"
                    , kinyarwanda = Nothing
                    }

                LowBirthWeigh ->
                    { english = "Low Birth Weigh (Y/N)"
                    , kinyarwanda = Nothing
                    }

        NCDAAcuteMalnutritionItemLabel item ->
            case item of
                SevereAcuteMalnutrition ->
                    { english = "Severe Acute Malnutrition"
                    , kinyarwanda = Nothing
                    }

                ModerateAcuteMalnutrition ->
                    { english = "Moderate Acute Malnutrition"
                    , kinyarwanda = Nothing
                    }

                GoodNutrition ->
                    { english = "Good Nutrition"
                    , kinyarwanda = Nothing
                    }

        NCDAStuntingItemLabel item ->
            case item of
                SevereStunting ->
                    { english = "Severe Stunting"
                    , kinyarwanda = Nothing
                    }

                ModerateStunting ->
                    { english = "Moderate Stunting"
                    , kinyarwanda = Nothing
                    }

                NoStunting ->
                    { english = "No Stunting"
                    , kinyarwanda = Nothing
                    }

        NCDAANCNewbornItemLabel item ->
            case item of
                RegularCheckups ->
                    { english = "Regular prenatal and postpartum checkups"
                    , kinyarwanda = Just "Yisuzumishije uko bikwiye atwite na nyuma yo kubyara"
                    }

                IronDuringPregnancy ->
                    { english = "Iron during pregnancy"
                    , kinyarwanda = Just "Yafashe umuti wongera amaraso atwite"
                    }

        NCDAInfrastructureEnvironmentWashItemLabel item ->
            case item of
                HasToilets ->
                    { english = "Household has toilets"
                    , kinyarwanda = Just "Urugo rufite ubwiherero"
                    }

                HasCleanWater ->
                    { english = "Household has clean water"
                    , kinyarwanda = Just "Urugo rufite amazi meza"
                    }

                HasHandwashingFacility ->
                    { english = "Household has handwashing facility"
                    , kinyarwanda = Just "Urugo rufite kandagirukarabe"
                    }

                HasKitchenGarden ->
                    { english = "Household has kitchen garden"
                    , kinyarwanda = Just "Urugo rufite akarima k'igikoni"
                    }

                InsecticideTreatedBedNets ->
                    { english = "Insecticide treated bed nets"
                    , kinyarwanda = Just "Urugo rufite nzitiramibu ikoranye umuti"
                    }

        NCDANutritionBehaviorItemLabel item ->
            case item of
                BreastfedSixMonths ->
                    { english = "Breastfed baby for 6 mo without interruption"
                    , kinyarwanda = Just "Konsa umwana amezi 6 utamuvangiye"
                    }

                AppropriateComplementaryFeeding ->
                    { english = "Appropriate complementary feeding (6-24 mo)"
                    , kinyarwanda = Just "Imfashabere igizwe n’indyo yuzuye (Amezi 6-24)"
                    }

                DiverseDiet ->
                    { english = "Does the child have a diverse diet?"
                    , kinyarwanda = Just "Umwana afata indyo yuzuye"
                    }

                MealsADay ->
                    { english = "Number of times a child eats a day"
                    , kinyarwanda = Just "Inshuro umwana afata ifunguro ku munsi"
                    }

        NCDATargetedInterventionsItemLabel item ->
            case item of
                FBFGiven ->
                    { english = "FBF"
                    , kinyarwanda = Nothing
                    }

                TreatmentForAcuteMalnutrition ->
                    { english = "Treatment for acute malnutrition (severe or moderate)"
                    , kinyarwanda = Just "Kuvura imiritre mibi  ifatiyeho(Ikabije cg yoroheje)"
                    }

                TreatmentForDiarrhea ->
                    { english = "Treatment of diarrhea (ORS & Zinc)"
                    , kinyarwanda = Just "Kuvura impiswi(Ukoresheje Zinc cg ORS)"
                    }

                SupportChildWithDisability ->
                    { english = "Provide support to a child with a disability "
                    , kinyarwanda = Just "Guha umwana ufite ubumuga ubufasha bwihariye"
                    }

                ConditionalCashTransfer ->
                    { english = "Receipt of conditional cash transfer e.g. NSDS, VUP"
                    , kinyarwanda = Just "Gufata amafaranga y’inkunga agenerwa umugore utwite n’uwonsa bo mu miryango ikennye (icyiciro cya 1 n’icya 2) – NSDS, VUP"
                    }

                ConditionalFoodItems ->
                    { english = "Receipt of conditional food items including small livestock"
                    , kinyarwanda = Just "Gufata inkunga z’ingoboka harimo ibiryo n'amatungo magufi"
                    }

        NCDAUniversalInterventionItemLabel item ->
            case item of
                Immunization ->
                    { english = "Immunization"
                    , kinyarwanda = Just "Ikingira"
                    }

                VitaminA ->
                    { english = "Vitamin A"
                    , kinyarwanda = Nothing
                    }

                Deworming ->
                    { english = "Deworming"
                    , kinyarwanda = Just "Imiti y'inzoka"
                    }

                OngeraMNP ->
                    { english = "Use additional nutrients (Ongera)"
                    , kinyarwanda = Just "Koresha Ongera intungamubiri"
                    }

                ECDServices ->
                    { english = "ECD services provided to child"
                    , kinyarwanda = Just "Umwana yahawe servise n'ikigo mboneza mikurire"
                    }

        NCDAFillTheBlanksItemLabel item ->
            case item of
                HeightToAge ->
                    { english = "Level of stuning using child length mat"
                    , kinyarwanda = Just "Ikigero cyo kugwingira hakoreshejwe agasambi"
                    }

                WeightToAge ->
                    { english = "Weight"
                    , kinyarwanda = Just "Ibiro"
                    }

                MuacValue ->
                    { english = "MUAC"
                    , kinyarwanda = Just "Ikizigira"
                    }

                EdemaPresent ->
                    { english = "Edema"
                    , kinyarwanda = Just "Kubyimba"
                    }

        NewSelection ->
            { english = "New Selection"
            , kinyarwanda = Nothing
            }

        NutritionBehavior ->
            { english = "Nutrition Behavior"
            , kinyarwanda = Nothing
            }

        Province ->
            { english = "Province"
            , kinyarwanda = Nothing
            }

        Sector ->
            { english = "Sector"
            , kinyarwanda = Nothing
            }

        SelectedEntity entity ->
            case entity of
                EntityDistrict ->
                    translationSet District

                EntitySector ->
                    translationSet Sector

                EntityCell ->
                    translationSet Cell

                EntityVillage ->
                    translationSet Village

        Stunting ->
            { english = "Stunting"
            , kinyarwanda = Nothing
            }

        Status ->
            { english = "Status"
            , kinyarwanda = Nothing
            }

        TargetedInterventions ->
            { english = "Targeted Interventions"
            , kinyarwanda = Nothing
            }

        Village ->
            { english = "Village"
            , kinyarwanda = Nothing
            }

        UniversalIntervention ->
            { english = "Univeral Intervention"
            , kinyarwanda = Nothing
            }


translateHttpError : StringIdHttpError -> TranslationSet
translateHttpError transId =
    case transId of
        ErrorBadUrl ->
            { english = "URL is not valid."
            , kinyarwanda = Nothing
            }

        ErrorBadPayload message ->
            { english = "The server responded with data of an unexpected type: " ++ message
            , kinyarwanda = Nothing
            }

        ErrorBadStatus err ->
            { english = err
            , kinyarwanda = Nothing
            }

        ErrorNetworkError ->
            { english = "There was a network error."
            , kinyarwanda = Nothing
            }

        ErrorTimeout ->
            { english = "The network request timed out."
            , kinyarwanda = Nothing
            }


translateMonth : Month -> Bool -> TranslationSet
translateMonth month short =
    case month of
        Jan ->
            if short then
                { english = "Jan"
                , kinyarwanda = Just "Mut"
                }

            else
                { english = "January"
                , kinyarwanda = Just "Mutarama"
                }

        Feb ->
            if short then
                { english = "Feb"
                , kinyarwanda = Just "Gas"
                }

            else
                { english = "February"
                , kinyarwanda = Just "Gashyantare"
                }

        Mar ->
            if short then
                { english = "Mar"
                , kinyarwanda = Just "Wer"
                }

            else
                { english = "March"
                , kinyarwanda = Just "Werurwe"
                }

        Apr ->
            if short then
                { english = "Apr"
                , kinyarwanda = Just "Mat"
                }

            else
                { english = "April"
                , kinyarwanda = Just "Mata"
                }

        May ->
            if short then
                { english = "May"
                , kinyarwanda = Just "Gic"
                }

            else
                { english = "May"
                , kinyarwanda = Just "Gicurasi"
                }

        Jun ->
            if short then
                { english = "Jun"
                , kinyarwanda = Just "Kam"
                }

            else
                { english = "June"
                , kinyarwanda = Just "Kamena"
                }

        Jul ->
            if short then
                { english = "Jul"
                , kinyarwanda = Just "Nya"
                }

            else
                { english = "July"
                , kinyarwanda = Just "Nyakanga"
                }

        Aug ->
            if short then
                { english = "Aug"
                , kinyarwanda = Just "Kan"
                }

            else
                { english = "August"
                , kinyarwanda = Just "Kanama"
                }

        Sep ->
            if short then
                { english = "Sep"
                , kinyarwanda = Just "Nze"
                }

            else
                { english = "September"
                , kinyarwanda = Just "Nzeri"
                }

        Oct ->
            if short then
                { english = "Oct"
                , kinyarwanda = Just "Ukw"
                }

            else
                { english = "October"
                , kinyarwanda = Just "Ukwakira"
                }

        Nov ->
            if short then
                { english = "Nov"
                , kinyarwanda = Just "Ugu"
                }

            else
                { english = "November"
                , kinyarwanda = Just "Ugushyingo"
                }

        Dec ->
            if short then
                { english = "Dec"
                , kinyarwanda = Just "Uku"
                }

            else
                { english = "December"
                , kinyarwanda = Just "Ukuboza"
                }
