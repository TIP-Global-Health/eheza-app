module Translate exposing
    ( StringIdHttpError(..)
    , TranslationId(..)
    , translate
    )

import App.Types exposing (Language(..))
import Backend.Scoreboard.Model exposing (SelectedEntity(..))
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

        Kirundi ->
            .kirundi set
                |> Maybe.withDefault (.english set)


type alias TranslationSet =
    { english : String
    , kinyarwanda : Maybe String
    , kirundi : Maybe String
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
    | Colline
    | CollineSub
    | Commune
    | District
    | Demographics
    | EmptyString
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
    | NewSelection
    | NutritionBehavior
    | PleaseWaitMessage
    | Province
    | Sector
    | SelectedEntity SelectedEntity
    | Stunting
    | Status
    | TargetedInterventions
    | Village
    | UniversalIntervention
    | Zone


translationSet : TranslationId -> TranslationSet
translationSet transId =
    case transId of
        AcuteMalnutrition ->
            { english = "Acute Malnutrition"
            , kinyarwanda = Nothing
            , kirundi = Nothing
            }

        AggregatedChildScoreboard ->
            { english = "Aggregated Child Scoreboard"
            , kinyarwanda = Nothing
            , kirundi = Nothing
            }

        ANCNewborn ->
            { english = "ANC + Newborn"
            , kinyarwanda = Nothing
            , kirundi = Nothing
            }

        Colline ->
            { english = "Colline"
            , kinyarwanda = Nothing
            , kirundi = Nothing
            }

        CollineSub ->
            { english = "Sub-Colline"
            , kinyarwanda = Nothing
            , kirundi = Nothing
            }

        Commune ->
            { english = "Commune"
            , kinyarwanda = Nothing
            , kirundi = Nothing
            }

        Cell ->
            { english = "Cell"
            , kinyarwanda = Nothing
            , kirundi = Nothing
            }

        District ->
            { english = "District"
            , kinyarwanda = Nothing
            , kirundi = Nothing
            }

        Demographics ->
            { english = "Demographics"
            , kinyarwanda = Nothing
            , kirundi = Nothing
            }

        EmptyString ->
            { english = ""
            , kinyarwanda = Nothing
            , kirundi = Nothing
            }

        GenerateReport ->
            { english = "Generate Report"
            , kinyarwanda = Nothing
            , kirundi = Nothing
            }

        HttpError val ->
            translateHttpError val

        InfrastructureEnvironmentWash ->
            { english = "Infrastructure, Environment & Wash"
            , kinyarwanda = Nothing
            , kirundi = Nothing
            }

        Month month ->
            translateMonth month False

        NCDADemographicsItemLabel item ->
            case item of
                ChildrenUnder2 ->
                    { english = "Children under 2"
                    , kinyarwanda = Nothing
                    , kirundi = Nothing
                    }

                NewbornsThisMonth ->
                    { english = "Number of Newborns this month"
                    , kinyarwanda = Nothing
                    , kirundi = Nothing
                    }

                LowBirthWeigh ->
                    { english = "Low Birth Weigh (Y/N)"
                    , kinyarwanda = Nothing
                    , kirundi = Nothing
                    }

        NCDAAcuteMalnutritionItemLabel item ->
            case item of
                SevereAcuteMalnutrition ->
                    { english = "Severe Acute Malnutrition"
                    , kinyarwanda = Nothing
                    , kirundi = Nothing
                    }

                ModerateAcuteMalnutrition ->
                    { english = "Moderate Acute Malnutrition"
                    , kinyarwanda = Nothing
                    , kirundi = Nothing
                    }

                GoodNutrition ->
                    { english = "Good Nutrition"
                    , kinyarwanda = Nothing
                    , kirundi = Nothing
                    }

        NCDAStuntingItemLabel item ->
            case item of
                SevereStunting ->
                    { english = "Severe Stunting"
                    , kinyarwanda = Nothing
                    , kirundi = Nothing
                    }

                ModerateStunting ->
                    { english = "Moderate Stunting"
                    , kinyarwanda = Nothing
                    , kirundi = Nothing
                    }

                NoStunting ->
                    { english = "No Stunting"
                    , kinyarwanda = Nothing
                    , kirundi = Nothing
                    }

        NCDAANCNewbornItemLabel item ->
            case item of
                RegularCheckups ->
                    { english = "Regular prenatal and postpartum checkups"
                    , kinyarwanda = Just "Yisuzumishije uko bikwiye atwite na nyuma yo kubyara"
                    , kirundi = Nothing
                    }

                IronDuringPregnancy ->
                    { english = "Iron during pregnancy"
                    , kinyarwanda = Just "Yafashe umuti wongera amaraso atwite"
                    , kirundi = Nothing
                    }

        NCDAInfrastructureEnvironmentWashItemLabel item ->
            case item of
                HasToilets ->
                    { english = "Household has toilets"
                    , kinyarwanda = Just "Urugo rufite ubwiherero"
                    , kirundi = Nothing
                    }

                HasCleanWater ->
                    { english = "Household has clean water"
                    , kinyarwanda = Just "Urugo rufite amazi meza"
                    , kirundi = Nothing
                    }

                HasHandwashingFacility ->
                    { english = "Household has handwashing facility"
                    , kinyarwanda = Just "Urugo rufite kandagirukarabe"
                    , kirundi = Nothing
                    }

                HasKitchenGarden ->
                    { english = "Household has kitchen garden"
                    , kinyarwanda = Just "Urugo rufite akarima k'igikoni"
                    , kirundi = Nothing
                    }

                InsecticideTreatedBedNets ->
                    { english = "Insecticide treated bed nets"
                    , kinyarwanda = Just "Urugo rufite nzitiramibu ikoranye umuti"
                    , kirundi = Nothing
                    }

        NCDANutritionBehaviorItemLabel item ->
            case item of
                BreastfedSixMonths ->
                    { english = "Breastfed baby for 6 mo without interruption"
                    , kinyarwanda = Just "Konsa umwana amezi 6 utamuvangiye"
                    , kirundi = Nothing
                    }

                AppropriateComplementaryFeeding ->
                    { english = "Appropriate complementary feeding (6-24 mo)"
                    , kinyarwanda = Just "Imfashabere igizwe n’indyo yuzuye (Amezi 6-24)"
                    , kirundi = Nothing
                    }

                DiverseDiet ->
                    { english = "Diverse diet"
                    , kinyarwanda = Nothing
                    , kirundi = Nothing
                    }

                MealsADay ->
                    { english = "Appropriate frequency of meals"
                    , kinyarwanda = Nothing
                    , kirundi = Nothing
                    }

        NCDATargetedInterventionsItemLabel item ->
            case item of
                FBFGiven ->
                    { english = "FBF"
                    , kinyarwanda = Nothing
                    , kirundi = Nothing
                    }

                TreatmentForAcuteMalnutrition ->
                    { english = "Treatment for acute malnutrition (severe or moderate)"
                    , kinyarwanda = Just "Kuvura imiritre mibi  ifatiyeho(Ikabije cg yoroheje)"
                    , kirundi = Nothing
                    }

                TreatmentForDiarrhea ->
                    { english = "Treatment of diarrhea (ORS & Zinc)"
                    , kinyarwanda = Just "Kuvura impiswi(Ukoresheje Zinc cg ORS)"
                    , kirundi = Nothing
                    }

                SupportChildWithDisability ->
                    { english = "Provide support to a child with a disability "
                    , kinyarwanda = Just "Guha umwana ufite ubumuga ubufasha bwihariye"
                    , kirundi = Nothing
                    }

                ConditionalCashTransfer ->
                    { english = "Receipt of conditional cash transfer e.g. NSDS, VUP"
                    , kinyarwanda = Just "Gufata amafaranga y’inkunga agenerwa umugore utwite n’uwonsa bo mu miryango ikennye (icyiciro cya 1 n’icya 2) – NSDS, VUP"
                    , kirundi = Nothing
                    }

                ConditionalFoodItems ->
                    { english = "Receipt of conditional food items including small livestock"
                    , kinyarwanda = Just "Gufata inkunga z’ingoboka harimo ibiryo n'amatungo magufi"
                    , kirundi = Nothing
                    }

        NCDAUniversalInterventionItemLabel item ->
            case item of
                Immunization ->
                    { english = "Immunization"
                    , kinyarwanda = Just "Ikingira"
                    , kirundi = Nothing
                    }

                VitaminA ->
                    { english = "Vitamin A"
                    , kinyarwanda = Nothing
                    , kirundi = Nothing
                    }

                Deworming ->
                    { english = "Deworming"
                    , kinyarwanda = Just "Imiti y'inzoka"
                    , kirundi = Nothing
                    }

                OngeraMNP ->
                    { english = "Use additional nutrients (Ongera)"
                    , kinyarwanda = Just "Koresha Ongera intungamubiri"
                    , kirundi = Nothing
                    }

                ECDServices ->
                    { english = "ECD services provided to child"
                    , kinyarwanda = Just "Umwana yahawe servise n'ikigo mboneza mikurire"
                    , kirundi = Nothing
                    }

        NewSelection ->
            { english = "New Selection"
            , kinyarwanda = Nothing
            , kirundi = Nothing
            }

        NutritionBehavior ->
            { english = "Nutrition Behavior"
            , kinyarwanda = Nothing
            , kirundi = Nothing
            }

        PleaseWaitMessage ->
            { english = "Please wait. This action may take a couple of minutes to complete."
            , kinyarwanda = Nothing
            , kirundi = Nothing
            }

        Province ->
            { english = "Province"
            , kinyarwanda = Nothing
            , kirundi = Nothing
            }

        Sector ->
            { english = "Sector"
            , kinyarwanda = Nothing
            , kirundi = Nothing
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
            , kirundi = Nothing
            }

        Status ->
            { english = "Status"
            , kinyarwanda = Nothing
            , kirundi = Nothing
            }

        TargetedInterventions ->
            { english = "Targeted Interventions"
            , kinyarwanda = Nothing
            , kirundi = Nothing
            }

        Village ->
            { english = "Village"
            , kinyarwanda = Nothing
            , kirundi = Nothing
            }

        UniversalIntervention ->
            { english = "Universal Intervention"
            , kinyarwanda = Nothing
            , kirundi = Nothing
            }

        Zone ->
            { english = "Zone"
            , kinyarwanda = Nothing
            , kirundi = Nothing
            }


translateHttpError : StringIdHttpError -> TranslationSet
translateHttpError transId =
    case transId of
        ErrorBadUrl ->
            { english = "URL is not valid."
            , kinyarwanda = Nothing
            , kirundi = Nothing
            }

        ErrorBadPayload message ->
            { english = "The server responded with data of an unexpected type: " ++ message
            , kinyarwanda = Nothing
            , kirundi = Nothing
            }

        ErrorBadStatus err ->
            { english = err
            , kinyarwanda = Nothing
            , kirundi = Nothing
            }

        ErrorNetworkError ->
            { english = "There was a network error."
            , kinyarwanda = Nothing
            , kirundi = Nothing
            }

        ErrorTimeout ->
            { english = "The network request timed out."
            , kinyarwanda = Nothing
            , kirundi = Nothing
            }


translateMonth : Month -> Bool -> TranslationSet
translateMonth month short =
    case month of
        Jan ->
            if short then
                { english = "Jan"
                , kinyarwanda = Just "Mut"
                , kirundi = Nothing
                }

            else
                { english = "January"
                , kinyarwanda = Just "Mutarama"
                , kirundi = Nothing
                }

        Feb ->
            if short then
                { english = "Feb"
                , kinyarwanda = Just "Gas"
                , kirundi = Nothing
                }

            else
                { english = "February"
                , kinyarwanda = Just "Gashyantare"
                , kirundi = Nothing
                }

        Mar ->
            if short then
                { english = "Mar"
                , kinyarwanda = Just "Wer"
                , kirundi = Nothing
                }

            else
                { english = "March"
                , kinyarwanda = Just "Werurwe"
                , kirundi = Nothing
                }

        Apr ->
            if short then
                { english = "Apr"
                , kinyarwanda = Just "Mat"
                , kirundi = Nothing
                }

            else
                { english = "April"
                , kinyarwanda = Just "Mata"
                , kirundi = Nothing
                }

        May ->
            if short then
                { english = "May"
                , kinyarwanda = Just "Gic"
                , kirundi = Nothing
                }

            else
                { english = "May"
                , kinyarwanda = Just "Gicurasi"
                , kirundi = Nothing
                }

        Jun ->
            if short then
                { english = "Jun"
                , kinyarwanda = Just "Kam"
                , kirundi = Nothing
                }

            else
                { english = "June"
                , kinyarwanda = Just "Kamena"
                , kirundi = Nothing
                }

        Jul ->
            if short then
                { english = "Jul"
                , kinyarwanda = Just "Nya"
                , kirundi = Nothing
                }

            else
                { english = "July"
                , kinyarwanda = Just "Nyakanga"
                , kirundi = Nothing
                }

        Aug ->
            if short then
                { english = "Aug"
                , kinyarwanda = Just "Kan"
                , kirundi = Nothing
                }

            else
                { english = "August"
                , kinyarwanda = Just "Kanama"
                , kirundi = Nothing
                }

        Sep ->
            if short then
                { english = "Sep"
                , kinyarwanda = Just "Nze"
                , kirundi = Nothing
                }

            else
                { english = "September"
                , kinyarwanda = Just "Nzeri"
                , kirundi = Nothing
                }

        Oct ->
            if short then
                { english = "Oct"
                , kinyarwanda = Just "Ukw"
                , kirundi = Nothing
                }

            else
                { english = "October"
                , kinyarwanda = Just "Ukwakira"
                , kirundi = Nothing
                }

        Nov ->
            if short then
                { english = "Nov"
                , kinyarwanda = Just "Ugu"
                , kirundi = Nothing
                }

            else
                { english = "November"
                , kinyarwanda = Just "Ugushyingo"
                , kirundi = Nothing
                }

        Dec ->
            if short then
                { english = "Dec"
                , kinyarwanda = Just "Uku"
                , kirundi = Nothing
                }

            else
                { english = "December"
                , kinyarwanda = Just "Ukuboza"
                , kirundi = Nothing
                }
