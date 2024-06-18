module Translate exposing
    ( StringIdHttpError(..)
    , TranslationId(..)
    , translate
    )

import App.Types exposing (Language(..))
import Backend.Scoreboard.Model exposing (SelectedEntity(..))
import Pages.Reports.Model exposing (ReportType(..))
import Pages.ReportsMenu.Types exposing (PopulationSelectionOption(..))
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
    = ACHI
    | AcuteIllnessTotal
    | AcuteMalnutrition
    | AggregatedChildScoreboard
    | All
    | ANCNewborn
    | ANCTotal
    | CBNP
    | Cell
    | CHW
    | Colline
    | CollineSub
    | Commune
    | District
    | Demographics
    | EmptyString
    | Encounters
    | EncounterType
    | FBF
    | Female
    | GenerateReport
    | HealthCenter
    | HomeVisit
    | HttpError StringIdHttpError
    | Impacted
    | IncidenceByMonthOneVisitOrMore
    | IncidenceByMonthTwoVisitsOrMore
    | IncidenceByQuarterOneVisitOrMore
    | IncidenceByQuarterTwoVisitsOrMore
    | IncidenceByYearOneVisitOrMore
    | IncidenceByYearTwoVisitsOrMore
    | Individual
    | InfrastructureEnvironmentWash
    | Male
    | Month Month
    | MonthLabel
    | MonthYear Month Int Bool
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
    | NutritionTotal
    | PleaseWaitMessage
    | PMTCT
    | PopulationSelectionOption PopulationSelectionOption
    | PrevalenceByMonthOneVisitOrMore
    | PrevalenceByMonthTwoVisitsOrMore
    | Province
    | QuarterYear Int Int
    | Registered
    | RegisteredPatients
    | ReportType ReportType
    | ReportTypeLabel
    | ResolveMonth Bool Month
    | Result
    | Save
    | Sector
    | SelectedEntity SelectedEntity
    | SelectLimitDate
    | SelectViewMode
    | Sorwathe
    | StandardPediatricVisit
    | Stunting
    | StuntingModerate
    | StuntingSevere
    | Status
    | TargetedInterventions
    | Total
    | ViewMode
    | Village
    | UnderweightModerate
    | UnderweightSevere
    | Unique
    | UniversalIntervention
    | WastingModerate
    | WastingSevere
    | WideScopeNote
    | Year Int
    | YearLabel
    | Zone


translationSet : TranslationId -> TranslationSet
translationSet transId =
    case transId of
        ACHI ->
            { english = "ACHI"
            , kinyarwanda = Nothing
            , kirundi = Nothing
            }

        AcuteIllnessTotal ->
            { english = "Acute Illness (total)"
            , kinyarwanda = Nothing
            , kirundi = Nothing
            }

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

        All ->
            { english = "All"
            , kinyarwanda = Nothing
            , kirundi = Nothing
            }

        ANCNewborn ->
            { english = "ANC + Newborn"
            , kinyarwanda = Nothing
            , kirundi = Nothing
            }

        ANCTotal ->
            { english = "ANC (total)"
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

        CBNP ->
            { english = "CBNP"
            , kinyarwanda = Nothing
            , kirundi = Nothing
            }

        Cell ->
            { english = "Cell"
            , kinyarwanda = Nothing
            , kirundi = Nothing
            }

        CHW ->
            { english = "CHW"
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

        Encounters ->
            { english = "Encounters"
            , kinyarwanda = Nothing
            , kirundi = Nothing
            }

        EncounterType ->
            { english = "Encounter Type"
            , kinyarwanda = Nothing
            , kirundi = Nothing
            }

        FBF ->
            { english = "FBF"
            , kinyarwanda = Nothing
            , kirundi = Nothing
            }

        Female ->
            { english = "Female"
            , kinyarwanda = Nothing
            , kirundi = Nothing
            }

        GenerateReport ->
            { english = "Generate Report"
            , kinyarwanda = Nothing
            , kirundi = Nothing
            }

        HealthCenter ->
            { english = "Health Center"
            , kinyarwanda = Nothing
            , kirundi = Nothing
            }

        HomeVisit ->
            { english = "Home Visit"
            , kinyarwanda = Nothing
            , kirundi = Nothing
            }

        HttpError val ->
            translateHttpError val

        Impacted ->
            { english = "Impacted (2+ visits)"
            , kinyarwanda = Nothing
            , kirundi = Nothing
            }

        IncidenceByMonthOneVisitOrMore ->
            { english = "Incidence by month, one visit or more"
            , kinyarwanda = Nothing
            , kirundi = Nothing
            }

        IncidenceByMonthTwoVisitsOrMore ->
            { english = "Incidence by month, two visits or more"
            , kinyarwanda = Nothing
            , kirundi = Nothing
            }

        IncidenceByQuarterOneVisitOrMore ->
            { english = "Incidence by quarter, one visit or more"
            , kinyarwanda = Nothing
            , kirundi = Nothing
            }

        IncidenceByQuarterTwoVisitsOrMore ->
            { english = "Incidence by quarter, two visits or more"
            , kinyarwanda = Nothing
            , kirundi = Nothing
            }

        IncidenceByYearOneVisitOrMore ->
            { english = "Incidence by year, one visit or more"
            , kinyarwanda = Nothing
            , kirundi = Nothing
            }

        IncidenceByYearTwoVisitsOrMore ->
            { english = "Incidence by year, two visits or more"
            , kinyarwanda = Nothing
            , kirundi = Nothing
            }

        Individual ->
            { english = "Individual"
            , kinyarwanda = Nothing
            , kirundi = Nothing
            }

        InfrastructureEnvironmentWash ->
            { english = "Infrastructure, Environment & Wash"
            , kinyarwanda = Nothing
            , kirundi = Nothing
            }

        Male ->
            { english = "Male"
            , kinyarwanda = Nothing
            , kirundi = Nothing
            }

        Month month ->
            translateMonth month False

        MonthLabel ->
            { english = "Month"
            , kinyarwanda = Just "Ukwezi"
            , kirundi = Just "Ukwezi"
            }

        MonthYear month year short ->
            translateMonthYY month year short

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

        NutritionTotal ->
            { english = "Nutrition (total)"
            , kinyarwanda = Nothing
            , kirundi = Nothing
            }

        PleaseWaitMessage ->
            { english = "Please wait. This action may take a couple of minutes to complete."
            , kinyarwanda = Nothing
            , kirundi = Nothing
            }

        PMTCT ->
            { english = "PMTCT"
            , kinyarwanda = Nothing
            , kirundi = Nothing
            }

        PopulationSelectionOption selectionOption ->
            case selectionOption of
                SelectionOptionGlobal ->
                    { english = "Gloabal"
                    , kinyarwanda = Nothing
                    , kirundi = Nothing
                    }

                SelectionOptionDemographics ->
                    { english = "Demographics"
                    , kinyarwanda = Nothing
                    , kirundi = Nothing
                    }

                SelectionOptionHealthCenter ->
                    { english = "Health Center"
                    , kinyarwanda = Nothing
                    , kirundi = Nothing
                    }

        PrevalenceByMonthOneVisitOrMore ->
            { english = "Prevalence by month, one visit or more"
            , kinyarwanda = Nothing
            , kirundi = Nothing
            }

        PrevalenceByMonthTwoVisitsOrMore ->
            { english = "Prevalence by month, two visits or more"
            , kinyarwanda = Nothing
            , kirundi = Nothing
            }

        Province ->
            { english = "Province"
            , kinyarwanda = Nothing
            , kirundi = Nothing
            }

        QuarterYear quarter year ->
            { english = String.fromInt year ++ " Q" ++ String.fromInt quarter
            , kinyarwanda = Nothing
            , kirundi = Nothing
            }

        Registered ->
            { english = "Registered"
            , kinyarwanda = Nothing
            , kirundi = Nothing
            }

        RegisteredPatients ->
            { english = "Registered Patients"
            , kinyarwanda = Nothing
            , kirundi = Nothing
            }

        ReportType reportType ->
            case reportType of
                ReportDemographics ->
                    { english = "Demographics"
                    , kinyarwanda = Nothing
                    , kirundi = Nothing
                    }

                ReportNutrition ->
                    { english = "Nutrition"
                    , kinyarwanda = Nothing
                    , kirundi = Nothing
                    }

        ReportTypeLabel ->
            { english = "Report Type"
            , kinyarwanda = Nothing
            , kirundi = Nothing
            }

        ResolveMonth short month ->
            translateMonth month short

        Result ->
            { english = "Result"
            , kinyarwanda = Nothing
            , kirundi = Nothing
            }

        Save ->
            { english = "Save"
            , kinyarwanda = Just "Kubika"
            , kirundi = Just "Emeza"
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

        SelectLimitDate ->
            { english = "Limit date"
            , kinyarwanda = Nothing
            , kirundi = Nothing
            }

        SelectViewMode ->
            { english = "Please select desired view mode"
            , kinyarwanda = Nothing
            , kirundi = Nothing
            }

        Sorwathe ->
            { english = "Sorwathe"
            , kinyarwanda = Nothing
            , kirundi = Nothing
            }

        StandardPediatricVisit ->
            { english = "Standard Pediatric Visit"
            , kinyarwanda = Nothing
            , kirundi = Nothing
            }

        Stunting ->
            { english = "Stunting"
            , kinyarwanda = Nothing
            , kirundi = Nothing
            }

        StuntingModerate ->
            { english = "Stunting Moderate"
            , kinyarwanda = Nothing
            , kirundi = Nothing
            }

        StuntingSevere ->
            { english = "Stunting Severe"
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

        Total ->
            { english = "Total"
            , kinyarwanda = Nothing
            , kirundi = Nothing
            }

        ViewMode ->
            { english = "View Mode"
            , kinyarwanda = Nothing
            , kirundi = Nothing
            }

        Village ->
            { english = "Village"
            , kinyarwanda = Nothing
            , kirundi = Nothing
            }

        UnderweightModerate ->
            { english = "Underweight Moderate"
            , kinyarwanda = Nothing
            , kirundi = Nothing
            }

        UnderweightSevere ->
            { english = "Underweight Severe"
            , kinyarwanda = Nothing
            , kirundi = Nothing
            }

        Unique ->
            { english = "Unique"
            , kinyarwanda = Nothing
            , kirundi = Nothing
            }

        UniversalIntervention ->
            { english = "Universal Intervention"
            , kinyarwanda = Nothing
            , kirundi = Nothing
            }

        WastingModerate ->
            { english = "Wasting Moderate"
            , kinyarwanda = Nothing
            , kirundi = Nothing
            }

        WastingSevere ->
            { english = "Wasting Severe"
            , kinyarwanda = Nothing
            , kirundi = Nothing
            }

        WideScopeNote ->
            { english = "Note: Selected scope contains large number of patients, so it may take SEVERAL MINUTES to generate the report. Please wait patiently."
            , kinyarwanda = Nothing
            , kirundi = Nothing
            }

        Year year ->
            { english = String.fromInt year
            , kinyarwanda = Nothing
            , kirundi = Nothing
            }

        YearLabel ->
            { english = "Year"
            , kinyarwanda = Just "Umwaka"
            , kirundi = Just "Umwaka"
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


translateMonthYY : Month -> Int -> Bool -> TranslationSet
translateMonthYY month year short =
    translateMonth month short
        |> (\set ->
                { english = String.fromInt year ++ " " ++ set.english
                , kinyarwanda = Maybe.map (\kinyarwanda -> String.fromInt year ++ " " ++ kinyarwanda) set.kinyarwanda
                , kirundi = Maybe.map (\kirundi -> String.fromInt year ++ " " ++ kirundi) set.kirundi
                }
           )


translateMonth : Month -> Bool -> TranslationSet
translateMonth month short =
    case month of
        Jan ->
            if short then
                { english = "Jan"
                , kinyarwanda = Just "Mut"
                , kirundi = Just "Nze"
                }

            else
                { english = "January"
                , kinyarwanda = Just "Mutarama"
                , kirundi = Just "Nzero"
                }

        Feb ->
            if short then
                { english = "Feb"
                , kinyarwanda = Just "Gas"
                , kirundi = Just "Ruh"
                }

            else
                { english = "February"
                , kinyarwanda = Just "Gashyantare"
                , kirundi = Just "Ruhuhuma"
                }

        Mar ->
            if short then
                { english = "Mar"
                , kinyarwanda = Just "Wer"
                , kirundi = Just "Ntw"
                }

            else
                { english = "March"
                , kinyarwanda = Just "Werurwe"
                , kirundi = Just "Ntwarante"
                }

        Apr ->
            if short then
                { english = "Apr"
                , kinyarwanda = Just "Mat"
                , kirundi = Just "Nda"
                }

            else
                { english = "April"
                , kinyarwanda = Just "Mata"
                , kirundi = Just "Ndamukiza"
                }

        May ->
            if short then
                { english = "May"
                , kinyarwanda = Just "Gic"
                , kirundi = Just "Rus"
                }

            else
                { english = "May"
                , kinyarwanda = Just "Gicurasi"
                , kirundi = Just "Rusama"
                }

        Jun ->
            if short then
                { english = "Jun"
                , kinyarwanda = Just "Kam"
                , kirundi = Just "Ruh"
                }

            else
                { english = "June"
                , kinyarwanda = Just "Kamena"
                , kirundi = Just "Ruheshi"
                }

        Jul ->
            if short then
                { english = "Jul"
                , kinyarwanda = Just "Nya"
                , kirundi = Just "Muk"
                }

            else
                { english = "July"
                , kinyarwanda = Just "Nyakanga"
                , kirundi = Just "Mukakaro"
                }

        Aug ->
            if short then
                { english = "Aug"
                , kinyarwanda = Just "Kan"
                , kirundi = Just "Mya"
                }

            else
                { english = "August"
                , kinyarwanda = Just "Kanama"
                , kirundi = Just "Myandagaro"
                }

        Sep ->
            if short then
                { english = "Sep"
                , kinyarwanda = Just "Nze"
                , kirundi = Just "Nya"
                }

            else
                { english = "September"
                , kinyarwanda = Just "Nzeri"
                , kirundi = Just "Nyakanga"
                }

        Oct ->
            if short then
                { english = "Oct"
                , kinyarwanda = Just "Ukw"
                , kirundi = Just "Git"
                }

            else
                { english = "October"
                , kinyarwanda = Just "Ukwakira"
                , kirundi = Just "Gitugutu"
                }

        Nov ->
            if short then
                { english = "Nov"
                , kinyarwanda = Just "Ukw"
                , kirundi = Just "Muny"
                }

            else
                { english = "November"
                , kinyarwanda = Just "Ugushyingo"
                , kirundi = Just "Munyonyo"
                }

        Dec ->
            if short then
                { english = "Dec"
                , kinyarwanda = Just "Uku"
                , kirundi = Just "Kig"
                }

            else
                { english = "December"
                , kinyarwanda = Just "Ukuboza"
                , kirundi = Just "Kigarama"
                }
