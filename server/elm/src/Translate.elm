module Translate exposing
    ( StringIdHttpError(..)
    , TranslationId(..)
    , translate
    )

import App.Types exposing (Language(..))
import Backend.Completion.Model
    exposing
        ( AcuteIllnessActivity(..)
        , ChildScoreboardActivity(..)
        , HIVActivity(..)
        , HomeVisitActivity(..)
        , NCDActivity(..)
        , NutritionChildActivity(..)
        , NutritionMotherActivity(..)
        , PrenatalActivity(..)
        , TakenBy(..)
        , TuberculosisActivity(..)
        , WellChildActivity(..)
        )
import Backend.Reports.Model exposing (AcuteIllnessDiagnosis(..), NutritionReportTableType(..))
import Backend.Scoreboard.Model
import Date
import Pages.Completion.Model
import Pages.Components.Types exposing (PopulationSelectionOption(..))
import Pages.Reports.Model exposing (ReportType(..))
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
    | Activity
    | AcuteIllness
    | AcuteIllnessActivity AcuteIllnessActivity
    | AcuteIllnessDiagnosis AcuteIllnessDiagnosis
    | AcuteIllnessTotal
    | AcuteMalnutrition
    | AggregatedChildScoreboard
    | Antenatal
    | All
    | ANCNewborn
    | ANCTotal
    | Any
    | Caring
    | CBNP
    | Cell
    | ChildScorecard
    | ChildScoreboardActivity ChildScoreboardActivity
    | CHW
    | Colline
    | CollineSub
    | Commune
    | Completed
    | CompletionReportType Pages.Completion.Model.ReportType
    | CoreExam
    | DangerSigns
    | Diagnosis
    | Diagnostics
    | District
    | Demographics
    | DownloadCSV
    | EmptyString
    | Encounters
    | EncounterType
    | Expected
    | FamilyPlanning
    | FBF
    | Feeding
    | Female
    | FollowUp
    | FoodSecurity
    | GenerateReport
    | Global
    | HC
    | HealthCenter
    | HealthEducation
    | HIV
    | HIVActivity HIVActivity
    | HIVTest
    | HomeVisit
    | HomeVisitActivity HomeVisitActivity
    | HttpError StringIdHttpError
    | Hygiene
    | ImmunisationBCG
    | ImmunisationDTP
    | ImmunisationDTPSA
    | ImmunisationHPV
    | ImmunisationIPV
    | ImmunisationMR
    | ImmunisationOPV
    | ImmunisationPCV13
    | ImmunisationRotarix
    | Impacted
    | IncidenceByMonthOneVisitOrMore
    | IncidenceByMonthTwoVisitsOrMore
    | IncidenceByQuarterOneVisitOrMore
    | IncidenceByQuarterTwoVisitsOrMore
    | IncidenceByYearOneVisitOrMore
    | IncidenceByYearTwoVisitsOrMore
    | Individual
    | InfrastructureEnvironmentWash
    | LoadData
    | Male
    | Medication
    | MedicationDistribution
    | Month Month
    | MonthLabel
    | MonthYear Int Int Bool
    | NCD
    | NCDA
    | NCDActivity NCDActivity
    | NCDADemographicsItemLabel NCDADemographicsItem
    | NCDAAcuteMalnutritionItemLabel NCDAAcuteMalnutritionItem
    | NCDAStuntingItemLabel NCDAStuntingItem
    | NCDAANCNewbornItemLabel NCDAANCNewbornItem
    | NCDAInfrastructureEnvironmentWashItemLabel NCDAInfrastructureEnvironmentWashItem
    | NCDANutritionBehaviorItemLabel NCDANutritionBehaviorItem
    | NCDATargetedInterventionsItemLabel NCDATargetedInterventionsItem
    | NCDAUniversalInterventionItemLabel NCDAUniversalInterventionItem
    | NewbornExam
    | NewScope
    | NewSelection
    | NoDiagnosis
    | NumberOfVisits Int
    | NumberOfVisitsLabel
    | Nutrition
    | NutritionChildActivity NutritionChildActivity
    | NutritionMotherActivity NutritionMotherActivity
    | NutritionBehavior
    | NutritionIndividual
    | NutritionGroup
    | NutritionReportTableType NutritionReportTableType
    | NutritionTotal
    | OutsideCare
    | Photo
    | PleaseWaitMessage
    | PMTCT
    | PopulationSelectionOption PopulationSelectionOption
    | PregnanciesActive
    | PregnanciesAll
    | PregnanciesCompleted
    | PregnancyTest
    | PrenatalActivity PrenatalActivity
    | PrevalenceByMonthOneVisitOrMore
    | PrevalenceByMonthTwoVisitsOrMore
    | Province
    | QuarterYear Int Int
    | RandomBloodSugarTest
    | RandomBloodSugarTestResult
    | Referral
    | Registered
    | RegisteredPatients
    | ReportType ReportType
    | ReportTypeLabel
    | ResolveMonth Bool Month
    | Save
    | Scope
    | Sector
    | SelectedEntity Backend.Scoreboard.Model.SelectedEntity
    | SelectedScope Backend.Reports.Model.SelectedEntity
    | SelectLimitDate
    | SelectStartDate
    | SelectScope
    | SelectViewMode
    | SocialHistory
    | Sorwathe
    | StandardPediatricVisit
    | Stunting
    | StuntingModerate
    | StuntingSevere
    | Status
    | SymptomsReview
    | TakenBy TakenBy
    | TakenByLabel
    | TargetedInterventions
    | Total
    | TreatmentReview
    | Tuberculosis
    | TuberculosisActivity TuberculosisActivity
    | Vitals
    | ViewMode
    | Village
    | UnderweightModerate
    | UnderweightSevere
    | Unique
    | UniversalIntervention
    | UrineDipstickTest
    | UrineDipstickTestResult
    | WastingModerate
    | WastingSevere
    | WellChildActivity WellChildActivity
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

        Activity ->
            { english = "Activity"
            , kinyarwanda = Nothing
            , kirundi = Nothing
            }

        AcuteIllness ->
            { english = "Acute Illness"
            , kinyarwanda = Nothing
            , kirundi = Nothing
            }

        AcuteIllnessActivity activity ->
            case activity of
                AcuteIllnessAcuteFindings ->
                    { english = "Acute Findings"
                    , kinyarwanda = Nothing
                    , kirundi = Nothing
                    }

                AcuteIllnessContactsTracing ->
                    { english = "Contacts Tracing"
                    , kinyarwanda = Nothing
                    , kirundi = Nothing
                    }

                AcuteIllnessCoreExam ->
                    translationSet CoreExam

                AcuteIllnessDangerSigns ->
                    translationSet DangerSigns

                AcuteIllnessFollowUp ->
                    translationSet FollowUp

                AcuteIllnessMUAC ->
                    { english = "MUAC"
                    , kinyarwanda = Nothing
                    , kirundi = Nothing
                    }

                AcuteIllnessNutrition ->
                    translationSet Nutrition

                AcuteIllnessVitals ->
                    translationSet Vitals

                AcuteIllnessCall114 ->
                    { english = "Call 114"
                    , kinyarwanda = Nothing
                    , kirundi = Nothing
                    }

                AcuteIllnessCOVIDTesting ->
                    { english = "COVID Testing"
                    , kinyarwanda = Nothing
                    , kirundi = Nothing
                    }

                AcuteIllnessExposure ->
                    { english = "Exposure"
                    , kinyarwanda = Nothing
                    , kirundi = Nothing
                    }

                AcuteIllnessContactHC ->
                    { english = "Contact HC"
                    , kinyarwanda = Nothing
                    , kirundi = Nothing
                    }

                AcuteIllnessHealthEducation ->
                    translationSet HealthEducation

                AcuteIllnessIsolation ->
                    { english = "Isolation"
                    , kinyarwanda = Nothing
                    , kirundi = Nothing
                    }

                AcuteIllnessMalariaTesting ->
                    { english = "Malaria Testing"
                    , kinyarwanda = Nothing
                    , kirundi = Nothing
                    }

                AcuteIllnessMedicationDistribution ->
                    translationSet MedicationDistribution

                AcuteIllnessSendToHC ->
                    { english = "Referal"
                    , kinyarwanda = Nothing
                    , kirundi = Nothing
                    }

                AcuteIllnessSymptomsGeneral ->
                    { english = "Symptoms General"
                    , kinyarwanda = Nothing
                    , kirundi = Nothing
                    }

                AcuteIllnessSymptomsGI ->
                    { english = "Symptoms GI"
                    , kinyarwanda = Nothing
                    , kirundi = Nothing
                    }

                AcuteIllnessSymptomsRespiratory ->
                    { english = "Symptoms Respiratory"
                    , kinyarwanda = Nothing
                    , kirundi = Nothing
                    }

                AcuteIllnessTravelHistory ->
                    { english = "Travel History"
                    , kinyarwanda = Nothing
                    , kirundi = Nothing
                    }

                AcuteIllnessPriorTreatment ->
                    { english = "Prior Treatment"
                    , kinyarwanda = Nothing
                    , kirundi = Nothing
                    }

                AcuteIllnessOngoingTreatment ->
                    { english = "Ongoing Treatment"
                    , kinyarwanda = Nothing
                    , kirundi = Nothing
                    }

        AcuteIllnessDiagnosis diagnosis ->
            case diagnosis of
                DiagnosisCovid19Suspect ->
                    { english = "Suspected COVID-19"
                    , kinyarwanda = Just "Aracyekwaho indwara ya COVID-19"
                    , kirundi = Just "Hiketswe umugera wa COVID-19"
                    }

                DiagnosisSevereCovid19 ->
                    { english = "Severe COVID-19"
                    , kinyarwanda = Just "Uburwayi bwa Covid-19 bukabije"
                    , kirundi = Just "COVID-19 ikaze"
                    }

                DiagnosisPneuminialCovid19 ->
                    { english = "COVID-19 with signs of Pneumonia"
                    , kinyarwanda = Just "Uburwayi bwa Covid-19 hamwe n'ibimenyetso by'Umusonga"
                    , kirundi = Just "Virisi ya Korona - 19 n'ibimenyetso vy'umusonga"
                    }

                DiagnosisLowRiskCovid19 ->
                    { english = "Simple COVID-19"
                    , kinyarwanda = Just "Uburwayi bwa Covid-19 bworoheje"
                    , kirundi = Just "Korona (COVID-19) isanzwe"
                    }

                DiagnosisMalariaComplicated ->
                    { english = "Complicated Malaria"
                    , kinyarwanda = Just "Malariya y'igikatu"
                    , kirundi = Just "Malariya ikomeye"
                    }

                DiagnosisMalariaUncomplicated ->
                    { english = "Uncomplicated Malaria"
                    , kinyarwanda = Just "Malariya yoroheje"
                    , kirundi = Just "Malariya yoroshe/isanzwe"
                    }

                DiagnosisMalariaUncomplicatedAndPregnant ->
                    { english = "Uncomplicated Malaria"
                    , kinyarwanda = Just "Malariya yoroheje"
                    , kirundi = Just "Malariya yoroshe/isanzwe"
                    }

                DiagnosisGastrointestinalInfectionComplicated ->
                    { english = "Gastrointestinal Infection with Complications"
                    , kinyarwanda = Just "Indwara yo mu nda ikabije"
                    , kirundi = Just "Ingwara yo mu mara/m'umushishito hamwe n'ingorane zijanye nazo"
                    }

                DiagnosisGastrointestinalInfectionUncomplicated ->
                    { english = "Gastrointestinal Infection without Complications"
                    , kinyarwanda = Just "Indwara yo mu nda yoroheje"
                    , kirundi = Just "Ingwara yo mu mara/m'umushishito ata ngorane zijanye nazo"
                    }

                DiagnosisSimpleColdAndCough ->
                    { english = "Simple Cold and Cough"
                    , kinyarwanda = Just "Ibicurane n'inkorora byoroheje"
                    , kirundi = Just "Imbeho hamwe n'inkorora biswnzwe"
                    }

                DiagnosisRespiratoryInfectionComplicated ->
                    { english = "Acute Respiratory Infection with Complications"
                    , kinyarwanda = Just "Indwara y'ubuhumekero ikabije"
                    , kirundi = Just "Ingwara yo guhema nabi ibabaje/uguhema nabi bibabaje hamwe n'ingorane bijanye"
                    }

                DiagnosisRespiratoryInfectionUncomplicated ->
                    { english = "Uncomplicated Pneumonia"
                    , kinyarwanda = Just "Umusonga woroheje"
                    , kirundi = Just "Hiketswe ingwara y'umusonga igoye"
                    }

                DiagnosisFeverOfUnknownOrigin ->
                    { english = "Fever of Unknown Origin"
                    , kinyarwanda = Just "Umuriro utazi icyawuteye"
                    , kirundi = Just "Ubushuhe bitazwi iyo bwazananye"
                    }

                DiagnosisUndeterminedMoreEvaluationNeeded ->
                    { english = "Undetermined - More Evaluation Needed"
                    , kinyarwanda = Just "Ntibisobanutse - Hakenewe Isuzuma Ryimbitse"
                    , kirundi = Just "Ntibimenyekana - Isuzuma ryinshi rirakenewe"
                    }

                DiagnosisTuberculosisSuspect ->
                    { english = "Tuberculosis Suspect"
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

        Antenatal ->
            { english = "Antenatal"
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

        Any ->
            { english = "Any"
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

        Diagnosis ->
            { english = "Diagnosis"
            , kinyarwanda = Just "Uburwayi bwabonetse"
            , kirundi = Just "Isuzumwa"
            }

        Commune ->
            { english = "Commune"
            , kinyarwanda = Nothing
            , kirundi = Nothing
            }

        Completed ->
            { english = "Completed"
            , kinyarwanda = Nothing
            , kirundi = Nothing
            }

        CompletionReportType reportType ->
            case reportType of
                Pages.Completion.Model.ReportAcuteIllness ->
                    translationSet AcuteIllness

                Pages.Completion.Model.ReportChildScoreboard ->
                    translationSet ChildScorecard

                Pages.Completion.Model.ReportHIV ->
                    translationSet HIV

                Pages.Completion.Model.ReportHomeVisit ->
                    translationSet HomeVisit

                Pages.Completion.Model.ReportNCD ->
                    translationSet NCD

                Pages.Completion.Model.ReportNewbornExam ->
                    translationSet NewbornExam

                Pages.Completion.Model.ReportNutritionGroup ->
                    translationSet NutritionGroup

                Pages.Completion.Model.ReportNutritionIndividual ->
                    translationSet NutritionIndividual

                Pages.Completion.Model.ReportPrenatal ->
                    translationSet Antenatal

                Pages.Completion.Model.ReportTuberculosis ->
                    translationSet Tuberculosis

                Pages.Completion.Model.ReportWellChild ->
                    translationSet StandardPediatricVisit

        CBNP ->
            { english = "CBNP"
            , kinyarwanda = Nothing
            , kirundi = Nothing
            }

        Caring ->
            { english = "Caring"
            , kinyarwanda = Nothing
            , kirundi = Nothing
            }

        Cell ->
            { english = "Cell"
            , kinyarwanda = Nothing
            , kirundi = Nothing
            }

        ChildScorecard ->
            { english = "Child Scorecard"
            , kinyarwanda = Nothing
            , kirundi = Nothing
            }

        ChildScoreboardActivity activity ->
            case activity of
                ChildScoreboardNCDA ->
                    translationSet NCDA

                ChildScoreboardBCGImmunisation ->
                    translationSet ImmunisationBCG

                ChildScoreboardDTPImmunisation ->
                    translationSet ImmunisationDTP

                ChildScoreboardDTPSAImmunisation ->
                    translationSet ImmunisationDTPSA

                ChildScoreboardIPVImmunisation ->
                    translationSet ImmunisationIPV

                ChildScoreboardMRImmunisation ->
                    translationSet ImmunisationMR

                ChildScoreboardOPVImmunisation ->
                    translationSet ImmunisationOPV

                ChildScoreboardPCV13Immunisation ->
                    translationSet ImmunisationPCV13

                ChildScoreboardRotarixImmunisation ->
                    translationSet ImmunisationRotarix

        CHW ->
            { english = "CHW"
            , kinyarwanda = Nothing
            , kirundi = Nothing
            }

        CoreExam ->
            { english = "Core Exam"
            , kinyarwanda = Nothing
            , kirundi = Nothing
            }

        DangerSigns ->
            { english = "Danger Signs"
            , kinyarwanda = Nothing
            , kirundi = Nothing
            }

        Diagnostics ->
            { english = "Diagnostics"
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

        DownloadCSV ->
            { english = "Download CSV"
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

        Expected ->
            { english = "Expected"
            , kinyarwanda = Nothing
            , kirundi = Nothing
            }

        FamilyPlanning ->
            { english = "Family Planning"
            , kinyarwanda = Nothing
            , kirundi = Nothing
            }

        FBF ->
            { english = "FBF"
            , kinyarwanda = Nothing
            , kirundi = Nothing
            }

        Feeding ->
            { english = "Feeding"
            , kinyarwanda = Nothing
            , kirundi = Nothing
            }

        Female ->
            { english = "Female"
            , kinyarwanda = Nothing
            , kirundi = Nothing
            }

        Global ->
            { english = "Global"
            , kinyarwanda = Nothing
            , kirundi = Nothing
            }

        FollowUp ->
            { english = "Follow Up"
            , kinyarwanda = Nothing
            , kirundi = Nothing
            }

        FoodSecurity ->
            { english = "Food Security"
            , kinyarwanda = Nothing
            , kirundi = Nothing
            }

        GenerateReport ->
            { english = "Generate Report"
            , kinyarwanda = Nothing
            , kirundi = Nothing
            }

        HC ->
            { english = "HC"
            , kinyarwanda = Nothing
            , kirundi = Nothing
            }

        HealthCenter ->
            { english = "Health Center"
            , kinyarwanda = Nothing
            , kirundi = Nothing
            }

        HealthEducation ->
            { english = "Health Education"
            , kinyarwanda = Nothing
            , kirundi = Nothing
            }

        HIV ->
            { english = "HIV"
            , kinyarwanda = Nothing
            , kirundi = Nothing
            }

        HIVActivity activity ->
            case activity of
                HIVDiagnostics ->
                    translationSet Diagnostics

                HIVFollowUp ->
                    translationSet FollowUp

                HIVHealthEducation ->
                    translationSet HealthEducation

                HIVMedication ->
                    translationSet Medication

                HIVReferral ->
                    translationSet Referral

                HIVSymptomReview ->
                    translationSet SymptomsReview

                HIVTreatmentReview ->
                    translationSet TreatmentReview

        HIVTest ->
            { english = "HIV Test"
            , kinyarwanda = Nothing
            , kirundi = Nothing
            }

        HomeVisit ->
            { english = "Home Visit"
            , kinyarwanda = Nothing
            , kirundi = Nothing
            }

        HomeVisitActivity activity ->
            case activity of
                HomeVisitCaring ->
                    translationSet Caring

                HomeVisitFeeding ->
                    translationSet Feeding

                HomeVisitFoodSecurity ->
                    translationSet FoodSecurity

                HomeVisitHygiene ->
                    translationSet Hygiene

        HttpError val ->
            translateHttpError val

        Hygiene ->
            { english = "Hygiene"
            , kinyarwanda = Nothing
            , kirundi = Nothing
            }

        ImmunisationBCG ->
            { english = "BCG Immunisation"
            , kinyarwanda = Nothing
            , kirundi = Nothing
            }

        ImmunisationDTP ->
            { english = "DTP Immunisation"
            , kinyarwanda = Nothing
            , kirundi = Nothing
            }

        ImmunisationDTPSA ->
            { english = "DTP Standalone Immunisation"
            , kinyarwanda = Nothing
            , kirundi = Nothing
            }

        ImmunisationHPV ->
            { english = "HPV Immunisation"
            , kinyarwanda = Nothing
            , kirundi = Nothing
            }

        ImmunisationIPV ->
            { english = "IPV Immunisation"
            , kinyarwanda = Nothing
            , kirundi = Nothing
            }

        ImmunisationMR ->
            { english = "MR Immunisation"
            , kinyarwanda = Nothing
            , kirundi = Nothing
            }

        ImmunisationOPV ->
            { english = "OPV Immunisation"
            , kinyarwanda = Nothing
            , kirundi = Nothing
            }

        ImmunisationPCV13 ->
            { english = "PCV13 Immunisation"
            , kinyarwanda = Nothing
            , kirundi = Nothing
            }

        ImmunisationRotarix ->
            { english = "Rotarix Immunisation"
            , kinyarwanda = Nothing
            , kirundi = Nothing
            }

        Impacted ->
            { english = "Impacted (2+ visits)"
            , kinyarwanda = Nothing
            , kirundi = Nothing
            }

        IncidenceByMonthOneVisitOrMore ->
            { english = "Incidence by month - one visit or more"
            , kinyarwanda = Nothing
            , kirundi = Nothing
            }

        IncidenceByMonthTwoVisitsOrMore ->
            { english = "Incidence by month - two visits or more"
            , kinyarwanda = Nothing
            , kirundi = Nothing
            }

        IncidenceByQuarterOneVisitOrMore ->
            { english = "Incidence by quarter - one visit or more"
            , kinyarwanda = Nothing
            , kirundi = Nothing
            }

        IncidenceByQuarterTwoVisitsOrMore ->
            { english = "Incidence by quarter - two visits or more"
            , kinyarwanda = Nothing
            , kirundi = Nothing
            }

        IncidenceByYearOneVisitOrMore ->
            { english = "Incidence by year - one visit or more"
            , kinyarwanda = Nothing
            , kirundi = Nothing
            }

        IncidenceByYearTwoVisitsOrMore ->
            { english = "Incidence by year - two visits or more"
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

        LoadData ->
            { english = "Load Data"
            , kinyarwanda = Nothing
            , kirundi = Nothing
            }

        Male ->
            { english = "Male"
            , kinyarwanda = Nothing
            , kirundi = Nothing
            }

        Medication ->
            { english = "Medication"
            , kinyarwanda = Nothing
            , kirundi = Nothing
            }

        MedicationDistribution ->
            { english = "Medication Distribution"
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

        NCD ->
            { english = "NCD"
            , kinyarwanda = Nothing
            , kirundi = Nothing
            }

        NCDA ->
            { english = "NCDA"
            , kinyarwanda = Nothing
            , kirundi = Nothing
            }

        NCDActivity activity ->
            case activity of
                NCDCoreExam ->
                    translationSet CoreExam

                NCDCoMorbidities ->
                    { english = "Co-Morbidities"
                    , kinyarwanda = Nothing
                    , kirundi = Nothing
                    }

                NCDCreatinineTest ->
                    { english = "Creatinine Test"
                    , kinyarwanda = Nothing
                    , kirundi = Nothing
                    }

                NCDDangerSigns ->
                    translationSet DangerSigns

                NCDFamilyHistory ->
                    { english = "Family History"
                    , kinyarwanda = Nothing
                    , kirundi = Nothing
                    }

                NCDFamilyPlanning ->
                    translationSet FamilyPlanning

                NCDHba1cTest ->
                    { english = "HBA1C Test"
                    , kinyarwanda = Nothing
                    , kirundi = Nothing
                    }

                NCDHealthEducation ->
                    translationSet HealthEducation

                NCDHIVTest ->
                    translationSet HIVTest

                NCDLipidPanelTest ->
                    { english = "Lipid Panel Test"
                    , kinyarwanda = Nothing
                    , kirundi = Nothing
                    }

                NCDLiverFunctionTest ->
                    { english = "Liver Function Test"
                    , kinyarwanda = Nothing
                    , kirundi = Nothing
                    }

                NCDMedicationDistribution ->
                    translationSet MedicationDistribution

                NCDMedicationHistory ->
                    { english = "Medication History"
                    , kinyarwanda = Nothing
                    , kirundi = Nothing
                    }

                NCDOutsideCare ->
                    translationSet OutsideCare

                NCDPregnancyTest ->
                    translationSet PregnancyTest

                NCDRandomBloodSugarTest ->
                    translationSet RandomBloodSugarTest

                NCDReferral ->
                    translationSet Referral

                NCDSocialHistory ->
                    translationSet SocialHistory

                NCDSymptomReview ->
                    translationSet SymptomsReview

                NCDUrineDipstickTest ->
                    translationSet UrineDipstickTest

                NCDVitals ->
                    translationSet Vitals

                NCDCreatinineTestResult ->
                    { english = "Creatinine Result"
                    , kinyarwanda = Nothing
                    , kirundi = Nothing
                    }

                NCDLipidPanelTestResult ->
                    { english = "Lipid Panel Result"
                    , kinyarwanda = Nothing
                    , kirundi = Nothing
                    }

                NCDLiverFunctionTestResult ->
                    { english = "Liver Function Result"
                    , kinyarwanda = Nothing
                    , kirundi = Nothing
                    }

                NCDRandomBloodSugarTestResult ->
                    translationSet RandomBloodSugarTestResult

                NCDUrineDipstickTestResult ->
                    translationSet UrineDipstickTestResult

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

        NewbornExam ->
            { english = "Newborn Exam"
            , kinyarwanda = Nothing
            , kirundi = Nothing
            }

        NewScope ->
            { english = "New Scope"
            , kinyarwanda = Nothing
            , kirundi = Nothing
            }

        NewSelection ->
            { english = "New Selection"
            , kinyarwanda = Nothing
            , kirundi = Nothing
            }

        NoDiagnosis ->
            { english = "No Diagnosis"
            , kinyarwanda = Nothing
            , kirundi = Just "Nta Gupima/gusuzuma"
            }

        NumberOfVisits number ->
            if number == 1 then
                { english = "1 visit"
                , kinyarwanda = Nothing
                , kirundi = Nothing
                }

            else if number > 5 then
                { english = "5+ visits"
                , kinyarwanda = Nothing
                , kirundi = Nothing
                }

            else
                { english = String.fromInt number ++ " visits"
                , kinyarwanda = Nothing
                , kirundi = Nothing
                }

        NumberOfVisitsLabel ->
            { english = "# Visits"
            , kinyarwanda = Nothing
            , kirundi = Nothing
            }

        Nutrition ->
            { english = "Nutrition"
            , kinyarwanda = Nothing
            , kirundi = Nothing
            }

        NutritionChildActivity activity ->
            case activity of
                NutritionHeight ->
                    { english = "Height"
                    , kinyarwanda = Nothing
                    , kirundi = Nothing
                    }

                NutritionNutrition ->
                    translationSet Nutrition

                NutritionPhoto ->
                    translationSet Photo

                NutritionWeight ->
                    { english = "Weight"
                    , kinyarwanda = Nothing
                    , kirundi = Nothing
                    }

                NutritionMUAC ->
                    { english = "MUAC"
                    , kinyarwanda = Nothing
                    , kirundi = Nothing
                    }

                NutritionContributingFactors ->
                    { english = "Contributing Factors"
                    , kinyarwanda = Nothing
                    , kirundi = Nothing
                    }

                NutritionFollowUp ->
                    translationSet FollowUp

                NutritionHealthEducation ->
                    translationSet HealthEducation

                NutritionSendToHC ->
                    translationSet Referral

                NutritionNCDA ->
                    { english = "NCDA"
                    , kinyarwanda = Nothing
                    , kirundi = Nothing
                    }

                NutritionChildFbf ->
                    { english = "Child FBF"
                    , kinyarwanda = Nothing
                    , kirundi = Nothing
                    }

        NutritionMotherActivity activity ->
            case activity of
                NutritionFamilyPlanning ->
                    translationSet FamilyPlanning

                NutritionLactation ->
                    { english = "Lactation"
                    , kinyarwanda = Nothing
                    , kirundi = Nothing
                    }

                NutritionMotherFbf ->
                    { english = "Mother FBF"
                    , kinyarwanda = Nothing
                    , kirundi = Nothing
                    }

        NutritionBehavior ->
            { english = "Nutrition Behavior"
            , kinyarwanda = Nothing
            , kirundi = Nothing
            }

        NutritionIndividual ->
            { english = "Nutrition Individual"
            , kinyarwanda = Nothing
            , kirundi = Nothing
            }

        NutritionGroup ->
            { english = "Nutrition Group"
            , kinyarwanda = Nothing
            , kirundi = Nothing
            }

        NutritionReportTableType tableType ->
            case tableType of
                NutritionTablePrevalanceOneOrMore ->
                    translationSet PrevalenceByMonthOneVisitOrMore

                NutritionTablePrevalanceTwoOrMore ->
                    translationSet PrevalenceByMonthTwoVisitsOrMore

                NutritionTableIncidenceMonthOneOrMore ->
                    translationSet IncidenceByMonthOneVisitOrMore

                NutritionTableIncidenceMonthTwoOrMore ->
                    translationSet IncidenceByMonthTwoVisitsOrMore

                NutritionTableIncidenceQuarterOneOrMore ->
                    translationSet IncidenceByQuarterOneVisitOrMore

                NutritionTableIncidenceQuarterTwoOrMore ->
                    translationSet IncidenceByQuarterTwoVisitsOrMore

                NutritionTableIncidenceYearOneOrMore ->
                    translationSet IncidenceByYearOneVisitOrMore

                NutritionTableIncidenceYearTwoOrMore ->
                    translationSet IncidenceByYearTwoVisitsOrMore

        NutritionTotal ->
            { english = "Nutrition (total)"
            , kinyarwanda = Nothing
            , kirundi = Nothing
            }

        OutsideCare ->
            { english = "Outside Care"
            , kinyarwanda = Nothing
            , kirundi = Nothing
            }

        Photo ->
            { english = "Photo"
            , kinyarwanda = Nothing
            , kirundi = Nothing
            }

        PleaseWaitMessage ->
            { english = "This action may take several minutes to complete."
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
                    { english = "Entire Population"
                    , kinyarwanda = Nothing
                    , kirundi = Nothing
                    }

                SelectionOptionDemographics ->
                    { english = "Demographic Region"
                    , kinyarwanda = Nothing
                    , kirundi = Nothing
                    }

                SelectionOptionHealthCenter ->
                    { english = "Health Center Catchment"
                    , kinyarwanda = Nothing
                    , kirundi = Nothing
                    }

        PregnanciesActive ->
            { english = "Active Pregnancies"
            , kinyarwanda = Nothing
            , kirundi = Nothing
            }

        PregnanciesAll ->
            { english = "All Pregnancies"
            , kinyarwanda = Nothing
            , kirundi = Nothing
            }

        PregnanciesCompleted ->
            { english = "Completed Pregnancies"
            , kinyarwanda = Nothing
            , kirundi = Nothing
            }

        PregnancyTest ->
            { english = "Pregnancy Test"
            , kinyarwanda = Nothing
            , kirundi = Nothing
            }

        PrenatalActivity activity ->
            case activity of
                PrenatalAppointmentConfirmation ->
                    { english = "Appointment Confirmation"
                    , kinyarwanda = Nothing
                    , kirundi = Nothing
                    }

                PrenatalBirthPlan ->
                    { english = "Birth Plan"
                    , kinyarwanda = Nothing
                    , kirundi = Nothing
                    }

                PrenatalBloodGprsTest ->
                    { english = "Blood Group and Rhesus Test"
                    , kinyarwanda = Nothing
                    , kirundi = Nothing
                    }

                PrenatalBloodGprsTestResult ->
                    { english = "Blood Group and Rhesus Test Result"
                    , kinyarwanda = Nothing
                    , kirundi = Nothing
                    }

                PrenatalBreastExam ->
                    { english = "Breast Exam"
                    , kinyarwanda = Nothing
                    , kirundi = Nothing
                    }

                PrenatalBreastfeeding ->
                    { english = "Breastfeeding"
                    , kinyarwanda = Nothing
                    , kirundi = Nothing
                    }

                PrenatalCorePhysicalExam ->
                    { english = "Core Physical Exam"
                    , kinyarwanda = Nothing
                    , kirundi = Nothing
                    }

                PrenatalDangerSigns ->
                    translationSet DangerSigns

                PrenatalFamilyPlanning ->
                    translationSet FamilyPlanning

                PrenatalFollowUp ->
                    translationSet FollowUp

                PrenatalGuExam ->
                    { english = "GU Exam"
                    , kinyarwanda = Nothing
                    , kirundi = Nothing
                    }

                PrenatalHealthEducation ->
                    translationSet HealthEducation

                PrenatalHemoglobinTest ->
                    { english = "Hemoglobin Test"
                    , kinyarwanda = Nothing
                    , kirundi = Nothing
                    }

                PrenatalHemoglobinTestResult ->
                    { english = "Hemoglobin Test Result"
                    , kinyarwanda = Nothing
                    , kirundi = Nothing
                    }

                PrenatalHepatitisBTest ->
                    { english = "Hepatitis B Test"
                    , kinyarwanda = Nothing
                    , kirundi = Nothing
                    }

                PrenatalHepatitisBTestResult ->
                    { english = "Hepatitis B Test Result"
                    , kinyarwanda = Nothing
                    , kirundi = Nothing
                    }

                PrenatalHIVPCRTest ->
                    { english = "HIV PCR Test"
                    , kinyarwanda = Nothing
                    , kirundi = Nothing
                    }

                PrenatalHIVPCRTestResult ->
                    { english = "HIV PCR Test Result"
                    , kinyarwanda = Nothing
                    , kirundi = Nothing
                    }

                PrenatalHIVTest ->
                    translationSet HIVTest

                PrenatalHIVTestResult ->
                    { english = "HIV Test Result"
                    , kinyarwanda = Nothing
                    , kirundi = Nothing
                    }

                PrenatalLastMenstrualPeriod ->
                    { english = "Last Menstrual Period"
                    , kinyarwanda = Nothing
                    , kirundi = Nothing
                    }

                PrenatalMalariaTest ->
                    { english = "Malaria Test"
                    , kinyarwanda = Nothing
                    , kirundi = Nothing
                    }

                PrenatalMalariaTestResult ->
                    { english = "Malaria Test Result"
                    , kinyarwanda = Nothing
                    , kirundi = Nothing
                    }

                PrenatalMedicalHistory ->
                    { english = "Medical History"
                    , kinyarwanda = Nothing
                    , kirundi = Nothing
                    }

                PrenatalMedication ->
                    translationSet Medication

                PrenatalMedicationDistribution ->
                    translationSet MedicationDistribution

                PrenatalMentalHealth ->
                    { english = "Mental Health"
                    , kinyarwanda = Nothing
                    , kirundi = Nothing
                    }

                PrenatalNutrition ->
                    translationSet Nutrition

                PrenatalObstetricalExam ->
                    { english = "Obstetrical Exam"
                    , kinyarwanda = Nothing
                    , kirundi = Nothing
                    }

                PrenatalObstetricHistory ->
                    { english = "Obstetric History"
                    , kinyarwanda = Nothing
                    , kirundi = Nothing
                    }

                PrenatalObstetricHistoryStep2 ->
                    { english = "Obstetric History Second Step"
                    , kinyarwanda = Nothing
                    , kirundi = Nothing
                    }

                PrenatalOutsideCare ->
                    translationSet OutsideCare

                PrenatalPartnerHIVTest ->
                    { english = "Partner HIV Test"
                    , kinyarwanda = Nothing
                    , kirundi = Nothing
                    }

                PrenatalPartnerHIVTestResult ->
                    { english = "Partner HIV Test Result"
                    , kinyarwanda = Nothing
                    , kirundi = Nothing
                    }

                PrenatalPhoto ->
                    translationSet Photo

                PrenatalPostpartumTreatmentReview ->
                    { english = "Postpartum Treatment Review"
                    , kinyarwanda = Nothing
                    , kirundi = Nothing
                    }

                PrenatalPregnancyOutcome ->
                    { english = "Pregnancy Outcome"
                    , kinyarwanda = Nothing
                    , kirundi = Nothing
                    }

                PrenatalPregnancyTesting ->
                    { english = "Pregnancy Testing"
                    , kinyarwanda = Nothing
                    , kirundi = Nothing
                    }

                PrenatalRandomBloodSugarTest ->
                    translationSet RandomBloodSugarTest

                PrenatalRandomBloodSugarTestResult ->
                    translationSet RandomBloodSugarTestResult

                PrenatalResource ->
                    { english = "Resource"
                    , kinyarwanda = Nothing
                    , kirundi = Nothing
                    }

                PrenatalSendToHC ->
                    translationSet Referral

                PrenatalSocialHistory ->
                    translationSet SocialHistory

                PrenatalSpecialityCare ->
                    { english = "Speciality Care"
                    , kinyarwanda = Nothing
                    , kirundi = Nothing
                    }

                PrenatalSymptomReview ->
                    translationSet SymptomsReview

                PrenatalSyphilisTest ->
                    { english = "Syphilis Test"
                    , kinyarwanda = Nothing
                    , kirundi = Nothing
                    }

                PrenatalSyphilisTestResult ->
                    { english = "Syphilis Test Result"
                    , kinyarwanda = Nothing
                    , kirundi = Nothing
                    }

                PrenatalTetanusImmunisation ->
                    { english = "Tetanus Immunisation"
                    , kinyarwanda = Nothing
                    , kirundi = Nothing
                    }

                PrenatalTreatmentReview ->
                    translationSet TreatmentReview

                PrenatalUrineDipstickTest ->
                    translationSet UrineDipstickTest

                PrenatalUrineDipstickTestResult ->
                    translationSet UrineDipstickTestResult

                PrenatalVitals ->
                    translationSet Vitals

                PrenatalVitalsRecheck ->
                    { english = "Vitals Recheck"
                    , kinyarwanda = Nothing
                    , kirundi = Nothing
                    }

        PrevalenceByMonthOneVisitOrMore ->
            { english = "Prevalence by month - one visit or more"
            , kinyarwanda = Nothing
            , kirundi = Nothing
            }

        PrevalenceByMonthTwoVisitsOrMore ->
            { english = "Prevalence by month - two visits or more"
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

        RandomBloodSugarTest ->
            { english = "Random Blood Sugar Test"
            , kinyarwanda = Nothing
            , kirundi = Nothing
            }

        RandomBloodSugarTestResult ->
            { english = "Random Blood Sugar Test Result"
            , kinyarwanda = Nothing
            , kirundi = Nothing
            }

        Referral ->
            { english = "Referral"
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
                ReportAcuteIllness ->
                    translationSet AcuteIllness

                ReportDemographics ->
                    { english = "Demographics"
                    , kinyarwanda = Nothing
                    , kirundi = Nothing
                    }

                ReportNutrition ->
                    translationSet Nutrition

                ReportPrenatal ->
                    translationSet Antenatal

        ReportTypeLabel ->
            { english = "Report Type"
            , kinyarwanda = Nothing
            , kirundi = Nothing
            }

        ResolveMonth short month ->
            translateMonth month short

        Save ->
            { english = "Save"
            , kinyarwanda = Just "Kubika"
            , kirundi = Just "Emeza"
            }

        Scope ->
            { english = "Scope"
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
                Backend.Scoreboard.Model.EntityDistrict ->
                    translationSet District

                Backend.Scoreboard.Model.EntitySector ->
                    translationSet Sector

                Backend.Scoreboard.Model.EntityCell ->
                    translationSet Cell

                Backend.Scoreboard.Model.EntityVillage ->
                    translationSet Village

        SelectedScope entity ->
            case entity of
                Backend.Reports.Model.EntityGlobal ->
                    translationSet Global

                Backend.Reports.Model.EntityHealthCenter ->
                    translationSet HealthCenter

                Backend.Reports.Model.EntityProvince ->
                    translationSet Province

                Backend.Reports.Model.EntityDistrict ->
                    translationSet District

                Backend.Reports.Model.EntitySector ->
                    translationSet Sector

                Backend.Reports.Model.EntityCell ->
                    translationSet Cell

                Backend.Reports.Model.EntityVillage ->
                    translationSet Village

        SelectLimitDate ->
            { english = "End date"
            , kinyarwanda = Nothing
            , kirundi = Nothing
            }

        SelectStartDate ->
            { english = "Start date"
            , kinyarwanda = Nothing
            , kirundi = Nothing
            }

        SelectScope ->
            { english = "Please select desired scope"
            , kinyarwanda = Nothing
            , kirundi = Nothing
            }

        SelectViewMode ->
            { english = "Please select desired view mode"
            , kinyarwanda = Nothing
            , kirundi = Nothing
            }

        SocialHistory ->
            { english = "Social History"
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

        SymptomsReview ->
            { english = "Symptoms Review"
            , kinyarwanda = Nothing
            , kirundi = Nothing
            }

        TakenBy value ->
            case value of
                TakenByNurse ->
                    { english = "Nurse"
                    , kinyarwanda = Nothing
                    , kirundi = Nothing
                    }

                TakenByCHW ->
                    { english = "CHW"
                    , kinyarwanda = Nothing
                    , kirundi = Nothing
                    }

                TakenByUnknown ->
                    { english = "Unknown"
                    , kinyarwanda = Nothing
                    , kirundi = Nothing
                    }

        TakenByLabel ->
            { english = "Taken By"
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

        TreatmentReview ->
            { english = "Treatment Review"
            , kinyarwanda = Nothing
            , kirundi = Nothing
            }

        Tuberculosis ->
            { english = "Tuberculosis"
            , kinyarwanda = Nothing
            , kirundi = Nothing
            }

        TuberculosisActivity activity ->
            case activity of
                TuberculosisDiagnostics ->
                    translationSet Diagnostics

                TuberculosisDOT ->
                    { english = "DOT"
                    , kinyarwanda = Nothing
                    , kirundi = Nothing
                    }

                TuberculosisFollowUp ->
                    translationSet FollowUp

                TuberculosisHealthEducation ->
                    translationSet HealthEducation

                TuberculosisMedication ->
                    translationSet Medication

                TuberculosisReferral ->
                    translationSet Referral

                TuberculosisSymptomReview ->
                    translationSet SymptomsReview

                TuberculosisTreatmentReview ->
                    translationSet TreatmentReview

        ViewMode ->
            { english = "View Mode"
            , kinyarwanda = Nothing
            , kirundi = Nothing
            }

        Vitals ->
            { english = "Vitals"
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

        UrineDipstickTest ->
            { english = "Urine Dipstick Test"
            , kinyarwanda = Nothing
            , kirundi = Nothing
            }

        UrineDipstickTestResult ->
            { english = "Urine Dipstick Test Result"
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

        WellChildActivity activity ->
            case activity of
                WellChildAlbendazole ->
                    { english = "Albendazole"
                    , kinyarwanda = Nothing
                    , kirundi = Nothing
                    }

                WellChildBCGImmunisation ->
                    translationSet ImmunisationBCG

                WellChildCaring ->
                    translationSet Caring

                WellChildContributingFactors ->
                    { english = "Contributing Factors"
                    , kinyarwanda = Nothing
                    , kirundi = Nothing
                    }

                WellChildDTPImmunisation ->
                    translationSet ImmunisationDTP

                WellChildDTPSAImmunisation ->
                    translationSet ImmunisationDTPSA

                WellChildECD ->
                    { english = "ECD"
                    , kinyarwanda = Nothing
                    , kirundi = Nothing
                    }

                WellChildFeeding ->
                    translationSet Feeding

                WellChildFollowUp ->
                    translationSet FollowUp

                WellChildFoodSecurity ->
                    translationSet FoodSecurity

                WellChildHeadCircumference ->
                    { english = "Head Circumference"
                    , kinyarwanda = Nothing
                    , kirundi = Nothing
                    }

                WellChildHealthEducation ->
                    translationSet HealthEducation

                WellChildHeight ->
                    { english = "Height"
                    , kinyarwanda = Nothing
                    , kirundi = Nothing
                    }

                WellChildHPVImmunisation ->
                    translationSet ImmunisationHPV

                WellChildHygiene ->
                    translationSet Hygiene

                WellChildIPVImmunisation ->
                    translationSet ImmunisationIPV

                WellChildMebendezole ->
                    { english = "Mebendezole"
                    , kinyarwanda = Nothing
                    , kirundi = Nothing
                    }

                WellChildMRImmunisation ->
                    translationSet ImmunisationMR

                WellChildMUAC ->
                    { english = "MUAC"
                    , kinyarwanda = Nothing
                    , kirundi = Nothing
                    }

                WellChildNCDA ->
                    translationSet NCDA

                WellChildNextVisit ->
                    { english = "Next Visit"
                    , kinyarwanda = Nothing
                    , kirundi = Nothing
                    }

                WellChildNutrition ->
                    translationSet Nutrition

                WellChildOPVImmunisation ->
                    translationSet ImmunisationOPV

                WellChildPCV13Immunisation ->
                    translationSet ImmunisationPCV13

                WellChildPhoto ->
                    translationSet Photo

                WellChildPregnancySummary ->
                    { english = "Pregnancy Summary"
                    , kinyarwanda = Nothing
                    , kirundi = Nothing
                    }

                WellChildRotarixImmunisation ->
                    translationSet ImmunisationRotarix

                WellChildSendToHC ->
                    translationSet Referral

                WellChildSymptomsReview ->
                    translationSet SymptomsReview

                WellChildVitals ->
                    translationSet Vitals

                WellChildVitaminA ->
                    { english = "Vitamin A"
                    , kinyarwanda = Nothing
                    , kirundi = Nothing
                    }

                WellChildWeight ->
                    { english = "Weight"
                    , kinyarwanda = Nothing
                    , kirundi = Nothing
                    }

        WideScopeNote ->
            { english = "The selected scope may contain a large number of patients and report generation could take several minutes."
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


translateMonthYY : Int -> Int -> Bool -> TranslationSet
translateMonthYY month year short =
    translateMonth (Date.numberToMonth month) short
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
