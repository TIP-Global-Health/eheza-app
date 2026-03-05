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
import Backend.Reports.Model
    exposing
        ( AcuteIllnessDiagnosis(..)
        , DeliveryLocation(..)
        , NutritionReportTableType(..)
        , PregnancyOutcome(..)
        , PrenatalDiagnosis(..)
        )
import Backend.Scoreboard.Model
import Date
import Pages.Completion.Model
import Pages.Components.Types exposing (PopulationSelectionOption(..))
import Pages.Reports.Model exposing (PregnancyTrimester(..), ReportType(..))
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

        Somali ->
            .somali set
                |> Maybe.withDefault (.english set)


type alias TranslationSet =
    { english : String
    , kinyarwanda : Maybe String
    , kirundi : Maybe String
    , somali : Maybe String
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
    | DeliveryLocation DeliveryLocation
    | DeliveryLocationsTableHeading
    | DeliveryLocationsTablePercentage
    | DeliveryLocationsTableTotals
    | Demographics
    | Diagnosis
    | Diagnostics
    | District
    | DownloadCSV
    | EmptyString
    | Encounters
    | EncounterType
    | Expected
    | FamilyPlanning
    | FBF
    | Feeding
    | Female
    | FirstVisit
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
    | Location
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
    | OutcomesTableHeading
    | OutsideCare
    | PatientsWith3OrMoreVisitsPercentage
    | PatientsWith4OrMoreVisitsPercentage
    | Photo
    | PleaseWaitMessage
    | PMTCT
    | PopulationSelectionOption PopulationSelectionOption
    | PregnanciesActive
    | PregnanciesAll
    | PregnanciesCompleted
    | PregnancyOutcome PregnancyOutcome
    | PregnancyOutcomeLabel
    | PregnancyTest
    | PregnancyTrimester PregnancyTrimester
    | PrenatalActivity PrenatalActivity
    | PrenatalDiagnosis PrenatalDiagnosis
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
    | Trimester
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
            , somali = Nothing
            }

        Activity ->
            { english = "Activity"
            , kinyarwanda = Nothing
            , kirundi = Nothing
            , somali = Nothing
            }

        AcuteIllness ->
            { english = "Acute Illness"
            , kinyarwanda = Nothing
            , kirundi = Nothing
            , somali = Nothing
            }

        AcuteIllnessActivity activity ->
            case activity of
                AcuteIllnessAcuteFindings ->
                    { english = "Acute Findings"
                    , kinyarwanda = Nothing
                    , kirundi = Nothing
                    , somali = Nothing
                    }

                AcuteIllnessContactsTracing ->
                    { english = "Contacts Tracing"
                    , kinyarwanda = Nothing
                    , kirundi = Nothing
                    , somali = Nothing
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
                    , somali = Nothing
                    }

                AcuteIllnessNutrition ->
                    translationSet Nutrition

                AcuteIllnessVitals ->
                    translationSet Vitals

                AcuteIllnessCall114 ->
                    { english = "Call 114"
                    , kinyarwanda = Nothing
                    , kirundi = Nothing
                    , somali = Nothing
                    }

                AcuteIllnessCOVIDTesting ->
                    { english = "COVID Testing"
                    , kinyarwanda = Nothing
                    , kirundi = Nothing
                    , somali = Nothing
                    }

                AcuteIllnessExposure ->
                    { english = "Exposure"
                    , kinyarwanda = Nothing
                    , kirundi = Nothing
                    , somali = Nothing
                    }

                AcuteIllnessContactHC ->
                    { english = "Contact HC"
                    , kinyarwanda = Nothing
                    , kirundi = Nothing
                    , somali = Nothing
                    }

                AcuteIllnessHealthEducation ->
                    translationSet HealthEducation

                AcuteIllnessIsolation ->
                    { english = "Isolation"
                    , kinyarwanda = Nothing
                    , kirundi = Nothing
                    , somali = Nothing
                    }

                AcuteIllnessMalariaTesting ->
                    { english = "Malaria Testing"
                    , kinyarwanda = Nothing
                    , kirundi = Nothing
                    , somali = Nothing
                    }

                AcuteIllnessMedicationDistribution ->
                    translationSet MedicationDistribution

                AcuteIllnessSendToHC ->
                    { english = "Referral"
                    , kinyarwanda = Nothing
                    , kirundi = Nothing
                    , somali = Nothing
                    }

                AcuteIllnessSymptomsGeneral ->
                    { english = "Symptoms General"
                    , kinyarwanda = Nothing
                    , kirundi = Nothing
                    , somali = Nothing
                    }

                AcuteIllnessSymptomsGI ->
                    { english = "Symptoms GI"
                    , kinyarwanda = Nothing
                    , kirundi = Nothing
                    , somali = Nothing
                    }

                AcuteIllnessSymptomsRespiratory ->
                    { english = "Symptoms Respiratory"
                    , kinyarwanda = Nothing
                    , kirundi = Nothing
                    , somali = Nothing
                    }

                AcuteIllnessTravelHistory ->
                    { english = "Travel History"
                    , kinyarwanda = Nothing
                    , kirundi = Nothing
                    , somali = Nothing
                    }

                AcuteIllnessPriorTreatment ->
                    { english = "Prior Treatment"
                    , kinyarwanda = Nothing
                    , kirundi = Nothing
                    , somali = Nothing
                    }

                AcuteIllnessOngoingTreatment ->
                    { english = "Ongoing Treatment"
                    , kinyarwanda = Nothing
                    , kirundi = Nothing
                    , somali = Nothing
                    }

        AcuteIllnessDiagnosis diagnosis ->
            case diagnosis of
                DiagnosisCovid19Suspect ->
                    { english = "Suspected COVID-19"
                    , kinyarwanda = Just "Aracyekwaho indwara ya COVID-19"
                    , kirundi = Just "Hiketswe umugera wa COVID-19"
                    , somali = Just "Looga shakisan yahay COVID-19"
                    }

                DiagnosisSevereCovid19 ->
                    { english = "Severe COVID-19"
                    , kinyarwanda = Just "Uburwayi bwa Covid-19 bukabije"
                    , kirundi = Just "COVID-19 ikaze"
                    , somali = Just "COVID-19 aad u daran"
                    }

                DiagnosisPneuminialCovid19 ->
                    { english = "COVID-19 with signs of Pneumonia"
                    , kinyarwanda = Just "Uburwayi bwa Covid-19 hamwe n'ibimenyetso by'Umusonga"
                    , kirundi = Just "Virisi ya Korona - 19 n'ibimenyetso vy'umusonga"
                    , somali = Just "COVID-19 oo leh calaamadaha Oof wareenka"
                    }

                DiagnosisLowRiskCovid19 ->
                    { english = "Simple COVID-19"
                    , kinyarwanda = Just "Uburwayi bwa Covid-19 bworoheje"
                    , kirundi = Just "Korona (COVID-19) isanzwe"
                    , somali = Just "COVID-19 Fudud"
                    }

                DiagnosisMalariaComplicated ->
                    { english = "Complicated Malaria"
                    , kinyarwanda = Just "Malariya y'igikatu"
                    , kirundi = Just "Malariya ikomeye"
                    , somali = Just "Duumo Liidata"
                    }

                DiagnosisMalariaUncomplicated ->
                    { english = "Uncomplicated Malaria"
                    , kinyarwanda = Just "Malariya yoroheje"
                    , kirundi = Just "Malariya yoroshe/isanzwe"
                    , somali = Just "Duumo aan Waxyeello lahayn"
                    }

                DiagnosisMalariaUncomplicatedAndPregnant ->
                    { english = "Uncomplicated Malaria"
                    , kinyarwanda = Just "Malariya yoroheje"
                    , kirundi = Just "Malariya yoroshe/isanzwe"
                    , somali = Just "Duumo aan Waxyeello lahayn"
                    }

                DiagnosisGastrointestinalInfectionComplicated ->
                    { english = "Gastrointestinal Infection with Complications"
                    , kinyarwanda = Just "Indwara yo mu nda ikabije"
                    , kirundi = Just "Ingwara yo mu mara/m'umushishito hamwe n'ingorane zijanye nazo"
                    , somali = Just "Caabuqa Caloosha oo leh Waxyeello"
                    }

                DiagnosisGastrointestinalInfectionUncomplicated ->
                    { english = "Gastrointestinal Infection without Complications"
                    , kinyarwanda = Just "Indwara yo mu nda yoroheje"
                    , kirundi = Just "Ingwara yo mu mara/m'umushishito ata ngorane zijanye nazo"
                    , somali = Just "Caabuqa Caloosha aan lahayn Waxyeello"
                    }

                DiagnosisSimpleColdAndCough ->
                    { english = "Simple Cold and Cough"
                    , kinyarwanda = Just "Ibicurane n'inkorora byoroheje"
                    , kirundi = Just "Imbeho hamwe n'inkorora biswnzwe"
                    , somali = Just "Qufac iyo Qarqaryo fudud"
                    }

                DiagnosisRespiratoryInfectionComplicated ->
                    { english = "Acute Respiratory Infection with Complications"
                    , kinyarwanda = Just "Indwara y'ubuhumekero ikabije"
                    , kirundi = Just "Ingwara yo guhema nabi ibabaje/uguhema nabi bibabaje hamwe n'ingorane bijanye"
                    , somali = Just "Looga Shakisan yahay Caabuqa Saableyda (leh waxyeello)"
                    }

                DiagnosisRespiratoryInfectionUncomplicated ->
                    { english = "Uncomplicated Pneumonia"
                    , kinyarwanda = Just "Umusonga woroheje"
                    , kirundi = Just "Hiketswe ingwara y'umusonga igoye"
                    , somali = Just "Oof wareen aan waxyeello lahayn`"
                    }

                DiagnosisFeverOfUnknownOrigin ->
                    { english = "Fever of Unknown Origin"
                    , kinyarwanda = Just "Umuriro utazi icyawuteye"
                    , kirundi = Just "Ubushuhe bitazwi iyo bwazananye"
                    , somali = Just "Qandho aan asalkeeda la garanayn"
                    }

                DiagnosisUndeterminedMoreEvaluationNeeded ->
                    { english = "Undetermined - More Evaluation Needed"
                    , kinyarwanda = Just "Ntibisobanutse - Hakenewe Isuzuma Ryimbitse"
                    , kirundi = Just "Ntibimenyekana - Isuzuma ryinshi rirakenewe"
                    , somali = Just "Aan la xaqiijinin - U baahan Qiimeyn dheeraad ah"
                    }

                DiagnosisTuberculosisSuspect ->
                    { english = "Tuberculosis Suspect"
                    , kinyarwanda = Nothing
                    , kirundi = Nothing
                    , somali = Nothing
                    }

        AcuteIllnessTotal ->
            { english = "Acute Illness (total)"
            , kinyarwanda = Nothing
            , kirundi = Nothing
            , somali = Nothing
            }

        AcuteMalnutrition ->
            { english = "Acute Malnutrition"
            , kinyarwanda = Nothing
            , kirundi = Nothing
            , somali = Nothing
            }

        AggregatedChildScoreboard ->
            { english = "Aggregated Child Scoreboard"
            , kinyarwanda = Nothing
            , kirundi = Nothing
            , somali = Nothing
            }

        Antenatal ->
            { english = "Antenatal"
            , kinyarwanda = Nothing
            , kirundi = Nothing
            , somali = Nothing
            }

        All ->
            { english = "All"
            , kinyarwanda = Nothing
            , kirundi = Nothing
            , somali = Nothing
            }

        ANCNewborn ->
            { english = "ANC + Newborn"
            , kinyarwanda = Nothing
            , kirundi = Nothing
            , somali = Nothing
            }

        ANCTotal ->
            { english = "ANC (total)"
            , kinyarwanda = Nothing
            , kirundi = Nothing
            , somali = Nothing
            }

        Any ->
            { english = "Any"
            , kinyarwanda = Nothing
            , kirundi = Nothing
            , somali = Nothing
            }

        Colline ->
            { english = "Colline"
            , kinyarwanda = Nothing
            , kirundi = Nothing
            , somali = Nothing
            }

        CollineSub ->
            { english = "Sub-Colline"
            , kinyarwanda = Nothing
            , kirundi = Nothing
            , somali = Nothing
            }

        Diagnosis ->
            { english = "Diagnosis"
            , kinyarwanda = Just "Uburwayi bwabonetse"
            , kirundi = Just "Isuzumwa"
            , somali = Just "Baaritaan"
            }

        Commune ->
            { english = "Commune"
            , kinyarwanda = Nothing
            , kirundi = Nothing
            , somali = Nothing
            }

        Completed ->
            { english = "Completed"
            , kinyarwanda = Nothing
            , kirundi = Nothing
            , somali = Nothing
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
            , somali = Nothing
            }

        Caring ->
            { english = "Caring"
            , kinyarwanda = Nothing
            , kirundi = Nothing
            , somali = Nothing
            }

        Cell ->
            { english = "Cell"
            , kinyarwanda = Nothing
            , kirundi = Nothing
            , somali = Nothing
            }

        ChildScorecard ->
            { english = "Child Scorecard"
            , kinyarwanda = Nothing
            , kirundi = Nothing
            , somali = Nothing
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
            , somali = Nothing
            }

        CoreExam ->
            { english = "Core Exam"
            , kinyarwanda = Nothing
            , kirundi = Nothing
            , somali = Nothing
            }

        DangerSigns ->
            { english = "Danger Signs"
            , kinyarwanda = Nothing
            , kirundi = Nothing
            , somali = Nothing
            }

        DeliveryLocation location ->
            case location of
                FacilityDelivery ->
                    { english = "Facility"
                    , kinyarwanda = Just "Ivuriro"
                    , kirundi = Just "Ikigo"
                    , somali = Just "Adeega"
                    }

                HomeDelivery ->
                    { english = "Home"
                    , kinyarwanda = Just "Mu rugo"
                    , kirundi = Just "Muhira"
                    , somali = Just "Guri"
                    }

        DeliveryLocationsTableHeading ->
            { english = "Outcome Location"
            , kinyarwanda = Nothing
            , kirundi = Nothing
            , somali = Nothing
            }

        DeliveryLocationsTablePercentage ->
            { english = "Percentage of completed pregnancies reported"
            , kinyarwanda = Nothing
            , kirundi = Nothing
            , somali = Nothing
            }

        DeliveryLocationsTableTotals ->
            { english = "Total # delivery outcomes documented"
            , kinyarwanda = Nothing
            , kirundi = Nothing
            , somali = Nothing
            }

        Demographics ->
            { english = "Demographics"
            , kinyarwanda = Nothing
            , kirundi = Nothing
            , somali = Nothing
            }

        Diagnostics ->
            { english = "Diagnostics"
            , kinyarwanda = Nothing
            , kirundi = Nothing
            , somali = Nothing
            }

        District ->
            { english = "District"
            , kinyarwanda = Nothing
            , kirundi = Nothing
            , somali = Nothing
            }

        DownloadCSV ->
            { english = "Download CSV"
            , kinyarwanda = Nothing
            , kirundi = Nothing
            , somali = Nothing
            }

        EmptyString ->
            { english = ""
            , kinyarwanda = Nothing
            , kirundi = Nothing
            , somali = Nothing
            }

        Encounters ->
            { english = "Encounters"
            , kinyarwanda = Nothing
            , kirundi = Nothing
            , somali = Nothing
            }

        EncounterType ->
            { english = "Encounter Type"
            , kinyarwanda = Nothing
            , kirundi = Nothing
            , somali = Nothing
            }

        Expected ->
            { english = "Expected"
            , kinyarwanda = Nothing
            , kirundi = Nothing
            , somali = Nothing
            }

        FamilyPlanning ->
            { english = "Family Planning"
            , kinyarwanda = Nothing
            , kirundi = Nothing
            , somali = Nothing
            }

        FBF ->
            { english = "FBF"
            , kinyarwanda = Nothing
            , kirundi = Nothing
            , somali = Nothing
            }

        Feeding ->
            { english = "Feeding"
            , kinyarwanda = Nothing
            , kirundi = Nothing
            , somali = Nothing
            }

        Female ->
            { english = "Female"
            , kinyarwanda = Nothing
            , kirundi = Nothing
            , somali = Nothing
            }

        Global ->
            { english = "Global"
            , kinyarwanda = Nothing
            , kirundi = Nothing
            , somali = Nothing
            }

        FirstVisit ->
            { english = "First Visit"
            , kinyarwanda = Nothing
            , kirundi = Nothing
            , somali = Nothing
            }

        FollowUp ->
            { english = "Follow Up"
            , kinyarwanda = Nothing
            , kirundi = Nothing
            , somali = Nothing
            }

        FoodSecurity ->
            { english = "Food Security"
            , kinyarwanda = Nothing
            , kirundi = Nothing
            , somali = Nothing
            }

        GenerateReport ->
            { english = "Generate Report"
            , kinyarwanda = Nothing
            , kirundi = Nothing
            , somali = Nothing
            }

        HC ->
            { english = "HC"
            , kinyarwanda = Nothing
            , kirundi = Nothing
            , somali = Nothing
            }

        HealthCenter ->
            { english = "Health Center"
            , kinyarwanda = Nothing
            , kirundi = Nothing
            , somali = Nothing
            }

        HealthEducation ->
            { english = "Health Education"
            , kinyarwanda = Nothing
            , kirundi = Nothing
            , somali = Nothing
            }

        HIV ->
            { english = "HIV"
            , kinyarwanda = Nothing
            , kirundi = Nothing
            , somali = Nothing
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
            , somali = Nothing
            }

        HomeVisit ->
            { english = "Home Visit"
            , kinyarwanda = Nothing
            , kirundi = Nothing
            , somali = Nothing
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
            , somali = Nothing
            }

        ImmunisationBCG ->
            { english = "BCG Immunisation"
            , kinyarwanda = Nothing
            , kirundi = Nothing
            , somali = Nothing
            }

        ImmunisationDTP ->
            { english = "DTP Immunisation"
            , kinyarwanda = Nothing
            , kirundi = Nothing
            , somali = Nothing
            }

        ImmunisationDTPSA ->
            { english = "DTP Standalone Immunisation"
            , kinyarwanda = Nothing
            , kirundi = Nothing
            , somali = Nothing
            }

        ImmunisationHPV ->
            { english = "HPV Immunisation"
            , kinyarwanda = Nothing
            , kirundi = Nothing
            , somali = Nothing
            }

        ImmunisationIPV ->
            { english = "IPV Immunisation"
            , kinyarwanda = Nothing
            , kirundi = Nothing
            , somali = Nothing
            }

        ImmunisationMR ->
            { english = "MR Immunisation"
            , kinyarwanda = Nothing
            , kirundi = Nothing
            , somali = Nothing
            }

        ImmunisationOPV ->
            { english = "OPV Immunisation"
            , kinyarwanda = Nothing
            , kirundi = Nothing
            , somali = Nothing
            }

        ImmunisationPCV13 ->
            { english = "PCV13 Immunisation"
            , kinyarwanda = Nothing
            , kirundi = Nothing
            , somali = Nothing
            }

        ImmunisationRotarix ->
            { english = "Rotarix Immunisation"
            , kinyarwanda = Nothing
            , kirundi = Nothing
            , somali = Nothing
            }

        Impacted ->
            { english = "Impacted (2+ visits)"
            , kinyarwanda = Nothing
            , kirundi = Nothing
            , somali = Nothing
            }

        IncidenceByMonthOneVisitOrMore ->
            { english = "Incidence by month - one visit or more"
            , kinyarwanda = Nothing
            , kirundi = Nothing
            , somali = Nothing
            }

        IncidenceByMonthTwoVisitsOrMore ->
            { english = "Incidence by month - two visits or more"
            , kinyarwanda = Nothing
            , kirundi = Nothing
            , somali = Nothing
            }

        IncidenceByQuarterOneVisitOrMore ->
            { english = "Incidence by quarter - one visit or more"
            , kinyarwanda = Nothing
            , kirundi = Nothing
            , somali = Nothing
            }

        IncidenceByQuarterTwoVisitsOrMore ->
            { english = "Incidence by quarter - two visits or more"
            , kinyarwanda = Nothing
            , kirundi = Nothing
            , somali = Nothing
            }

        IncidenceByYearOneVisitOrMore ->
            { english = "Incidence by year - one visit or more"
            , kinyarwanda = Nothing
            , kirundi = Nothing
            , somali = Nothing
            }

        IncidenceByYearTwoVisitsOrMore ->
            { english = "Incidence by year - two visits or more"
            , kinyarwanda = Nothing
            , kirundi = Nothing
            , somali = Nothing
            }

        Individual ->
            { english = "Individual"
            , kinyarwanda = Nothing
            , kirundi = Nothing
            , somali = Nothing
            }

        InfrastructureEnvironmentWash ->
            { english = "Infrastructure, Environment & Wash"
            , kinyarwanda = Nothing
            , kirundi = Nothing
            , somali = Nothing
            }

        LoadData ->
            { english = "Load Data"
            , kinyarwanda = Nothing
            , kirundi = Nothing
            , somali = Nothing
            }

        Location ->
            { english = "Location"
            , kinyarwanda = Nothing
            , kirundi = Nothing
            , somali = Nothing
            }

        Male ->
            { english = "Male"
            , kinyarwanda = Nothing
            , kirundi = Nothing
            , somali = Nothing
            }

        Medication ->
            { english = "Medication"
            , kinyarwanda = Nothing
            , kirundi = Nothing
            , somali = Nothing
            }

        MedicationDistribution ->
            { english = "Medication Distribution"
            , kinyarwanda = Nothing
            , kirundi = Nothing
            , somali = Nothing
            }

        Month month ->
            translateMonth month False

        MonthLabel ->
            { english = "Month"
            , kinyarwanda = Just "Ukwezi"
            , kirundi = Just "Ukwezi"
            , somali = Just "Bil"
            }

        MonthYear month year short ->
            translateMonthYY month year short

        NCD ->
            { english = "NCD"
            , kinyarwanda = Nothing
            , kirundi = Nothing
            , somali = Nothing
            }

        NCDA ->
            { english = "NCDA"
            , kinyarwanda = Nothing
            , kirundi = Nothing
            , somali = Nothing
            }

        NCDActivity activity ->
            case activity of
                NCDCoreExam ->
                    translationSet CoreExam

                NCDCoMorbidities ->
                    { english = "Co-Morbidities"
                    , kinyarwanda = Nothing
                    , kirundi = Nothing
                    , somali = Nothing
                    }

                NCDCreatinineTest ->
                    { english = "Creatinine Test"
                    , kinyarwanda = Nothing
                    , kirundi = Nothing
                    , somali = Nothing
                    }

                NCDDangerSigns ->
                    translationSet DangerSigns

                NCDFamilyHistory ->
                    { english = "Family History"
                    , kinyarwanda = Nothing
                    , kirundi = Nothing
                    , somali = Nothing
                    }

                NCDFamilyPlanning ->
                    translationSet FamilyPlanning

                NCDHba1cTest ->
                    { english = "HBA1C Test"
                    , kinyarwanda = Nothing
                    , kirundi = Nothing
                    , somali = Nothing
                    }

                NCDHealthEducation ->
                    translationSet HealthEducation

                NCDHIVTest ->
                    translationSet HIVTest

                NCDLipidPanelTest ->
                    { english = "Lipid Panel Test"
                    , kinyarwanda = Nothing
                    , kirundi = Nothing
                    , somali = Nothing
                    }

                NCDLiverFunctionTest ->
                    { english = "Liver Function Test"
                    , kinyarwanda = Nothing
                    , kirundi = Nothing
                    , somali = Nothing
                    }

                NCDMedicationDistribution ->
                    translationSet MedicationDistribution

                NCDMedicationHistory ->
                    { english = "Medication History"
                    , kinyarwanda = Nothing
                    , kirundi = Nothing
                    , somali = Nothing
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
                    , somali = Nothing
                    }

                NCDLipidPanelTestResult ->
                    { english = "Lipid Panel Result"
                    , kinyarwanda = Nothing
                    , kirundi = Nothing
                    , somali = Nothing
                    }

                NCDLiverFunctionTestResult ->
                    { english = "Liver Function Result"
                    , kinyarwanda = Nothing
                    , kirundi = Nothing
                    , somali = Nothing
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
                    , somali = Nothing
                    }

                NewbornsThisMonth ->
                    { english = "Number of Newborns this month"
                    , kinyarwanda = Nothing
                    , kirundi = Nothing
                    , somali = Nothing
                    }

                LowBirthWeigh ->
                    { english = "Low Birth Weigh (Y/N)"
                    , kinyarwanda = Nothing
                    , kirundi = Nothing
                    , somali = Nothing
                    }

        NCDAAcuteMalnutritionItemLabel item ->
            case item of
                SevereAcuteMalnutrition ->
                    { english = "Severe Acute Malnutrition"
                    , kinyarwanda = Nothing
                    , kirundi = Nothing
                    , somali = Nothing
                    }

                ModerateAcuteMalnutrition ->
                    { english = "Moderate Acute Malnutrition"
                    , kinyarwanda = Nothing
                    , kirundi = Nothing
                    , somali = Nothing
                    }

                GoodNutrition ->
                    { english = "Good Nutrition"
                    , kinyarwanda = Nothing
                    , kirundi = Nothing
                    , somali = Nothing
                    }

        NCDAStuntingItemLabel item ->
            case item of
                SevereStunting ->
                    { english = "Severe Stunting"
                    , kinyarwanda = Nothing
                    , kirundi = Nothing
                    , somali = Nothing
                    }

                ModerateStunting ->
                    { english = "Moderate Stunting"
                    , kinyarwanda = Nothing
                    , kirundi = Nothing
                    , somali = Nothing
                    }

                NoStunting ->
                    { english = "No Stunting"
                    , kinyarwanda = Nothing
                    , kirundi = Nothing
                    , somali = Nothing
                    }

        NCDAANCNewbornItemLabel item ->
            case item of
                RegularCheckups ->
                    { english = "Regular prenatal and postpartum checkups"
                    , kinyarwanda = Just "Yisuzumishije uko bikwiye atwite na nyuma yo kubyara"
                    , kirundi = Nothing
                    , somali = Nothing
                    }

                IronDuringPregnancy ->
                    { english = "Iron during pregnancy"
                    , kinyarwanda = Just "Yafashe umuti wongera amaraso atwite"
                    , kirundi = Nothing
                    , somali = Nothing
                    }

        NCDAInfrastructureEnvironmentWashItemLabel item ->
            case item of
                HasToilets ->
                    { english = "Household has toilets"
                    , kinyarwanda = Just "Urugo rufite ubwiherero"
                    , kirundi = Nothing
                    , somali = Nothing
                    }

                HasCleanWater ->
                    { english = "Household has clean water"
                    , kinyarwanda = Just "Urugo rufite amazi meza"
                    , kirundi = Nothing
                    , somali = Nothing
                    }

                HasHandwashingFacility ->
                    { english = "Household has handwashing facility"
                    , kinyarwanda = Just "Urugo rufite kandagirukarabe"
                    , kirundi = Nothing
                    , somali = Nothing
                    }

                HasKitchenGarden ->
                    { english = "Household has kitchen garden"
                    , kinyarwanda = Just "Urugo rufite akarima k'igikoni"
                    , kirundi = Nothing
                    , somali = Nothing
                    }

                InsecticideTreatedBedNets ->
                    { english = "Insecticide treated bed nets"
                    , kinyarwanda = Just "Urugo rufite nzitiramibu ikoranye umuti"
                    , kirundi = Nothing
                    , somali = Nothing
                    }

        NCDANutritionBehaviorItemLabel item ->
            case item of
                BreastfedSixMonths ->
                    { english = "Breastfed baby for 6 mo without interruption"
                    , kinyarwanda = Just "Konsa umwana amezi 6 utamuvangiye"
                    , kirundi = Nothing
                    , somali = Nothing
                    }

                AppropriateComplementaryFeeding ->
                    { english = "Appropriate complementary feeding (6-24 mo)"
                    , kinyarwanda = Just "Imfashabere igizwe n’indyo yuzuye (Amezi 6-24)"
                    , kirundi = Nothing
                    , somali = Nothing
                    }

                DiverseDiet ->
                    { english = "Diverse diet"
                    , kinyarwanda = Nothing
                    , kirundi = Nothing
                    , somali = Nothing
                    }

                MealsADay ->
                    { english = "Appropriate frequency of meals"
                    , kinyarwanda = Nothing
                    , kirundi = Nothing
                    , somali = Nothing
                    }

        NCDATargetedInterventionsItemLabel item ->
            case item of
                FBFGiven ->
                    { english = "FBF"
                    , kinyarwanda = Nothing
                    , kirundi = Nothing
                    , somali = Nothing
                    }

                TreatmentForAcuteMalnutrition ->
                    { english = "Treatment for acute malnutrition (severe or moderate)"
                    , kinyarwanda = Just "Kuvura imiritre mibi  ifatiyeho(Ikabije cg yoroheje)"
                    , kirundi = Nothing
                    , somali = Nothing
                    }

                TreatmentForDiarrhea ->
                    { english = "Treatment of diarrhea (ORS & Zinc)"
                    , kinyarwanda = Just "Kuvura impiswi(Ukoresheje Zinc cg ORS)"
                    , kirundi = Nothing
                    , somali = Nothing
                    }

                SupportChildWithDisability ->
                    { english = "Provide support to a child with a disability "
                    , kinyarwanda = Just "Guha umwana ufite ubumuga ubufasha bwihariye"
                    , kirundi = Nothing
                    , somali = Nothing
                    }

                ConditionalCashTransfer ->
                    { english = "Receipt of conditional cash transfer e.g. NSDS, VUP"
                    , kinyarwanda = Just "Gufata amafaranga y’inkunga agenerwa umugore utwite n’uwonsa bo mu miryango ikennye (icyiciro cya 1 n’icya 2) – NSDS, VUP"
                    , kirundi = Nothing
                    , somali = Nothing
                    }

                ConditionalFoodItems ->
                    { english = "Receipt of conditional food items including small livestock"
                    , kinyarwanda = Just "Gufata inkunga z’ingoboka harimo ibiryo n'amatungo magufi"
                    , kirundi = Nothing
                    , somali = Nothing
                    }

        NCDAUniversalInterventionItemLabel item ->
            case item of
                Immunization ->
                    { english = "Immunization"
                    , kinyarwanda = Just "Ikingira"
                    , kirundi = Nothing
                    , somali = Nothing
                    }

                VitaminA ->
                    { english = "Vitamin A"
                    , kinyarwanda = Nothing
                    , kirundi = Nothing
                    , somali = Nothing
                    }

                Deworming ->
                    { english = "Deworming"
                    , kinyarwanda = Just "Imiti y'inzoka"
                    , kirundi = Nothing
                    , somali = Nothing
                    }

                OngeraMNP ->
                    { english = "Use additional nutrients (Ongera)"
                    , kinyarwanda = Just "Koresha Ongera intungamubiri"
                    , kirundi = Nothing
                    , somali = Nothing
                    }

                ECDServices ->
                    { english = "ECD services provided to child"
                    , kinyarwanda = Just "Umwana yahawe servise n'ikigo mboneza mikurire"
                    , kirundi = Nothing
                    , somali = Nothing
                    }

        NewbornExam ->
            { english = "Newborn Exam"
            , kinyarwanda = Nothing
            , kirundi = Nothing
            , somali = Nothing
            }

        NewScope ->
            { english = "New Scope"
            , kinyarwanda = Nothing
            , kirundi = Nothing
            , somali = Nothing
            }

        NewSelection ->
            { english = "New Selection"
            , kinyarwanda = Nothing
            , kirundi = Nothing
            , somali = Nothing
            }

        NoDiagnosis ->
            { english = "No Diagnosis"
            , kinyarwanda = Nothing
            , kirundi = Just "Nta Gupima/gusuzuma"
            , somali = Just "Ma jiro Baaritaan"
            }

        NumberOfVisits number ->
            if number == 1 then
                { english = "1 visit"
                , kinyarwanda = Nothing
                , kirundi = Nothing
                , somali = Nothing
                }

            else if number > 4 then
                { english = "5+ visits"
                , kinyarwanda = Nothing
                , kirundi = Nothing
                , somali = Nothing
                }

            else
                { english = String.fromInt number ++ " visits"
                , kinyarwanda = Nothing
                , kirundi = Nothing
                , somali = Nothing
                }

        NumberOfVisitsLabel ->
            { english = "# Visits"
            , kinyarwanda = Nothing
            , kirundi = Nothing
            , somali = Nothing
            }

        Nutrition ->
            { english = "Nutrition"
            , kinyarwanda = Nothing
            , kirundi = Nothing
            , somali = Nothing
            }

        NutritionChildActivity activity ->
            case activity of
                NutritionHeight ->
                    { english = "Height"
                    , kinyarwanda = Nothing
                    , kirundi = Nothing
                    , somali = Nothing
                    }

                NutritionNutrition ->
                    translationSet Nutrition

                NutritionPhoto ->
                    translationSet Photo

                NutritionWeight ->
                    { english = "Weight"
                    , kinyarwanda = Nothing
                    , kirundi = Nothing
                    , somali = Nothing
                    }

                NutritionMUAC ->
                    { english = "MUAC"
                    , kinyarwanda = Nothing
                    , kirundi = Nothing
                    , somali = Nothing
                    }

                NutritionContributingFactors ->
                    { english = "Contributing Factors"
                    , kinyarwanda = Nothing
                    , kirundi = Nothing
                    , somali = Nothing
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
                    , somali = Nothing
                    }

                NutritionChildFbf ->
                    { english = "Child FBF"
                    , kinyarwanda = Nothing
                    , kirundi = Nothing
                    , somali = Nothing
                    }

        NutritionMotherActivity activity ->
            case activity of
                NutritionFamilyPlanning ->
                    translationSet FamilyPlanning

                NutritionLactation ->
                    { english = "Lactation"
                    , kinyarwanda = Nothing
                    , kirundi = Nothing
                    , somali = Nothing
                    }

                NutritionMotherFbf ->
                    { english = "Mother FBF"
                    , kinyarwanda = Nothing
                    , kirundi = Nothing
                    , somali = Nothing
                    }

        NutritionBehavior ->
            { english = "Nutrition Behavior"
            , kinyarwanda = Nothing
            , kirundi = Nothing
            , somali = Nothing
            }

        NutritionIndividual ->
            { english = "Nutrition Individual"
            , kinyarwanda = Nothing
            , kirundi = Nothing
            , somali = Nothing
            }

        NutritionGroup ->
            { english = "Nutrition Group"
            , kinyarwanda = Nothing
            , kirundi = Nothing
            , somali = Nothing
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
            , somali = Nothing
            }

        OutcomesTableHeading ->
            { english = "Outcomes of completed pregnancies (anything 30 days beyond EDD)"
            , kinyarwanda = Nothing
            , kirundi = Nothing
            , somali = Nothing
            }

        OutsideCare ->
            { english = "Outside Care"
            , kinyarwanda = Nothing
            , kirundi = Nothing
            , somali = Nothing
            }

        PatientsWith3OrMoreVisitsPercentage ->
            { english = "3+ visits (%)"
            , kinyarwanda = Nothing
            , kirundi = Nothing
            , somali = Nothing
            }

        PatientsWith4OrMoreVisitsPercentage ->
            { english = "4+ visits (%)"
            , kinyarwanda = Nothing
            , kirundi = Nothing
            , somali = Nothing
            }

        Photo ->
            { english = "Photo"
            , kinyarwanda = Nothing
            , kirundi = Nothing
            , somali = Nothing
            }

        PleaseWaitMessage ->
            { english = "This action may take several minutes to complete."
            , kinyarwanda = Nothing
            , kirundi = Nothing
            , somali = Nothing
            }

        PMTCT ->
            { english = "PMTCT"
            , kinyarwanda = Nothing
            , kirundi = Nothing
            , somali = Nothing
            }

        PopulationSelectionOption selectionOption ->
            case selectionOption of
                SelectionOptionGlobal ->
                    { english = "Entire Population"
                    , kinyarwanda = Nothing
                    , kirundi = Nothing
                    , somali = Nothing
                    }

                SelectionOptionDemographics ->
                    { english = "Demographic Region"
                    , kinyarwanda = Nothing
                    , kirundi = Nothing
                    , somali = Nothing
                    }

                SelectionOptionHealthCenter ->
                    { english = "Health Center Catchment"
                    , kinyarwanda = Nothing
                    , kirundi = Nothing
                    , somali = Nothing
                    }

        PregnanciesActive ->
            { english = "Active Pregnancies"
            , kinyarwanda = Nothing
            , kirundi = Nothing
            , somali = Nothing
            }

        PregnanciesAll ->
            { english = "All Pregnancies"
            , kinyarwanda = Nothing
            , kirundi = Nothing
            , somali = Nothing
            }

        PregnanciesCompleted ->
            { english = "Completed Pregnancies"
            , kinyarwanda = Nothing
            , kirundi = Nothing
            , somali = Nothing
            }

        PregnancyOutcome outcome ->
            case outcome of
                OutcomeLiveAtTerm ->
                    { english = "Live Birth at Term (38 weeks EGA or more)"
                    , kinyarwanda = Just "Kubyara umwana muzima/Ushyitse (ku byumweru 38 kuzamura)"
                    , kirundi = Just "Imbanyi ivutse ikomeye ikiringo kigeze (Indwi 38 z'imbanyi canke zirenga)"
                    , somali = Just "Nolol ku Dhashay oo Bilo Dhameestay (38 usbuuc EGA ama ka badan)"
                    }

                OutcomeLivePreTerm ->
                    { english = "Live Birth Preterm (less than 38 weeks EGA)"
                    , kinyarwanda = Just "Kubyara mwana udashyitse (munsi y'ibyumweru 38)"
                    , kirundi = Just "Imbanyi ivutse imbere y'ikiringo (mbere y'indwi 38)"
                    , somali = Just "Nolol ku dhashay oo aan Bilo dhameesan (ka yar 38 usbuuc EGA)"
                    }

                OutcomeStillAtTerm ->
                    { english = "Stillbirth at Term (38 weeks EGA or more)"
                    , kinyarwanda = Just "Abana bapfiriye mu nda bageze igihe cyo kuvuka (ku byumweru 38 kuzamura)"
                    , kirundi = Just "Kuvyarira ku gihe (Indwi 38 - AGE (z'Igihe co Kwibungenga Caharuwe ) canke zirenga)"
                    , somali = Just "Uur meyd bilo dhameystay (38 usbuuc EGA ama ka badan)"
                    }

                OutcomeStillPreTerm ->
                    { english = "Stillbirth Preterm (less than 38 weeks EGA)"
                    , kinyarwanda = Just "Abana bapfiriye mu nda batagejeje igihe cyo kuvuka (munsi y'ibyumweru 38)"
                    , kirundi = Just "Kuvyara imbere yuko igihe kigera (imbere y'indwi 38 - AGE (Igihe co Kwibungenga Caharuwe)"
                    , somali = Just "Uur meyd aan bilo dhameysan (ka yar 38 usbuuc EGA)"
                    }

                OutcomeAbortions ->
                    { english = "Abortions (before 24 weeks EGA)"
                    , kinyarwanda = Just "Kuvanamo inda (mbere y'ibyumweru 24)"
                    , kirundi = Just "Ugukoroka kw'imbanyi (imbere y'indwi 24 ugereranije nigihe imbanyi imaze)"
                    , somali = Just "Dhicis (ka hor 24 todobaad EGA)"
                    }

        PregnancyOutcomeLabel ->
            { english = "Pregnancy Outcome"
            , kinyarwanda = Nothing
            , kirundi = Nothing
            , somali = Nothing
            }

        PregnancyTest ->
            { english = "Pregnancy Test"
            , kinyarwanda = Nothing
            , kirundi = Nothing
            , somali = Nothing
            }

        PregnancyTrimester trimester ->
            case trimester of
                FirstTrimester ->
                    { english = "First Trimester"
                    , kinyarwanda = Just "Igihembwe cya mbere"
                    , kirundi = Just "Igice ca mbere"
                    , somali = Just "Saddex-biloodka Koowaad"
                    }

                SecondTrimester ->
                    { english = "Second Trimester"
                    , kinyarwanda = Just "Igihembwe cya kabiri"
                    , kirundi = Just "Igice ca kabiri"
                    , somali = Just "Saddex-biloodka labaad"
                    }

                ThirdTrimester ->
                    { english = "Third Trimester"
                    , kinyarwanda = Just "Igihembwe cya gatatu"
                    , kirundi = Just "Igice ca 3"
                    , somali = Just "Saddex-biloodka Saddexaad"
                    }

        PrenatalActivity activity ->
            case activity of
                PrenatalAspirin ->
                    { english = "Low dose Aspirin"
                    , kinyarwanda = Nothing
                    , kirundi = Nothing
                    , somali = Nothing
                    }

                PrenatalAppointmentConfirmation ->
                    { english = "Appointment Confirmation"
                    , kinyarwanda = Nothing
                    , kirundi = Nothing
                    , somali = Nothing
                    }

                PrenatalBirthPlan ->
                    { english = "Birth Plan"
                    , kinyarwanda = Nothing
                    , kirundi = Nothing
                    , somali = Nothing
                    }

                PrenatalBloodGprsTest ->
                    { english = "Blood Group and Rhesus Test"
                    , kinyarwanda = Nothing
                    , kirundi = Nothing
                    , somali = Nothing
                    }

                PrenatalBloodGprsTestResult ->
                    { english = "Blood Group and Rhesus Test Result"
                    , kinyarwanda = Nothing
                    , kirundi = Nothing
                    , somali = Nothing
                    }

                PrenatalBreastExam ->
                    { english = "Breast Exam"
                    , kinyarwanda = Nothing
                    , kirundi = Nothing
                    , somali = Nothing
                    }

                PrenatalCalcium ->
                    { english = "Calcium"
                    , kinyarwanda = Nothing
                    , kirundi = Nothing
                    , somali = Nothing
                    }

                PrenatalBreastfeeding ->
                    { english = "Breastfeeding"
                    , kinyarwanda = Nothing
                    , kirundi = Nothing
                    , somali = Nothing
                    }

                PrenatalCorePhysicalExam ->
                    { english = "Core Physical Exam"
                    , kinyarwanda = Nothing
                    , kirundi = Nothing
                    , somali = Nothing
                    }

                PrenatalDangerSigns ->
                    translationSet DangerSigns

                PrenatalFamilyPlanning ->
                    translationSet FamilyPlanning

                PrenatalFefol ->
                    { english = "Fefol"
                    , kinyarwanda = Nothing
                    , kirundi = Nothing
                    , somali = Nothing
                    }

                PrenatalFolate ->
                    { english = "Folate"
                    , kinyarwanda = Nothing
                    , kirundi = Nothing
                    , somali = Nothing
                    }

                PrenatalFollowUp ->
                    translationSet FollowUp

                PrenatalGuExam ->
                    { english = "GU Exam"
                    , kinyarwanda = Nothing
                    , kirundi = Nothing
                    , somali = Nothing
                    }

                PrenatalHealthEducation ->
                    translationSet HealthEducation

                PrenatalHemoglobinTest ->
                    { english = "Hemoglobin Test"
                    , kinyarwanda = Nothing
                    , kirundi = Nothing
                    , somali = Nothing
                    }

                PrenatalHemoglobinTestResult ->
                    { english = "Hemoglobin Test Result"
                    , kinyarwanda = Nothing
                    , kirundi = Nothing
                    , somali = Nothing
                    }

                PrenatalHepatitisBTest ->
                    { english = "Hepatitis B Test"
                    , kinyarwanda = Nothing
                    , kirundi = Nothing
                    , somali = Nothing
                    }

                PrenatalHepatitisBTestResult ->
                    { english = "Hepatitis B Test Result"
                    , kinyarwanda = Nothing
                    , kirundi = Nothing
                    , somali = Nothing
                    }

                PrenatalHIVPCRTest ->
                    { english = "HIV PCR Test"
                    , kinyarwanda = Nothing
                    , kirundi = Nothing
                    , somali = Nothing
                    }

                PrenatalHIVPCRTestResult ->
                    { english = "HIV PCR Test Result"
                    , kinyarwanda = Nothing
                    , kirundi = Nothing
                    , somali = Nothing
                    }

                PrenatalHIVTest ->
                    translationSet HIVTest

                PrenatalHIVTestResult ->
                    { english = "HIV Test Result"
                    , kinyarwanda = Nothing
                    , kirundi = Nothing
                    , somali = Nothing
                    }

                PrenatalIron ->
                    { english = "Iron"
                    , kinyarwanda = Nothing
                    , kirundi = Nothing
                    , somali = Nothing
                    }

                PrenatalLastMenstrualPeriod ->
                    { english = "Last Menstrual Period"
                    , kinyarwanda = Nothing
                    , kirundi = Nothing
                    , somali = Nothing
                    }

                PrenatalMalariaTest ->
                    { english = "Malaria Test"
                    , kinyarwanda = Nothing
                    , kirundi = Nothing
                    , somali = Nothing
                    }

                PrenatalMalariaTestResult ->
                    { english = "Malaria Test Result"
                    , kinyarwanda = Nothing
                    , kirundi = Nothing
                    , somali = Nothing
                    }

                PrenatalMebendazole ->
                    { english = "Mebendazole"
                    , kinyarwanda = Nothing
                    , kirundi = Nothing
                    , somali = Nothing
                    }

                PrenatalMedicalHistory ->
                    { english = "Medical History"
                    , kinyarwanda = Nothing
                    , kirundi = Nothing
                    , somali = Nothing
                    }

                PrenatalMedication ->
                    translationSet Medication

                PrenatalMedicationDistribution ->
                    translationSet MedicationDistribution

                PrenatalMentalHealth ->
                    { english = "Mental Health"
                    , kinyarwanda = Nothing
                    , kirundi = Nothing
                    , somali = Nothing
                    }

                PrenatalMMS ->
                    { english = "MMS"
                    , kinyarwanda = Nothing
                    , kirundi = Nothing
                    , somali = Nothing
                    }

                PrenatalNutrition ->
                    translationSet Nutrition

                PrenatalObstetricalExam ->
                    { english = "Obstetrical Exam"
                    , kinyarwanda = Nothing
                    , kirundi = Nothing
                    , somali = Nothing
                    }

                PrenatalObstetricHistory ->
                    { english = "Obstetric History"
                    , kinyarwanda = Nothing
                    , kirundi = Nothing
                    , somali = Nothing
                    }

                PrenatalObstetricHistoryStep2 ->
                    { english = "Obstetric History Second Step"
                    , kinyarwanda = Nothing
                    , kirundi = Nothing
                    , somali = Nothing
                    }

                PrenatalOutsideCare ->
                    translationSet OutsideCare

                PrenatalPartnerHIVTest ->
                    { english = "Partner HIV Test"
                    , kinyarwanda = Nothing
                    , kirundi = Nothing
                    , somali = Nothing
                    }

                PrenatalPartnerHIVTestResult ->
                    { english = "Partner HIV Test Result"
                    , kinyarwanda = Nothing
                    , kirundi = Nothing
                    , somali = Nothing
                    }

                PrenatalPhoto ->
                    translationSet Photo

                PrenatalPostpartumTreatmentReview ->
                    { english = "Postpartum Treatment Review"
                    , kinyarwanda = Nothing
                    , kirundi = Nothing
                    , somali = Nothing
                    }

                PrenatalPregnancyOutcome ->
                    translationSet PregnancyOutcomeLabel

                PrenatalPregnancyTesting ->
                    { english = "Pregnancy Testing"
                    , kinyarwanda = Nothing
                    , kirundi = Nothing
                    , somali = Nothing
                    }

                PrenatalRandomBloodSugarTest ->
                    translationSet RandomBloodSugarTest

                PrenatalRandomBloodSugarTestResult ->
                    translationSet RandomBloodSugarTestResult

                PrenatalResource ->
                    { english = "Resource"
                    , kinyarwanda = Nothing
                    , kirundi = Nothing
                    , somali = Nothing
                    }

                PrenatalSendToHC ->
                    translationSet Referral

                PrenatalSocialHistory ->
                    translationSet SocialHistory

                PrenatalSpecialityCare ->
                    { english = "Speciality Care"
                    , kinyarwanda = Nothing
                    , kirundi = Nothing
                    , somali = Nothing
                    }

                PrenatalSymptomReview ->
                    translationSet SymptomsReview

                PrenatalSyphilisTest ->
                    { english = "Syphilis Test"
                    , kinyarwanda = Nothing
                    , kirundi = Nothing
                    , somali = Nothing
                    }

                PrenatalSyphilisTestResult ->
                    { english = "Syphilis Test Result"
                    , kinyarwanda = Nothing
                    , kirundi = Nothing
                    , somali = Nothing
                    }

                PrenatalTetanusImmunisation ->
                    { english = "Tetanus Immunisation"
                    , kinyarwanda = Nothing
                    , kirundi = Nothing
                    , somali = Nothing
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
                    , somali = Nothing
                    }

        PrenatalDiagnosis diagnosis ->
            case diagnosis of
                DiagnosisChronicHypertension ->
                    { english = "Chronic Hypertension"
                    , kinyarwanda = Just "Indwara y'Umuvuduko w'Amaraso Imaze Igihe Kirekire"
                    , kirundi = Just "Umuvuduko ukabije w'amaraso wamaho"
                    , somali = Nothing
                    }

                DiagnosisGestationalHypertension ->
                    { english = "Pregnancy-Induced Hypertension"
                    , kinyarwanda = Just "Umuvuduko w'amaraso watewe no gutwita"
                    , kirundi = Just "Umuvuduko w'amaraso utewe n'imbanyi"
                    , somali = Nothing
                    }

                DiagnosisModeratePreeclampsia ->
                    { english = "Mild to Moderate Preeclampsia"
                    , kinyarwanda = Just "Preklampusi Yoroheje"
                    , kirundi = Just "Umuvuduko w'amaraso mu gihe c'imbanyi woroshe"
                    , somali = Just "Dhiig karka Uurka u dhaxeeya mid hoose iyo mid dhexe"
                    }

                DiagnosisSeverePreeclampsia ->
                    { english = "Severe Preeclampsia"
                    , kinyarwanda = Just "Preklampusi Ikabije"
                    , kirundi = Just "Severe Preeclampsia"
                    , somali = Just "Dhiig karka uurka oo aad u daran"
                    }

                DiagnosisEclampsia ->
                    { english = "Eclampsia"
                    , kinyarwanda = Just "Ekalampusi"
                    , kirundi = Just "Éclampsie"
                    , somali = Just "Ekalaamsiyo"
                    }

                DiagnosisHIV ->
                    { english = "HIV"
                    , kinyarwanda = Just "Virusi itera SIDA"
                    , kirundi = Just "Umugera wa SIDA"
                    , somali = Nothing
                    }

                DiagnosisHIVDetectableViralLoad ->
                    { english = "Detectable HIV Viral Load"
                    , kinyarwanda = Just "Agaragaza udukoko dutera virusi ya SIDA mu maraso"
                    , kirundi = Just "Afise umugera wa SIDA ugaragara"
                    , somali = Nothing
                    }

                DiagnosisDiscordantPartnership ->
                    { english = "Discordant Partnership"
                    , kinyarwanda = Just "Umwe mubo babana afite ubwandu"
                    , kirundi = Just "Umwe afise umugera wa SIDA kandi uwundi atawafise"
                    , somali = Nothing
                    }

                DiagnosisSyphilis ->
                    { english = "Syphilis"
                    , kinyarwanda = Just "Mburugu"
                    , kirundi = Just "Ingwara yo mu bihimba vy'irondoka"
                    , somali = Just "Waraabow"
                    }

                DiagnosisSyphilisWithComplications ->
                    { english = "Syphilis with Complications"
                    , kinyarwanda = Just "Mburugu n'ibibazo bishamikiyeho"
                    , kirundi = Just "Syphilis hamwe n'ingorane"
                    , somali = Nothing
                    }

                DiagnosisNeurosyphilis ->
                    { english = "Neurosyphilis"
                    , kinyarwanda = Just "Mburugu yageze mu bwonko"
                    , kirundi = Just "Ingwara yo m'ubwonko"
                    , somali = Just "Jabtada Dareen Sidaha"
                    }

                DiagnosisHepatitisB ->
                    { english = "Hepatitis B"
                    , kinyarwanda = Just "Umwijima wo mu bwoko bwa B"
                    , kirundi = Just "Ingwara y'igitigu"
                    , somali = Nothing
                    }

                DiagnosisMalaria ->
                    { english = "Malaria"
                    , kinyarwanda = Just "Malariya"
                    , kirundi = Just "Malariya"
                    , somali = Just "Duumo"
                    }

                DiagnosisMalariaWithAnemia ->
                    { english = "Malaria with Anemia"
                    , kinyarwanda = Just "Malariya n'Amaraso Macye"
                    , kirundi = Just "Malariya hamwe n'igabanuka ry'amaraso m'umubiri"
                    , somali = Nothing
                    }

                DiagnosisMalariaWithSevereAnemia ->
                    { english = "Malaria with Severe Anemia"
                    , kinyarwanda = Just "Malariya n'Amaraso Macye Cyane"
                    , kirundi = Just "Malariya kumwe n'igabanuka ry'amaraso m'umubiri ridasanzwe"
                    , somali = Just "Duumo leh Dhiig la`aan Daran"
                    }

                DiagnosisModerateAnemia ->
                    { english = "Mild to Moderate Anemia"
                    , kinyarwanda = Just "Amaraso Macye byoroheje"
                    , kirundi = Just "Igabanuka ry'amaraso kuva bisanzwe"
                    , somali = Just "Dhiig la`aan u dhaxeysa mid hoose iyo mid dhexe"
                    }

                DiagnosisSevereAnemia ->
                    { english = "Severe Anemia"
                    , kinyarwanda = Just "Amaraso Macye Cyane"
                    , kirundi = Just "Ibura ry'amaraso rikaze"
                    , somali = Just "Dhiig yari aad u daran"
                    }

                DiagnosisSevereAnemiaWithComplications ->
                    { english = "Severe Anemia with Complications"
                    , kinyarwanda = Just "Amaraso Macye Cyane n'Ibibazo Bishamikiyeho"
                    , kirundi = Just "Ibura ry'amaraso rikaze hamwe n'ingorane bijanye"
                    , somali = Just "Dhiig yari aad u daran oo leh waxyeello"
                    }

                DiagnosisMiscarriage ->
                    { english = "Miscarriage"
                    , kinyarwanda = Just "Inda yavuyemo"
                    , kirundi = Just "Ugukoroka kw'imbanyi"
                    , somali = Just "Umuliso biyaan"
                    }

                DiagnosisMolarPregnancy ->
                    { english = "Molar Pregnancy"
                    , kinyarwanda = Just "Atwite amahuri"
                    , kirundi = Just "Imbanyi idakomeye"
                    , somali = Just "Uur Beenaad"
                    }

                DiagnosisPlacentaPrevia ->
                    { english = "Placenta Previa"
                    , kinyarwanda = Just "Ingobyi iri hasi ku nkondo y'umura"
                    , kirundi = Just "Igitereko cugaye isohokera"
                    , somali = Just "Ibida Qalloocatay"
                    }

                DiagnosisPlacentalAbruption ->
                    { english = "Placental Abruption"
                    , kinyarwanda = Just "Ingobyi yomotse hakiri kare"
                    , kirundi = Just "Itabuka ry'igitereko"
                    , somali = Just "Ibida Go`day"
                    }

                DiagnosisUterineRupture ->
                    { english = "Uterine Rupture"
                    , kinyarwanda = Just "Nyababyeyi yaturitse"
                    , kirundi = Just "uguturika kw'igitereko"
                    , somali = Just "Dillaaca Ilmo galeenka"
                    }

                DiagnosisObstructedLabor ->
                    { english = "Obstructed Labor"
                    , kinyarwanda = Just "Inda yanze kuvuka "
                    , kirundi = Just "Igikorwa cabujijwe"
                    , somali = Nothing
                    }

                DiagnosisPostAbortionSepsis ->
                    { english = "Post Abortion Sepsis"
                    , kinyarwanda = Just "Afite uburwayi bwa infegisiyo yo mu maraso bwatewe no gukuramo inda"
                    , kirundi = Just "Birashoboka ko ingwara y'igitereko izamwo"
                    , somali = Just "Caabuqa Dhiciska kadib"
                    }

                DiagnosisEctopicPregnancy ->
                    { english = "Ectopic Pregnancy"
                    , kinyarwanda = Just "Yasamiye hanze y'umura"
                    , kirundi = Just "Imbanyi iri hanze y'Igitereko"
                    , somali = Nothing
                    }

                DiagnosisPROM ->
                    { english = "Premature Rupture of Membranes (PROM)"
                    , kinyarwanda = Just "Isuha yamenetse hakiri kare"
                    , kirundi = Just "Itabuka ry'isimbizo y'umwana mu gitereko imbere yuko imbanyi ishika kw'itarike yayo/igihe cayo"
                    , somali = Nothing
                    }

                DiagnosisPPROM ->
                    { english = "Preterm Premature Rupture of Membranes (PPROM)"
                    , kinyarwanda = Just "Isuha yamenetse hakiri kare inda itarageza igihe"
                    , kirundi = Just "Itabuka ry'isimbizo y'umwana mu gitereko imbere yuko imbanyi ishika kw'itarike yayo"
                    , somali = Nothing
                    }

                DiagnosisHyperemesisGravidum ->
                    { english = "Hyperemesis Gravidum"
                    , kinyarwanda = Just "Kuruka bikabije k'umugore utwite"
                    , kirundi = Just "Hyperémèse gravidique"
                    , somali = Nothing
                    }

                DiagnosisSevereVomiting ->
                    { english = "Severe Vomiting"
                    , kinyarwanda = Just "Kuruka bikabije"
                    , kirundi = Just "Ukudahwa gukaze"
                    , somali = Nothing
                    }

                DiagnosisMaternalComplications ->
                    { english = "Maternal Complications"
                    , kinyarwanda = Just "Ibibazo bishobora kwibasira umugore utwite"
                    , kirundi = Just "Ingorane z'abavyeyi"
                    , somali = Nothing
                    }

                DiagnosisInfection ->
                    { english = "Infection"
                    , kinyarwanda = Just "Indwara iterwa n'udukoko tutabonwa n'amaso (Microbes)"
                    , kirundi = Just "Ivyanduza"
                    , somali = Just "Caabuq"
                    }

                DiagnosisImminentDelivery ->
                    { english = "Imminent Delivery"
                    , kinyarwanda = Just "Kubyara biri hafi"
                    , kirundi = Just "Gutanga bigaragara/"
                    , somali = Nothing
                    }

                DiagnosisLaborAndDelivery ->
                    { english = "Labor + Delivery"
                    , kinyarwanda = Just "Kujya ku nda + Kubyara"
                    , kirundi = Just "Ibise + Kuvyara"
                    , somali = Just "Fool + Dhalmo"
                    }

                DiagnosisHeartburn ->
                    { english = "Heartburn"
                    , kinyarwanda = Just "Ikirungurira"
                    , kirundi = Just "Ugusha k'umutima"
                    , somali = Nothing
                    }

                DiagnosisDeepVeinThrombosis ->
                    { english = "Deep Vein Thrombosis"
                    , kinyarwanda = Just "Gufatana(Kuvura) gukabije kw'amaraso"
                    , kirundi = Just "Umutsi w'indani ufise Thrombose"
                    , somali = Nothing
                    }

                DiagnosisPelvicPainIntense ->
                    { english = "Intense Pelvic Pain"
                    , kinyarwanda = Just "Ububabare bukabije mu kiziba cy'inda"
                    , kirundi = Just "Ububabare bukomeye bwo mu nda yo hepfo"
                    , somali = Just "Xanuun Dhabarka ah oo daran"
                    }

                DiagnosisUrinaryTractInfection ->
                    { english = "Urinary Tract Infection"
                    , kinyarwanda = Just "Indwara y'ubwandu bw'umuyoboro w'inkari"
                    , kirundi = Just "Ingwara yo mu miringoti y'umukoyo"
                    , somali = Nothing
                    }

                DiagnosisPyelonephritis ->
                    { english = "Pyelonephritis"
                    , kinyarwanda = Just "Indwara yo kubyimba impyiko"
                    , kirundi = Just "Ingwara y'Amafyigo"
                    , somali = Just "Caabuqa Kellida"
                    }

                DiagnosisCandidiasis ->
                    { english = "Candidiasis"
                    , kinyarwanda = Just "Kandidoze"
                    , kirundi = Just "Candidose"
                    , somali = Nothing
                    }

                DiagnosisGonorrhea ->
                    { english = "Gonorrhea"
                    , kinyarwanda = Just "Indwara y'umutezi"
                    , kirundi = Just "Ingwara yo mu bihimba vy'irondoka"
                    , somali = Nothing
                    }

                DiagnosisTrichomonasOrBacterialVaginosis ->
                    { english = "Trichomonas or Bacterial Vaginosis"
                    , kinyarwanda = Just "Tirikomonasi cyangwa Mikorobe zo mu nda ibyara"
                    , kirundi = Just "Ingwara yo mu bihimba vy'irondoka igaragazwa kenshi no kuhiyagaza"
                    , somali = Just "Trichomonas ama Bakteeriyada Makaanka"
                    }

                DiagnosisTuberculosis ->
                    { english = "Tuberculosis"
                    , kinyarwanda = Just "Igituntu"
                    , kirundi = Just "Igituntu"
                    , somali = Just "Qaaxo"
                    }

                DiagnosisDiabetes ->
                    { english = "Diabetes"
                    , kinyarwanda = Just "Diyabete (Indwara y'igisukari)"
                    , kirundi = Just "Diyabete"
                    , somali = Nothing
                    }

                DiagnosisGestationalDiabetes ->
                    { english = "Gestational Diabetes"
                    , kinyarwanda = Just "Diyabete iterwa no gutwita"
                    , kirundi = Just "Diyabete y'imbanyi"
                    , somali = Nothing
                    }

                DiagnosisRhesusNegative ->
                    { english = "RH Factor Negative"
                    , kinyarwanda = Just "Rezisi Negatifu"
                    , kirundi = Just "Rhesus negatif"
                    , somali = Nothing
                    }

                DiagnosisDepressionNotLikely ->
                    { english = "Depression not Likely"
                    , kinyarwanda = Just "Birashoboka ko adafite indwara y'agahinda gakabije"
                    , kirundi = Just "Kwihebura ntibishoboka"
                    , somali = Nothing
                    }

                DiagnosisDepressionPossible ->
                    { english = "Depression Possible"
                    , kinyarwanda = Just "Birashoboka ko yagira indwara y'agahinda gakabije"
                    , kirundi = Just "Kwihebura birashoboka"
                    , somali = Nothing
                    }

                DiagnosisDepressionHighlyPossible ->
                    { english = "Fairly High Possibility of Depression"
                    , kinyarwanda = Just "Birashoboka cyane ko afite indwara y'agahinda gakabije"
                    , kirundi = Just "Birashoboka cane kwihebura"
                    , somali = Nothing
                    }

                DiagnosisDepressionProbable ->
                    { english = "Probable Depression"
                    , kinyarwanda = Just "Birashoboka ko afite indwara y'agahinda gakabije"
                    , kirundi = Just "Ukwihebura gushoboka"
                    , somali = Nothing
                    }

                DiagnosisSuicideRisk ->
                    { english = "Suicide Risk"
                    , kinyarwanda = Just "Afite ibyago byo kwiyahura"
                    , kirundi = Just "Ingorane zimutuma ashobora kwiyahura"
                    , somali = Nothing
                    }

                DiagnosisOther ->
                    { english = "Other"
                    , kinyarwanda = Just "Ibindi"
                    , kirundi = Just "Ibindi"
                    , somali = Just "Kale"
                    }

                DiagnosisPostpartumAbdominalPain ->
                    { english = "Abdominal Pain"
                    , kinyarwanda = Just "Kubabara mu nda"
                    , kirundi = Just "Ukubabara mu nda"
                    , somali = Just "Calool Xanuun"
                    }

                DiagnosisPostpartumUrinaryIncontinence ->
                    { english = "Urinary Incontinence"
                    , kinyarwanda = Just "Ntabasha kunyara"
                    , kirundi = Nothing
                    , somali = Nothing
                    }

                DiagnosisPostpartumHeadache ->
                    { english = "Headache"
                    , kinyarwanda = Just "Kubabara umutwe"
                    , kirundi = Just "Kumeneka umutwe"
                    , somali = Just "Madax xanuun"
                    }

                DiagnosisPostpartumFatigue ->
                    { english = "Fatigue"
                    , kinyarwanda = Just "umunaniro"
                    , kirundi = Just "Uburuhe"
                    , somali = Just "Daal"
                    }

                DiagnosisPostpartumFever ->
                    { english = "Fever"
                    , kinyarwanda = Just "Umuriro"
                    , kirundi = Just "Ubushuhe"
                    , somali = Just "Qandho"
                    }

                DiagnosisPostpartumPerinealPainOrDischarge ->
                    { english = "Perineal Pain or Discharge"
                    , kinyarwanda = Just "Arababara perine cg aratakaza ibintu budasanzwe"
                    , kirundi = Just "Ububabare bw'umugongo hepfo"
                    , somali = Just "Xanuun qaska ah ama Dheecaan"
                    }

                DiagnosisPostpartumInfection ->
                    { english = "Infection (Postpartum)"
                    , kinyarwanda = Nothing
                    , kirundi = Nothing
                    , somali = Nothing
                    }

                DiagnosisPostpartumExcessiveBleeding ->
                    { english = "Excessive Bleeding"
                    , kinyarwanda = Just "Kuva cyane"
                    , kirundi = Just "Kuva amaraso cane"
                    , somali = Just "Dhiigbax aad u daran"
                    }

                DiagnosisPostpartumEarlyMastitisOrEngorgment ->
                    { english = "Early Mastitis or Engorgement"
                    , kinyarwanda = Just "Uburwayi bwo kubyimba amabere bwaje kare cyane"
                    , kirundi = Just "Iyuzura ry'amaberebere (Mastite précoce)"
                    , somali = Nothing
                    }

                DiagnosisPostpartumMastitis ->
                    { english = "Mastitis"
                    , kinyarwanda = Just "Uburwayi bw'amabere"
                    , kirundi = Just "Ingwara y'imoko ituma amaberebere adasohoka"
                    , somali = Nothing
                    }

                NoPrenatalDiagnosis ->
                    { english = "None"
                    , kinyarwanda = Just "Ntabyo"
                    , kirundi = Just "Nta na kimwe"
                    , somali = Just "Midna"
                    }

        PrevalenceByMonthOneVisitOrMore ->
            { english = "Prevalence by month - one visit or more"
            , kinyarwanda = Nothing
            , kirundi = Nothing
            , somali = Nothing
            }

        PrevalenceByMonthTwoVisitsOrMore ->
            { english = "Prevalence by month - two visits or more"
            , kinyarwanda = Nothing
            , kirundi = Nothing
            , somali = Nothing
            }

        Province ->
            { english = "Province"
            , kinyarwanda = Nothing
            , kirundi = Nothing
            , somali = Nothing
            }

        QuarterYear quarter year ->
            { english = String.fromInt year ++ " Q" ++ String.fromInt quarter
            , kinyarwanda = Nothing
            , kirundi = Nothing
            , somali = Nothing
            }

        RandomBloodSugarTest ->
            { english = "Random Blood Sugar Test"
            , kinyarwanda = Nothing
            , kirundi = Nothing
            , somali = Nothing
            }

        RandomBloodSugarTestResult ->
            { english = "Random Blood Sugar Test Result"
            , kinyarwanda = Nothing
            , kirundi = Nothing
            , somali = Nothing
            }

        Referral ->
            { english = "Referral"
            , kinyarwanda = Nothing
            , kirundi = Nothing
            , somali = Nothing
            }

        Registered ->
            { english = "Registered"
            , kinyarwanda = Nothing
            , kirundi = Nothing
            , somali = Nothing
            }

        RegisteredPatients ->
            { english = "Registered Patients"
            , kinyarwanda = Nothing
            , kirundi = Nothing
            , somali = Nothing
            }

        ReportType reportType ->
            case reportType of
                ReportAcuteIllness ->
                    translationSet AcuteIllness

                ReportDemographics ->
                    { english = "Demographics"
                    , kinyarwanda = Nothing
                    , kirundi = Nothing
                    , somali = Nothing
                    }

                ReportNutrition ->
                    translationSet Nutrition

                ReportPrenatal ->
                    translationSet Antenatal

                ReportPrenatalDiagnoses ->
                    { english = "ANC Diagnoses"
                    , kinyarwanda = Nothing
                    , kirundi = Nothing
                    , somali = Nothing
                    }

        ReportTypeLabel ->
            { english = "Report Type"
            , kinyarwanda = Nothing
            , kirundi = Nothing
            , somali = Nothing
            }

        ResolveMonth short month ->
            translateMonth month short

        Save ->
            { english = "Save"
            , kinyarwanda = Just "Kubika"
            , kirundi = Just "Emeza"
            , somali = Just "Xaree"
            }

        Scope ->
            { english = "Scope"
            , kinyarwanda = Nothing
            , kirundi = Nothing
            , somali = Nothing
            }

        Sector ->
            { english = "Sector"
            , kinyarwanda = Nothing
            , kirundi = Nothing
            , somali = Nothing
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
            , somali = Nothing
            }

        SelectStartDate ->
            { english = "Start date"
            , kinyarwanda = Nothing
            , kirundi = Nothing
            , somali = Nothing
            }

        SelectScope ->
            { english = "Please select desired scope"
            , kinyarwanda = Nothing
            , kirundi = Nothing
            , somali = Nothing
            }

        SelectViewMode ->
            { english = "Please select desired view mode"
            , kinyarwanda = Nothing
            , kirundi = Nothing
            , somali = Nothing
            }

        SocialHistory ->
            { english = "Social History"
            , kinyarwanda = Nothing
            , kirundi = Nothing
            , somali = Nothing
            }

        Sorwathe ->
            { english = "Sorwathe"
            , kinyarwanda = Nothing
            , kirundi = Nothing
            , somali = Nothing
            }

        StandardPediatricVisit ->
            { english = "Standard Pediatric Visit"
            , kinyarwanda = Nothing
            , kirundi = Nothing
            , somali = Nothing
            }

        Stunting ->
            { english = "Stunting"
            , kinyarwanda = Nothing
            , kirundi = Nothing
            , somali = Nothing
            }

        StuntingModerate ->
            { english = "Stunting Moderate"
            , kinyarwanda = Nothing
            , kirundi = Nothing
            , somali = Nothing
            }

        StuntingSevere ->
            { english = "Stunting Severe"
            , kinyarwanda = Nothing
            , kirundi = Nothing
            , somali = Nothing
            }

        Status ->
            { english = "Status"
            , kinyarwanda = Nothing
            , kirundi = Nothing
            , somali = Nothing
            }

        SymptomsReview ->
            { english = "Symptoms Review"
            , kinyarwanda = Nothing
            , kirundi = Nothing
            , somali = Nothing
            }

        TakenBy value ->
            case value of
                TakenByNurse ->
                    { english = "Nurse"
                    , kinyarwanda = Nothing
                    , kirundi = Nothing
                    , somali = Nothing
                    }

                TakenByCHW ->
                    { english = "CHW"
                    , kinyarwanda = Nothing
                    , kirundi = Nothing
                    , somali = Nothing
                    }

                TakenByUnknown ->
                    { english = "Unknown"
                    , kinyarwanda = Nothing
                    , kirundi = Nothing
                    , somali = Nothing
                    }

        TakenByLabel ->
            { english = "Taken By"
            , kinyarwanda = Nothing
            , kirundi = Nothing
            , somali = Nothing
            }

        TargetedInterventions ->
            { english = "Targeted Interventions"
            , kinyarwanda = Nothing
            , kirundi = Nothing
            , somali = Nothing
            }

        Total ->
            { english = "Total"
            , kinyarwanda = Nothing
            , kirundi = Nothing
            , somali = Nothing
            }

        TreatmentReview ->
            { english = "Treatment Review"
            , kinyarwanda = Nothing
            , kirundi = Nothing
            , somali = Nothing
            }

        Trimester ->
            { english = "Trimester"
            , kinyarwanda = Just "Igihembwe"
            , kirundi = Just "Igice"
            , somali = Nothing
            }

        Tuberculosis ->
            { english = "Tuberculosis"
            , kinyarwanda = Nothing
            , kirundi = Nothing
            , somali = Nothing
            }

        TuberculosisActivity activity ->
            case activity of
                TuberculosisDiagnostics ->
                    translationSet Diagnostics

                TuberculosisDOT ->
                    { english = "DOT"
                    , kinyarwanda = Nothing
                    , kirundi = Nothing
                    , somali = Nothing
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
            , somali = Nothing
            }

        Vitals ->
            { english = "Vitals"
            , kinyarwanda = Nothing
            , kirundi = Nothing
            , somali = Nothing
            }

        Village ->
            { english = "Village"
            , kinyarwanda = Nothing
            , kirundi = Nothing
            , somali = Nothing
            }

        UnderweightModerate ->
            { english = "Underweight Moderate"
            , kinyarwanda = Nothing
            , kirundi = Nothing
            , somali = Nothing
            }

        UnderweightSevere ->
            { english = "Underweight Severe"
            , kinyarwanda = Nothing
            , kirundi = Nothing
            , somali = Nothing
            }

        Unique ->
            { english = "Unique"
            , kinyarwanda = Nothing
            , kirundi = Nothing
            , somali = Nothing
            }

        UniversalIntervention ->
            { english = "Universal Intervention"
            , kinyarwanda = Nothing
            , kirundi = Nothing
            , somali = Nothing
            }

        UrineDipstickTest ->
            { english = "Urine Dipstick Test"
            , kinyarwanda = Nothing
            , kirundi = Nothing
            , somali = Nothing
            }

        UrineDipstickTestResult ->
            { english = "Urine Dipstick Test Result"
            , kinyarwanda = Nothing
            , kirundi = Nothing
            , somali = Nothing
            }

        WastingModerate ->
            { english = "Wasting Moderate"
            , kinyarwanda = Nothing
            , kirundi = Nothing
            , somali = Nothing
            }

        WastingSevere ->
            { english = "Wasting Severe"
            , kinyarwanda = Nothing
            , kirundi = Nothing
            , somali = Nothing
            }

        WellChildActivity activity ->
            case activity of
                WellChildAlbendazole ->
                    { english = "Albendazole"
                    , kinyarwanda = Nothing
                    , kirundi = Nothing
                    , somali = Nothing
                    }

                WellChildBCGImmunisation ->
                    translationSet ImmunisationBCG

                WellChildCaring ->
                    translationSet Caring

                WellChildContributingFactors ->
                    { english = "Contributing Factors"
                    , kinyarwanda = Nothing
                    , kirundi = Nothing
                    , somali = Nothing
                    }

                WellChildDTPImmunisation ->
                    translationSet ImmunisationDTP

                WellChildDTPSAImmunisation ->
                    translationSet ImmunisationDTPSA

                WellChildECD ->
                    { english = "ECD"
                    , kinyarwanda = Nothing
                    , kirundi = Nothing
                    , somali = Nothing
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
                    , somali = Nothing
                    }

                WellChildHealthEducation ->
                    translationSet HealthEducation

                WellChildHeight ->
                    { english = "Height"
                    , kinyarwanda = Nothing
                    , kirundi = Nothing
                    , somali = Nothing
                    }

                WellChildHPVImmunisation ->
                    translationSet ImmunisationHPV

                WellChildHygiene ->
                    translationSet Hygiene

                WellChildIPVImmunisation ->
                    translationSet ImmunisationIPV

                WellChildMebendezole ->
                    { english = "Mebendazole"
                    , kinyarwanda = Nothing
                    , kirundi = Nothing
                    , somali = Nothing
                    }

                WellChildMRImmunisation ->
                    translationSet ImmunisationMR

                WellChildMUAC ->
                    { english = "MUAC"
                    , kinyarwanda = Nothing
                    , kirundi = Nothing
                    , somali = Nothing
                    }

                WellChildNCDA ->
                    translationSet NCDA

                WellChildNextVisit ->
                    { english = "Next Visit"
                    , kinyarwanda = Nothing
                    , kirundi = Nothing
                    , somali = Nothing
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
                    , somali = Nothing
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
                    , somali = Nothing
                    }

                WellChildWeight ->
                    { english = "Weight"
                    , kinyarwanda = Nothing
                    , kirundi = Nothing
                    , somali = Nothing
                    }

        WideScopeNote ->
            { english = "The selected scope may contain a large number of patients and report generation could take several minutes."
            , kinyarwanda = Nothing
            , kirundi = Nothing
            , somali = Nothing
            }

        Year year ->
            { english = String.fromInt year
            , kinyarwanda = Nothing
            , kirundi = Nothing
            , somali = Nothing
            }

        YearLabel ->
            { english = "Year"
            , kinyarwanda = Just "Umwaka"
            , kirundi = Just "Umwaka"
            , somali = Just "Sanadka"
            }

        Zone ->
            { english = "Zone"
            , kinyarwanda = Nothing
            , kirundi = Nothing
            , somali = Nothing
            }


translateHttpError : StringIdHttpError -> TranslationSet
translateHttpError transId =
    case transId of
        ErrorBadUrl ->
            { english = "URL is not valid."
            , kinyarwanda = Nothing
            , kirundi = Nothing
            , somali = Nothing
            }

        ErrorBadPayload message ->
            { english = "The server responded with data of an unexpected type: " ++ message
            , kinyarwanda = Nothing
            , kirundi = Nothing
            , somali = Nothing
            }

        ErrorBadStatus err ->
            { english = err
            , kinyarwanda = Nothing
            , kirundi = Nothing
            , somali = Nothing
            }

        ErrorNetworkError ->
            { english = "There was a network error."
            , kinyarwanda = Nothing
            , kirundi = Nothing
            , somali = Nothing
            }

        ErrorTimeout ->
            { english = "The network request timed out."
            , kinyarwanda = Nothing
            , kirundi = Nothing
            , somali = Nothing
            }


translateMonthYY : Int -> Int -> Bool -> TranslationSet
translateMonthYY month year short =
    translateMonth (Date.numberToMonth month) short
        |> (\set ->
                { english = String.fromInt year ++ " " ++ set.english
                , kinyarwanda = Maybe.map (\kinyarwanda -> String.fromInt year ++ " " ++ kinyarwanda) set.kinyarwanda
                , kirundi = Maybe.map (\kirundi -> String.fromInt year ++ " " ++ kirundi) set.kirundi
                , somali = Maybe.map (\somali -> String.fromInt year ++ " " ++ somali) set.somali
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
                , somali = Nothing
                }

            else
                { english = "January"
                , kinyarwanda = Just "Mutarama"
                , kirundi = Just "Nzero"
                , somali = Just "Janaayo"
                }

        Feb ->
            if short then
                { english = "Feb"
                , kinyarwanda = Just "Gas"
                , kirundi = Just "Ruh"
                , somali = Nothing
                }

            else
                { english = "February"
                , kinyarwanda = Just "Gashyantare"
                , kirundi = Just "Ruhuhuma"
                , somali = Just "Febaraayo"
                }

        Mar ->
            if short then
                { english = "Mar"
                , kinyarwanda = Just "Wer"
                , kirundi = Just "Ntw"
                , somali = Nothing
                }

            else
                { english = "March"
                , kinyarwanda = Just "Werurwe"
                , kirundi = Just "Ntwarante"
                , somali = Nothing
                }

        Apr ->
            if short then
                { english = "Apr"
                , kinyarwanda = Just "Mat"
                , kirundi = Just "Nda"
                , somali = Nothing
                }

            else
                { english = "April"
                , kinyarwanda = Just "Mata"
                , kirundi = Just "Ndamukiza"
                , somali = Nothing
                }

        May ->
            if short then
                { english = "May"
                , kinyarwanda = Just "Gic"
                , kirundi = Just "Rus"
                , somali = Nothing
                }

            else
                { english = "May"
                , kinyarwanda = Just "Gicurasi"
                , kirundi = Just "Rusama"
                , somali = Nothing
                }

        Jun ->
            if short then
                { english = "Jun"
                , kinyarwanda = Just "Kam"
                , kirundi = Just "Ruh"
                , somali = Nothing
                }

            else
                { english = "June"
                , kinyarwanda = Just "Kamena"
                , kirundi = Just "Ruheshi"
                , somali = Just "Juun"
                }

        Jul ->
            if short then
                { english = "Jul"
                , kinyarwanda = Just "Nya"
                , kirundi = Just "Muk"
                , somali = Nothing
                }

            else
                { english = "July"
                , kinyarwanda = Just "Nyakanga"
                , kirundi = Just "Mukakaro"
                , somali = Just "Yuulyo"
                }

        Aug ->
            if short then
                { english = "Aug"
                , kinyarwanda = Just "Kan"
                , kirundi = Just "Mya"
                , somali = Nothing
                }

            else
                { english = "August"
                , kinyarwanda = Just "Kanama"
                , kirundi = Just "Myandagaro"
                , somali = Just "Agoosto"
                }

        Sep ->
            if short then
                { english = "Sep"
                , kinyarwanda = Just "Nze"
                , kirundi = Just "Nya"
                , somali = Nothing
                }

            else
                { english = "September"
                , kinyarwanda = Just "Nzeri"
                , kirundi = Just "Nyakanga"
                , somali = Just "Sebteembar"
                }

        Oct ->
            if short then
                { english = "Oct"
                , kinyarwanda = Just "Ukw"
                , kirundi = Just "Git"
                , somali = Nothing
                }

            else
                { english = "October"
                , kinyarwanda = Just "Ukwakira"
                , kirundi = Just "Gitugutu"
                , somali = Just "Oktoobar"
                }

        Nov ->
            if short then
                { english = "Nov"
                , kinyarwanda = Just "Ukw"
                , kirundi = Just "Muny"
                , somali = Nothing
                }

            else
                { english = "November"
                , kinyarwanda = Just "Ugushyingo"
                , kirundi = Just "Munyonyo"
                , somali = Just "Noofeembar"
                }

        Dec ->
            if short then
                { english = "Dec"
                , kinyarwanda = Just "Uku"
                , kirundi = Just "Kig"
                , somali = Nothing
                }

            else
                { english = "December"
                , kinyarwanda = Just "Ukuboza"
                , kirundi = Just "Kigarama"
                , somali = Just "Diseembar"
                }
