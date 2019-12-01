module Translate exposing
    ( Adherence(..)
    , ChartPhrase(..)
    , Language
    , LoginPhrase(..)
    , TranslationId(..)
    , ValidationError(..)
    , translate
    , translateActivePage
    , translateAdherence
    , translateChartPhrase
    , translateCounselingTimingHeading
    , translateFormError
    , translateFormField
    , translateHttpError
    , translateLoginPhrase
    , translateMonth
    , translateValidationError
    , translationSet
    )

{-| This module has just the translations ... for types and
general utilities, see `Translate.Model` and `Translate.Utils`.
-}

import Activity.Model exposing (Activity(..), ChildActivity(..), MotherActivity(..))
import Backend.Clinic.Model exposing (ClinicType(..))
import Backend.Counseling.Model exposing (CounselingTiming(..), CounselingTopic)
import Backend.Entities exposing (..)
import Backend.Measurement.Model exposing (..)
import Backend.Person.Model
    exposing
        ( EducationLevel(..)
        , Gender(..)
        , HIVStatus(..)
        , MaritalStatus(..)
        , ModeOfDelivery(..)
        , VaginalDelivery(..)
        )
import Backend.PrenatalParticipant.Model exposing (EncounterType(..))
import Backend.Relationship.Model exposing (MyRelatedBy(..))
import Date exposing (Month(..))
import Form.Error exposing (ErrorValue(..))
import Http
import Pages.Page exposing (..)
import Pages.PrenatalActivity.Model
    exposing
        ( ExaminationTask(..)
        , HistoryTask(..)
        , LmpRange(..)
        , PatientProvisionsTask(..)
        )
import PrenatalActivity.Model
    exposing
        ( HighRiskFactor(..)
        , HighSeverityAlert(..)
        , MedicalDiagnosis(..)
        , ObstetricalDiagnosis(..)
        , PregnancyTrimester(..)
        , PrenatalActivity(..)
        , RiskFactor(..)
        )
import Restful.Endpoint exposing (fromEntityUuid)
import Restful.Login exposing (LoginError(..), LoginMethod(..))
import Translate.Model exposing (TranslationSet)
import Translate.Utils exposing (..)


{-| We re-export this one for convenience, so you don't have to import
`Translate.Model` in simple cases. That is, you can do this, which will be
enough for most "view" modules:

    import Translate exposing (translate, Language)

Note that importing `Language` from here gives you only the type, not the
constructors. For more complex cases, where you need `English` and
`Kinyarwanda` as well, you have to do this instead:

    import Translate.Model exposing (Language(..))

-}
type alias Language =
    Translate.Model.Language


translate : Language -> TranslationId -> String
translate lang trans =
    selectLanguage lang (translationSet trans)


type LoginPhrase
    = CheckingCachedCredentials
    | ForgotPassword1
    | ForgotPassword2
    | LoggedInAs
    | LoginError Http.Error
    | LoginRejected LoginMethod
    | LoginOrWorkOffline
    | Logout
    | LogoutInProgress
    | LogoutFailed
    | Password
    | PinCode
    | PinCodeRejected
    | SignIn
    | SignOut
    | Username
    | WorkOffline
    | YouMustLoginBefore


type ChartPhrase
    = AgeCompletedMonthsYears
    | Birth
    | BirthToTwoYears
    | LengthCm
    | LengthForAgeBoys
    | LengthForAgeGirls
    | Months
    | OneYear
    | WeightForAgeBoys
    | WeightForAgeGirls
    | WeightForLengthBoys
    | WeightForLengthGirls
    | WeightKg
    | YearsPlural Int
    | ZScoreChartsAvailableAt


type ValidationError
    = DigitsOnly
    | InvalidBirthDate
    | InvalidBirthDateForAdult
    | InvalidBirthDateForChild
    | InvalidHmisNumber
    | LengthError Int
    | LettersOnly
    | RequiredField
    | UnknownGroup
    | UnknownProvince
    | UnknownDistrict
    | UnknownSector
    | UnknownCell
    | UnknownVillage
    | DecoderError String


type Adherence
    = PrescribedAVRs
    | CorrectDosage
    | TimeOfDay
    | Adhering


type TranslationId
    = Abdomen
    | AbdomenCPESign AbdomenCPESign
    | Abnormal
    | Abortions
    | AccompaniedByPartner
    | AccessDenied
    | Activities
    | ActivitiesCompleted Int
    | ActivitiesHelp Activity
    | ActivitiesLabel Activity
    | ActivitiesTitle Activity
    | ActivitiesToComplete Int
    | ActivityProgressReport Activity
    | ActivePage Page
    | AddChild
    | AddFamilyMember
    | AddFamilyMemberFor String
    | AddParentOrCaregiver
    | AddToGroup
    | Admin
    | AddressInformation
    | Adherence Adherence
    | AgeWord
    | Age Int Int
    | AgeDays Int
    | AgeMonthsWithoutDay Int
    | AgeSingleBoth Int Int
    | AgeSingleMonth Int Int
    | AgeSingleMonthWithoutDay Int
    | AgeSingleDayWithMonth Int Int
    | AgeSingleDayWithoutMonth Int Int
    | AppName
    | AreYouSure
    | Assessment
    | Asthma
    | Attendance
    | Baby
    | BabyDiedOnDayOfBirthPreviousDelivery
    | BabyName String
    | Back
    | BackendError
    | BloodPressure
    | BloodPressureDiaLabel
    | BloodPressureSysLabel
    | BMI
    | BMIHelper
    | BodyTemperature
    | Born
    | BowedLegs
    | BpmUnit
    | BreastExam
    | BreastExamSign BreastExamSign
    | BreastExamQuestion
    | BrittleHair
    | Cancel
    | CardiacDisease
    | CaregiverName
    | CaregiverNationalId
    | CentimeterShorthand
    | Celsius
    | Cell
    | ChartPhrase ChartPhrase
    | CheckIn
    | ChildHmisNumber
    | ChildDemographicInformation
    | ChildNutritionSignLabel ChildNutritionSign
    | ChildNutritionSignReport ChildNutritionSign
    | ChildOf
    | Children
    | ChildrenNames
    | ChildrenNationalId
    | ClickTheCheckMark
    | ClinicType ClinicType
    | Clinical
    | ClinicalProgressReport
    | ConvulsionsAndUnconsciousPreviousDelivery
    | ConvulsionsPreviousDelivery
    | CSectionScar CSectionScar
    | GroupNotFound
    | Group
    | Groups
    | GroupUnauthorized
    | Close
    | Closed
    | ConfirmationRequired
    | ConfirmDeleteTrainingGroupEncounters
    | ConfirmRegisterParticipant
    | Connected
    | ContactInformation
    | Continue
    | CounselingTimingHeading CounselingTiming
    | CounselingTopic CounselingTopic
    | CounselorReviewed
    | CounselorSignature
    | CSectionInPreviousDelivery
    | CSectionReason
    | CSectionReasons CSectionReason
    | CreateGroupEncounter
    | CreateRelationship
    | CreateTrainingGroupEncounters
    | CurrentlyPregnant
    | DangerSign DangerSign
    | Dashboard
    | DateOfLastAssessment
    | Day
    | DaySinglePlural Int
    | DateOfBirth
    | Days
    | Delete
    | DeleteTrainingGroupEncounters
    | DemographicInformation
    | DemographicsReport
    | Device
    | DeviceNotAuthorized
    | DeviceStatus
    | Diabetes
    | District
    | DOB
    | DropzoneDefaultMessage
    | DueDate
    | Edd
    | EddHeader
    | Edema
    | EditRelationship
    | Ega
    | EgaHeader
    | EgaWeeks
    | EmptyString
    | EncounterType EncounterType
    | EndEncounter
    | EndGroupEncounter
    | EnterPairingCode
    | ErrorCheckLocalConfig
    | ErrorConfigurationError
    | Estimated
    | ExaminationTask ExaminationTask
    | Extremities
    | Eyes
    | Failure
    | FamilyInformation
    | FamilyMembers
    | FamilyPlanningInFutureQuestion
    | FamilyPlanningSignLabel FamilyPlanningSign
    | FamilyUbudehe
    | FetalHeartRate
    | FetalMovement
    | FetalPresentationLabel
    | FetalPresentation FetalPresentation
    | Fetch
    | FatherName
    | FatherNationalId
    | FilterByName
    | FirstAntenatalVisit
    | FirstName
    | FiveVisits
    | ForIllustrativePurposesOnly
    | FormError (ErrorValue ValidationError)
    | FormField String
    | FundalHeight
    | Gender Gender
    | GenderLabel
    | GestationalDiabetesPreviousPregnancy
    | GoHome
    | GroupAssessment
    | Gravida
    | Hands
    | HandsCPESign HandsCPESign
    | HeadHair
    | HealthCenter
    | Heart
    | HeartMurmur
    | HeartCPESign HeartCPESign
    | HeartRate
    | Height
    | High
    | HighRiskFactor HighRiskFactor
    | HighRiskFactors
    | HighSeverityAlert HighSeverityAlert
    | HighSeverityAlerts
    | HistoryTask HistoryTask
    | HIV
    | HIVStatus HIVStatus
    | HIVStatusLabel
    | HouseholdSize
    | HttpError Http.Error
    | HypertensionBeforePregnancy
    | IncompleteCervixPreviousPregnancy
    | IndividualEncounter
    | IndividualEncounterTypes
    | KilogramShorthand
    | LastChecked
    | Legs
    | LegsCPESign LegsCPESign
    | LevelOfEducationLabel
    | LevelOfEducation EducationLevel
    | LinkToMother
    | LiveChildren
    | LmpDateConfidentHeader
    | LmpDateHeader
    | LmpRangeHeader
    | LmpRange LmpRange
    | LoginPhrase LoginPhrase
    | Low
    | Lungs
    | LungsCPESign LungsCPESign
    | MakeSureYouAreConnected
    | MaritalStatusLabel
    | MaritalStatus MaritalStatus
    | MeasurementNoChange
    | MeasurementGained Float
    | MeasurementLost Float
    | MedicalDiagnosis
    | MedicalDiagnosisAlert MedicalDiagnosis
    | MedicalFormHelper
    | MemoryQuota { totalJSHeapSize : Int, usedJSHeapSize : Int, jsHeapSizeLimit : Int }
    | MMHGUnit
    | MentalHealthHistory
    | MiddleName
    | MinutesAgo Int
    | ModeOfDelivery ModeOfDelivery
    | ModeOfDeliveryLabel
    | Month
    | MonthAbbrev
    | MonthsOld
    | Mother
    | MotherDemographicInformation
    | MotherName String
    | MotherNameLabel
    | MotherNationalId
    | Mothers
    | MUAC
    | MuacIndication MuacIndication
    | MyAccount
    | MyRelatedBy MyRelatedBy
    | MyRelatedByQuestion MyRelatedBy
    | Name
    | NationalIdNumber
    | Neck
    | NeckCPESign NeckCPESign
    | Next
    | No
    | NoActivitiesCompleted
    | NoActivitiesCompletedForThisParticipant
    | NoActivitiesPending
    | NoActivitiesPendingForThisParticipant
    | NoGroupsFound
    | NoMatchesFound
    | NoParticipantsPending
    | NoParticipantsPendingForThisActivity
    | NoParticipantsCompleted
    | NoParticipantsCompletedForThisActivity
    | Normal
    | NoChildrenRegisteredInTheSystem
    | NoParticipantsFound
    | NotAvailable
    | NotConnected
    | NumberOfAbortions
    | NumberOfChildrenUnder5
    | NumberOfCSections
    | NumberOfLiveChildren
    | NumberOfStillbirthsAtTerm
    | NumberOfStillbirthsPreTerm
    | ObstetricalDiagnosis
    | ObstetricalDiagnosisAlert ObstetricalDiagnosis
    | OK
    | Old
    | OneVisit
    | OnceYouEndYourGroupEncounter
    | Page
    | Page404
    | PageNotFoundMsg
    | PaleConjuctiva
    | Pallor
    | Para
    | PartialPlacentaPreviousDelivery
    | ParticipantDirectory
    | Participants
    | ParticipantReviewed
    | ParticipantSignature
    | ParticipantSummary
    | ParticipantDemographicInformation
    | ParticipantInformation
    | PartnerReceivedCounseling
    | PatientProgress
    | PatientInformation
    | PatientProvisionsTask PatientProvisionsTask
    | People
    | PersistentStorage Bool
    | Person
    | PersonHasBeenSaved
    | PlaceholderEnterHeight
    | PlaceholderEnterMUAC
    | PlaceholderEnterParticipantName
    | PlaceholderEnterWeight
    | PleaseSelectGroup
    | PleaseSync
    | PreeclampsiaPreviousPregnancy
    | PregnancyTrimester PregnancyTrimester
    | PrenatalActivitiesTitle PrenatalActivity
    | PrenatalEncounter
    | PreTerm
    | PregnancyConcludedLabel
    | PreviousCSectionScar
    | PreviousDelivery
    | PreviousDeliveryPeriods PreviousDeliveryPeriod
    | PreviousFloatMeasurement Float
    | PreviousMeasurementNotFound
    | Profession
    | Programs
    | ProgressReport
    | ProgressTimeline
    | ProgressTrends
    | PrenatalParticipant
    | PrenatalParticipants
    | PreTermPregnancy
    | Province
    | ReasonForCSection
    | ReceivedDewormingPill
    | ReceivedIronFolicAcid
    | ReceivedMosquitoNet
    | RecordPregnancyOutcome
    | Register
    | RegisterAParticipant
    | RegisterHelper
    | RegisterNewParticipant
    | RegistratingHealthCenter
    | RegistrationSuccessful
    | RegistrationSuccessfulParticipantAdded
    | RegistrationSuccessfulSuggestAddingChild
    | RegistrationSuccessfulSuggestAddingMother
    | RelationSuccessful
    | RelationSuccessfulChildWithMother
    | RelationSuccessfulMotherWithChild
    | RenalDisease
    | ReportAge String
    | ReportDOB String
    | ReportRemaining Int
    | ReportResultsOfSearch Int
    | Reports
    | RecentAndUpcomingGroupEncounters
    | ReportCompleted { pending : Int, completed : Int }
    | ResolveMonth Month
    | RespiratoryRate
    | Retry
    | RhNegative
    | RiskFactorAlert RiskFactor
    | RiskFactors
    | Save
    | SaveAndNext
    | SaveError
    | Search
    | SearchByName
    | SearchHelper
    | SearchHelperFamilyMember
    | SecondName
    | Sector
    | SelectAntenatalVisit
    | SelectDangerSigns
    | SelectEncounterType
    | SelectGroup
    | SelectProgram
    | SelectLanguage
    | SelectYourGroup
    | SelectYourHealthCenter
    | SelectedHCDownloading
    | SelectedHCNotSynced
    | SelectedHCSyncing
    | SelectedHCUploading
    | ServiceWorkerActive
    | ServiceWorkerCurrent
    | ServiceWorkerCheckForUpdates
    | ServiceWorkerInstalling
    | ServiceWorkerInstalled
    | ServiceWorkerSkipWaiting
    | ServiceWorkerRestarting
    | ServiceWorkerActivating
    | ServiceWorkerActivated
    | ServiceWorkerRedundant
    | ServiceWorkerInactive
    | ServiceWorkerRegNotAsked
    | ServiceWorkerRegLoading
    | ServiceWorkerRegErr
    | ServiceWorkerRegSuccess
    | ServiceWorkerStatus
    | SevereHemorrhagingPreviousDelivery
    | StillbornPreviousDelivery
    | SubsequentAntenatalVisit
    | SuccessiveAbortions
    | SuccessivePrematureDeliveries
    | GroupEncounterClosed
    | GroupEncounterClosed2 SessionId
    | GroupEncounterLoading SessionId
    | GroupEncounterUnauthorized
    | GroupEncounterUnauthorized2
    | ShowAll
    | StartEndDate
    | StartDate
    | EndDate
    | StartSyncing
    | StopSyncing
    | StorageQuota { usage : Int, quota : Int }
    | Submit
    | SubmitPairingCode
    | Success
    | SyncGeneral
    | TakenCareOfBy
    | TasksCompleted Int Int
    | TelephoneNumber
    | Term
    | TermPregnancy
    | ThisActionCannotBeUndone
    | ThisGroupHasNoMothers
    | Training
    | TrainingGroupEncounterCreateSuccessMessage
    | TrainingGroupEncounterDeleteSuccessMessage
    | TrySyncing
    | TuberculosisPast
    | TuberculosisPresent
    | TwoVisits
    | UbudeheLabel
    | Unknown
    | Update
    | UpdateError
    | UterineMyoma
    | ValidationErrors
    | Version
    | View
    | ViewProgressReport
    | Village
    | WeekSinglePlural Int
    | Weight
    | WelcomeUser String
    | WhatDoYouWantToDo
    | Year
    | YearsOld Int
    | Yes
    | YouAreNotAnAdmin
    | YourGroupEncounterHasBeenSaved
    | ZScoreHeightForAge
    | ZScoreMuacForAge
    | ZScoreWeightForAge
    | ZScoreWeightForHeight


translationSet : TranslationId -> TranslationSet String
translationSet trans =
    case trans of
        Abdomen ->
            { english = "Abdomen"
            , kinyarwanda = Nothing
            }

        AbdomenCPESign option ->
            case option of
                Hepatomegaly ->
                    { english = "Hepatomegaly"
                    , kinyarwanda = Nothing
                    }

                Splenomegaly ->
                    { english = "Splenomegaly"
                    , kinyarwanda = Nothing
                    }

                TPRightUpper ->
                    { english = "Tender to Palpation right upper"
                    , kinyarwanda = Nothing
                    }

                TPRightLower ->
                    { english = "Tender to Palpation right lower"
                    , kinyarwanda = Nothing
                    }

                TPLeftUpper ->
                    { english = "Tender to Palpation left upper"
                    , kinyarwanda = Nothing
                    }

                TPLeftLower ->
                    { english = "Tender to Palpation left lower"
                    , kinyarwanda = Nothing
                    }

                Hernia ->
                    { english = "Hernia"
                    , kinyarwanda = Nothing
                    }

                NormalAbdomen ->
                    translationSet Normal

        Abnormal ->
            { english = "Abnormal"
            , kinyarwanda = Nothing
            }

        Abortions ->
            { english = "Abortions"
            , kinyarwanda = Nothing
            }

        AccompaniedByPartner ->
            { english = "Was the patient accompanied by partner during the assessment"
            , kinyarwanda = Nothing
            }

        AccessDenied ->
            { english = "Access denied"
            , kinyarwanda = Just "Kwinjira ntibyemera"
            }

        AddChild ->
            { english = "Add Child"
            , kinyarwanda = Nothing
            }

        AddFamilyMember ->
            { english = "Add Family Member"
            , kinyarwanda = Nothing
            }

        AddFamilyMemberFor name ->
            { english = "Add Family Member for " ++ name
            , kinyarwanda = Nothing
            }

        AddParentOrCaregiver ->
            { english = "Add Parent or Caregiver"
            , kinyarwanda = Nothing
            }

        AddToGroup ->
            { english = "Add to Group..."
            , kinyarwanda = Nothing
            }

        Admin ->
            { english = "Administration"
            , kinyarwanda = Just "Abakuriye"
            }

        AddressInformation ->
            { english = "Address Information"
            , kinyarwanda = Nothing
            }

        AgeWord ->
            { english = "Age"
            , kinyarwanda = Just "Imyaka"
            }

        Activities ->
            { english = "Activities"
            , kinyarwanda = Just "Ibikorwa"
            }

        ActivitiesCompleted count ->
            { english = "Completed (" ++ toString count ++ ")"
            , kinyarwanda = Just <| "Ibyarangiye (" ++ toString count ++ ")"
            }

        ActivitiesHelp activity ->
            case activity of
                MotherActivity Activity.Model.FamilyPlanning ->
                    { english = "Every mother should be asked about her family planning method(s) each month. If a mother needs family planning, refer her to a clinic."
                    , kinyarwanda = Just "Buri mubyeyi agomba kubazwa uburyo bwo kuboneza urubyaro akoresha buri kwezi. Niba umubyeyi akeneye kuboneza urubyaro mwohereze ku kigo nderabuzima k'ubishinzwe"
                    }

                {- MotherActivity ParticipantConsent ->
                       { english = "Please review the following forms with the participant."
                       , kinyarwanda = Nothing
                       }

                   ChildActivity Counseling ->
                       { english = "Please refer to this list during counseling sessions and ensure that each task has been completed."
                       , kinyarwanda = Just "Kurikiza iyi lisiti mu gihe utanga ubujyanama, witondere kureba ko buri gikorwa cyakozwe."
                       }
                -}
                ChildActivity Activity.Model.Height ->
                    { english = "Ask the mother to hold the baby’s head at the end of the measuring board. Move the slider to the baby’s heel and pull their leg straight."
                    , kinyarwanda = Just "Saba Umubyeyi guhagarara inyuma y’umwana we agaramye, afata umutwe ku gice cy’amatwi. Sunikira akabaho ku buryo gakora mu bworo by’ibirenge byombi."
                    }

                ChildActivity Activity.Model.Muac ->
                    { english = "Make sure to measure at the center of the baby’s upper arm."
                    , kinyarwanda = Just "Ibuka gupima icya kabiri cy'akaboko ko hejuru kugira bigufashe guoima ikizigira cy'akaboko"
                    }

                ChildActivity Activity.Model.NutritionSigns ->
                    { english = "Explain to the mother how to check the malnutrition signs for their own child."
                    , kinyarwanda = Just "Sobanurira umubyeyi gupima ibimenyetso by'imirire mibi ku giti cye"
                    }

                ChildActivity Activity.Model.ChildPicture ->
                    { english = "Take each baby’s photo at each health assessment. Photos should show the entire body of each child."
                    , kinyarwanda = Just "Fata ifoto ya buri mwana kuri buri bikorwa by'ipimwa Ifoto igomba kwerekana ibice by'umubiri wose by'umwana"
                    }

                ChildActivity Activity.Model.Weight ->
                    { english = "Calibrate the scale before taking the first baby's weight. Place baby in harness with no clothes on."
                    , kinyarwanda = Just "Ibuka kuregera umunzani mbere yo gupima ibiro by'umwana wa mbere. Ambika umwana ikariso y'ibiro wabanje kumukuramo imyenda iremereye"
                    }

        ActivitiesLabel activity ->
            case activity of
                MotherActivity Activity.Model.FamilyPlanning ->
                    { english = "Which, if any, of the following methods do you use?"
                    , kinyarwanda = Just "Ni ubuhe buryo, niba hari ubuhari, mu buryo bukurikira bwo kuboneza urubyaro ukoresha? Muri ubu buryo bukurikira bwo kuboneza urubyaro, ni ubuhe buryo mukoresha?"
                    }

                {- MotherActivity ParticipantConsent ->
                       { english = "Forms:"
                       , kinyarwanda = Nothing
                       }

                   ChildActivity Counseling ->
                       { english = "Please refer to this list during counseling sessions and ensure that each task has been completed."
                       , kinyarwanda = Just "Kurikiza iyi lisiti mu gihe utanga ubujyanama, witondere kureba ko buri gikorwa cyakozwe."
                       }
                -}
                ChildActivity Activity.Model.Height ->
                    { english = "Height:"
                    , kinyarwanda = Just "Uburere:"
                    }

                ChildActivity Activity.Model.Muac ->
                    { english = "MUAC:"
                    , kinyarwanda = Just "Ikizigira cy'akaboko:"
                    }

                ChildActivity Activity.Model.NutritionSigns ->
                    { english = "Select all signs that are present:"
                    , kinyarwanda = Just "Hitamo ibimenyetso by'imirire byose bishoboka umwana afite:"
                    }

                ChildActivity Activity.Model.ChildPicture ->
                    { english = "Photo:"
                    , kinyarwanda = Just "Ifoto"
                    }

                ChildActivity Activity.Model.Weight ->
                    { english = "Weight:"
                    , kinyarwanda = Just "Ibiro:"
                    }

        ActivitiesTitle activity ->
            case activity of
                MotherActivity Activity.Model.FamilyPlanning ->
                    { english = "Family Planning"
                    , kinyarwanda = Just "Kuboneza Urubyaro? nticyaza muri raporo yimikurire yumwana"
                    }

                {- MotherActivity ParticipantConsent ->
                       { english = "Forms"
                       , kinyarwanda = Nothing
                       }

                   ChildActivity Counseling ->
                       { english = "Counseling"
                       , kinyarwanda = Just "Ubujyanama"
                       }
                -}
                ChildActivity Activity.Model.Height ->
                    { english = "Height"
                    , kinyarwanda = Just "Uburebure"
                    }

                ChildActivity Activity.Model.Muac ->
                    { english = "MUAC"
                    , kinyarwanda = Just "Ikizigira cy'akaboko"
                    }

                ChildActivity Activity.Model.NutritionSigns ->
                    { english = "Nutrition"
                    , kinyarwanda = Just "Imirire"
                    }

                ChildActivity Activity.Model.ChildPicture ->
                    { english = "Photo"
                    , kinyarwanda = Just "Ifoto"
                    }

                ChildActivity Activity.Model.Weight ->
                    { english = "Weight"
                    , kinyarwanda = Just "Ibiro"
                    }

        ActivityProgressReport activity ->
            case activity of
                MotherActivity Activity.Model.FamilyPlanning ->
                    { english = "Family Planning"
                    , kinyarwanda = Just "Kuboneza Urubyaro? nticyaza muri raporo yimikurire yumwana"
                    }

                {- MotherActivity ParticipantConsent ->
                       { english = "Forms"
                       , kinyarwanda = Nothing
                       }

                   ChildActivity Counseling ->
                       { english = "Counseling"
                       , kinyarwanda = Nothing
                       }
                -}
                ChildActivity Activity.Model.Height ->
                    { english = "Height"
                    , kinyarwanda = Just "Uburebure"
                    }

                ChildActivity Activity.Model.Muac ->
                    { english = "MUAC"
                    , kinyarwanda = Just "Ikizigira cy'akaboko"
                    }

                ChildActivity Activity.Model.NutritionSigns ->
                    { english = "Nutrition Signs"
                    , kinyarwanda = Just "Ibimenyetso by'imirire"
                    }

                ChildActivity Activity.Model.ChildPicture ->
                    { english = "Photo"
                    , kinyarwanda = Just "Ifoto"
                    }

                ChildActivity Activity.Model.Weight ->
                    { english = "Weight"
                    , kinyarwanda = Just "Ibiro"
                    }

        ActivitiesToComplete count ->
            { english = "To Do (" ++ toString count ++ ")"
            , kinyarwanda = Just <| "Ibisabwa gukora (" ++ toString count ++ ")"
            }

        ActivePage page ->
            translateActivePage page

        Adherence adherence ->
            translateAdherence adherence

        Age months days ->
            { english = toString months ++ " months " ++ toString days ++ " days"
            , kinyarwanda = Just <| toString months ++ " Amezi " ++ toString days ++ " iminsi"
            }

        AgeDays days ->
            { english = toString days ++ " days"
            , kinyarwanda = Just <| toString days ++ " Iminsi"
            }

        AgeMonthsWithoutDay months ->
            { english = toString months ++ " month"
            , kinyarwanda = Just <| toString months ++ " Ukwezi"
            }

        AgeSingleBoth months days ->
            { english = toString months ++ " month " ++ toString days ++ " day"
            , kinyarwanda = Just <| toString months ++ " Ukwezi " ++ toString days ++ " Umunsi"
            }

        AgeSingleMonth months days ->
            { english = toString months ++ " month " ++ toString days ++ " days"
            , kinyarwanda = Just <| toString months ++ " Ukwezi " ++ toString days ++ " Iminsi"
            }

        AgeSingleDayWithMonth months days ->
            { english = toString months ++ " months " ++ toString days ++ " day"
            , kinyarwanda = Just <| toString months ++ " Amezi " ++ toString days ++ " Umunsi"
            }

        AgeSingleDayWithoutMonth months days ->
            { english = toString days ++ " day"
            , kinyarwanda = Just <| toString days ++ " Umunsi"
            }

        AgeSingleMonthWithoutDay month ->
            { english = toString month ++ " month"
            , kinyarwanda = Just <| toString month ++ " Ukwezi"
            }

        AppName ->
            { english = "E-Heza System"
            , kinyarwanda = Just "E-heza sisiteme"
            }

        AreYouSure ->
            { english = "Are you sure?"
            , kinyarwanda = Just "Urabyizeye?"
            }

        Assessment ->
            { english = "Assessment"
            , kinyarwanda = Just "Ipimwa"
            }

        Asthma ->
            { english = "Asthma"
            , kinyarwanda = Nothing
            }

        Attendance ->
            { english = "Attendance"
            , kinyarwanda = Just "Ubwitabire"
            }

        Baby ->
            { english = "Baby"
            , kinyarwanda = Just "Umwana"
            }

        BabyDiedOnDayOfBirthPreviousDelivery ->
            { english = "Live Birth but the baby died the same day in previous delivery"
            , kinyarwanda = Nothing
            }

        BabyName name ->
            { english = "Baby: " ++ name
            , kinyarwanda = Just <| "Umwana: " ++ name
            }

        Back ->
            { english = "Back"
            , kinyarwanda = Nothing
            }

        BackendError ->
            { english = "Error contacting backend"
            , kinyarwanda = Just "Seriveri yerekanye amakosa akurikira"
            }

        BloodPressure ->
            { english = "Blood Pressure"
            , kinyarwanda = Nothing
            }

        BloodPressureDiaLabel ->
            { english = "Diastolic"
            , kinyarwanda = Nothing
            }

        BloodPressureSysLabel ->
            { english = "Systolic"
            , kinyarwanda = Nothing
            }

        BMI ->
            { english = "BMI"
            , kinyarwanda = Nothing
            }

        BMIHelper ->
            { english = "Calculated based on Height and Weight"
            , kinyarwanda = Nothing
            }

        BodyTemperature ->
            { english = "Body Temperature"
            , kinyarwanda = Nothing
            }

        Born ->
            { english = "Born"
            , kinyarwanda = Just "Kuvuka/ itariki y'amavuko"
            }

        BowedLegs ->
            { english = "Bowed Legs"
            , kinyarwanda = Nothing
            }

        BpmUnit ->
            { english = "bpm"
            , kinyarwanda = Nothing
            }

        BreastExam ->
            { english = "Breast Exam"
            , kinyarwanda = Nothing
            }

        BreastExamQuestion ->
            { english = "Did you show the patient how to perform a self breast exam"
            , kinyarwanda = Nothing
            }

        BreastExamSign option ->
            case option of
                Mass ->
                    { english = "Mass"
                    , kinyarwanda = Nothing
                    }

                Discharge ->
                    { english = "Discharge"
                    , kinyarwanda = Nothing
                    }

                Infection ->
                    { english = "Infection"
                    , kinyarwanda = Nothing
                    }

                NormalBreast ->
                    translationSet Normal

        BrittleHair ->
            { english = "Brittle Hair"
            , kinyarwanda = Just "Gucurama no guhindura ibara ku misatsi"
            }

        Cancel ->
            { english = "Cancel"
            , kinyarwanda = Just "Guhagarika"
            }

        CardiacDisease ->
            { english = "Cardiac Disease"
            , kinyarwanda = Nothing
            }

        CaregiverName ->
            { english = "Caregiver's Name"
            , kinyarwanda = Nothing
            }

        CaregiverNationalId ->
            { english = "Caregiver's National ID"
            , kinyarwanda = Nothing
            }

        Cell ->
            { english = "Cell"
            , kinyarwanda = Nothing
            }

        CentimeterShorthand ->
            { english = "cm"
            , kinyarwanda = Just "cm"
            }

        Celsius ->
            { english = "Celsius"
            , kinyarwanda = Nothing
            }

        ChartPhrase phrase ->
            translateChartPhrase phrase

        CheckIn ->
            { english = "Check in:"
            , kinyarwanda = Just "Kureba abaje"
            }

        ChildHmisNumber ->
            { english = "Child HMIS Number"
            , kinyarwanda = Nothing
            }

        ChildDemographicInformation ->
            { english = "Child Demographic Information"
            , kinyarwanda = Nothing
            }

        ChildNutritionSignLabel sign ->
            case sign of
                AbdominalDistension ->
                    { english = "Abdominal Distension"
                    , kinyarwanda = Just "Kubyimba inda"
                    }

                Apathy ->
                    { english = "Apathy"
                    , kinyarwanda = Just "Kwigunga"
                    }

                Backend.Measurement.Model.BrittleHair ->
                    translationSet BrittleHair

                DrySkin ->
                    { english = "Dry Skin"
                    , kinyarwanda = Just "Uruhu ryumye"
                    }

                Backend.Measurement.Model.Edema ->
                    translationSet Edema

                NormalChildNutrition ->
                    { english = "None of these"
                    , kinyarwanda = Just "Nta bimenyetso "
                    }

                PoorAppetite ->
                    { english = "Poor Appetite"
                    , kinyarwanda = Just "Kubura apeti /kunanirwa kurya"
                    }

        ChildNutritionSignReport sign ->
            case sign of
                AbdominalDistension ->
                    { english = "Abdominal Distension"
                    , kinyarwanda = Just "Kubyimba inda"
                    }

                Apathy ->
                    { english = "Apathy"
                    , kinyarwanda = Just "Kwigunga"
                    }

                Backend.Measurement.Model.BrittleHair ->
                    translationSet BrittleHair

                DrySkin ->
                    { english = "Dry Skin"
                    , kinyarwanda = Just "Uruhu ryumye"
                    }

                Backend.Measurement.Model.Edema ->
                    translationSet Edema

                NormalChildNutrition ->
                    { english = "None"
                    , kinyarwanda = Just "Nta bimenyetso"
                    }

                PoorAppetite ->
                    { english = "Poor Appetite"
                    , kinyarwanda = Just "kubura apeti (kunanirwa kurya)"
                    }

        Children ->
            { english = "Children"
            , kinyarwanda = Just "Abana"
            }

        ChildrenNames ->
            { english = "Children's names"
            , kinyarwanda = Nothing
            }

        ChildrenNationalId ->
            { english = "Children's National ID"
            , kinyarwanda = Nothing
            }

        ChildOf ->
            { english = "Child of"
            , kinyarwanda = Just "Umwana wa"
            }

        ClickTheCheckMark ->
            { english = "Click the check mark if the mother / caregiver is in attendance. The check mark will appear green when a mother / caregiver has been signed in."
            , kinyarwanda = Just "Kanda (kuri) ku kazu niba umubyeyi ahari. Ku kazu harahita hahindura ibara habe icyaytsi niba wemeje ko umubyeyi ahari"
            }

        ClinicType clinicType ->
            case clinicType of
                Fbf ->
                    { english = "Fbf"
                    , kinyarwanda = Nothing
                    }

                Pmtct ->
                    { english = "Pmtct"
                    , kinyarwanda = Nothing
                    }

                Sorwathe ->
                    { english = "Sorwathe"
                    , kinyarwanda = Nothing
                    }

        Clinical ->
            { english = "Clinical"
            , kinyarwanda = Nothing
            }

        ClinicalProgressReport ->
            { english = "Clinical Progress Report"
            , kinyarwanda = Nothing
            }

        ConvulsionsAndUnconsciousPreviousDelivery ->
            { english = "Experienced convulsions and resulted in becoming unconscious after delivery"
            , kinyarwanda = Nothing
            }

        ConvulsionsPreviousDelivery ->
            { english = "Experienced convulsions in previous delivery"
            , kinyarwanda = Nothing
            }

        CSectionScar scar ->
            case scar of
                Vertical ->
                    { english = "Vertical"
                    , kinyarwanda = Nothing
                    }

                Horizontal ->
                    { english = "Horizontal"
                    , kinyarwanda = Nothing
                    }

                NoScar ->
                    { english = "None"
                    , kinyarwanda = Nothing
                    }

        GroupNotFound ->
            { english = "Group not found"
            , kinyarwanda = Nothing
            }

        Group ->
            { english = "Group"
            , kinyarwanda = Nothing
            }

        Groups ->
            { english = "Groups"
            , kinyarwanda = Nothing
            }

        Close ->
            { english = "Close"
            , kinyarwanda = Nothing
            }

        Closed ->
            { english = "Closed"
            , kinyarwanda = Just "Gufunga"
            }

        GroupUnauthorized ->
            { english = "You are not authorized to work with this Group."
            , kinyarwanda = Nothing
            }

        ConfirmDeleteTrainingGroupEncounters ->
            { english = "Are you sure you want to delete all training Group Encounters?"
            , kinyarwanda = Nothing
            }

        ConfirmRegisterParticipant ->
            { english = "Are you sure you want to save this participant's data?"
            , kinyarwanda = Nothing
            }

        ConfirmationRequired ->
            { english = "Please confirm:"
            , kinyarwanda = Nothing
            }

        Connected ->
            { english = "Connected"
            , kinyarwanda = Just "Ufite interineti (murandasi)"
            }

        ContactInformation ->
            { english = "Contact Information"
            , kinyarwanda = Nothing
            }

        Continue ->
            { english = "Continue"
            , kinyarwanda = Just "Gukomeza"
            }

        CounselingTimingHeading timing ->
            translateCounselingTimingHeading timing

        CounselingTopic topic ->
            { english = topic.english
            , kinyarwanda = topic.kinyarwanda
            }

        CounselorReviewed ->
            { english = "I have reviewed the above with the participant."
            , kinyarwanda = Nothing
            }

        CounselorSignature ->
            { english = "Entry Counselor Signature"
            , kinyarwanda = Nothing
            }

        CSectionInPreviousDelivery ->
            { english = "C-section in previous delivery"
            , kinyarwanda = Nothing
            }

        CSectionReason ->
            { english = "Reason for C-section"
            , kinyarwanda = Nothing
            }

        CSectionReasons reason ->
            case reason of
                Breech ->
                    { english = "Breech"
                    , kinyarwanda = Nothing
                    }

                Emergency ->
                    { english = "Emergency"
                    , kinyarwanda = Nothing
                    }

                FailureToProgress ->
                    { english = "Failure to Progress"
                    , kinyarwanda = Nothing
                    }

                Backend.Measurement.Model.None ->
                    { english = "None"
                    , kinyarwanda = Nothing
                    }

                Other ->
                    { english = "Other"
                    , kinyarwanda = Nothing
                    }

        CreateGroupEncounter ->
            { english = "Create Group Encounter"
            , kinyarwanda = Just "Tangira igikorwa"
            }

        CreateRelationship ->
            { english = "Create Relationship"
            , kinyarwanda = Nothing
            }

        CreateTrainingGroupEncounters ->
            { english = "Create All Training Group Encounters"
            , kinyarwanda = Nothing
            }

        CurrentlyPregnant ->
            { english = "Currently Pregnant"
            , kinyarwanda = Nothing
            }

        DeleteTrainingGroupEncounters ->
            { english = "Delete All Training Group Encounters"
            , kinyarwanda = Nothing
            }

        DangerSign sign ->
            case sign of
                VaginalBleeding ->
                    { english = "Vaginal bleeding"
                    , kinyarwanda = Nothing
                    }

                HeadacheBlurredVision ->
                    { english = "Severe headaches with blurred vision"
                    , kinyarwanda = Nothing
                    }

                Convulsions ->
                    { english = "Convulsions"
                    , kinyarwanda = Nothing
                    }

                AbdominalPain ->
                    { english = "Abdominal pain"
                    , kinyarwanda = Nothing
                    }

                DifficultyBreathing ->
                    { english = "Difficulty breathing"
                    , kinyarwanda = Nothing
                    }

                Fever ->
                    { english = "Fever"
                    , kinyarwanda = Nothing
                    }

                ExtremeWeakness ->
                    { english = "Extreme weakness"
                    , kinyarwanda = Nothing
                    }

                NoDangerSign ->
                    { english = "None of these"
                    , kinyarwanda = Nothing
                    }

        Dashboard ->
            { english = "Dashboard"
            , kinyarwanda = Just "Tabeau de bord"
            }

        DateOfLastAssessment ->
            { english = "Date of last Assessment"
            , kinyarwanda = Just "Amakuru y'ipimwa ry'ubushize"
            }

        Day ->
            { english = "Day"
            , kinyarwanda = Just "Umunsi"
            }

        DaySinglePlural value ->
            if value == 1 then
                { english = "1 Day"
                , kinyarwanda = Nothing
                }

            else
                { english = toString value ++ " Days"
                , kinyarwanda = Nothing
                }

        DateOfBirth ->
            { english = "Date of Birth"
            , kinyarwanda = Nothing
            }

        Days ->
            { english = "days"
            , kinyarwanda = Just "Iminsi"
            }

        Delete ->
            { english = "Delete"
            , kinyarwanda = Nothing
            }

        DemographicInformation ->
            { english = "Demographic Information"
            , kinyarwanda = Nothing
            }

        DemographicsReport ->
            { english = "Demographics Report"
            , kinyarwanda = Nothing
            }

        Device ->
            { english = "Device"
            , kinyarwanda = Nothing
            }

        DeviceNotAuthorized ->
            { english =
                """This device has not yet been authorized to sync data with the backend, or the
                authorization has expired or been revoked. To authorize or re-authorize this
                device, enter a pairing code below. This will permit sensitive data to be stored
                on this device and updated to the backend. You should only authorize devices that
                are under your control and which are secure."""
            , kinyarwanda = Nothing
            }

        DeviceStatus ->
            { english = "Device Status"
            , kinyarwanda = Nothing
            }

        Diabetes ->
            { english = "Diabetes"
            , kinyarwanda = Nothing
            }

        District ->
            { english = "District"
            , kinyarwanda = Nothing
            }

        DOB ->
            { english = "DOB"
            , kinyarwanda = Nothing
            }

        DropzoneDefaultMessage ->
            { english = "Touch here to take a photo, or drop a photo file here."
            , kinyarwanda = Just "Kanda hano niba ushaka gufotora cg ukure ifoto mu bubiko hano."
            }

        DueDate ->
            { english = "Due Date"
            , kinyarwanda = Nothing
            }

        Edd ->
            { english = "EDD"
            , kinyarwanda = Nothing
            }

        EddHeader ->
            { english = "Estimated Date of Delivery"
            , kinyarwanda = Nothing
            }

        Edema ->
            { english = "Edema"
            , kinyarwanda = Just "Kubyimba"
            }

        EditRelationship ->
            { english = "Edit Relationship"
            , kinyarwanda = Nothing
            }

        Ega ->
            { english = "EGA"
            , kinyarwanda = Nothing
            }

        EgaHeader ->
            { english = "Estimated Gestational Age"
            , kinyarwanda = Nothing
            }

        EgaWeeks ->
            { english = "EGA (Weeks)"
            , kinyarwanda = Nothing
            }

        EmptyString ->
            { english = ""
            , kinyarwanda = Just ""
            }

        EncounterType type_ ->
            case type_ of
                AntenatalEncounter ->
                    { english = "Antenatal"
                    , kinyarwanda = Nothing
                    }

                InmmunizationEncounter ->
                    { english = "Inmmunization"
                    , kinyarwanda = Nothing
                    }

                NutritionEncounter ->
                    { english = "Nutrition"
                    , kinyarwanda = Nothing
                    }

        EndEncounter ->
            { english = "End Encounter"
            , kinyarwanda = Nothing
            }

        EndGroupEncounter ->
            { english = "End Group Encounter"
            , kinyarwanda = Nothing
            }

        EnterPairingCode ->
            { english = "Enter pairing code"
            , kinyarwanda = Nothing
            }

        MemoryQuota quota ->
            { english = "Memory used " ++ toString (quota.usedJSHeapSize // (1024 * 1024)) ++ " MB of available " ++ toString (quota.jsHeapSizeLimit // (1024 * 1024)) ++ " MB"
            , kinyarwanda = Nothing
            }

        StorageQuota quota ->
            { english = "Storage used " ++ toString (quota.usage // (1024 * 1024)) ++ " MB of available " ++ toString (quota.quota // (1024 * 1024)) ++ " MB"
            , kinyarwanda = Nothing
            }

        SubmitPairingCode ->
            { english = "Submit Pairing Code"
            , kinyarwanda = Nothing
            }

        ErrorCheckLocalConfig ->
            { english = "Check your LocalConfig.elm file and make sure you have defined the enviorement properly"
            , kinyarwanda = Nothing
            }

        ErrorConfigurationError ->
            { english = "Configuration error"
            , kinyarwanda = Just "Ikosa mu igena miterere"
            }

        Estimated ->
            { english = "Estimated"
            , kinyarwanda = Nothing
            }

        ExaminationTask task ->
            case task of
                Vitals ->
                    { english = "Vitals"
                    , kinyarwanda = Nothing
                    }

                NutritionAssessment ->
                    { english = "Nutrition Assessment"
                    , kinyarwanda = Nothing
                    }

                CorePhysicalExam ->
                    { english = "Core Physical Exam"
                    , kinyarwanda = Nothing
                    }

                ObstetricalExam ->
                    { english = "Obstetrical Exam"
                    , kinyarwanda = Nothing
                    }

                Pages.PrenatalActivity.Model.BreastExam ->
                    translationSet BreastExam

        Failure ->
            { english = "Failure"
            , kinyarwanda = Nothing
            }

        Extremities ->
            { english = "Extremities"
            , kinyarwanda = Nothing
            }

        Eyes ->
            { english = "Eyes"
            , kinyarwanda = Nothing
            }

        FamilyInformation ->
            { english = "Family Information"
            , kinyarwanda = Nothing
            }

        FamilyMembers ->
            { english = "Family Members"
            , kinyarwanda = Nothing
            }

        FamilyPlanningInFutureQuestion ->
            { english = "Which, if any, of these methods will you use after your pregnancy"
            , kinyarwanda = Nothing
            }

        FamilyPlanningSignLabel sign ->
            case sign of
                AutoObservation ->
                    { english = "Auto-observation"
                    , kinyarwanda = Nothing
                    }

                Condoms ->
                    { english = "Condoms"
                    , kinyarwanda = Just "Udukingirizo"
                    }

                CycleBeads ->
                    { english = "Cycle beads"
                    , kinyarwanda = Nothing
                    }

                CycleCounting ->
                    { english = "Cycle counting"
                    , kinyarwanda = Nothing
                    }

                Hysterectomy ->
                    { english = "Hysterectomy"
                    , kinyarwanda = Nothing
                    }

                Implants ->
                    { english = "Implants"
                    , kinyarwanda = Nothing
                    }

                Injectables ->
                    { english = "Injectables"
                    , kinyarwanda = Nothing
                    }

                IUD ->
                    { english = "IUD"
                    , kinyarwanda = Just "Akapira ko mu mura (agapira ko munda ibyara)"
                    }

                LactationAmenorrhea ->
                    { english = "Lactation amenorrhea"
                    , kinyarwanda = Nothing
                    }

                NoFamilyPlanning ->
                    { english = "None of these"
                    , kinyarwanda = Just "nta buryo bwo kuboneza urubyaro akoresha"
                    }

                OralContraceptives ->
                    { english = "Oral contraceptives"
                    , kinyarwanda = Nothing
                    }

                Spermicide ->
                    { english = "Spermicide"
                    , kinyarwanda = Nothing
                    }

                TubalLigatures ->
                    { english = "Tubal ligatures"
                    , kinyarwanda = Nothing
                    }

                Vasectomy ->
                    { english = "Vasectomy"
                    , kinyarwanda = Nothing
                    }

        FamilyUbudehe ->
            { english = "Family Ubudehe"
            , kinyarwanda = Nothing
            }

        FatherName ->
            { english = "Father's Name"
            , kinyarwanda = Nothing
            }

        FatherNationalId ->
            { english = "Father's National ID"
            , kinyarwanda = Nothing
            }

        FetalHeartRate ->
            { english = "Fetal Heart Rate"
            , kinyarwanda = Nothing
            }

        FetalMovement ->
            { english = "Fetal Movement"
            , kinyarwanda = Nothing
            }

        FetalPresentationLabel ->
            { english = "Fetal Presentation"
            , kinyarwanda = Nothing
            }

        FetalPresentation option ->
            case option of
                FetalBreech ->
                    { english = "Breech"
                    , kinyarwanda = Nothing
                    }

                Cephalic ->
                    { english = "Cephalic"
                    , kinyarwanda = Nothing
                    }

                Transverse ->
                    { english = "Transverse"
                    , kinyarwanda = Nothing
                    }

                Twins ->
                    { english = "Twins"
                    , kinyarwanda = Nothing
                    }

                Backend.Measurement.Model.Unknown ->
                    { english = "Unknown"
                    , kinyarwanda = Nothing
                    }

        Fetch ->
            { english = "Fetch"
            , kinyarwanda = Just "Gushakisha"
            }

        FilterByName ->
            { english = "Filter by name"
            , kinyarwanda = Nothing
            }

        FirstAntenatalVisit ->
            { english = "First Antenatal Visit"
            , kinyarwanda = Nothing
            }

        FirstName ->
            { english = "First Name"
            , kinyarwanda = Nothing
            }

        FiveVisits ->
            { english = "Five visits"
            , kinyarwanda = Nothing
            }

        ForIllustrativePurposesOnly ->
            { english = "For illustrative purposes only"
            , kinyarwanda = Nothing
            }

        FormError errorValue ->
            translateFormError errorValue

        FormField field ->
            translateFormField field

        FundalHeight ->
            { english = "Fundal Height"
            , kinyarwanda = Nothing
            }

        Gender gender ->
            case gender of
                Male ->
                    { english = "Male"
                    , kinyarwanda = Just "Gabo"
                    }

                Female ->
                    { english = "Female"
                    , kinyarwanda = Just "Gore"
                    }

        GenderLabel ->
            { english = "Gender"
            , kinyarwanda = Nothing
            }

        GestationalDiabetesPreviousPregnancy ->
            { english = "Gestational Diabetes in previous pregnancy"
            , kinyarwanda = Nothing
            }

        GoHome ->
            { english = "Go to main page"
            , kinyarwanda = Just "Kujya ahabanza"
            }

        GroupAssessment ->
            { english = "Group Assessment"
            , kinyarwanda = Nothing
            }

        Gravida ->
            { english = "Gravida"
            , kinyarwanda = Nothing
            }

        Hands ->
            { english = "Hands"
            , kinyarwanda = Nothing
            }

        HandsCPESign option ->
            case option of
                PallorHands ->
                    translationSet Pallor

                EdemaHands ->
                    translationSet Edema

                NormalHands ->
                    translationSet Normal

        HeadHair ->
            { english = "Head/Hair"
            , kinyarwanda = Nothing
            }

        HealthCenter ->
            { english = "Health Center"
            , kinyarwanda = Nothing
            }

        Heart ->
            { english = "Heart"
            , kinyarwanda = Nothing
            }

        HeartMurmur ->
            { english = "Heart Murmur"
            , kinyarwanda = Nothing
            }

        HeartCPESign sign ->
            case sign of
                IrregularRhythm ->
                    { english = "Irregular Rhythm"
                    , kinyarwanda = Nothing
                    }

                NormalRateAndRhythm ->
                    { english = "Normal Rate And Rhythm"
                    , kinyarwanda = Nothing
                    }

                SinusTachycardia ->
                    { english = "Sinus Tachycardia"
                    , kinyarwanda = Nothing
                    }

        HeartRate ->
            { english = "Heart Rate"
            , kinyarwanda = Nothing
            }

        Height ->
            { english = "Height"
            , kinyarwanda = Just "Uburebure"
            }

        High ->
            { english = "High"
            , kinyarwanda = Nothing
            }

        HighRiskFactor factor ->
            case factor of
                PrenatalActivity.Model.ConvulsionsAndUnconsciousPreviousDelivery ->
                    { english = "Patient experienced convulsions in previous delivery and became unconscious after delivery"
                    , kinyarwanda = Nothing
                    }

                PrenatalActivity.Model.ConvulsionsPreviousDelivery ->
                    { english = "Patient experienced convulsions in previous delivery"
                    , kinyarwanda = Nothing
                    }

        HighRiskFactors ->
            { english = "High Risk Factors"
            , kinyarwanda = Nothing
            }

        HighSeverityAlert alert ->
            case alert of
                PrenatalActivity.Model.BodyTemperature ->
                    { english = "Body Temperature"
                    , kinyarwanda = Nothing
                    }

                PrenatalActivity.Model.BloodPressure ->
                    { english = "Blood Pressure"
                    , kinyarwanda = Nothing
                    }

                PrenatalActivity.Model.FetalHeartRate ->
                    { english = "No fetal heart rate noted"
                    , kinyarwanda = Nothing
                    }

                PrenatalActivity.Model.FetalMovement ->
                    { english = "No fetal movement noted"
                    , kinyarwanda = Nothing
                    }

                PrenatalActivity.Model.HeartRate ->
                    { english = "Heart Rate"
                    , kinyarwanda = Nothing
                    }

                PrenatalActivity.Model.RespiratoryRate ->
                    { english = "Respiratory Rate"
                    , kinyarwanda = Nothing
                    }

        HighSeverityAlerts ->
            { english = "High Severity Alerts"
            , kinyarwanda = Nothing
            }

        HistoryTask task ->
            case task of
                Obstetric ->
                    { english = "Obstetric History"
                    , kinyarwanda = Nothing
                    }

                Medical ->
                    { english = "Medical History"
                    , kinyarwanda = Nothing
                    }

                Social ->
                    { english = "Social History"
                    , kinyarwanda = Nothing
                    }

        HIV ->
            { english = "HIV"
            , kinyarwanda = Nothing
            }

        HIVStatus status ->
            case status of
                HIVExposedInfant ->
                    { english = "HIV-exposed Infant"
                    , kinyarwanda = Nothing
                    }

                Negative ->
                    { english = "Negative"
                    , kinyarwanda = Nothing
                    }

                NegativeDiscordantCouple ->
                    { english = "Negative - discordant couple"
                    , kinyarwanda = Nothing
                    }

                Positive ->
                    { english = "Positive"
                    , kinyarwanda = Nothing
                    }

                Backend.Person.Model.Unknown ->
                    { english = "Unknown"
                    , kinyarwanda = Nothing
                    }

        HIVStatusLabel ->
            { english = "HIV Status"
            , kinyarwanda = Nothing
            }

        HouseholdSize ->
            { english = "Household Size"
            , kinyarwanda = Nothing
            }

        HttpError error ->
            translateHttpError error

        HypertensionBeforePregnancy ->
            { english = "Hypertension before pregnancy"
            , kinyarwanda = Nothing
            }

        IncompleteCervixPreviousPregnancy ->
            { english = "Incomplete Cervix in previous pregnancy"
            , kinyarwanda = Nothing
            }

        IndividualEncounter ->
            { english = "Individual Encounter"
            , kinyarwanda = Nothing
            }

        IndividualEncounterTypes ->
            { english = "Individual Encounter Types"
            , kinyarwanda = Nothing
            }

        KilogramShorthand ->
            { english = "kg"
            , kinyarwanda = Just "kg"
            }

        LastChecked ->
            { english = "Last checked"
            , kinyarwanda = Nothing
            }

        Legs ->
            { english = "Legs"
            , kinyarwanda = Nothing
            }

        LegsCPESign option ->
            case option of
                PallorLegs ->
                    translationSet Pallor

                EdemaLegs ->
                    translationSet Edema

                NormalLegs ->
                    translationSet Normal

        LevelOfEducationLabel ->
            { english = "Level of Education"
            , kinyarwanda = Just <| "Amashuri wize"
            }

        LevelOfEducation educationLevel ->
            case educationLevel of
                NoSchooling ->
                    { english = "No Schooling"
                    , kinyarwanda = Just "Ntayo"
                    }

                PrimarySchool ->
                    { english = "Primary School"
                    , kinyarwanda = Just "Abanza"
                    }

                VocationalTrainingSchool ->
                    { english = "Vocational Training School"
                    , kinyarwanda = Just "Imyuga"
                    }

                SecondarySchool ->
                    { english = "Secondary School"
                    , kinyarwanda = Just "Ayisumbuye"
                    }

                DiplomaProgram ->
                    { english = "Diploma Program (2 years of University)"
                    , kinyarwanda = Just "Amashuri 2 ya Kaminuza"
                    }

                HigherEducation ->
                    { english = "Higher Education (University)"
                    , kinyarwanda = Just "(A0)"
                    }

                AdvancedDiploma ->
                    { english = "Advanced Diploma"
                    , kinyarwanda = Just "(A1)"
                    }

        LinkToMother ->
            { english = "Link to mother"
            , kinyarwanda = Just "Guhuza n'amakuru y'umubyeyi"
            }

        LiveChildren ->
            { english = "Live Children"
            , kinyarwanda = Nothing
            }

        LmpDateConfidentHeader ->
            { english = "Is the Patient confident of LMP Date"
            , kinyarwanda = Nothing
            }

        LmpDateHeader ->
            { english = "Last Menstrual Period Date"
            , kinyarwanda = Nothing
            }

        LmpRangeHeader ->
            { english = "When was the Patient's Last Menstrual Period"
            , kinyarwanda = Nothing
            }

        LmpRange range ->
            case range of
                OneMonth ->
                    { english = "Within 1 month"
                    , kinyarwanda = Nothing
                    }

                ThreeMonth ->
                    { english = "Within 3 months"
                    , kinyarwanda = Nothing
                    }

                SixMonth ->
                    { english = "Within 6 months"
                    , kinyarwanda = Nothing
                    }

        LoginPhrase phrase ->
            translateLoginPhrase phrase

        Low ->
            { english = "Low"
            , kinyarwanda = Nothing
            }

        Lungs ->
            { english = "Lungs"
            , kinyarwanda = Nothing
            }

        LungsCPESign option ->
            case option of
                Wheezes ->
                    { english = "Wheezes"
                    , kinyarwanda = Nothing
                    }

                Crackles ->
                    { english = "Crackles"
                    , kinyarwanda = Nothing
                    }

                NormalLungs ->
                    translationSet Normal

        MakeSureYouAreConnected ->
            { english = "Make sure you are connected to the internet. If the issue continues, call The Ihangane Project at +250 788 817 542."
            , kinyarwanda = Just "Banza urebe ko ufite interineti. Ikibazo nigikomeza, hamagara The Ihangane Project kuri +250 788 817 542"
            }

        MaritalStatusLabel ->
            { english = "Marital Status"
            , kinyarwanda = Nothing
            }

        MaritalStatus status ->
            case status of
                Divorced ->
                    { english = "Divorced"
                    , kinyarwanda = Nothing
                    }

                Married ->
                    { english = "Married"
                    , kinyarwanda = Nothing
                    }

                Single ->
                    { english = "Single"
                    , kinyarwanda = Nothing
                    }

                Widowed ->
                    { english = "Widowed"
                    , kinyarwanda = Nothing
                    }

        MeasurementNoChange ->
            { english = "No Change"
            , kinyarwanda = Just "nta cyahindutse"
            }

        MeasurementGained amount ->
            { english = "Gained " ++ toString amount
            , kinyarwanda = Just <| "Kwiyongera " ++ toString amount
            }

        MeasurementLost amount ->
            { english = "Lost " ++ toString amount
            , kinyarwanda = Just <| "Kwiyongera " ++ toString amount
            }

        MedicalDiagnosis ->
            { english = "Medical Diagnosis"
            , kinyarwanda = Nothing
            }

        MedicalDiagnosisAlert diagnosis ->
            case diagnosis of
                DiagnosisUterineMyoma ->
                    { english = "Uterine Myoma"
                    , kinyarwanda = Nothing
                    }

                DiagnosisDiabetes ->
                    { english = "Diabetes"
                    , kinyarwanda = Nothing
                    }

                DiagnosisCardiacDisease ->
                    { english = "Cardiac Disease"
                    , kinyarwanda = Nothing
                    }

                DiagnosisRenalDisease ->
                    { english = "Renal Disease"
                    , kinyarwanda = Nothing
                    }

                DiagnosisHypertensionBeforePregnancy ->
                    { english = "Hypertension"
                    , kinyarwanda = Nothing
                    }

                DiagnosisTuberculosis ->
                    { english = "Tuberculosis"
                    , kinyarwanda = Nothing
                    }

                DiagnosisAsthma ->
                    { english = "Asthma"
                    , kinyarwanda = Nothing
                    }

                DiagnosisBowedLegs ->
                    { english = "Bowed Legs"
                    , kinyarwanda = Nothing
                    }

                DiagnosisHIV ->
                    { english = "HIV"
                    , kinyarwanda = Nothing
                    }

                DiagnosisMentalHealthHistory ->
                    { english = "History of Mental Health Problems"
                    , kinyarwanda = Nothing
                    }

        MedicalFormHelper ->
            { english = "Please record if the mother was diagnosed with the following medical issues"
            , kinyarwanda = Nothing
            }

        MMHGUnit ->
            { english = "mmHG"
            , kinyarwanda = Nothing
            }

        MentalHealthHistory ->
            { english = "History of Mental Health Problems"
            , kinyarwanda = Nothing
            }

        MiddleName ->
            { english = "Middle Name"
            , kinyarwanda = Nothing
            }

        MinutesAgo minutes ->
            { english =
                if minutes == 0 then
                    "just now"

                else if minutes == 1 then
                    "one minute ago"

                else
                    toString minutes ++ " minutes ago"
            , kinyarwanda = Nothing
            }

        ModeOfDelivery mode ->
            case mode of
                VaginalDelivery (Spontaneous True) ->
                    { english = "Spontaneous vaginal delivery with episiotomy"
                    , kinyarwanda = Nothing
                    }

                VaginalDelivery (Spontaneous False) ->
                    { english = "Spontaneous vaginal delivery without episiotomy"
                    , kinyarwanda = Nothing
                    }

                VaginalDelivery WithVacuumExtraction ->
                    { english = "Vaginal delivery with vacuum extraction"
                    , kinyarwanda = Nothing
                    }

                CesareanDelivery ->
                    { english = "Cesarean delivery"
                    , kinyarwanda = Nothing
                    }

        ModeOfDeliveryLabel ->
            { english = "Mode of delivery"
            , kinyarwanda = Nothing
            }

        Month ->
            { english = "Month"
            , kinyarwanda = Nothing
            }

        MonthAbbrev ->
            { english = "mo"
            , kinyarwanda = Just "amezi"
            }

        MonthsOld ->
            { english = "months old"
            , kinyarwanda = Just "Amezi"
            }

        Mother ->
            { english = "Mother"
            , kinyarwanda = Just "Umubyeyi"
            }

        MotherDemographicInformation ->
            { english = "Mother Demographic Information"
            , kinyarwanda = Nothing
            }

        MotherName name ->
            { english = "Mother/Caregiver: " ++ name
            , kinyarwanda = Just <| "Umubyeyi: " ++ name
            }

        MotherNameLabel ->
            { english = "Mother's Name"
            , kinyarwanda = Nothing
            }

        MotherNationalId ->
            { english = "Mother's National ID"
            , kinyarwanda = Nothing
            }

        Mothers ->
            { english = "Mothers"
            , kinyarwanda = Just "Ababyeyi"
            }

        MUAC ->
            { english = "MUAC"
            , kinyarwanda = Just "Ikizigira"
            }

        MuacIndication indication ->
            case indication of
                MuacRed ->
                    { english = "red"
                    , kinyarwanda = Just "Umutuku"
                    }

                MuacYellow ->
                    { english = "yellow"
                    , kinyarwanda = Just "Umuhondo"
                    }

                MuacGreen ->
                    { english = "green"
                    , kinyarwanda = Just "Icyatsi"
                    }

        MyAccount ->
            { english = "My Account"
            , kinyarwanda = Just "Konti yanjye"
            }

        MyRelatedBy relationship ->
            translateMyRelatedBy relationship

        MyRelatedByQuestion relationship ->
            translateMyRelatedByQuestion relationship

        Name ->
            { english = "Name"
            , kinyarwanda = Nothing
            }

        NationalIdNumber ->
            { english = "National ID Number"
            , kinyarwanda = Nothing
            }

        Neck ->
            { english = "Neck"
            , kinyarwanda = Nothing
            }

        NeckCPESign option ->
            case option of
                EnlargedThyroid ->
                    { english = "Enlarged Thyroid"
                    , kinyarwanda = Nothing
                    }

                EnlargedLymphNodes ->
                    { english = "Enlarged Lymph Nodes"
                    , kinyarwanda = Nothing
                    }

                NormalNeck ->
                    translationSet Normal

        Next ->
            { english = "Next"
            , kinyarwanda = Nothing
            }

        No ->
            { english = "No"
            , kinyarwanda = Nothing
            }

        NoActivitiesCompleted ->
            { english = "No activities are entirely completed for the attending participants."
            , kinyarwanda = Just "Nta gikorwa cyarangiye cyose kubitabiriye."
            }

        NoActivitiesPending ->
            { english = "All activities are completed for the attending participants."
            , kinyarwanda = Just "Ibikorwa byose byarangiye kubitabiriye."
            }

        NoActivitiesCompletedForThisParticipant ->
            { english = "No activities are completed for this participant."
            , kinyarwanda = Just "Nta gikorwa cyarangiye kubitabiriye."
            }

        NoActivitiesPendingForThisParticipant ->
            { english = "All activities are completed for this participant."
            , kinyarwanda = Just "Ibikorwa byose byarangiye kubitabiriye."
            }

        NoGroupsFound ->
            { english = "No groups found."
            , kinyarwanda = Nothing
            }

        NoMatchesFound ->
            { english = "No matches found"
            , kinyarwanda = Nothing
            }

        NoParticipantsCompleted ->
            { english = "No participants have completed all their activities yet."
            , kinyarwanda = Just "Ntagikorwa nakimwe kirarangira kubitabiriye."
            }

        NoParticipantsPending ->
            { english = "All attending participants have completed their activities."
            , kinyarwanda = Just "Abaje bose barangirijwe"
            }

        NoParticipantsCompletedForThisActivity ->
            { english = "No participants have completed this activity yet."
            , kinyarwanda = Just "Ntawaje warangirijwe kukorerwa."
            }

        NoParticipantsPendingForThisActivity ->
            { english = "All attending participants have completed this activitity."
            , kinyarwanda = Just "Ababje bose barangirijwe."
            }

        Normal ->
            { english = "Normal"
            , kinyarwanda = Nothing
            }

        NoChildrenRegisteredInTheSystem ->
            { english = "No children registered in the system"
            , kinyarwanda = Just "Ntamwana wanditswe muriyi sisiteme"
            }

        NoParticipantsFound ->
            { english = "No participants found"
            , kinyarwanda = Just "Ntamuntu ugaragaye"
            }

        NotAvailable ->
            { english = "not available"
            , kinyarwanda = Just "Ntibiboneste"
            }

        NotConnected ->
            { english = "Not Connected"
            , kinyarwanda = Just "Ntamurandasi"
            }

        NumberOfAbortions ->
            { english = "Number of Abortions"
            , kinyarwanda = Nothing
            }

        NumberOfChildrenUnder5 ->
            { english = "Number of Children under 5"
            , kinyarwanda = Nothing
            }

        NumberOfCSections ->
            { english = "Number of C-Sections"
            , kinyarwanda = Nothing
            }

        NumberOfLiveChildren ->
            { english = "Number of Live Children"
            , kinyarwanda = Nothing
            }

        NumberOfStillbirthsAtTerm ->
            { english = "Number of Stillbirths at Term"
            , kinyarwanda = Nothing
            }

        NumberOfStillbirthsPreTerm ->
            { english = "Number of Stillbirths pre Term"
            , kinyarwanda = Nothing
            }

        ObstetricalDiagnosis ->
            { english = "Obstetrical Diagnosis"
            , kinyarwanda = Nothing
            }

        ObstetricalDiagnosisAlert diagnosis ->
            case diagnosis of
                DiagnosisRhNegative ->
                    { english = "Patient is RH Negative"
                    , kinyarwanda = Nothing
                    }

                DiagnosisModerateUnderweight ->
                    { english = "Moderate underweight"
                    , kinyarwanda = Nothing
                    }

                DiagnosisSevereUnderweight ->
                    { english = "Severe underweight"
                    , kinyarwanda = Nothing
                    }

                DiagnosisOverweight ->
                    { english = "Overweight"
                    , kinyarwanda = Nothing
                    }

                DiagnosisObese ->
                    { english = "Obese"
                    , kinyarwanda = Nothing
                    }

                DisgnosisPeripheralEdema ->
                    { english = "Peripheral Edema"
                    , kinyarwanda = Nothing
                    }

                DiagnosisFetusBreech ->
                    { english = "Fetus is in breech"
                    , kinyarwanda = Nothing
                    }

                DiagnosisFetusTransverse ->
                    { english = "Fetus is transverse"
                    , kinyarwanda = Nothing
                    }

                DiagnosisBreastExamination ->
                    { english = "Breast exam showed"
                    , kinyarwanda = Nothing
                    }

                DiagnosisHypertension ->
                    { english = "Hypertension"
                    , kinyarwanda = Nothing
                    }

                DiagnosisPregnancyInducedHypertension ->
                    { english = "Pregnancy-induced hypertension"
                    , kinyarwanda = Nothing
                    }

                DiagnosisPreeclampsiaHighRisk ->
                    { english = "High Risk for Preeclampsia"
                    , kinyarwanda = Nothing
                    }

        OK ->
            { english = "OK"
            , kinyarwanda = Just "Nibyo, yego"
            }

        Old ->
            { english = "old"
            , kinyarwanda = Just "imyaka"
            }

        OneVisit ->
            { english = "One visit"
            , kinyarwanda = Nothing
            }

        OnceYouEndYourGroupEncounter ->
            { english = "Once you end your Group Encounter, you will no longer be able to edit or add data."
            , kinyarwanda = Nothing
            }

        Page ->
            { english = "Page"
            , kinyarwanda = Just "Paji"
            }

        Page404 ->
            { english = "404 page"
            , kinyarwanda = Just "404 paji"
            }

        PageNotFoundMsg ->
            { english = "Sorry, nothing found in this URL."
            , kinyarwanda = Just "Mutwihanganire ntabwo ubufasha mwasabye mubashije kuboneka."
            }

        Pallor ->
            { english = "Pallor"
            , kinyarwanda = Nothing
            }

        Para ->
            { english = "Para"
            , kinyarwanda = Nothing
            }

        PaleConjuctiva ->
            { english = "Pale Conjuctiva"
            , kinyarwanda = Nothing
            }

        PartialPlacentaPreviousDelivery ->
            { english = "Partial Placenta in previous delivery"
            , kinyarwanda = Nothing
            }

        ParticipantDirectory ->
            { english = "Participant Directory"
            , kinyarwanda = Nothing
            }

        Participants ->
            { english = "Participants"
            , kinyarwanda = Just "Ubwitabire"
            }

        ParticipantReviewed ->
            { english = "I have reviewed and understand the above."
            , kinyarwanda = Nothing
            }

        ParticipantSignature ->
            { english = "Participant Signature"
            , kinyarwanda = Nothing
            }

        ParticipantSummary ->
            { english = "Participant Summary"
            , kinyarwanda = Just "Umwirondoro w’urera umwana"
            }

        ParticipantDemographicInformation ->
            { english = "Participant Demographic Information"
            , kinyarwanda = Nothing
            }

        ParticipantInformation ->
            { english = "Participant Information"
            , kinyarwanda = Nothing
            }

        PartnerReceivedCounseling ->
            { english = "Did partner receive HIV Counseling and Testing during this pregnancy"
            , kinyarwanda = Nothing
            }

        PatientProgress ->
            { english = "Patient Progress"
            , kinyarwanda = Nothing
            }

        PatientInformation ->
            { english = "Patient Information"
            , kinyarwanda = Nothing
            }

        PatientProvisionsTask task ->
            case task of
                Medication ->
                    { english = "Medication"
                    , kinyarwanda = Nothing
                    }

                Resources ->
                    { english = "Resources"
                    , kinyarwanda = Nothing
                    }

        People ->
            { english = "People"
            , kinyarwanda = Nothing
            }

        PersistentStorage authorized ->
            if authorized then
                { english = "Persistent storage has been authorized. The browser will not delete locally cached data without your approval."
                , kinyarwanda = Nothing
                }

            else
                { english = "Persistent storage has not been authorized. The browser may delete locally cached data if storage runs low."
                , kinyarwanda = Nothing
                }

        Person ->
            { english = "Person"
            , kinyarwanda = Nothing
            }

        PersonHasBeenSaved ->
            { english = "Person has been saved"
            , kinyarwanda = Nothing
            }

        PlaceholderEnterHeight ->
            { english = "Enter height here…"
            , kinyarwanda = Just "Andika uburebure hano…"
            }

        PlaceholderEnterMUAC ->
            { english = "Enter MUAC here…"
            , kinyarwanda = Just "Andika uburebure hano…"
            }

        PlaceholderEnterParticipantName ->
            { english = "Enter participant name here"
            , kinyarwanda = Nothing
            }

        PlaceholderEnterWeight ->
            { english = "Enter weight here…"
            , kinyarwanda = Just "Andika ibiro hano…"
            }

        PleaseSelectGroup ->
            { english = "Please select the relevant Group for the new encounter"
            , kinyarwanda = Nothing
            }

        PleaseSync ->
            { english = "Please sync data for selected Health Center."
            , kinyarwanda = Nothing
            }

        PreeclampsiaPreviousPregnancy ->
            { english = "Preeclampsia in previous pregnancy "
            , kinyarwanda = Nothing
            }

        PregnancyTrimester trimester ->
            case trimester of
                FirstTrimester ->
                    { english = "First Trimester"
                    , kinyarwanda = Nothing
                    }

                SecondTrimester ->
                    { english = "Second Trimester"
                    , kinyarwanda = Nothing
                    }

                ThirdTrimester ->
                    { english = "Third Trimester"
                    , kinyarwanda = Nothing
                    }

        PrenatalActivitiesTitle activity ->
            case activity of
                DangerSigns ->
                    { english = "Danger Signs"
                    , kinyarwanda = Nothing
                    }

                Examination ->
                    { english = "Examination"
                    , kinyarwanda = Nothing
                    }

                PrenatalActivity.Model.FamilyPlanning ->
                    { english = "Family Planning"
                    , kinyarwanda = Nothing
                    }

                History ->
                    { english = "History"
                    , kinyarwanda = Nothing
                    }

                PatientProvisions ->
                    { english = "Patient Provisions"
                    , kinyarwanda = Nothing
                    }

                PregnancyDating ->
                    { english = "Pregnancy Dating"
                    , kinyarwanda = Nothing
                    }

        PrenatalEncounter ->
            { english = "Antenatal Encounter"
            , kinyarwanda = Nothing
            }

        PreTerm ->
            { english = "Pre Term"
            , kinyarwanda = Nothing
            }

        PregnancyConcludedLabel ->
            { english = "or Pregnancy Concluded"
            , kinyarwanda = Nothing
            }

        PreviousCSectionScar ->
            { english = "Previous C-section scar"
            , kinyarwanda = Nothing
            }

        PreviousDelivery ->
            { english = "Previous Delivery"
            , kinyarwanda = Nothing
            }

        PreviousDeliveryPeriods period ->
            case period of
                LessThan18Month ->
                    { english = "Less than 18 month ago"
                    , kinyarwanda = Nothing
                    }

                MoreThan5Years ->
                    { english = "More than 5 years ago"
                    , kinyarwanda = Nothing
                    }

                Neither ->
                    { english = "Neither"
                    , kinyarwanda = Nothing
                    }

        PreviousFloatMeasurement value ->
            { english = "Previous measurement: " ++ toString value
            , kinyarwanda = Just <| "Ibipimo by'ubushize: " ++ toString value
            }

        PreviousMeasurementNotFound ->
            { english = "No previous measurement on record"
            , kinyarwanda = Nothing
            }

        Profession ->
            { english = "Profession"
            , kinyarwanda = Nothing
            }

        Programs ->
            { english = "Programs"
            , kinyarwanda = Nothing
            }

        ProgressReport ->
            { english = "Progress Report"
            , kinyarwanda = Just "Raporo igaragaza imikurire y'umwana"
            }

        ProgressTimeline ->
            { english = "Progress Timeline"
            , kinyarwanda = Nothing
            }

        ProgressTrends ->
            { english = "Progress Trends"
            , kinyarwanda = Nothing
            }

        PrenatalParticipant ->
            { english = "Antenatal Participant"
            , kinyarwanda = Nothing
            }

        PrenatalParticipants ->
            { english = "Antenatal Participants"
            , kinyarwanda = Nothing
            }

        PreTermPregnancy ->
            { english = "Number of Pre-term Pregnancies (Live Birth)"
            , kinyarwanda = Nothing
            }

        Province ->
            { english = "Province"
            , kinyarwanda = Nothing
            }

        ReasonForCSection ->
            { english = "Reason for C-section"
            , kinyarwanda = Nothing
            }

        ReceivedDewormingPill ->
            { english = "Has the mother received deworming pill"
            , kinyarwanda = Nothing
            }

        ReceivedIronFolicAcid ->
            { english = "Has the mother received iron and folic acid supplement"
            , kinyarwanda = Nothing
            }

        ReceivedMosquitoNet ->
            { english = "Has the mother received a mosquito net"
            , kinyarwanda = Nothing
            }

        RecordPregnancyOutcome ->
            { english = "Record Pregnancy Outcome"
            , kinyarwanda = Nothing
            }

        Register ->
            { english = "Register"
            , kinyarwanda = Nothing
            }

        RegisterAParticipant ->
            { english = "Register a participant"
            , kinyarwanda = Nothing
            }

        RegisterHelper ->
            { english = "Not the participant you were looking for?"
            , kinyarwanda = Nothing
            }

        RegisterNewParticipant ->
            { english = "Register a new participant"
            , kinyarwanda = Nothing
            }

        RegistratingHealthCenter ->
            { english = "Registrating Health Center"
            , kinyarwanda = Nothing
            }

        RegistrationSuccessful ->
            { english = "Registration Successful"
            , kinyarwanda = Nothing
            }

        RegistrationSuccessfulParticipantAdded ->
            { english = "The participant has been added to E-Heza."
            , kinyarwanda = Nothing
            }

        RegistrationSuccessfulSuggestAddingChild ->
            { english = "The participant has been added to E-Heza. Would you like to add a child for this participant?"
            , kinyarwanda = Nothing
            }

        RegistrationSuccessfulSuggestAddingMother ->
            { english = "The participant has been added to E-Heza. Would you like to add a mother for this participant?"
            , kinyarwanda = Nothing
            }

        RelationSuccessful ->
            { english = "Relation Successful"
            , kinyarwanda = Nothing
            }

        RelationSuccessfulChildWithMother ->
            { english = "Child succesfully assocoated with mother."
            , kinyarwanda = Nothing
            }

        RelationSuccessfulMotherWithChild ->
            { english = "Mother succesfully assocoated with child."
            , kinyarwanda = Nothing
            }

        RenalDisease ->
            { english = "Renal Disease"
            , kinyarwanda = Nothing
            }

        ReportAge age ->
            { english = "Age: " ++ age
            , kinyarwanda = Just <| "Imyaka: " ++ age
            }

        ReportDOB dob ->
            { english = "DOB: " ++ dob
            , kinyarwanda = Just <| "Itariki y'amavuko: " ++ dob
            }

        ReportRemaining remaining ->
            { english = toString remaining ++ " remaning"
            , kinyarwanda = Just <| toString remaining ++ " iyibutswa rya raporo"
            }

        ReportResultsOfSearch total ->
            case total of
                1 ->
                    { english = "There is 1 participant that matches your search."
                    , kinyarwanda = Nothing
                    }

                _ ->
                    { english = "There are " ++ toString total ++ " participants that match your search."
                    , kinyarwanda = Nothing
                    }

        Reports ->
            { english = "Reports"
            , kinyarwanda = Nothing
            }

        RecentAndUpcomingGroupEncounters ->
            { english = "Recent and upcoming Group Encounters"
            , kinyarwanda = Nothing
            }

        ReportCompleted { pending, completed } ->
            { english = toString completed ++ " / " ++ toString (pending + completed) ++ " Completed"
            , kinyarwanda = Just <| toString completed ++ " / " ++ toString (pending + completed) ++ " Raporo irarangiye"
            }

        ResolveMonth month ->
            translateMonth month

        RespiratoryRate ->
            { english = "Respiratory Rate"
            , kinyarwanda = Nothing
            }

        Retry ->
            { english = "Retry"
            , kinyarwanda = Just "Kongera kugerageza"
            }

        RhNegative ->
            { english = "RH Negative"
            , kinyarwanda = Nothing
            }

        RiskFactorAlert factor ->
            case factor of
                FactorNumberOfCSections number ->
                    if number == 1 then
                        { english = "1 previous C-section"
                        , kinyarwanda = Nothing
                        }

                    else
                        { english = toString number ++ " previous C-sections"
                        , kinyarwanda = Nothing
                        }

                FactorCSectionInPreviousDelivery ->
                    { english = "C-section in previous delivery"
                    , kinyarwanda = Nothing
                    }

                FactorCSectionReason ->
                    { english = "C-section in previous delivery due to"
                    , kinyarwanda = Nothing
                    }

                FactorPreviousDeliveryPeriod ->
                    { english = "Previous delivery"
                    , kinyarwanda = Nothing
                    }

                FactorSuccessiveAbortions ->
                    { english = "Patient experienced successive abortions"
                    , kinyarwanda = Nothing
                    }

                FactorSuccessivePrematureDeliveries ->
                    { english = "Patient experienced successive preterm deliveries"
                    , kinyarwanda = Nothing
                    }

                FactorStillbornPreviousDelivery ->
                    { english = "Stillbirth in previous delivery"
                    , kinyarwanda = Nothing
                    }

                FactorBabyDiedOnDayOfBirthPreviousDelivery ->
                    { english = "Live Birth but the baby died the same day in previous delivery"
                    , kinyarwanda = Nothing
                    }

                FactorPartialPlacentaPreviousDelivery ->
                    { english = "Patient had partial placenta in previous pregnancy"
                    , kinyarwanda = Nothing
                    }

                FactorSevereHemorrhagingPreviousDelivery ->
                    { english = "Patient experienced severe hemorrhage in previous pregnancy"
                    , kinyarwanda = Nothing
                    }

                FactorPreeclampsiaPreviousPregnancy ->
                    { english = "Patient had preeclampsia in previous pregnancy"
                    , kinyarwanda = Nothing
                    }

                FactorConvulsionsPreviousDelivery ->
                    { english = "Patient experienced convulsions in previous delivery"
                    , kinyarwanda = Nothing
                    }

                FactorConvulsionsAndUnconsciousPreviousDelivery ->
                    { english = "Patient experienced convulsions and resulted in becoming unconscious after delivery"
                    , kinyarwanda = Nothing
                    }

                FactorIncompleteCervixPreviousPregnancy ->
                    { english = "Patient had an Incomplete Cervix in previous pregnancy"
                    , kinyarwanda = Nothing
                    }

                FactorVerticalCSectionScar ->
                    { english = "Vertical C-Section Scar"
                    , kinyarwanda = Nothing
                    }

                FactorGestationalDiabetesPreviousPregnancy ->
                    { english = "Patient had Gestational Diabetes in previous pregnancy"
                    , kinyarwanda = Nothing
                    }

        RiskFactors ->
            { english = "Risk Factors"
            , kinyarwanda = Nothing
            }

        Save ->
            { english = "Save"
            , kinyarwanda = Just "Kubika"
            }

        SaveAndNext ->
            { english = "Save & Next"
            , kinyarwanda = Nothing
            }

        SaveError ->
            { english = "Save Error"
            , kinyarwanda = Just "Kubika error (ikosa mu kubika)"
            }

        Search ->
            { english = "Search"
            , kinyarwanda = Nothing
            }

        SearchByName ->
            { english = "Search by Name"
            , kinyarwanda = Just "Gushakisha izina"
            }

        SearchHelper ->
            { english = "Search to see if the participant already exists in E-Heza. If the person you are looking for does not appear in the search, please create a new record for them."
            , kinyarwanda = Nothing
            }

        SearchHelperFamilyMember ->
            { english = "Search to see if the additional family member already exists in E-Heza. If the person you are looking for does not appear in the search, please create a new record for them."
            , kinyarwanda = Nothing
            }

        SecondName ->
            { english = "Second Name"
            , kinyarwanda = Nothing
            }

        Sector ->
            { english = "Sector"
            , kinyarwanda = Nothing
            }

        SelectAntenatalVisit ->
            { english = "Select an Antenatal Visit"
            , kinyarwanda = Nothing
            }

        SelectDangerSigns ->
            { english = "Please select one or more of the danger signs the patient is experiencing"
            , kinyarwanda = Nothing
            }

        SelectEncounterType ->
            { english = "Select encounter type"
            , kinyarwanda = Nothing
            }

        SelectLanguage ->
            { english = "Select language"
            , kinyarwanda = Nothing
            }

        SelectGroup ->
            { english = "Select Group..."
            , kinyarwanda = Nothing
            }

        SelectProgram ->
            { english = "Select Program"
            , kinyarwanda = Nothing
            }

        SelectYourGroup ->
            { english = "Select your Group"
            , kinyarwanda = Nothing
            }

        SelectYourHealthCenter ->
            { english = "Select your Health Center"
            , kinyarwanda = Nothing
            }

        SelectedHCDownloading ->
            { english = "Downloading data for selected Health Center. Please wait until completed."
            , kinyarwanda = Nothing
            }

        SelectedHCNotSynced ->
            { english = "Data is not synced"
            , kinyarwanda = Nothing
            }

        SelectedHCSyncing ->
            { english = "Data is syncing"
            , kinyarwanda = Nothing
            }

        SelectedHCUploading ->
            { english = "Uploading data for selected Health Center. Please wait until completed."
            , kinyarwanda = Nothing
            }

        ServiceWorkerActive ->
            { english = "The app is installed on this device."
            , kinyarwanda = Nothing
            }

        ServiceWorkerCurrent ->
            { english = "You have the current version of the app."
            , kinyarwanda = Nothing
            }

        ServiceWorkerCheckForUpdates ->
            { english = "Check for updates"
            , kinyarwanda = Nothing
            }

        ServiceWorkerInstalling ->
            { english = "A new version of the app has been detected and is being downloaded. You can continue to work while this is in progress."
            , kinyarwanda = Nothing
            }

        ServiceWorkerInstalled ->
            { english = "A new version of the app has been downloaded."
            , kinyarwanda = Nothing
            }

        ServiceWorkerSkipWaiting ->
            { english = "Activate new version of the app"
            , kinyarwanda = Nothing
            }

        ServiceWorkerRestarting ->
            { english = "The app should reload momentarily with the new version."
            , kinyarwanda = Nothing
            }

        ServiceWorkerActivating ->
            { english = "A new version of the app is preparing itself for use."
            , kinyarwanda = Nothing
            }

        ServiceWorkerActivated ->
            { english = "A new version of the app is ready for use."
            , kinyarwanda = Nothing
            }

        ServiceWorkerRedundant ->
            { english = "An error occurred installing a new version of the app."
            , kinyarwanda = Nothing
            }

        ServiceWorkerInactive ->
            { english = "The app is not yet installed on this device."
            , kinyarwanda = Nothing
            }

        ServiceWorkerRegNotAsked ->
            { english = "We have not yet attempted to install the app on this device."
            , kinyarwanda = Nothing
            }

        ServiceWorkerRegLoading ->
            { english = "Installation of the app on this device is progressing."
            , kinyarwanda = Nothing
            }

        ServiceWorkerRegErr ->
            { english = "There was an error installing the app on this device. To try again, reload this page."
            , kinyarwanda = Nothing
            }

        ServiceWorkerRegSuccess ->
            { english = "The app was successfully registered with this device."
            , kinyarwanda = Nothing
            }

        ServiceWorkerStatus ->
            { english = "Deployment Status"
            , kinyarwanda = Nothing
            }

        SevereHemorrhagingPreviousDelivery ->
            { english = "Severe Hemorrhaging in previous delivery (>500 ml)"
            , kinyarwanda = Nothing
            }

        StillbornPreviousDelivery ->
            { english = "Stillborn in previous delivery"
            , kinyarwanda = Nothing
            }

        SubsequentAntenatalVisit ->
            { english = "Subsequent Antenatal Visit"
            , kinyarwanda = Nothing
            }

        SuccessiveAbortions ->
            { english = "Successive Abortions"
            , kinyarwanda = Nothing
            }

        SuccessivePrematureDeliveries ->
            { english = "Successive Premature Deliveries"
            , kinyarwanda = Nothing
            }

        GroupEncounterClosed ->
            { english = "Group Encounter closed"
            , kinyarwanda = Nothing
            }

        GroupEncounterClosed2 sessionId ->
            { english =
                String.join " "
                    [ "Group Encounter"
                    , fromEntityUuid sessionId
                    , """is closed. If you need to make further modifications
            to it, please contact an administrator to have it
            re-opened."""
                    ]
            , kinyarwanda = Nothing
            }

        GroupEncounterLoading sessionId ->
            { english = "Loading Group Encounter " ++ fromEntityUuid sessionId
            , kinyarwanda = Nothing
            }

        GroupEncounterUnauthorized ->
            { english = "Group Encounter unauthorized"
            , kinyarwanda = Nothing
            }

        GroupEncounterUnauthorized2 ->
            { english =
                """You are not authorized to view this health assessment.
        Please contact the Ihangane project for further
        instructions."""
            , kinyarwanda = Nothing
            }

        ShowAll ->
            { english = "Show All"
            , kinyarwanda = Nothing
            }

        StartEndDate ->
            { english = "Start - End"
            , kinyarwanda = Nothing
            }

        StartDate ->
            { english = "Start Date"
            , kinyarwanda = Just "Itariki utangireyeho"
            }

        EndDate ->
            { english = "End Date"
            , kinyarwanda = Just "Itariki urangirijeho"
            }

        StartSyncing ->
            { english = "Start Syncing"
            , kinyarwanda = Nothing
            }

        StopSyncing ->
            { english = "Stop Syncing"
            , kinyarwanda = Nothing
            }

        Submit ->
            { english = "Submit"
            , kinyarwanda = Nothing
            }

        Success ->
            { english = "Success"
            , kinyarwanda = Just "Byagezweho"
            }

        SyncGeneral ->
            { english = "Sync Status (General)"
            , kinyarwanda = Nothing
            }

        TakenCareOfBy ->
            { english = "Taken care of by"
            , kinyarwanda = Nothing
            }

        TasksCompleted completed total ->
            { english = toString completed ++ "/" ++ toString total ++ " Tasks Completed"
            , kinyarwanda = Nothing
            }

        TelephoneNumber ->
            { english = "Telephone Number"
            , kinyarwanda = Nothing
            }

        Term ->
            { english = "Term"
            , kinyarwanda = Nothing
            }

        TermPregnancy ->
            { english = "Number of Term Pregnancies (Live Birth)"
            , kinyarwanda = Nothing
            }

        ThisActionCannotBeUndone ->
            { english = "This action cannot be undone."
            , kinyarwanda = Nothing
            }

        ThisGroupHasNoMothers ->
            { english = "This Group has no mothers assigned to it."
            , kinyarwanda = Nothing
            }

        Training ->
            { english = "Training"
            , kinyarwanda = Nothing
            }

        TrainingGroupEncounterCreateSuccessMessage ->
            { english = "Training encounters were created."
            , kinyarwanda = Nothing
            }

        TrainingGroupEncounterDeleteSuccessMessage ->
            { english = "Training encounters were deleted."
            , kinyarwanda = Nothing
            }

        TrySyncing ->
            { english = "Try syncing with backend"
            , kinyarwanda = Nothing
            }

        TuberculosisPast ->
            { english = "Tuberculosis in the past"
            , kinyarwanda = Nothing
            }

        TuberculosisPresent ->
            { english = "Tuberculosis in the present"
            , kinyarwanda = Nothing
            }

        TwoVisits ->
            { english = "Two visits"
            , kinyarwanda = Nothing
            }

        UbudeheLabel ->
            { english = "Ubudehe: "
            , kinyarwanda = Nothing
            }

        Unknown ->
            { english = "Unknown"
            , kinyarwanda = Nothing
            }

        Update ->
            { english = "Update"
            , kinyarwanda = Just "Kuvugurura"
            }

        UpdateError ->
            { english = "Update Error"
            , kinyarwanda = Just "ikosa mwivugurura"
            }

        UterineMyoma ->
            { english = "Uterine Myoma"
            , kinyarwanda = Nothing
            }

        ValidationErrors ->
            { english = "Validation Errors"
            , kinyarwanda = Nothing
            }

        -- As in, the version the app
        Version ->
            { english = "Version"
            , kinyarwanda = Nothing
            }

        View ->
            { english = "View"
            , kinyarwanda = Nothing
            }

        ViewProgressReport ->
            { english = "View Progress Report"
            , kinyarwanda = Just "Garagaza uruhererekane rw'imikurire y'umwana"
            }

        Village ->
            { english = "Village"
            , kinyarwanda = Nothing
            }

        WeekSinglePlural value ->
            if value == 1 then
                { english = "1 Week"
                , kinyarwanda = Nothing
                }

            else
                { english = toString value ++ " Weeks"
                , kinyarwanda = Nothing
                }

        Weight ->
            { english = "Weight"
            , kinyarwanda = Just "Ibiro"
            }

        WelcomeUser name ->
            { english = "Welcome " ++ name
            , kinyarwanda = Just <| "Murakaza neza " ++ name
            }

        WhatDoYouWantToDo ->
            { english = "What do you want to do?"
            , kinyarwanda = Nothing
            }

        Year ->
            { english = "Year"
            , kinyarwanda = Nothing
            }

        YearsOld int ->
            { english = toString int ++ " years old"
            , kinyarwanda = Nothing
            }

        Yes ->
            { english = "Yes"
            , kinyarwanda = Nothing
            }

        YouAreNotAnAdmin ->
            { english = "You are not logged in as an Administrator."
            , kinyarwanda = Nothing
            }

        YourGroupEncounterHasBeenSaved ->
            { english = "Your Group Encounter has been saved."
            , kinyarwanda = Nothing
            }

        ZScoreHeightForAge ->
            { english = "Z-Score Height for Age: "
            , kinyarwanda = Just "Z-score Uburebure ku myaka: "
            }

        ZScoreMuacForAge ->
            { english = "MUAC for Age: "
            , kinyarwanda = Just "MUAC ku myaka: "
            }

        ZScoreWeightForAge ->
            { english = "Z-Score Weight for Age: "
            , kinyarwanda = Just "Z-score Ibiro ku myaka: "
            }

        ZScoreWeightForHeight ->
            { english = "Z-Score Weight for Height: "
            , kinyarwanda = Just "Z-score Ibiro ku uburebure: "
            }


translateMyRelatedBy : MyRelatedBy -> TranslationSet String
translateMyRelatedBy relationship =
    case relationship of
        MyChild ->
            { english = "Child"
            , kinyarwanda = Just "Umwana"
            }

        MyParent ->
            { english = "Parent"
            , kinyarwanda = Nothing
            }

        MyCaregiven ->
            { english = "Care given"
            , kinyarwanda = Nothing
            }

        MyCaregiver ->
            { english = "Caregiver"
            , kinyarwanda = Nothing
            }


{-| Basically, this is backwards. Our data is showing what the second
person is from the first person's point of view, but we want to
ask the question the opposite way.
-}
translateMyRelatedByQuestion : MyRelatedBy -> TranslationSet String
translateMyRelatedByQuestion relationship =
    case relationship of
        MyChild ->
            { english = "is the parent of"
            , kinyarwanda = Nothing
            }

        MyParent ->
            { english = "is the child of"
            , kinyarwanda = Nothing
            }

        MyCaregiven ->
            { english = "is the caregiver for"
            , kinyarwanda = Nothing
            }

        MyCaregiver ->
            { english = "is given care by"
            , kinyarwanda = Nothing
            }


translateActivePage : Page -> TranslationSet String
translateActivePage page =
    case page of
        DevicePage ->
            { english = "Device Status"
            , kinyarwanda = Nothing
            }

        PinCodePage ->
            { english = "PIN Code"
            , kinyarwanda = Nothing
            }

        PageNotFound url ->
            { english = "Missing"
            , kinyarwanda = Just "Ibibura"
            }

        ServiceWorkerPage ->
            { english = "Deployment"
            , kinyarwanda = Nothing
            }

        UserPage userPage ->
            case userPage of
                ClinicalPage ->
                    { english = "Clinical"
                    , kinyarwanda = Nothing
                    }

                ClinicsPage _ ->
                    { english = "Groups"
                    , kinyarwanda = Nothing
                    }

                ClinicalProgressReportPage _ ->
                    { english = "Clinical Progress Report"
                    , kinyarwanda = Nothing
                    }

                CreatePersonPage relationId ->
                    { english = "Create Person"
                    , kinyarwanda = Nothing
                    }

                DemographicsReportPage _ ->
                    { english = "Demographics Report"
                    , kinyarwanda = Nothing
                    }

                MyAccountPage ->
                    { english = "My Account"
                    , kinyarwanda = Just "Compte"
                    }

                PersonPage id ->
                    { english = "Person"
                    , kinyarwanda = Nothing
                    }

                PersonsPage _ ->
                    { english = "Participant Directory"
                    , kinyarwanda = Nothing
                    }

                PrenatalParticipantPage _ ->
                    { english = "Antenatal Participant"
                    , kinyarwanda = Nothing
                    }

                PrenatalParticipantsPage ->
                    { english = "Antenatal Participants"
                    , kinyarwanda = Nothing
                    }

                RelationshipPage _ _ ->
                    { english = "Relationship"
                    , kinyarwanda = Nothing
                    }

                SessionPage sessionId sessionPage ->
                    case sessionPage of
                        ActivitiesPage ->
                            { english = "Activities"
                            , kinyarwanda = Just "Ibikorwa"
                            }

                        ActivityPage activityType ->
                            { english = "Activity"
                            , kinyarwanda = Just "Igikorwa"
                            }

                        AttendancePage ->
                            { english = "Attendance"
                            , kinyarwanda = Just "Ubwitabire"
                            }

                        ParticipantsPage ->
                            { english = "Participants"
                            , kinyarwanda = Just "Abagenerwabikorwa"
                            }

                        ChildPage childId ->
                            { english = "Child"
                            , kinyarwanda = Just "Umwana"
                            }

                        MotherPage motherId ->
                            { english = "Mother"
                            , kinyarwanda = Just "Umubyeyi"
                            }

                        ProgressReportPage childId ->
                            { english = "Progress Report"
                            , kinyarwanda = Just "Raporo igaragaza imikurire y'umwana"
                            }

                PrenatalEncounterPage _ ->
                    { english = "Antenatal Encounter"
                    , kinyarwanda = Nothing
                    }

                PrenatalActivityPage _ _ ->
                    { english = "Antenatal Activity"
                    , kinyarwanda = Nothing
                    }

                EncounterTypesPage _ ->
                    { english = "Encounter Types"
                    , kinyarwanda = Nothing
                    }


translateAdherence : Adherence -> TranslationSet String
translateAdherence adherence =
    case adherence of
        PrescribedAVRs ->
            { english = "Ask the mother to name or describe her prescribed AVRs. Can she correctly describe her medication?"
            , kinyarwanda = Just "Saba umubyeyi kuvuga izina ry’imiti igabanya ubukana bamuhaye. Ese abashije kuyivuga neza?"
            }

        CorrectDosage ->
            { english = "Can she tell you the correct dosage?"
            , kinyarwanda = Just "Yaba abasha kukubwira neza uburyo ayifata?"
            }

        TimeOfDay ->
            { english = "Can she tell you the correct time of day to make her ARVs?"
            , kinyarwanda = Just "Yaba abasha kukubwira amasaha ayifatiraho buri munsi?"
            }

        Adhering ->
            { english = "Based on your conversations with her, do you think she is adhering to her ARV regimen?"
            , kinyarwanda = Just "Ugendeye ku kiganiro mwagiranye, utekereza ko ari gufata imiti ye neza?"
            }


translateCounselingTimingHeading : CounselingTiming -> TranslationSet String
translateCounselingTimingHeading timing =
    case timing of
        Entry ->
            { english = "Entry Counseling Checklist:"
            , kinyarwanda = Just "Ibigomba kugirwaho inama ku ntangiriro:"
            }

        MidPoint ->
            { english = "Mid Program Review Checklist:"
            , kinyarwanda = Just "Ibigomba kugirwaho inama hagati mu gusubiramo gahunda:"
            }

        Exit ->
            { english = "Exit Counseling Checklist:"
            , kinyarwanda = Just "Ibigomba kugirwaho inama kumuntu usohotse muri gahunda:"
            }

        BeforeMidpoint ->
            { english = "Reminder"
            , kinyarwanda = Just "Kwibutsa"
            }

        BeforeExit ->
            { english = "Reminder"
            , kinyarwanda = Just "Kwibutsa"
            }


translateChartPhrase : ChartPhrase -> TranslationSet String
translateChartPhrase phrase =
    case phrase of
        AgeCompletedMonthsYears ->
            { english = "Age (completed months and years)"
            , kinyarwanda = Just "Imyaka uzuza amazi n'imyaka"
            }

        Birth ->
            { english = "Birth"
            , kinyarwanda = Just "kuvuka"
            }

        BirthToTwoYears ->
            { english = "Birth to 2 years (z-scores)"
            , kinyarwanda = Just "kuvuka (Kuva avutse)  kugeza ku myaka 2 Z-score"
            }

        LengthCm ->
            { english = "Length (cm)"
            , kinyarwanda = Just "Uburere cm"
            }

        LengthForAgeBoys ->
            { english = "Length-for-age BOYS"
            , kinyarwanda = Just "Uburebure ku myaka/ umuhungu"
            }

        LengthForAgeGirls ->
            { english = "Length-for-age GIRLS"
            , kinyarwanda = Just "uburebure ku myaka umukobwa"
            }

        Months ->
            { english = "Months"
            , kinyarwanda = Just "Amezi"
            }

        OneYear ->
            { english = "1 year"
            , kinyarwanda = Just "Umwaka umwe"
            }

        WeightForAgeBoys ->
            { english = "Weight-for-age BOYS"
            , kinyarwanda = Just "Ibiro ku myaka umuhungu"
            }

        WeightForAgeGirls ->
            { english = "Weight-for-age GIRLS"
            , kinyarwanda = Just "ibiro ku myaka umukobwa"
            }

        WeightForLengthBoys ->
            { english = "Weight-for-length BOYS"
            , kinyarwanda = Just "Ibiro ku Uburebure umuhungu"
            }

        WeightForLengthGirls ->
            { english = "Weight-for-length GIRLS"
            , kinyarwanda = Just "ibiro ku uburebure umukobwa"
            }

        WeightKg ->
            { english = "Weight (kg)"
            , kinyarwanda = Just "Ibiro kg"
            }

        YearsPlural value ->
            { english = toString value ++ " years"
            , kinyarwanda = Just <| "Imyaka " ++ toString value
            }

        ZScoreChartsAvailableAt ->
            { english = "Z-score charts available at"
            , kinyarwanda = Just "Raporo ku mikurire y'umwana"
            }


translateLoginPhrase : LoginPhrase -> TranslationSet String
translateLoginPhrase phrase =
    case phrase of
        CheckingCachedCredentials ->
            { english = "Checking cached credentials"
            , kinyarwanda = Nothing
            }

        ForgotPassword1 ->
            { english = "Forgot your password?"
            , kinyarwanda = Just "Wibagiwe ijambo ry'ibanga?"
            }

        ForgotPassword2 ->
            { english = "Call The Ihangane Project at +250 788 817 542"
            , kinyarwanda = Just "Hamagara The Ihangane Project kuri +250 788 817 542(Hamagara kumushinga wa ihangane"
            }

        LoggedInAs ->
            { english = "Logged in as"
            , kinyarwanda = Just "Kwinjira nka"
            }

        LoginRejected method ->
            case method of
                ByAccessToken ->
                    { english = "Your access token has expired. You will need to sign in again."
                    , kinyarwanda = Just "Igihe cyo gukoresha sisitemu cyarangiye . Ongera winjore muri sisitemu"
                    }

                ByPassword ->
                    { english = "The server rejected your username or password."
                    , kinyarwanda = Just "Seriveri yanze ijambo ryo kwinjira cg ijambo ry'ibanga"
                    }

        LoginError error ->
            translateHttpError error

        LoginOrWorkOffline ->
            { english = "Either login below, or work offline without logging in."
            , kinyarwanda = Nothing
            }

        Logout ->
            { english = "Logout"
            , kinyarwanda = Just "Gufunga"
            }

        LogoutInProgress ->
            { english = "Logout in progress ..."
            , kinyarwanda = Just "sisitemi irikwifunga"
            }

        LogoutFailed ->
            { english = "Logout Failed"
            , kinyarwanda = Just "Gufunga byanze"
            }

        Password ->
            { english = "Password"
            , kinyarwanda = Just "Ijambo ry'ibanga"
            }

        PinCode ->
            { english = "PIN code"
            , kinyarwanda = Nothing
            }

        PinCodeRejected ->
            { english = "Your PIN code was not recognized."
            , kinyarwanda = Nothing
            }

        SignIn ->
            { english = "Sign In"
            , kinyarwanda = Just "Kwinjira"
            }

        SignOut ->
            { english = "Sign Out"
            , kinyarwanda = Nothing
            }

        Username ->
            { english = "Username"
            , kinyarwanda = Just "Izina ryo kwinjira"
            }

        WorkOffline ->
            { english = "Work Offline"
            , kinyarwanda = Just "Gukora nta internet"
            }

        YouMustLoginBefore ->
            { english = "You must sign in before you can access the"
            , kinyarwanda = Just "Ugomba kubanza kwinjira muri sisitemi mbere yuko ubona"
            }


translateMonth : Month -> TranslationSet String
translateMonth month =
    case month of
        Jan ->
            { english = "January"
            , kinyarwanda = Just "Mutarama"
            }

        Feb ->
            { english = "February"
            , kinyarwanda = Just "Gashyantare"
            }

        Mar ->
            { english = "March"
            , kinyarwanda = Just "Werurwe"
            }

        Apr ->
            { english = "April"
            , kinyarwanda = Just "Mata"
            }

        May ->
            { english = "May"
            , kinyarwanda = Just "Gicurasi"
            }

        Jun ->
            { english = "June"
            , kinyarwanda = Just "Kamena"
            }

        Jul ->
            { english = "July"
            , kinyarwanda = Just "Nyakanga"
            }

        Aug ->
            { english = "August"
            , kinyarwanda = Just "Kanama"
            }

        Sep ->
            { english = "September"
            , kinyarwanda = Just "Nzeri"
            }

        Oct ->
            { english = "October"
            , kinyarwanda = Just "Ukwakira"
            }

        Nov ->
            { english = "November"
            , kinyarwanda = Just "Ugushyingo"
            }

        Dec ->
            { english = "December"
            , kinyarwanda = Just "Ukuboza"
            }


translateHttpError : Http.Error -> TranslationSet String
translateHttpError error =
    case error of
        Http.NetworkError ->
            { english = "A network error occurred contacting the server. Are you connected to the Internet?"
            , kinyarwanda = Just "Hari ikibazo cya reseau hamagara kuri seriveri. Ufite intereneti? (murandasi)"
            }

        Http.Timeout ->
            { english = "The request to the server timed out."
            , kinyarwanda = Just "Ibyo wasabye kuri seriveri byarengeje igihe."
            }

        Http.BadUrl url ->
            { english = "URL is not valid: " ++ url
            , kinyarwanda = Nothing
            }

        Http.BadStatus response ->
            { english = "The server indicated the following error:"
            , kinyarwanda = Just "Aya makosa yagaragaye hamagara kuri seriveri:"
            }

        Http.BadPayload message response ->
            { english = "The server responded with data of an unexpected type."
            , kinyarwanda = Nothing
            }


translateValidationError : ValidationError -> TranslationSet String
translateValidationError id =
    case id of
        DigitsOnly ->
            { english = "should contain only digit characters"
            , kinyarwanda = Nothing
            }

        InvalidBirthDate ->
            { english = "is invalid"
            , kinyarwanda = Nothing
            }

        InvalidBirthDateForAdult ->
            { english = "is invalid - adult should at least 13 years old"
            , kinyarwanda = Nothing
            }

        InvalidBirthDateForChild ->
            { english = "is invalid - child should be below the age of 13"
            , kinyarwanda = Nothing
            }

        InvalidHmisNumber ->
            { english = "is invalid - child should be between 1 and 15"
            , kinyarwanda = Nothing
            }

        LengthError correctLength ->
            { english = "should contain " ++ toString correctLength ++ " characters"
            , kinyarwanda = Nothing
            }

        LettersOnly ->
            { english = "should contain only letter characters"
            , kinyarwanda = Nothing
            }

        RequiredField ->
            { english = "is a required field"
            , kinyarwanda = Nothing
            }

        UnknownGroup ->
            { english = "is not a known Group"
            , kinyarwanda = Nothing
            }

        UnknownProvince ->
            { english = "is not a known province"
            , kinyarwanda = Nothing
            }

        UnknownDistrict ->
            { english = "is not a known district"
            , kinyarwanda = Nothing
            }

        UnknownSector ->
            { english = "is not a known sector"
            , kinyarwanda = Nothing
            }

        UnknownCell ->
            { english = "is not a known cell"
            , kinyarwanda = Nothing
            }

        UnknownVillage ->
            { english = "is not a known village"
            , kinyarwanda = Nothing
            }

        DecoderError err ->
            { english = "Decoder error: " ++ err
            , kinyarwanda = Nothing
            }


translateFormError : ErrorValue ValidationError -> TranslationSet String
translateFormError error =
    case error of
        Empty ->
            { english = "should not be empty"
            , kinyarwanda = Nothing
            }

        InvalidString ->
            { english = "is not a valid string"
            , kinyarwanda = Nothing
            }

        InvalidEmail ->
            { english = "is not a valid email"
            , kinyarwanda = Nothing
            }

        InvalidFormat ->
            { english = "is not a valid format"
            , kinyarwanda = Nothing
            }

        InvalidInt ->
            { english = "is not a valid integer"
            , kinyarwanda = Nothing
            }

        InvalidFloat ->
            { english = "is not a valid number"
            , kinyarwanda = Nothing
            }

        InvalidBool ->
            { english = "is not a valid boolean"
            , kinyarwanda = Nothing
            }

        InvalidDate ->
            { english = "is not a valid date"
            , kinyarwanda = Nothing
            }

        SmallerIntThan int ->
            { english = "must be smaller than " ++ toString int
            , kinyarwanda = Nothing
            }

        GreaterIntThan int ->
            { english = "must be larger than " ++ toString int
            , kinyarwanda = Nothing
            }

        SmallerFloatThan float ->
            { english = "must be smaller than " ++ toString float
            , kinyarwanda = Nothing
            }

        GreaterFloatThan float ->
            { english = "must be larger than " ++ toString float
            , kinyarwanda = Nothing
            }

        ShorterStringThan int ->
            { english = "must have fewer than " ++ toString int ++ " characters"
            , kinyarwanda = Nothing
            }

        LongerStringThan int ->
            { english = "must have more than " ++ toString int ++ " characters"
            , kinyarwanda = Nothing
            }

        NotIncludedIn ->
            { english = "was not among the valid options"
            , kinyarwanda = Nothing
            }

        CustomError e ->
            translateValidationError e


{-| This one is hampered by the fact that the field names in etaque/elm-form
are untyped strings, but we do our best.
-}
translateFormField : String -> TranslationSet String
translateFormField field =
    case field of
        "clinic_id" ->
            translationSet Group

        "closed" ->
            translationSet Closed

        "training" ->
            translationSet Group

        "scheduled_date.start" ->
            translationSet StartDate

        "scheduled_date.end" ->
            translationSet EndDate

        _ ->
            { english = field
            , kinyarwanda = Nothing
            }
