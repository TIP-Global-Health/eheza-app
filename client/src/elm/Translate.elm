module Translate exposing (..)

import Activity.Model exposing (ActivityType(..), ChildActivityType(..), MotherActivityType(..))
import Backend.Child.Model exposing (Gender(..))
import Backend.Entities exposing (..)
import Backend.Measurement.Model exposing (ChildNutritionSign(..), FamilyPlanningSign(..), MuacIndication(..))
import Date exposing (Month(..))
import Pages.Page exposing (..)
import Restful.Endpoint exposing (fromEntityId)
import Restful.Login exposing (LoginError(..))


type Language
    = English


allLanguages : List Language
allLanguages =
    [ English
    ]


type alias TranslationSet =
    { english : String
    }


type TranslationId
    = AccessDenied
    | Activities
    | ActivitiesCompleted Int
    | ActivitiesHelp ActivityType
    | ActivitiesLabel ActivityType
    | ActivitiesTitle ActivityType
    | ActivitiesToComplete Int
    | ActivityProgressReport ActivityType
    | ActivePage Page
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
    | Attendance
    | Baby
    | BabyName String
    | BeginHealthAssessment
    | Born
    | Cancel
    | CentimeterShorthand
    | ChartPhrase ChartPhrase
    | CheckIn
    | ChildNutritionSignLabel ChildNutritionSign
    | ChildNutritionSignReport ChildNutritionSign
    | ChildOf
    | Children
    | ClickTheCheckMark
    | ClinicNotFound
    | Clinics
    | Connected
    | Continue
    | Dashboard
    | DataIsNowSaved
    | DateOfLastAssessment
    | Day
    | Days
    | DownloadHealthAssessment
    | DownloadSession1
    | DownloadSession2
    | DownloadSuccessful
    | DownloadingSession1
    | DownloadingSession2
    | DropzoneDefaultMessage
    | EndSession
    | ErrorBadUrl
    | ErrorBadPayload
    | ErrorBadStatus
    | ErrorCheckLocalConfig
    | ErrorConfigurationError
    | ErrorFetchingCachedSession
    | ErrorNetworkError
    | ErrorTimeout
    | FamilyPlanningSignLabel FamilyPlanningSign
    | Fetch
    | Gender Gender
    | GoHome
    | KilogramShorthand
    | LinkToMother
    | LoginPhrase LoginPhrase
    | MakeSureYouAreConnected
    | MeasurementNoChange
    | MeasurementGained Float
    | MeasurementLost Float
    | MonthAbbrev
    | MonthsOld
    | Mother
    | MotherName String
    | Mothers
    | MuacIndication MuacIndication
    | MyAccount
    | NoActiveIncidents
    | NoActivitiesCompleted
    | NoActivitiesCompletedForThisParticipant
    | NoActivitiesPending
    | NoActivitiesPendingForThisParticipant
    | NoParticipantsPending
    | NoParticipantsPendingForThisActivity
    | NoParticipantsCompleted
    | NoParticipantsCompletedForThisActivity
    | NoCachedSession
    | NoChildrenRegisteredInTheSystem
    | NoParticipantsFound
    | NotAvailable
    | NotConnected
    | OK
    | Old
    | OnceYouEndYourSession
    | Page
    | Page404
    | PageNotFoundMsg
    | Participants
    | ParticipantSummary
    | PlaceholderEnterHeight
    | PlaceholderEnterMUAC
    | PlaceholderEnterWeight
    | PlaceholderTextGroupDate
    | PlaceholderTextJoined
    | PreviousFloatMeasurement Float
    | ReadyToBeginSession
    | ReportAge String
    | ReportDOB String
    | ReportRemaining Int
    | ReloadParticipant
    | ReportCompleted { pending : Int, total : Int }
    | ResolveMonth Month
    | Retry
    | Save
    | SaveError
    | SearchByName
    | SelectYourClinic
    | SessionClosed
    | SessionClosed2 SessionId
    | SessionInProgress
    | SessionUnauthorized
    | SessionUnauthorized2
    | ThisClinicHasNoMothers
    | TitleHealthAssessment
    | UnableToDownload
    | UnableToUpload
    | Update
    | UpdateError
    | UploadHealthAssessment
    | UploadingSession1
    | UploadingSession2
    | UploadSuccessful
    | ViewProgressReport
    | WelcomeUser String
    | YouHaveACompletedSession
    | ZScoreHeightForAge
    | ZScoreMuacForAge
    | ZScoreWeightForAge
    | ZScoreWeightForHeight


type LoginPhrase
    = CheckingCachedCredentials
    | ForgotPassword1
    | ForgotPassword2
    | LoggedInAs
    | LoginError LoginError
    | LoginOrWorkOffline
    | Logout
    | LogoutInProgress
    | LogoutFailed
    | Password
    | SignIn
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
    | TwoYears
    | WeightForAgeBoys
    | WeightForAgeGirls
    | WeightForLengthBoys
    | WeightForLengthGirls
    | WeightKg
    | ZScoreChartsAvailableAt


translate : Language -> TranslationId -> String
translate lang trans =
    let
        translationSet =
            case trans of
                AccessDenied ->
                    { english = "Access denied" 
                    , kinyarwanda = "Kwinjira ntibyemera"
                    }

                AgeWord ->
                    { english = "Age"
                    , kinyarwanda = "Imyaka"
                    }

                Activities ->
                    { english = "Activities"
                    , kinyarwanda = "Ibikorwa"
                    }

                ActivitiesCompleted count ->
                    { english = "Completed (" ++ toString count ++ ")" }

                ActivitiesHelp activity ->
                    case activity of
                        MotherActivity FamilyPlanning ->
                            { english = "Every mother should be asked about her family planning method(s) each month. If a mother needs family planning, refer her to a clinic."
                            , kinyarwanda = "Buri mubyeyi agomba kubazwa uburyo bwo kuboneza urubyaro akoresha buri kwezi. Niba umubyeyi akeneye kuboneza urubyaro mwohereze ku kigo nderabuzima k'ubishinzwe"
                            }

                        ChildActivity Height ->
                            { english = "Ask the mother to hold the baby’s head at the end of the measuring board. Move the slider to the baby’s heel and pull their leg straight."
                            , kinyarwanda = "Saba Umubyeyi guhagarara inyuma y’umwana we agaramye, afata umutwe ku gice cy’amatwi. Sunikira akabaho ku buryo gakora mu bworo by’ibirenge byombi."
                            }

                        ChildActivity Muac ->
                            { english = "Make sure to measure at the center of the baby’s upper arm."
                            , kinyarwanda = "Ibuka gupima icya kabiri cy'akaboko ko hejuru kugira bigufashe guoima ikizigira cy'akaboko"
                            }

                        ChildActivity NutritionSigns ->
                            { english = "Explain to the mother how to check the malnutrition signs for their own child."
                            , kinyarwanda = "Sobanurira umubyeyi gupima ibimenyetso by'imirire mibi ku giti cye"
                            }

                        ChildActivity ChildPicture ->
                            { english = "Take each baby’s photo at each health assessment. Photos should show the entire body of each child."
                            , kinyarwanda = "Fata ifoto ya buri mwana kuri buri bikorwa by'ipimwa Ifoto igomba kwerekana ibice by'umubiri wose by'umwana"
                            }

                        ChildActivity ProgressReport ->
                            { english = "" }

                        ChildActivity Weight ->
                            { english = "Calibrate the scale before taking the first baby's weight. Place baby in harness with no clothes on."
                            , kinyarwanda = "Ibuka kuregera umunzani mbere yo gupima ibiro by'umwana wa mbere. Ambika umwana ikariso y'ibiro wabanje kumukuramo imyenda iremereye"
                            }

                ActivitiesLabel activity ->
                    case activity of
                        MotherActivity FamilyPlanning ->
                            { english = "Which, if any, of the following methods do you use?"
                            , kinyarwanda = "Ni ubuhe buryo, niba hari ubuhari, mu buryo bukurikira bwo kuboneza urubyaro ukoresha? Muri ubu buryo bukurikira bwo kuboneza urubyaro, ni ubuhe buryo mukoresha?"
                            }

                        ChildActivity Height ->
                            { english = "Height:"
                            , kinyarwanda = "Uburere:"
                            }

                        ChildActivity Muac ->
                            { english = "MUAC:"
                            , kinyarwanda = "Ikizigira cy'akaboko:"
                            }

                        ChildActivity NutritionSigns ->
                            { english = "Select all signs that are present:"
                            , kinyarwanda = "Hitamo ibimenyetso by'imirire byose bishoboka umwana afite:"
                            }

                        ChildActivity ChildPicture ->
                            { english = "Photo:"
                            , kinyarwanda = "Ifoto"
                            }

                        ChildActivity ProgressReport ->
                            { english = "Progress Report"
                            , kinyarwanda = "Raporo igaragaza imikurire y'umwana"
                            }

                        ChildActivity Weight ->
                            { english = "Weight:"
                            , kinyarwanda = "Ibiro:"
                            }

                ActivitiesTitle activity ->
                    case activity of
                        MotherActivity FamilyPlanning ->
                            { english = "Family Planning" 
                            , kinyarwanda = "Kuboneza Urubyaro? nticyaza muri raporo yimikurire yumwana"
                            }

                        ChildActivity Height ->
                            { english = "Height"
                            , kinyarwanda = "Uburebure"
                            }

                        ChildActivity Muac ->
                            { english = "MUAC"
                            , kinyarwanda = "Ikizigira cy'akaboko"
                            }

                        ChildActivity NutritionSigns ->
                            { english = "Nutrition"
                            , kinyarwanda = "Imirire"
                            }

                        ChildActivity ChildPicture ->
                            { english = "Photo"
                            , kinyarwanda = "Ifoto"
                            }

                        ChildActivity ProgressReport ->
                            { english = "Progress Report"
                            , kinyarwanda = "Raporo igaragaza imikurire y'umwana"
                            }

                        ChildActivity Weight ->
                            { english = "Weight"
                            , kinyarwanda = "Ibiro"
                            }

                ActivityProgressReport activity ->
                    case activity of
                        MotherActivity FamilyPlanning ->
                            { english = "Family Planning"
                            , kinyarwanda = "Kuboneza Urubyaro? nticyaza muri raporo yimikurire yumwana"}

                        ChildActivity Height ->
                            { english = "Height"
                            , kinyarwanda = "Uburebure"
                            }

                        ChildActivity Muac ->
                            { english = "MUAC"
                            , kinyarwanda = "Ikizigira cy'akaboko"
                            }

                        ChildActivity NutritionSigns ->
                            { english = "Nutrition Signs"
                            , kinyarwanda = "Ibimenyetso by'imirire"
                            }

                        ChildActivity ChildPicture ->
                            { english = "Photo"
                            , kinyarwanda = "Ifoto"
                            }

                        ChildActivity ProgressReport ->
                            { english = "Progress Report"
                            , kinyarwanda = "Raporo igaragaza imikurire y'umwana"
                            }

                        ChildActivity Weight ->
                            { english = "Weight"
                            , kinyarwanda = "Ibiro"
                            }

                ActivitiesToComplete count ->
                    { english = "To Do (" ++ toString count ++ ")"
                    , kinyarwanda = "Ibisabwa gukora"
                    }

                ActivePage page ->
                    case page of
                        LoginPage ->
                            { english = "Login"
                            , kinyarwanda = "Kwinjira"
                            }

                        PageNotFound url ->
                            { english = "Missing"
                            , kinyarwanda = "Ibibura"
                            }

                        SessionPage sessionPage ->
                            case sessionPage of
                                ActivitiesPage ->
                                    { english = "Activities"
                                    , kinyarwanda = "Ibikorwa"
                                    }

                                ActivityPage activityType ->
                                    { english = "Activity"
                                    , kinyarwanda = "Igikorwa"
                                    }

                                AttendancePage ->
                                    { english = "Attendance"
                                    , kinyarwanda = "Ubwitabire"
                                    }

                                ParticipantsPage ->
                                    { english = "Participants"
                                    kinyarwanda = "Abagenerwabikorwa"
                                    }

                                ChildPage childId ->
                                    { english = "Child"
                                    kinyarwanda = "Umwana"
                                    }

                                MotherPage motherId ->
                                    { english = "Mother"
                                    , kinyarwanda = "Umubyeyi"
                                    }

                                ProgressReportPage childId ->
                                    { english = "Progress Report"
                                    , kinyarwanda = "Raporo igaragaza imikurire y'umwana"
                                    }

                        UserPage (ClinicsPage _) ->
                            { english = "Clinics"
                            , kinyarwanda = "Ibigo nderabuzima"
                            }

                        UserPage MyAccountPage ->
                            { english = "'My Account'"
                            , kinyarwanda = "Compte"
                            }

                Age months days ->
                    { english = toString months ++ " months " ++ toString days ++ " days"
                    , kinyarwanda = toString months ++ "Amezi" ++ toString days ++ "iminsi"
                    }

                AgeDays days ->
                    { english = toString days ++ " days"
                    , kinyarwanda = toString days "Iminsi"
                    }

                AgeMonthsWithoutDay months ->
                    { english = toString months ++ " month"
                    , kinyarwanda = toString months "Ukwezi"
                    }

                AgeSingleBoth months days ->
                    { english = toString months ++ " month " ++ toString days ++ " day"
                    , kinyarwanda = toString months ++ "Ukwezi" ++ toString days ++ "Umunsi"
                    }

                AgeSingleMonth months days ->
                    { english = toString months ++ " month " ++ toString days ++ " days"
                    , kinyarwanda = toString months ++ "Ukwezi" ++ toString ++ "Iminsi"
                    }

                AgeSingleDayWithMonth months days ->
                    { english = toString months ++ " months " ++ toString days ++ " day"
                    , kinyarwanda = toString months ++ "Amezi" ++ toString days ++ "Umunsi"}

                AgeSingleDayWithoutMonth months days ->
                    { english = toString days ++ " day"
                    , kinyarwanda = toString day ++ "Umunsi"
                    }

                AgeSingleMonthWithoutDay month ->
                    { english = toString month ++ " month"
                    , kinyarwanda = toString month ++ "Ukwezi"
                    }

                AppName ->
                    { english = "E-Heza System"
                    , kinyarwanda = "E-heza sisiteme"
                    }

                AreYouSure ->
                    { english = "Are you sure?"
                    , kinyarwanda = "Urabyizeye?"
                    }

                Assessment ->
                    { english = "Assessment"
                    , kinyarwanda = "Ipimwa"
                    }

                Attendance ->
                    { english = "Attendance"
                    , kinyarwanda = "Ubwitabire"
                    }

                Baby ->
                    { english = "Baby"
                    , kinyarwanda = "Umwana"
                    }

                BabyName name ->
                    { english = "Baby: " ++ name
                    , kinyarwanda = "Umwana:" ++ name
                    }

                Born ->
                    { english = "Born"
                    , kinyarwanda = "Kuvuka/ itariki y'amavuko"
                    }

                BeginHealthAssessment ->
                    { english = "Begin Health Assessment"
                    , kinyarwanda = "Gutangira ibikorwa by'ipima"
                    }

                Cancel ->
                    { english = "Cancel"
                    , kinyarwanda = "Guhagarika"
                    }

                CentimeterShorthand ->
                    { english = "cm"
                    , kinyarwanda = "cm"
                    }

                ChartPhrase phrase ->
                    case phrase of
                        AgeCompletedMonthsYears ->
                            { english = "Age (completed months and years)"
                            , kinyarwanda = "Imyaka uzuza amazi n'imyaka"
                            }

                        Birth ->
                            { english = "Birth"
                            , kinyarwanda = "kuvuka"
                            }

                        BirthToTwoYears ->
                            { english = "Birth to 2 years (z-scores)"
                            , kinyarwanda = "kuvuka (Kuva avutse)  kugeza ku myaka 2 Z-score"
                            }

                        LengthCm ->
                            { english = "Length (cm)"
                            , kinyarwanda = "Uburere cm"
                            }

                        LengthForAgeBoys ->
                            { english = "Length-for-age BOYS"
                            , kinyarwanda = "Uburebure ku myaka/ umuhungu"}

                        LengthForAgeGirls ->
                            { english = "Length-for-age GIRLS"
                            , kinyarwanda = "uburebure ku myaka umukobwa"
                            }

                        Months ->
                            { english = "Months"
                            , kinyarwanda = "Amezi"
                            }

                        OneYear ->
                            { english = "1 year"
                            , kinyarwanda = "Umwaka umwe"
                            }

                        TwoYears ->
                            { english = "2 years"
                            , kinyarwanda = "Imyaka 2"
                            }

                        WeightForAgeBoys ->
                            { english = "Weight-for-age BOYS"
                            , kinyarwanda = "Ibiro ku myaka umuhungu"
                            }

                        WeightForAgeGirls ->
                            { english = "Weight-for-age GIRLS"
                            , kinyarwanda = "ibiro ku myaka umukobwa"
                            }

                        WeightForLengthBoys ->
                            { english = "Weight-for-length BOYS"
                            , kinyarwanda = "Ibiro ku Uburebure umuhungu"
                            }

                        WeightForLengthGirls ->
                            { english = "Weight-for-length GIRLS"
                            , kinyarwanda = "ibiro ku uburebure umukobwa"
                            }

                        WeightKg ->
                            { english = "Weight (kg)"
                            , kinyarwanda = "Ibiro kg"
                            }

                        ZScoreChartsAvailableAt ->
                            { english = "Z-score charts available at"
                            , kinyarwanda = "Raporo ku mikurire y'umwana"
                            }

                CheckIn ->
                    { english = "Check in:" 
                    , kinyarwanda = "Kureba abaje"
                    }

                ChildNutritionSignLabel sign ->
                    case sign of
                        AbdominalDistension ->
                            { english = "Abdominal Distension"
                            , kinyarwanda = "Kubyimba inda"
                            }

                        Apathy ->
                            { english = "Apathy"
                            , kinyarwanda = "Kwigunga"}

                        BrittleHair ->
                            { english = "Brittle Hair"
                            , kinyarwanda = "Gucurama no guhindura ibara ku misatsi"
                            }

                        DrySkin ->
                            { english = "Dry Skin"
                            , kinyarwanda = "Uruhu ryumye"
                            }

                        Edema ->
                            { english = "Edema"
                            , kinyarwanda = "Kubyimba"
                            }

                        None ->
                            { english = "None of these"
                            , kinyarwanda = "Nta bimenyetso "
                            }

                        PoorAppetite ->
                            { english = "Poor Appetite"
                            , kinyarwanda = "Kubura apeti /kunanirwa kurya"
                            }

                ChildNutritionSignReport sign ->
                    case sign of
                        AbdominalDistension ->
                            { english = "Abdominal Distension"
                            , kinyarwanda = "Kubyimba inda"
                            }

                        Apathy ->
                            { english = "Apathy"
                            , kinyarwanda = "Kwigunga"
                            }

                        BrittleHair ->
                            { english = "Brittle Hair"
                            , kinyarwanda = "Gucurama umusatsi"
                            }

                        DrySkin ->
                            { english = "Dry Skin"
                            , kinyarwanda = "Uruhu ryumye"
                            }

                        Edema ->
                            { english = "Edema"
                            , kinyarwanda = "Kubyimba"
                            }

                        None ->
                            { english = "None"
                            , kinyarwanda = "Nta bimenyetso "
                            }

                        PoorAppetite ->
                            { english = "Poor Appetite"
                            , kinyarwanda = "kubura apeti(kunanirwa kurya)"
                            }

                Children ->
                    { english = "Children"
                    , kinyarwanda = "Abana"
                    }

                ChildOf ->
                    { english = "Child of"
                    , kinyarwanda = "Umwana wa"
                    }

                ClickTheCheckMark ->
                    { english = "Click the check mark if the mother is in attendance. The check mark will appear green when a mother has been signed in."
                    , kinyarwanda = "Kanda (kuri) ku kazu niba umubyeyi ahari. Ku kazu harahita hahindura ibara habe icyaytsi niba wemeje ko umubyeyi ahari"
                    }

                ClinicNotFound ->
                    { english = "Clinic not found"
                    , kinyarwanda = "Ikigo nderabuzima nticyabonetse"
                    }

                Clinics ->
                    { english = "Clinics"
                    , kinyarwanda = "Ibigo nderebuzima"
                    }

                Connected ->
                    { english = "Connected"
                    , kinyarwanda = "Ufite interineti ( murandasi)"
                    }

                Continue ->
                    { english = "Continue"
                    , kinyarwanda = "Gukomeza"
                    }

                Dashboard ->
                    { english = "Dashboard"
                    , kinyarwanda = "Tabeau de bord"
                    }

                DataIsNowSaved ->
                    { english = "Data is now saved on the server."
                    , kinyarwanda = "Amakuru ubu abitswe kri seriveri."
                    }

                DateOfLastAssessment ->
                    { english = "Date of last Assessment"
                    , kinyarwanda = "Amakuru y'ipimwa ry'ubushize"
                    }

                Day ->
                    { english = "day"
                    , kinyarwanda = "Umunsi"
                    }

                Days ->
                    { english = "days"
                    , kinyarwanda = "Iminsi"
                    }

                DownloadHealthAssessment ->
                    { english = "Download Health Assessment"
                    , kinyarwanda = "Gukurura Health assessment (ibikorwa by'ubuzima)"
                    }

                DownloadSuccessful ->
                    { english = "Download Successful"
                    , kinyarwanda = "Gukurura Health assessment byagenze neza"
                    }

                DownloadingSession1 ->
                    { english = "Downloading…" 
                    , kinyarwanda = "Uri gukurura Health assessment(gukurura amakuru y'ipima)"
                    }

                DownloadingSession2 ->
                    { english = "Downloading may take a few minutes, or a few hours. Do not leave this page while data is downloading."
                    , kinyarwanda = "Gukurura Health Assessment bishobora gutwara iminota mike cg amasaha make. Ub uretse gufunga iyi paji mu gihe ugikurura amakuru."
                    }

                DropzoneDefaultMessage ->
                    { english = "Touch here to take a photo, or drop a photo file here."
                    , kinyarwanda = "Kanda hano niba ushaka gufotora cg ukure ifoto mu bubiko hano."
                    }

                DownloadSession1 ->
                    { english = "You have no sessions loaded to this device. Your next session will be available for download the day before it is scheduled to begin."
                    , kinyarwanda = "Nta bikirwa ry'ipimwa byinjijwe kuri tablet, ibikorwa by'ipimwa bikurikira bazaboneka kuba byakurwa kuri internet umunsi ubanziriza ipima. "
                    }

                DownloadSession2 ->
                    { english = "You must be connected to the internet to download a session."
                    , kinyarwanda = "Ugomba gukoresha internet (murandasi) kugirango ubone amakuru y'ipima."}

                EndSession ->
                    { english = "End Session"
                    , kinyarwanda = "Kurangiza ipima (gupima)"
                    }

                ErrorBadUrl ->
                    { english = "URL is not valid." }

                ErrorBadPayload ->
                    { english = "The server responded with data of an unexpected type." }

                ErrorBadStatus ->
                    { english = "The server indicated the following error:"
                    , kinyarwanda = "Seriveri yerekanye amakosa akurikira:"
                    }

                ErrorCheckLocalConfig ->
                    { english = "Check your LocalConfig.elm file and make sure you have defined the enviorement properly" }

                ErrorConfigurationError ->
                    { english = "Configuration error"
                    , kinyarwanda = "Ikosa mu igena miterere"
                    }

                ErrorFetchingCachedSession ->
                    { english = "There was an error fetchhing the session stored on this device." }

                ErrorNetworkError ->
                    { english = "A network error occurred contacting the server. Are you connected to the Internet?" }

                ErrorTimeout ->
                    { english = "The network request timed out." }

                FamilyPlanningSignLabel sign ->
                    case sign of
                        Condoms ->
                            { english = "Condoms"
                            , kinyarwanda = "Udukingirizo"
                            }

                        IUD ->
                            { english = "IUD"
                            , kinyarwanda = "Akapira ko mu mura(agapira ko munda ibyara)"
                            }

                        Implant ->
                            { english = "Implant"
                            , kinyarwanda = "Akapira ko mu kaboko"
                            }

                        Injection ->
                            { english = "Injection"
                            , kinyarwanda = "Urushinge"
                            }

                        Necklace ->
                            { english = "Necklace"
                            , kinyarwanda = "Urunigi"
                            }

                        Pill ->
                            { english = "Pill"
                            , kinyarwanda = "Ibinini"
                            }

                        NoFamilyPlanning ->
                            { english = "None of these"
                            , kinyarwanda = "nta buryo bwo kuboneza urubyaro akoresha"
                            }

                Fetch ->
                    { english = "Fetch"
                    , kinyarwanda = "Gushakisha"
                    }

                Gender gender ->
                    case gender of
                        Male ->
                            { english = "Male"
                            , kinyarwanda = "Gabo"
                            }

                        Female ->
                            { english = "Female"
                            , kinyarwanda = "Gore"
                            }

                GoHome ->
                    { english = "Go to main page"
                    , kinyarwanda = "Kujya ahabanza"
                    }

                KilogramShorthand ->
                    { english = "kg"
                    , kinyarwanda = "kg"
                    }

                LinkToMother ->
                    { english = "Link to mother"
                    , kinyarwanda = "Guhuza n'amakuru y'umubyeyi"
                    }

                LoginPhrase phrase ->
                    case phrase of
                        CheckingCachedCredentials ->
                            { english = "Checking cached credentials" }

                        ForgotPassword1 ->
                            { english = "Forgot your password?"
                            , kinyarwanda = "Wibagiwe ijambo ry'ibanga?"
                            }

                        ForgotPassword2 ->
                            { english = "Call The Ihangane Project at +250 788 817 542"
                            , kinyarwanda = "Hamagara The Ihangane Project kuri +250 788 817 542(Hamagara kumushinga wa ihangane"
                            }

                        LoggedInAs ->
                            { english = "Logged in as"
                            , kinyarwanda = "Kwinjira nka …"
                            }

                        LoginError error ->
                            case error of
                                AccessTokenRejected ->
                                    { english = "Your access token has expired. You will need to sign in again."
                                    , kinyarwanda = "Igihe cyo gukoresha sisitemu cyarangiye . Ongera winjore muri sisitemu"
                                    }

                                InternalError error ->
                                    { english = "The following error occurred contacting the server. " ++ toString error
                                    , kinyarwanda = "Aya makosa yagaragaye hamagara kuri seriveri." ++ toString error
                                    }

                                NetworkError ->
                                    { english = "A network error occurred contacting the server. Are you connected to the Internet?"
                                    , kinyarwanda = "hari ikibazo cya reseau hamagara kuri seriveri. ufite intereneti?(murandasi)"
                                    }

                                PasswordRejected ->
                                    { english = "The server rejected your username or password."
                                    , kinyarwanda = "Seriveri yanze ijambo ryo kwinjira cg ijambo ry'ibanga"
                                    }

                                Timeout ->
                                    { english = "The request to the server timed out."
                                    , kinyarwanda = "Ibyo wasabye kuri seriveri byarengeje igihe."
                                    }

                        LoginOrWorkOffline ->
                            { english = "Either login below, or work offline without logging in." }

                        Logout ->
                            { english = "Logout"
                            , kinyarwanda = "Gufunga"
                            }

                        LogoutInProgress ->
                            { english = "Logout in progress ..."
                            , kinyarwanda = "sisitemi irikwifunga"
                            }

                        LogoutFailed ->
                            { english = "Logout Failed"
                            , kinyarwanda = "Gufunga byanze"
                            }

                        Password ->
                            { english = "Password"
                            , kinyarwanda = "Ijambo ry'ibanga"
                            }

                        SignIn ->
                            { english = "Sign In"
                            , kinyarwanda = "Kwinjira"
                            }

                        Username ->
                            { english = "Username"
                            , kinyarwanda = "Izina ryo kwinjira"
                            }

                        WorkOffline ->
                            { english = "Work Offline"
                            , kinyarwanda = "Gukora nta internet"
                            }

                        YouMustLoginBefore ->
                            { english = "You must sign in before you can access the"
                            , kinyarwanda = "Ugomba kubanza kwinjira muri sisitemi mbere yuko ubona"
                            }

                MakeSureYouAreConnected ->
                    { english = "Make sure you are connected to the internet. If the issue continues, call The Ihangane Project at +250 788 817 542."
                    , kinyarwanda = "Banza urebe ko ufite interineti. Ikibazo nigikomeza, hamagara The Ihangane Project kuri +250 788 817 542"
                    }

                MeasurementNoChange ->
                    { english = "No Change"
                    , kinyarwanda = "nta cyahindutse"
                    }

                MeasurementGained amount ->
                    { english = "Gained " ++ toString amount
                    , kinyarwanda = "Kwiyongera" ++ toString amount
                    }

                MeasurementLost amount ->
                    { english = "Lost " ++ toString amount
                    , kinyarwanda = "Kwiyongera" ++ toString amount
                    }

                MonthAbbrev ->
                    { english = "mo"
                    , kinyarwanda = "amezi"
                    }

                MonthsOld ->
                    { english = "months old"
                    , kinyarwanda = "Amezi"
                    }

                Mother ->
                    { english = "Mother"
                    , kinyarwanda = "Umubyeyi"
                    }

                MotherName name ->
                    { english = "Mother: " ++ name 
                    , kinyarwanda "Umubyeyi" ++ name
                    }

                Mothers ->
                    { english = "Mothers"
                    , kinyarwanda = "Ababyeyi"
                    }

                MuacIndication indication ->
                    case indication of
                        MuacRed ->
                            { english = "red"
                            , kinyarwanda = "Umutuku"
                            }

                        MuacYellow ->
                            { english = "yellow"
                            , kinyarwanda = "Umuhondo"
                            }

                        MuacGreen ->
                            { english = "green"
                            , kinyarwanda = "Icyatsi"
                            }

                MyAccount ->
                    { english = "My Account"
                    , kinyarwanda = "Konti yanjye"
                    }

                NoActiveIncidents ->
                    { english = "No active incidents!" }

                NoActivitiesCompleted ->
                    { english = "No activities are entirely completed for the attending participants." 
                    , kinyarwanda = "Nta gikorwa cyarangiye cyose kubitabiriye "
                    }

                NoActivitiesPending ->
                    { english = "All activities are completed for the attending participants."
                    , kinyarwanda = "Ibikorwa byose byarangiye kubitabiriye."
                    }

                NoActivitiesCompletedForThisParticipant ->
                    { english = "No activities are completed for this participant." 
                    , kinyarwanda = "Nta gikorwa cyarangiye kubitabiriye."
                    }

                NoActivitiesPendingForThisParticipant ->
                    { english = "All activities are completed for this participant."
                    , kinyarwanda = "Ibikorwa byose byarangiye kubitabiriye."
                    }

                NoParticipantsCompleted ->
                    { english = "No participants have completed all their activities yet."
                    , kinyarwanda = "Ntagikorwa nakimwe kirarangira kubitabiriye." 
                    }

                NoParticipantsPending ->
                    { english = "All attending participants have completed their activities."
                    , kinyarwanda = "Abaje bose barangirijwe" }

                NoParticipantsCompletedForThisActivity ->
                    { english = "No participants have completed this activity yet." 
                    , kinyarwanda = "Ntawaje warangirijwe kukorerwa."
                    }

                NoParticipantsPendingForThisActivity ->
                    { english = "All attending participants have completed this activitity."
                    , kinyarwanda = "Ababje bose barangirijwe."
                    }

                NoCachedSession ->
                    { english = "No session was found on this device." }

                NoChildrenRegisteredInTheSystem ->
                    { english = "No children registered in the system"
                    , kinyarwanda = "Ntamwana wanditswe muriyi sisiteme"
                    }

                NoParticipantsFound ->
                    { english = "No participants found"
                    , kinyarwanda = "Ntamuntu ugaragaye"
                    }

                NotAvailable ->
                    { english = "not available"
                    , kinyarwanda = "Ntibiboneste"
                    }

                NotConnected ->
                    { english = "Not Connected"
                    , kinyarwanda = "Ntamurandasi"
                    }

                OK ->
                    { english = "OK"
                    , kinyarwanda = "Nibyo ,yego"
                    }

                Old ->
                    { english = "old"
                    , kinyarwanda = "imyaka"
                    }

                OnceYouEndYourSession ->
                    { english = "Once you end your session, you will no longer be able to edit or add data. Remember to upload this session within the next 48 hours." 
                    , kinyarwanda = "Igihe igikorwa cyawe ukirangije,ntubasha guhindura cyangwa kongera kubipimo,ibka kubyohereza mumasaha 48"
                    }

                Page ->
                    { english = "Page"
                    , , kinyarwanda = "Paji"
                    }

                Page404 ->
                    { english = "404 page"
                    , kinyarwanda = "404 paji"
                    }

                PageNotFoundMsg ->
                    { english = "Sorry, nothing found in this URL."
                    , kinyarwanda = "Mutwihanganire ntabwo ubufasha mwasabye mubashije kuboneka."
                    }

                Participants ->
                    { english = "Participants"
                    , kinyarwanda = "Ubwitabire"
                    }

                ParticipantSummary ->
                    { english = "Participant Summary" }

                PlaceholderEnterHeight ->
                    { english = "Enter height here…"
                    , kinyarwanda = "Andika uburebure hano…"
                    }

                PlaceholderEnterMUAC ->
                    { english = "Enter MUAC here…"
                    , kinyarwadna = "Andika uburebure hano…"}

                PlaceholderEnterWeight ->
                    { english = "Enter weight here…"
                    , kinyarwanda = "Andika ibiro hano…"
                    }

                PlaceholderTextGroupDate ->
                    { english = "Group Date"
                    , kinyarwanda = "Itariki y'itsinda"
                    }

                PlaceholderTextJoined ->
                    { english = "Joined in June 2017"
                    , kinyarwanda = "Yinjiye muri kamena 2017"
                    }

                PreviousFloatMeasurement value ->
                    { english = "Previous measurement: " ++ toString value
                    , kinyarwanda = "Ibipimo by'ubushize:" ++ toString value
                    }

                ReadyToBeginSession ->
                    { english = "You are now ready to begin your session."
                    , kinyarwanda = "Ubu ushobora gutangira ibikorwa byawe."
                    }

                ReportAge age ->
                    { english = "Age: " ++ age
                    , kinyarwanda = "Imyaka" ++ age
                    }

                ReportDOB dob ->
                    { english = "DOB: " ++ dob
                    , kinyarwanda = "Itariki y'amavuko" ++ dob
                    }

                ReportRemaining remaining ->
                    { english = toString remaining ++ " remaning"
                    , kinyarwanda = toString remaining ++ "iyibutswa rya raporo"
                    }

                ReloadParticipant ->
                    { english = "Re-load Participant"
                    , kinyarwanda = "Ishakisha ryabaritabira"
                    }

                ReportCompleted { pending, total } ->
                    { english = toString (total - pending) ++ "/" ++ toString total ++ " Completed"
                    , kinyarwanda = toString (total - pending) ++ "/" toString total ++ "Raporo irarangiye"
                    }

                ResolveMonth month ->
                    case month of
                        Jan ->
                            { english = "January"
                            , kinyarwanda = "Mutarama"
                            }

                        Feb ->
                            { english = "February"
                            , kinyarwanda = "Gashyantare"
                            }

                        Mar ->
                            { english = "March"
                            , kinyarwanda = "Werurwe"
                            }

                        Apr ->
                            { english = "April"
                            , kinyarwanda = "Mata"
                            }

                        May ->
                            { english = "May"
                            , kinyarwanda = "Gicurasi"
                            }

                        Jun ->
                            { english = "June"
                            , kinyarwanda = "Kamena"
                            }

                        Jul ->
                            { english = "July"
                            , kinyarwanda = "Nyakanga"
                            }

                        Aug ->
                            { english = "August"
                            , kinyarwanda = "Kanama"
                            }

                        Sep ->
                            { english = "September"
                            , kinyarwanda = "Nzeri"
                            }

                        Oct ->
                            { english = "October"
                            , kinyarwanda = "Ukwakira"
                            }

                        Nov ->
                            { english = "November"
                            , kinyarwanda = "Ugushyingo
                            }

                        Dec ->
                            { english = "December"
                            , kinyarwanda "Ukuboza"
                            }

                Retry ->
                    { english = "Retry"
                    , kinyarwanda "Kongera kugerageza"
                    }

                Save ->
                    { english = "Save"
                    , kinyarwanda "Kubika"
                    }

                SaveError ->
                    { english = "Save Error"
                    , kinyarwanda "Kubika error (ikosa mu kubika)"
                    }

                SearchByName ->
                    { english = "Search by Name"
                    , kinyarwanda "Gushakisha izina"
                    }

                SelectYourClinic ->
                    { english = "Select your clinic"
                    , kinyarwanda "Guhitamo ikigo nderabuzima"
                    }

                SessionClosed ->
                    { english = "Session closed"
                    , kinyarwanda "igikorwa kirafunze:
                    }

                SessionClosed2 sessionId ->
                    { english =
                        "You have stored data on the device for session "
                            ++ toString (fromEntityId sessionId)
                            ++ ", but it was not uploaded to the server and the session is closed. "
                            ++ "Please contact the Ihangane project for further instructions."
                    }

                SessionInProgress ->
                    { english = "A health assessment is already in progress for another clinic." }

                SessionUnauthorized ->
                    { english = "Session unauthorized" }

                SessionUnauthorized2 ->
                    { english =
                        """A health assessment is in progress on this device, but you are not authorized to view it.
                        Please contact the Ihangane project for further instructions."""
                    }

                ThisClinicHasNoMothers ->
                    { english = "This clinic has no mothers assigned to it." }

                TitleHealthAssessment ->
                    { english = "2017 July Health Assessment"
                    , kinyarwanda "Igikorwa kipima ,kamena2017"
                    }

                UnableToDownload ->
                    { english = "Unable to Download"
                    , kinyarwanda "ntibishoboka gukurura"
                    }

                UnableToUpload ->
                    { english = "Unable to Upload"
                    , kinyarwanda "Kwohereza health assessment ntibikunda(kohereza ntibikunda)"
                    }

                Update ->
                    { english = "Update"
                    , kinyarwanda "Kuvugurura"
                    }

                UpdateError ->
                    { english = "Update Error"
                    , kinyarwanda "ikosa mwivugurura"
                    }

                UploadHealthAssessment ->
                    { english = "Upload Health Assessment"
                    , kinyarwanda "Kwohereza health assessment"
                    }

                UploadingSession1 ->
                    { english = "Uploading…" 
                    , kinyarwanda "Kohereza"
                    }

                UploadingSession2 ->
                    { english = "Uploading may take a few minutes, or a few hours. Do not leave this page while data is uploading." }

                UploadSuccessful ->
                    { english = "Upload Successful"
                    , kinyarwanda "Kwohereza byagenze neza"
                    }

                ViewProgressReport ->
                    { english = "View Progress Report" 
                    , kinyarwanda "Garagaza uruhererekane rw'imikurire y'umwana"
                    }

                WelcomeUser name ->
                    { english = "Welcome " ++ name 
                    , kinyarwanda = "Murakaza neza" ++ name
                    }

                YouHaveACompletedSession ->
                    { english = "You have a completed session that needs to be uploaded. Please connect to the internet and upload this session within 48 hours." }

                ZScoreHeightForAge ->
                    { english = "Z-Score Height for Age: " 
                    , kinyarwanda = "Z-score Uburebure ku myaka:"
                    }

                ZScoreMuacForAge ->
                    { english = "MUAC for Age: "
                    , kinyarwand = "MUAC ku myaka"
                    }

                ZScoreWeightForAge ->
                    { english = "Z-Score Weight for Age: "
                    , kinyarwanda = "Z-score Ibiro ku myaka"
                    }

                ZScoreWeightForHeight ->
                    { english = "Z-Score Weight for Height: "
                    , kinyarwanda = "Z-score Ibiro ku uburebure"
                    }
    in
        case lang of
            English ->
                .english translationSet


languageFromString : String -> Result String Language
languageFromString str =
    case str of
        "English" ->
            Ok English

        _ ->
            Err "Not a language"


languageFromCode : String -> Result String Language
languageFromCode str =
    case str of
        "en" ->
            Ok English

        _ ->
            Err "Not a language"


languageToCode : Language -> String
languageToCode lang =
    case lang of
        English ->
            "en"
