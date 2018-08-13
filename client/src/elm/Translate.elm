module Translate exposing (..)

import Activity.Model exposing (Activity(..), ChildActivity(..), MotherActivity(..))
import Backend.Child.Model exposing (Gender(..))
import Backend.Entities exposing (..)
import Backend.Measurement.Model exposing (ChildNutritionSign(..), FamilyPlanningSign(..), MuacIndication(..))
import Date exposing (Month(..))
import Form.Error exposing (ErrorValue(..))
import Http
import Pages.Page exposing (..)
import Restful.Endpoint exposing (fromEntityId)
import Restful.Login exposing (LoginError(..), LoginMethod(..))


type Language
    = English
    | Kinyarwanda


allLanguages : List Language
allLanguages =
    [ English
    , Kinyarwanda
    ]


type alias TranslationSet =
    { english : String
    , kinyarwanda : Maybe String
    }


translate : Language -> TranslationId -> String
translate lang trans =
    selectLanguage lang (translationSet trans)


selectLanguage : Language -> TranslationSet -> String
selectLanguage lang set =
    case lang of
        English ->
            set.english

        Kinyarwanda ->
            case set.kinyarwanda of
                Just trans ->
                    trans

                Nothing ->
                    set.english


languageFromString : String -> Result String Language
languageFromString str =
    case str of
        "English" ->
            Ok English

        "Kinyarwanda" ->
            Ok Kinyarwanda

        _ ->
            Err "Not a language"


languageFromCode : String -> Result String Language
languageFromCode str =
    case str of
        "en" ->
            Ok English

        "rw" ->
            Ok Kinyarwanda

        _ ->
            Err "Not a language"


languageToCode : Language -> String
languageToCode lang =
    case lang of
        English ->
            "en"

        Kinyarwanda ->
            "rw"


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


type ValidationError
    = UnknownClinic


type TranslationId
    = AccessDenied
    | Activities
    | ActivitiesCompleted Int
    | ActivitiesHelp Activity
    | ActivitiesLabel Activity
    | ActivitiesTitle Activity
    | ActivitiesToComplete Int
    | ActivityProgressReport Activity
    | ActivePage Page
    | Admin
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
    | BackendError
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
    | Clinic
    | Clinics
    | Closed
    | ConfirmDeleteTrainingSessions
    | Connected
    | Continue
    | CreateSession
    | CreateTrainingSessions
    | DeleteTrainingSessions
    | Dashboard
    | DataIsNowSaved
    | DateOfLastAssessment
    | Day
    | Days
    | Delete
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
    | FormError (ErrorValue ValidationError)
    | FormField String
    | FutureSessions
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
    | PleaseSelectClinic
    | PreviousFloatMeasurement Float
    | ProgressReport
    | ReadyToBeginSession
    | ReportAge String
    | ReportDOB String
    | ReportRemaining Int
    | ReloadParticipant
    | ReportCompleted { pending : Int, completed : Int }
    | ResolveMonth Month
    | Retry
    | Save
    | SaveError
    | SearchByName
    | SelectClinic
    | SelectLanguage
    | SelectYourClinic
    | SessionClosed
    | SessionClosed2 SessionId
    | SessionInProgress
    | SessionUnauthorized
    | SessionUnauthorized2
    | StartEndDate
    | StartDate
    | EndDate
    | Success
    | TakenCareOfBy
    | ThisActionCannotBeUndone
    | ThisClinicHasNoMothers
    | TitleHealthAssessment
    | Training
    | TrainingSessionCreateSuccessMessage
    | TrainingSessionDeleteSuccessMessage
    | UnableToDownload
    | UnableToUpload
    | Unknown
    | Update
    | UpdateError
    | UploadHealthAssessment
    | UploadingSession1
    | UploadingSession2
    | UploadSuccessful
    | ValidationErrors
    | ViewProgressReport
    | WelcomeUser String
    | YouAreNotAnAdmin
    | YouHaveACompletedSession
    | YourSessionHasBeenSaved
    | ZScoreHeightForAge
    | ZScoreMuacForAge
    | ZScoreWeightForAge
    | ZScoreWeightForHeight


translationSet : TranslationId -> TranslationSet
translationSet trans =
    case trans of
        AccessDenied ->
            { english = "Access denied"
            , kinyarwanda = Just "Kwinjira ntibyemera"
            }

        Admin ->
            { english = "Administration"
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
            , kinyarwanda = Nothing
            }

        ActivitiesHelp activity ->
            case activity of
                MotherActivity FamilyPlanning ->
                    { english = "Every mother should be asked about her family planning method(s) each month. If a mother needs family planning, refer her to a clinic."
                    , kinyarwanda = Just "Buri mubyeyi agomba kubazwa uburyo bwo kuboneza urubyaro akoresha buri kwezi. Niba umubyeyi akeneye kuboneza urubyaro mwohereze ku kigo nderabuzima k'ubishinzwe"
                    }

                ChildActivity Height ->
                    { english = "Ask the mother to hold the baby’s head at the end of the measuring board. Move the slider to the baby’s heel and pull their leg straight."
                    , kinyarwanda = Just "Saba Umubyeyi guhagarara inyuma y’umwana we agaramye, afata umutwe ku gice cy’amatwi. Sunikira akabaho ku buryo gakora mu bworo by’ibirenge byombi."
                    }

                ChildActivity Muac ->
                    { english = "Make sure to measure at the center of the baby’s upper arm."
                    , kinyarwanda = Just "Ibuka gupima icya kabiri cy'akaboko ko hejuru kugira bigufashe guoima ikizigira cy'akaboko"
                    }

                ChildActivity NutritionSigns ->
                    { english = "Explain to the mother how to check the malnutrition signs for their own child."
                    , kinyarwanda = Just "Sobanurira umubyeyi gupima ibimenyetso by'imirire mibi ku giti cye"
                    }

                ChildActivity ChildPicture ->
                    { english = "Take each baby’s photo at each health assessment. Photos should show the entire body of each child."
                    , kinyarwanda = Just "Fata ifoto ya buri mwana kuri buri bikorwa by'ipimwa Ifoto igomba kwerekana ibice by'umubiri wose by'umwana"
                    }

                ChildActivity Weight ->
                    { english = "Calibrate the scale before taking the first baby's weight. Place baby in harness with no clothes on."
                    , kinyarwanda = Just "Ibuka kuregera umunzani mbere yo gupima ibiro by'umwana wa mbere. Ambika umwana ikariso y'ibiro wabanje kumukuramo imyenda iremereye"
                    }

        ActivitiesLabel activity ->
            case activity of
                MotherActivity FamilyPlanning ->
                    { english = "Which, if any, of the following methods do you use?"
                    , kinyarwanda = Just "Ni ubuhe buryo, niba hari ubuhari, mu buryo bukurikira bwo kuboneza urubyaro ukoresha? Muri ubu buryo bukurikira bwo kuboneza urubyaro, ni ubuhe buryo mukoresha?"
                    }

                ChildActivity Height ->
                    { english = "Height:"
                    , kinyarwanda = Just "Uburere:"
                    }

                ChildActivity Muac ->
                    { english = "MUAC:"
                    , kinyarwanda = Just "Ikizigira cy'akaboko:"
                    }

                ChildActivity NutritionSigns ->
                    { english = "Select all signs that are present:"
                    , kinyarwanda = Just "Hitamo ibimenyetso by'imirire byose bishoboka umwana afite:"
                    }

                ChildActivity ChildPicture ->
                    { english = "Photo:"
                    , kinyarwanda = Just "Ifoto"
                    }

                ChildActivity Weight ->
                    { english = "Weight:"
                    , kinyarwanda = Just "Ibiro:"
                    }

        ActivitiesTitle activity ->
            case activity of
                MotherActivity FamilyPlanning ->
                    { english = "Family Planning"
                    , kinyarwanda = Just "Kuboneza Urubyaro? nticyaza muri raporo yimikurire yumwana"
                    }

                ChildActivity Height ->
                    { english = "Height"
                    , kinyarwanda = Just "Uburebure"
                    }

                ChildActivity Muac ->
                    { english = "MUAC"
                    , kinyarwanda = Just "Ikizigira cy'akaboko"
                    }

                ChildActivity NutritionSigns ->
                    { english = "Nutrition"
                    , kinyarwanda = Just "Imirire"
                    }

                ChildActivity ChildPicture ->
                    { english = "Photo"
                    , kinyarwanda = Just "Ifoto"
                    }

                ChildActivity Weight ->
                    { english = "Weight"
                    , kinyarwanda = Just "Ibiro"
                    }

        ActivityProgressReport activity ->
            case activity of
                MotherActivity FamilyPlanning ->
                    { english = "Family Planning"
                    , kinyarwanda = Just "Kuboneza Urubyaro? nticyaza muri raporo yimikurire yumwana"
                    }

                ChildActivity Height ->
                    { english = "Height"
                    , kinyarwanda = Just "Uburebure"
                    }

                ChildActivity Muac ->
                    { english = "MUAC"
                    , kinyarwanda = Just "Ikizigira cy'akaboko"
                    }

                ChildActivity NutritionSigns ->
                    { english = "Nutrition Signs"
                    , kinyarwanda = Just "Ibimenyetso by'imirire"
                    }

                ChildActivity ChildPicture ->
                    { english = "Photo"
                    , kinyarwanda = Just "Ifoto"
                    }

                ChildActivity Weight ->
                    { english = "Weight"
                    , kinyarwanda = Just "Ibiro"
                    }

        ActivitiesToComplete count ->
            { english = "To Do (" ++ toString count ++ ")"
            , kinyarwanda = Just <| "Ibisabwa gukora (" ++ toString count ++ ")"
            }

        ActivePage page ->
            translateActivePage page

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

        Attendance ->
            { english = "Attendance"
            , kinyarwanda = Just "Ubwitabire"
            }

        Baby ->
            { english = "Baby"
            , kinyarwanda = Just "Umwana"
            }

        BabyName name ->
            { english = "Baby: " ++ name
            , kinyarwanda = Just <| "Umwana: " ++ name
            }

        BackendError ->
            { english = "Error contacting backend"
            , kinyarwanda = Nothing
            }

        Born ->
            { english = "Born"
            , kinyarwanda = Just "Kuvuka/ itariki y'amavuko"
            }

        BeginHealthAssessment ->
            { english = "Begin Health Assessment"
            , kinyarwanda = Just "Gutangira ibikorwa by'ipima"
            }

        Cancel ->
            { english = "Cancel"
            , kinyarwanda = Just "Guhagarika"
            }

        CentimeterShorthand ->
            { english = "cm"
            , kinyarwanda = Just "cm"
            }

        ChartPhrase phrase ->
            translateChartPhrase phrase

        CheckIn ->
            { english = "Check in:"
            , kinyarwanda = Just "Kureba abaje"
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

                BrittleHair ->
                    { english = "Brittle Hair"
                    , kinyarwanda = Just "Gucurama no guhindura ibara ku misatsi"
                    }

                DrySkin ->
                    { english = "Dry Skin"
                    , kinyarwanda = Just "Uruhu ryumye"
                    }

                Edema ->
                    { english = "Edema"
                    , kinyarwanda = Just "Kubyimba"
                    }

                None ->
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

                BrittleHair ->
                    { english = "Brittle Hair"
                    , kinyarwanda = Just "Gucurama umusatsi"
                    }

                DrySkin ->
                    { english = "Dry Skin"
                    , kinyarwanda = Just "Uruhu ryumye"
                    }

                Edema ->
                    { english = "Edema"
                    , kinyarwanda = Just "Kubyimba"
                    }

                None ->
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

        ChildOf ->
            { english = "Child of"
            , kinyarwanda = Just "Umwana wa"
            }

        ClickTheCheckMark ->
            { english = "Click the check mark if the mother / caregiver is in attendance. The check mark will appear green when a mother / caregiver has been signed in."
            , kinyarwanda = Just "Kanda (kuri) ku kazu niba umubyeyi ahari. Ku kazu harahita hahindura ibara habe icyaytsi niba wemeje ko umubyeyi ahari"
            }

        ClinicNotFound ->
            { english = "Clinic not found"
            , kinyarwanda = Just "Ikigo nderabuzima nticyabonetse"
            }

        Clinic ->
            { english = "Clinic"
            , kinyarwanda = Just "Ikigo nderabuzima"
            }

        Clinics ->
            { english = "Clinics"
            , kinyarwanda = Just "Ibigo nderebuzima"
            }

        Closed ->
            { english = "Closed"
            , kinyarwanda = Nothing
            }

        ConfirmDeleteTrainingSessions ->
            { english = "Are you sure you want to delete all training sessions?"
            , kinyarwanda = Nothing
            }

        Connected ->
            { english = "Connected"
            , kinyarwanda = Just "Ufite interineti (murandasi)"
            }

        Continue ->
            { english = "Continue"
            , kinyarwanda = Just "Gukomeza"
            }

        CreateSession ->
            { english = "Create Session"
            , kinyarwanda = Nothing
            }

        CreateTrainingSessions ->
            { english = "Create All Training Sessions"
            , kinyarwanda = Nothing
            }

        DeleteTrainingSessions ->
            { english = "Delete All Training Sessions"
            , kinyarwanda = Nothing
            }

        Dashboard ->
            { english = "Dashboard"
            , kinyarwanda = Just "Tabeau de bord"
            }

        DataIsNowSaved ->
            { english = "Data is now saved on the server."
            , kinyarwanda = Just "Amakuru ubu abitswe kri seriveri."
            }

        DateOfLastAssessment ->
            { english = "Date of last Assessment"
            , kinyarwanda = Just "Amakuru y'ipimwa ry'ubushize"
            }

        Day ->
            { english = "day"
            , kinyarwanda = Just "Umunsi"
            }

        Days ->
            { english = "days"
            , kinyarwanda = Just "Iminsi"
            }

        Delete ->
            { english = "Delete"
            , kinyarwanda = Nothing
            }

        DownloadHealthAssessment ->
            { english = "Download Health Assessment"
            , kinyarwanda = Just "Gukurura Health assessment (ibikorwa by'ubuzima)"
            }

        DownloadSuccessful ->
            { english = "Download Successful"
            , kinyarwanda = Just "Gukurura Health assessment byagenze neza"
            }

        DownloadingSession1 ->
            { english = "Downloading…"
            , kinyarwanda = Just "Uri gukurura Health assessment(gukurura amakuru y'ipima)"
            }

        DownloadingSession2 ->
            { english = "Downloading may take a few minutes, or a few hours. Do not leave this page while data is downloading."
            , kinyarwanda = Just "Gukurura Health Assessment bishobora gutwara iminota mike cg amasaha make. Ub uretse gufunga iyi paji mu gihe ugikurura amakuru."
            }

        DropzoneDefaultMessage ->
            { english = "Touch here to take a photo, or drop a photo file here."
            , kinyarwanda = Just "Kanda hano niba ushaka gufotora cg ukure ifoto mu bubiko hano."
            }

        DownloadSession1 ->
            { english = "You have no sessions loaded to this device. Your next session will be available for download the day before it is scheduled to begin."
            , kinyarwanda = Just "Nta bikirwa ry'ipimwa byinjijwe kuri tablet, ibikorwa by'ipimwa bikurikira bazaboneka kuba byakurwa kuri internet umunsi ubanziriza ipima. "
            }

        DownloadSession2 ->
            { english = "You must be connected to the internet to download a session."
            , kinyarwanda = Just "Ugomba gukoresha internet (murandasi) kugirango ubone amakuru y'ipima."
            }

        EndSession ->
            { english = "End Session"
            , kinyarwanda = Just "Kurangiza ipima (gupima)"
            }

        ErrorBadUrl ->
            { english = "URL is not valid."
            , kinyarwanda = Nothing
            }

        ErrorBadPayload ->
            { english = "The server responded with data of an unexpected type."
            , kinyarwanda = Nothing
            }

        ErrorBadStatus ->
            { english = "The server indicated the following error:"
            , kinyarwanda = Just "Seriveri yerekanye amakosa akurikira:"
            }

        ErrorCheckLocalConfig ->
            { english = "Check your LocalConfig.elm file and make sure you have defined the enviorement properly"
            , kinyarwanda = Nothing
            }

        ErrorConfigurationError ->
            { english = "Configuration error"
            , kinyarwanda = Just "Ikosa mu igena miterere"
            }

        ErrorFetchingCachedSession ->
            { english = "There was an error fetchhing the session stored on this device."
            , kinyarwanda = Nothing
            }

        ErrorNetworkError ->
            { english = "A network error occurred contacting the server. Are you connected to the Internet?"
            , kinyarwanda = Nothing
            }

        ErrorTimeout ->
            { english = "The network request timed out."
            , kinyarwanda = Nothing
            }

        FamilyPlanningSignLabel sign ->
            case sign of
                Condoms ->
                    { english = "Condoms"
                    , kinyarwanda = Just "Udukingirizo"
                    }

                IUD ->
                    { english = "IUD"
                    , kinyarwanda = Just "Akapira ko mu mura (agapira ko munda ibyara)"
                    }

                Implant ->
                    { english = "Implant"
                    , kinyarwanda = Just "Akapira ko mu kaboko"
                    }

                Injection ->
                    { english = "Injection"
                    , kinyarwanda = Just "Urushinge"
                    }

                Necklace ->
                    { english = "Necklace"
                    , kinyarwanda = Just "Urunigi"
                    }

                Pill ->
                    { english = "Pill"
                    , kinyarwanda = Just "Ibinini"
                    }

                NoFamilyPlanning ->
                    { english = "None of these"
                    , kinyarwanda = Just "nta buryo bwo kuboneza urubyaro akoresha"
                    }

        Fetch ->
            { english = "Fetch"
            , kinyarwanda = Just "Gushakisha"
            }

        FormError errorValue ->
            translateFormError errorValue

        FormField field ->
            translateFormField field

        FutureSessions ->
            { english = "Future Sessions"
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

        GoHome ->
            { english = "Go to main page"
            , kinyarwanda = Just "Kujya ahabanza"
            }

        KilogramShorthand ->
            { english = "kg"
            , kinyarwanda = Just "kg"
            }

        LinkToMother ->
            { english = "Link to mother"
            , kinyarwanda = Just "Guhuza n'amakuru y'umubyeyi"
            }

        LoginPhrase phrase ->
            translateLoginPhrase phrase

        MakeSureYouAreConnected ->
            { english = "Make sure you are connected to the internet. If the issue continues, call The Ihangane Project at +250 788 817 542."
            , kinyarwanda = Just "Banza urebe ko ufite interineti. Ikibazo nigikomeza, hamagara The Ihangane Project kuri +250 788 817 542"
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

        MotherName name ->
            { english = "Mother/Caregiver: " ++ name
            , kinyarwanda = Just <| "Umubyeyi: " ++ name
            }

        Mothers ->
            { english = "Mothers"
            , kinyarwanda = Just "Ababyeyi"
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

        NoActiveIncidents ->
            { english = "No active incidents!"
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

        NoCachedSession ->
            { english = "No session was found on this device."
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

        OK ->
            { english = "OK"
            , kinyarwanda = Just "Nibyo, yego"
            }

        Old ->
            { english = "old"
            , kinyarwanda = Just "imyaka"
            }

        OnceYouEndYourSession ->
            { english = "Once you end your session, you will no longer be able to edit or add data. Remember to upload this session within the next 48 hours."
            , kinyarwanda = Just "Igihe igikorwa cyawe ukirangije,ntubasha guhindura cyangwa kongera kubipimo, ibka kubyohereza mumasaha 48"
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

        Participants ->
            { english = "Participants"
            , kinyarwanda = Just "Ubwitabire"
            }

        ParticipantSummary ->
            { english = "Participant Summary"
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

        PlaceholderEnterWeight ->
            { english = "Enter weight here…"
            , kinyarwanda = Just "Andika ibiro hano…"
            }

        PlaceholderTextGroupDate ->
            { english = "Group Date"
            , kinyarwanda = Just "Itariki y'itsinda"
            }

        PlaceholderTextJoined ->
            { english = "Joined in June 2017"
            , kinyarwanda = Just "Yinjiye muri kamena 2017"
            }

        PleaseSelectClinic ->
            { english = "Please select the relevant clinic for the new session"
            , kinyarwanda = Nothing
            }

        PreviousFloatMeasurement value ->
            { english = "Previous measurement: " ++ toString value
            , kinyarwanda = Just <| "Ibipimo by'ubushize: " ++ toString value
            }

        ProgressReport ->
            { english = "Progress Report"
            , kinyarwanda = Just "Raporo igaragaza imikurire y'umwana"
            }

        ReadyToBeginSession ->
            { english = "You are now ready to begin your session."
            , kinyarwanda = Just "Ubu ushobora gutangira ibikorwa byawe."
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

        ReloadParticipant ->
            { english = "Re-load Participant"
            , kinyarwanda = Just "Ishakisha ryabaritabira"
            }

        ReportCompleted { pending, completed } ->
            { english = toString completed ++ " / " ++ toString (pending + completed) ++ " Completed"
            , kinyarwanda = Just <| toString completed ++ " / " ++ toString (pending + completed) ++ " Raporo irarangiye"
            }

        ResolveMonth month ->
            translateMonth month

        Retry ->
            { english = "Retry"
            , kinyarwanda = Just "Kongera kugerageza"
            }

        Save ->
            { english = "Save"
            , kinyarwanda = Just "Kubika"
            }

        SaveError ->
            { english = "Save Error"
            , kinyarwanda = Just "Kubika error (ikosa mu kubika)"
            }

        SearchByName ->
            { english = "Search by Name"
            , kinyarwanda = Just "Gushakisha izina"
            }

        SelectLanguage ->
            { english = "Select language"
            , kinyarwanda = Nothing
            }

        SelectClinic ->
            { english = "Select Clinic..."
            , kinyarwanda = Nothing
            }

        SelectYourClinic ->
            { english = "Select your clinic"
            , kinyarwanda = Just "Guhitamo ikigo nderabuzima"
            }

        SessionClosed ->
            { english = "Session closed"
            , kinyarwanda = Just "igikorwa kirafunze:"
            }

        SessionClosed2 sessionId ->
            { english =
                "You have stored data on the device for session "
                    ++ toString (fromEntityId sessionId)
                    ++ ", but it was not uploaded to the server and the session is closed. "
                    ++ "Please contact the Ihangane project for further instructions."
            , kinyarwanda = Nothing
            }

        SessionInProgress ->
            { english = "A health assessment is already in progress for another clinic."
            , kinyarwanda = Nothing
            }

        SessionUnauthorized ->
            { english = "Session unauthorized"
            , kinyarwanda = Nothing
            }

        SessionUnauthorized2 ->
            { english =
                """A health assessment is in progress on this device, but you are not authorized to view it.
        Please contact the Ihangane project for further instructions."""
            , kinyarwanda = Nothing
            }

        StartEndDate ->
            { english = "Start - End"
            , kinyarwanda = Nothing
            }

        StartDate ->
            { english = "Start Date"
            , kinyarwanda = Nothing
            }

        EndDate ->
            { english = "End Date"
            , kinyarwanda = Nothing
            }

        Success ->
            { english = "Success"
            , kinyarwanda = Nothing
            }

        TakenCareOfBy ->
            { english = "Taken care of by"
            , kinyarwanda = Nothing
            }

        ThisActionCannotBeUndone ->
            { english = "This action cannot be undone."
            , kinyarwanda = Nothing
            }

        ThisClinicHasNoMothers ->
            { english = "This clinic has no mothers assigned to it."
            , kinyarwanda = Nothing
            }

        TitleHealthAssessment ->
            { english = "2017 July Health Assessment"
            , kinyarwanda = Just "Igikorwa kipima ,kamena2017"
            }

        Training ->
            { english = "Training"
            , kinyarwanda = Nothing
            }

        TrainingSessionCreateSuccessMessage ->
            { english = "Training sessions were created."
            , kinyarwanda = Nothing
            }

        TrainingSessionDeleteSuccessMessage ->
            { english = "Training sessions were deleted."
            , kinyarwanda = Nothing
            }

        UnableToDownload ->
            { english = "Unable to Download"
            , kinyarwanda = Just "ntibishoboka gukurura"
            }

        UnableToUpload ->
            { english = "Unable to Upload"
            , kinyarwanda = Just "Kwohereza health assessment ntibikunda (kohereza ntibikunda)"
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

        UploadHealthAssessment ->
            { english = "Upload Health Assessment"
            , kinyarwanda = Just "Kwohereza health assessment"
            }

        UploadingSession1 ->
            { english = "Uploading…"
            , kinyarwanda = Just "Kohereza"
            }

        UploadingSession2 ->
            { english = "Uploading may take a few minutes, or a few hours. Do not leave this page while data is uploading."
            , kinyarwanda = Nothing
            }

        UploadSuccessful ->
            { english = "Upload Successful"
            , kinyarwanda = Just "Kwohereza byagenze neza"
            }

        ValidationErrors ->
            { english = "Validation Errors"
            , kinyarwanda = Nothing
            }

        ViewProgressReport ->
            { english = "View Progress Report"
            , kinyarwanda = Just "Garagaza uruhererekane rw'imikurire y'umwana"
            }

        WelcomeUser name ->
            { english = "Welcome " ++ name
            , kinyarwanda = Just <| "Murakaza neza " ++ name
            }

        YouAreNotAnAdmin ->
            { english = "You are not logged in as an Administrator."
            , kinyarwanda = Nothing
            }

        YouHaveACompletedSession ->
            { english = "You have a completed session that needs to be uploaded. Please connect to the internet and upload this session within 48 hours."
            , kinyarwanda = Nothing
            }

        YourSessionHasBeenSaved ->
            { english = "Your session has been saved."
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


translateActivePage : Page -> TranslationSet
translateActivePage page =
    case page of
        LoginPage ->
            { english = "Login"
            , kinyarwanda = Just "Kwinjira"
            }

        PageNotFound url ->
            { english = "Missing"
            , kinyarwanda = Just "Ibibura"
            }

        SessionPage sessionPage ->
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

        UserPage userPage ->
            case userPage of
                AdminPage ->
                    { english = "Admin"
                    , kinyarwanda = Nothing
                    }

                ClinicsPage _ ->
                    { english = "Clinics"
                    , kinyarwanda = Just "Ibigo nderabuzima"
                    }

                MyAccountPage ->
                    { english = "'My Account'"
                    , kinyarwanda = Just "Compte"
                    }


translateChartPhrase : ChartPhrase -> TranslationSet
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

        TwoYears ->
            { english = "2 years"
            , kinyarwanda = Just "Imyaka 2"
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

        ZScoreChartsAvailableAt ->
            { english = "Z-score charts available at"
            , kinyarwanda = Just "Raporo ku mikurire y'umwana"
            }


translateLoginPhrase : LoginPhrase -> TranslationSet
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
            case error of
                Http.NetworkError ->
                    { english = "A network error occurred contacting the server. Are you connected to the Internet?"
                    , kinyarwanda = Just "hari ikibazo cya reseau hamagara kuri seriveri. ufite intereneti?(murandasi)"
                    }

                Http.Timeout ->
                    { english = "The request to the server timed out."
                    , kinyarwanda = Just "Ibyo wasabye kuri seriveri byarengeje igihe."
                    }

                _ ->
                    { english = "The following error occurred while contacting the server. " ++ toString error
                    , kinyarwanda = Just <| "Aya makosa yagaragaye hamagara kuri seriveri. " ++ toString error
                    }

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

        SignIn ->
            { english = "Sign In"
            , kinyarwanda = Just "Kwinjira"
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


translateMonth : Month -> TranslationSet
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


translateValidationError : ValidationError -> TranslationSet
translateValidationError id =
    case id of
        UnknownClinic ->
            { english = "is not a known clinic"
            , kinyarwanda = Nothing
            }


translateFormError : ErrorValue ValidationError -> TranslationSet
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
translateFormField : String -> TranslationSet
translateFormField field =
    case field of
        "clinic_id" ->
            translationSet Clinic

        "closed" ->
            translationSet Closed

        "training" ->
            translationSet Clinic

        "scheduled_date.start" ->
            translationSet StartDate

        "scheduled_date.end" ->
            translationSet EndDate

        _ ->
            { english = field
            , kinyarwanda = Nothing
            }
