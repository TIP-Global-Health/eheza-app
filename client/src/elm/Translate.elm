module Translate exposing (..)

import Activity.Model exposing (ActivityType(..), MotherActivityType(..), ChildActivityType(..))
import Backend.Measurement.Model exposing (FamilyPlanningSign(..), ChildNutritionSign(..), MuacIndication(..))
import Backend.Child.Model exposing (Gender(..))
import Date exposing (Month(..))
import Pages.Page exposing (..)
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
    | ActivePage Page
    | Age Int Int
    | AgeDays Int
    | AgeMonthsWithoutDay Int
    | AgeSingleBoth Int Int
    | AgeSingleMonth Int Int
    | AgeSingleMonthWithoutDay Int
    | AgeSingleDayWithMonth Int Int
    | AgeSingleDayWithoutMonth Int Int
    | AppName
    | Assessment
    | Baby
    | BabyName String
    | CentimeterShorthand
    | ChildNutritionSignLabel ChildNutritionSign
    | Children
    | Clinics
    | CompletedSectionEmpty
    | Connected
    | Dashboard
    | DownloadHealthAssessment
    | DownloadSession1
    | DownloadSession2
    | DownloadingSession1
    | DownloadingSession2
    | DropzoneDefaultMessage
    | EndSession
    | ErrorBadUrl
    | ErrorBadPayload
    | ErrorBadStatus
    | ErrorCheckLocalConfig
    | ErrorConfigurationError
    | ErrorNetworkError
    | ErrorTimeout
    | FamilyPlanningSignLabel FamilyPlanningSign
    | Fetch
    | Gender Gender
    | KilogramShorthand
    | LinkToMother
    | LoginPhrase LoginPhrase
    | MakeSureYouAreConnected
    | MeasurementNoChange
    | MeasurementGained Float
    | MeasurementLost Float
    | Mother
    | MotherName String
    | Mothers
    | MuacIndication MuacIndication
    | MyAccount
    | NoActiveIncidents
    | NoChildrenRegisteredInTheSystem
    | NoParticipantsFound
    | NotAvailable
    | NotConnected
    | OK
    | Page
    | Page404
    | PageNotFoundMsg
    | Participants
    | PendingSectionEmpty
    | PlaceholderEnterHeight
    | PlaceholderEnterMUAC
    | PlaceholderEnterWeight
    | PlaceholderTextGroupDate
    | PlaceholderTextJoined
    | PreviousFloatMeasurement Float
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
    | TitleHealthAssessment
    | UnableToDownload
    | WelcomeUser String
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
    | Logout
    | Password
    | SignIn
    | Username
    | YouMustLoginBefore


translate : Language -> TranslationId -> String
translate lang trans =
    let
        translationSet =
            case trans of
                AccessDenied ->
                    { english = "Access denied" }

                Activities ->
                    { english = "Activities" }

                ActivitiesCompleted count ->
                    { english = "Completed (" ++ toString count ++ ")" }

                ActivitiesHelp activity ->
                    case activity of
                        MotherActivity FamilyPlanning ->
                            { english = "Every mother should be asked about her family planing method(s) each month. If a mother needs family planning, refer her to a clinic." }

                        ChildActivity Height ->
                            { english = "Ask the mother to hold the baby’s head at the end of the measuring board. Move the slider to the baby’s heel and pull their leg straight." }

                        ChildActivity Muac ->
                            { english = "Make sure to measure at the center of the baby’s upper arm." }

                        ChildActivity NutritionSigns ->
                            { english = "Explain to the mother how to check the malnutrition signs for their own child." }

                        ChildActivity ChildPicture ->
                            { english = "Take each baby’s photo at each health assessment. Photos should show the entire body of each child." }

                        ChildActivity ProgressReport ->
                            { english = "" }

                        ChildActivity Weight ->
                            { english = "Calibrate the scale before taking the first baby's weight. Place baby in harness with no clothes on." }

                ActivitiesLabel activity ->
                    case activity of
                        MotherActivity FamilyPlanning ->
                            { english = "Which, if any, of the following methods do you use?" }

                        ChildActivity Height ->
                            { english = "Height:" }

                        ChildActivity Muac ->
                            { english = "MUAC:" }

                        ChildActivity NutritionSigns ->
                            { english = "Select all signs that are present:" }

                        ChildActivity ChildPicture ->
                            { english = "Photo:" }

                        ChildActivity ProgressReport ->
                            { english = "Progress Report" }

                        ChildActivity Weight ->
                            { english = "Weight:" }

                ActivitiesTitle activity ->
                    case activity of
                        MotherActivity FamilyPlanning ->
                            { english = "Planning" }

                        ChildActivity Height ->
                            { english = "Height" }

                        ChildActivity Muac ->
                            { english = "Mid Upper Arm Circumference (MUAC)" }

                        ChildActivity NutritionSigns ->
                            { english = "Nutrition" }

                        ChildActivity ChildPicture ->
                            { english = "Photo" }

                        ChildActivity ProgressReport ->
                            { english = "Progress Report" }

                        ChildActivity Weight ->
                            { english = "Weight" }

                ActivitiesToComplete count ->
                    { english = "To Do (" ++ toString count ++ ")" }

                ActivePage page ->
                    case page of
                        LoginPage ->
                            { english = "Login" }

                        PageNotFound url ->
                            { english = "Missing" }

                        SessionPage sessionPage ->
                            case sessionPage of
                                ActivitiesPage ->
                                    { english = "Activities" }

                                ActivityPage activityType ->
                                    { english = "Activity" }

                                ParticipantsPage ->
                                    { english = "Participants" }

                                ChildPage childId ->
                                    { english = "Child" }

                                MotherPage motherId ->
                                    { english = "Mother" }

                        UserPage (ClinicsPage _) ->
                            { english = "Clinics" }

                        UserPage MyAccountPage ->
                            { english = "'My Account'" }

                Age months days ->
                    { english = toString months ++ " months and " ++ toString days ++ " days" }

                AgeDays days ->
                    { english = toString days ++ " days" }

                AgeMonthsWithoutDay months ->
                    { english = toString months ++ " month" }

                AgeSingleBoth months days ->
                    { english = toString months ++ " month and " ++ toString days ++ " day" }

                AgeSingleMonth months days ->
                    { english = toString months ++ " month and " ++ toString days ++ " days" }

                AgeSingleDayWithMonth months days ->
                    { english = toString months ++ " months and " ++ toString days ++ " day" }

                AgeSingleDayWithoutMonth months days ->
                    { english = toString days ++ " day" }

                AgeSingleMonthWithoutDay month ->
                    { english = toString month ++ " month" }

                AppName ->
                    { english = "E-Heza System" }

                Assessment ->
                    { english = "Assessment" }

                Baby ->
                    { english = "Baby" }

                BabyName name ->
                    { english = "Baby: " ++ name }

                CentimeterShorthand ->
                    { english = "cm" }

                ChildNutritionSignLabel sign ->
                    case sign of
                        AbdominalDisortion ->
                            { english = "Abdominal Disortion" }

                        Apathy ->
                            { english = "Apathy" }

                        BrittleHair ->
                            { english = "Brittle Hair" }

                        DrySkin ->
                            { english = "Dry Skin" }

                        Edema ->
                            { english = "Edema" }

                        None ->
                            { english = "None of these" }

                        PoorAppetite ->
                            { english = "Poor Appetite" }

                Children ->
                    { english = "Children" }

                Clinics ->
                    { english = "Clinics" }

                CompletedSectionEmpty ->
                    { english = "This section has not yet been completed." }

                Connected ->
                    { english = "Connected" }

                Dashboard ->
                    { english = "Dashboard" }

                DownloadHealthAssessment ->
                    { english = "Download Health Assessment" }

                DownloadingSession1 ->
                    { english = "Downloading…" }

                DownloadingSession2 ->
                    { english = "Downloading may take a few minutes, or a few hours. Do not leave this page while data is downloading." }

                DropzoneDefaultMessage ->
                    { english = "Touch here to take a photo, or drop a photo file here." }

                DownloadSession1 ->
                    { english = "You have no sessions loaded to this device. Your next session will be available for download the day before it is scheduled to begin." }

                DownloadSession2 ->
                    { english = "You must be connected to the internet to download a session." }

                EndSession ->
                    { english = "End Session" }

                ErrorBadUrl ->
                    { english = "URL is not valid." }

                ErrorBadPayload ->
                    { english = "The server responded with data of an unexpected type." }

                ErrorBadStatus ->
                    { english = "The server indicated the following error:" }

                ErrorCheckLocalConfig ->
                    { english = "Check your LocalConfig.elm file and make sure you have defined the enviorement properly" }

                ErrorConfigurationError ->
                    { english = "Configuration error" }

                ErrorNetworkError ->
                    { english = "There was a network error." }

                ErrorTimeout ->
                    { english = "The network request timed out." }

                FamilyPlanningSignLabel sign ->
                    case sign of
                        Condoms ->
                            { english = "Condoms" }

                        IUD ->
                            { english = "IUD" }

                        Injection ->
                            { english = "Injection" }

                        Necklace ->
                            { english = "Necklace" }

                        Pill ->
                            { english = "Pill" }

                        NoFamilyPlanning ->
                            { english = "None of these" }

                Fetch ->
                    { english = "Fetch" }

                Gender gender ->
                    case gender of
                        Male ->
                            { english = "Male" }

                        Female ->
                            { english = "Female" }

                KilogramShorthand ->
                    { english = "kg" }

                LinkToMother ->
                    { english = "Link to mother" }

                LoginPhrase phrase ->
                    case phrase of
                        CheckingCachedCredentials ->
                            { english = "Checking cached credentials" }

                        ForgotPassword1 ->
                            { english = "Forgot your password?" }

                        ForgotPassword2 ->
                            { english = "Call The Ihangane Project at +250 788 817 542" }

                        LoggedInAs ->
                            { english = "Logged in as" }

                        LoginError error ->
                            case error of
                                AccessTokenRejected ->
                                    { english = "Your access token has expired. You will need to sign in again." }

                                InternalError _ ->
                                    { english = "An internal error occurred contacting the server." }

                                NetworkError ->
                                    { english = "A network error occurred contacting the server. Are you connected to the Internet?" }

                                PasswordRejected ->
                                    { english = "The server rejected your username or password." }

                                Timeout ->
                                    { english = "The request to the server timed out." }

                        Logout ->
                            { english = "Logout" }

                        Password ->
                            { english = "Password" }

                        SignIn ->
                            { english = "Sign In" }

                        Username ->
                            { english = "Username" }

                        YouMustLoginBefore ->
                            { english = "You must sign in before you can access the" }

                MakeSureYouAreConnected ->
                    { english = "Make sure you are connected to the internet. If the issue continues, call The Ihangane Project at +250 788 817 542." }

                MeasurementNoChange ->
                    { english = "No Change" }

                MeasurementGained amount ->
                    { english = "Gained " ++ (toString amount) }

                MeasurementLost amount ->
                    { english = "Lost " ++ (toString amount) }

                Mother ->
                    { english = "Mother" }

                MotherName name ->
                    { english = "Mother: " ++ name }

                Mothers ->
                    { english = "Mothers" }

                MuacIndication indication ->
                    case indication of
                        MuacRed ->
                            { english = "red" }

                        MuacYellow ->
                            { english = "yellow" }

                        MuacGreen ->
                            { english = "green" }

                MyAccount ->
                    { english = "My Account" }

                NoActiveIncidents ->
                    { english = "No active incidents!" }

                NoChildrenRegisteredInTheSystem ->
                    { english = "No children registered in the system" }

                NoParticipantsFound ->
                    { english = "No participants found" }

                NotAvailable ->
                    { english = "not available" }

                NotConnected ->
                    { english = "Not Connected" }

                OK ->
                    { english = "OK" }

                Page ->
                    { english = "Page" }

                Page404 ->
                    { english = "404 page" }

                PageNotFoundMsg ->
                    { english = "Sorry, nothing found in this URL." }

                Participants ->
                    { english = "Participants" }

                PendingSectionEmpty ->
                    { english = "This section has been completed." }

                PlaceholderEnterHeight ->
                    { english = "Enter height here…" }

                PlaceholderEnterMUAC ->
                    { english = "Enter muac here…" }

                PlaceholderEnterWeight ->
                    { english = "Enter weight here…" }

                PlaceholderTextGroupDate ->
                    { english = "Group Date" }

                PlaceholderTextJoined ->
                    { english = "Joined in June 2017" }

                PreviousFloatMeasurement value ->
                    { english = "Previous measurement: " ++ (toString value) }

                ReportAge age ->
                    { english = "Age: " ++ age }

                ReportDOB dob ->
                    { english = "DOB: " ++ dob }

                ReportRemaining remaining ->
                    { english = toString remaining ++ " remaning" }

                ReloadParticipant ->
                    { english = "Re-load Participant" }

                ReportCompleted { pending, total } ->
                    { english = (toString (total - pending)) ++ "/" ++ (toString total) ++ " Completed" }

                ResolveMonth month ->
                    case month of
                        Jan ->
                            { english = "January" }

                        Feb ->
                            { english = "February" }

                        Mar ->
                            { english = "March" }

                        Apr ->
                            { english = "April" }

                        May ->
                            { english = "May" }

                        Jun ->
                            { english = "June" }

                        Jul ->
                            { english = "July" }

                        Aug ->
                            { english = "August" }

                        Sep ->
                            { english = "September" }

                        Oct ->
                            { english = "October" }

                        Nov ->
                            { english = "November" }

                        Dec ->
                            { english = "December" }

                Retry ->
                    { english = "Retry" }

                Save ->
                    { english = "Save" }

                SaveError ->
                    { english = "Save error" }

                SearchByName ->
                    { english = "Search by Name" }

                SelectYourClinic ->
                    { english = "Select your clinic" }

                TitleHealthAssessment ->
                    { english = "2017 July Health Assessment" }

                UnableToDownload ->
                    { english = "Unable to Download" }

                WelcomeUser name ->
                    { english = "Welcome " ++ name }

                ZScoreHeightForAge ->
                    { english = "Z-Score Height for Age: " }

                ZScoreMuacForAge ->
                    { english = "Z-Score MUAC for Age: " }

                ZScoreWeightForAge ->
                    { english = "Z-Score Weight for Age: " }

                ZScoreWeightForHeight ->
                    { english = "Z-Score Weight for Height: " }
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
