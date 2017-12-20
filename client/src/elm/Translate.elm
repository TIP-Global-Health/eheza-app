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
    | SessionClosed2
    | SessionInProgress
    | SessionUnauthorized
    | SessionUnauthorized2
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
                    { english = "Access denied" }

                AgeWord ->
                    { english = "Age" }

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
                            { english = "MUAC" }

                        ChildActivity NutritionSigns ->
                            { english = "Nutrition" }

                        ChildActivity ChildPicture ->
                            { english = "Photo" }

                        ChildActivity ProgressReport ->
                            { english = "Progress Report" }

                        ChildActivity Weight ->
                            { english = "Weight" }

                ActivityProgressReport activity ->
                    case activity of
                        MotherActivity FamilyPlanning ->
                            { english = "Planning" }

                        ChildActivity Height ->
                            { english = "Height" }

                        ChildActivity Muac ->
                            { english = "MUAC" }

                        ChildActivity NutritionSigns ->
                            { english = "Nutrition Signs" }

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

                                AttendancePage ->
                                    { english = "Attendance" }

                                ParticipantsPage ->
                                    { english = "Participants" }

                                ChildPage childId ->
                                    { english = "Child" }

                                MotherPage motherId ->
                                    { english = "Mother" }

                                ProgressReportPage childId ->
                                    { english = "Progress Report" }

                        UserPage (ClinicsPage _) ->
                            { english = "Clinics" }

                        UserPage MyAccountPage ->
                            { english = "'My Account'" }

                Age months days ->
                    { english = toString months ++ " months " ++ toString days ++ " days" }

                AgeDays days ->
                    { english = toString days ++ " days" }

                AgeMonthsWithoutDay months ->
                    { english = toString months ++ " month" }

                AgeSingleBoth months days ->
                    { english = toString months ++ " month " ++ toString days ++ " day" }

                AgeSingleMonth months days ->
                    { english = toString months ++ " month " ++ toString days ++ " days" }

                AgeSingleDayWithMonth months days ->
                    { english = toString months ++ " months " ++ toString days ++ " day" }

                AgeSingleDayWithoutMonth months days ->
                    { english = toString days ++ " day" }

                AgeSingleMonthWithoutDay month ->
                    { english = toString month ++ " month" }

                AppName ->
                    { english = "E-Heza System" }

                AreYouSure ->
                    { english = "Are you sure?" }

                Assessment ->
                    { english = "Assessment" }

                Attendance ->
                    { english = "Attendance" }

                Baby ->
                    { english = "Baby" }

                BabyName name ->
                    { english = "Baby: " ++ name }

                Born ->
                    { english = "Born" }

                BeginHealthAssessment ->
                    { english = "Begin Health Assessment" }

                Cancel ->
                    { english = "Cancel" }

                CentimeterShorthand ->
                    { english = "cm" }

                ChartPhrase phrase ->
                    case phrase of
                        AgeCompletedMonthsYears ->
                            { english = "Age (completed months and years)" }

                        Birth ->
                            { english = "Birth" }

                        BirthToTwoYears ->
                            { english = "Birth to 2 years (z-scores)" }

                        LengthCm ->
                            { english = "Length (cm)" }

                        LengthForAgeBoys ->
                            { english = "Length-for-age BOYS" }

                        LengthForAgeGirls ->
                            { english = "Length-for-age GIRLS" }

                        Months ->
                            { english = "Months" }

                        OneYear ->
                            { english = "1 year" }

                        TwoYears ->
                            { english = "2 years" }

                        WeightForAgeBoys ->
                            { english = "Weight-for-age BOYS" }

                        WeightForAgeGirls ->
                            { english = "Weight-for-age GIRLS" }

                        WeightForLengthBoys ->
                            { english = "Weight-for-length BOYS" }

                        WeightForLengthGirls ->
                            { english = "Weight-for-length GIRLS" }

                        WeightKg ->
                            { english = "Weight (kg)" }

                        ZScoreChartsAvailableAt ->
                            { english = "Z-score charts available at" }

                CheckIn ->
                    { english = "Check in:" }

                ChildNutritionSignLabel sign ->
                    case sign of
                        AbdominalDistention ->
                            { english = "Abdominal Distention" }

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

                ChildOf ->
                    { english = "Child of" }

                ClickTheCheckMark ->
                    { english = "Click the check mark if the mother is in attendance. The check mark will appear green when a mother has been signed in." }

                ClinicNotFound ->
                    { english = "Clinic not found" }

                Clinics ->
                    { english = "Clinics" }

                Connected ->
                    { english = "Connected" }

                Continue ->
                    { english = "Continue" }

                Dashboard ->
                    { english = "Dashboard" }

                DataIsNowSaved ->
                    { english = "Data is now saved on the server." }

                DateOfLastAssessment ->
                    { english = "Date of last Assessment" }

                DownloadHealthAssessment ->
                    { english = "Download Health Assessment" }

                DownloadSuccessful ->
                    { english = "Download Successful" }

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

                ErrorFetchingCachedSession ->
                    { english = "There was an error fetchhing the session stored on this device." }

                ErrorNetworkError ->
                    { english = "A network error occurred contacting the server. Are you connected to the Internet?" }

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

                GoHome ->
                    { english = "Go to main page" }

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

                        LoginOrWorkOffline ->
                            { english = "Either login below, or work offline without logging in." }

                        Logout ->
                            { english = "Logout" }

                        LogoutInProgress ->
                            { english = "Logout in progress ..." }

                        LogoutFailed ->
                            { english = "Logout Failed" }

                        Password ->
                            { english = "Password" }

                        SignIn ->
                            { english = "Sign In" }

                        Username ->
                            { english = "Username" }

                        WorkOffline ->
                            { english = "Work Offline" }

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

                MonthsOld ->
                    { english = "months old" }

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

                NoActivitiesCompleted ->
                    { english = "No activities are entirely completed for the attending participants." }

                NoActivitiesPending ->
                    { english = "All activities are completed for the attending participants." }

                NoActivitiesCompletedForThisParticipant ->
                    { english = "No activities are completed for this participant." }

                NoActivitiesPendingForThisParticipant ->
                    { english = "All activities are completed for this participant." }

                NoParticipantsCompleted ->
                    { english = "No participants have completed all their activities yet." }

                NoParticipantsPending ->
                    { english = "All attending participants have completed their activities." }

                NoParticipantsCompletedForThisActivity ->
                    { english = "No participants have completed this activity yet." }

                NoParticipantsPendingForThisActivity ->
                    { english = "All attending participants have completed this activitity." }

                NoCachedSession ->
                    { english = "No session was found on this device." }

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

                OnceYouEndYourSession ->
                    { english = "Once you end your session, you will no longer be able to edit or add data. Remember to upload this session within the next 48 hours." }

                Page ->
                    { english = "Page" }

                Page404 ->
                    { english = "404 page" }

                PageNotFoundMsg ->
                    { english = "Sorry, nothing found in this URL." }

                Participants ->
                    { english = "Participants" }

                ParticipantSummary ->
                    { english = "Participant Summary" }

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

                ReadyToBeginSession ->
                    { english = "You are now ready to begin your session." }

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
                    { english = "Save Error" }

                SearchByName ->
                    { english = "Search by Name" }

                SelectYourClinic ->
                    { english = "Select your clinic" }

                SessionClosed ->
                    { english = "Session closed" }

                SessionClosed2 ->
                    { english =
                        """You have stored data on the device, but it was not uploaded to the server and the session is closed.
                        Please contact the Ihangane project for further instructions."""
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

                TitleHealthAssessment ->
                    { english = "2017 July Health Assessment" }

                UnableToDownload ->
                    { english = "Unable to Download" }

                UnableToUpload ->
                    { english = "Unable to Upload" }

                Update ->
                    { english = "Update" }

                UpdateError ->
                    { english = "Update Error" }

                UploadHealthAssessment ->
                    { english = "Upload Health Assessment" }

                UploadingSession1 ->
                    { english = "Uploading…" }

                UploadingSession2 ->
                    { english = "Uploading may take a few minutes, or a few hours. Do not leave this page while data is uploading." }

                UploadSuccessful ->
                    { english = "Upload Successful" }

                ViewProgressReport ->
                    { english = "View Progress Report" }

                WelcomeUser name ->
                    { english = "Welcome " ++ name }

                YouHaveACompletedSession ->
                    { english = "You have a completed session that needs to be uploaded. Please connect to the internet and upload this session within 48 hours." }

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
