module Translate exposing (..)


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
    | ActivitiesFamilyPlanningSignsCondomsLabel
    | ActivitiesFamilyPlanningSignsHelp
    | ActivitiesFamilyPlanningSignsIUDLabel
    | ActivitiesFamilyPlanningSignsInjectionLabel
    | ActivitiesFamilyPlanningSignsLabel
    | ActivitiesFamilyPlanningSignsNecklaceLabel
    | ActivitiesFamilyPlanningSignsNoneLabel
    | ActivitiesFamilyPlanningSignsTitle
    | ActivitiesFamilyPlanningSignsPillLabel
    | ActivitiesHeightHelp
    | ActivitiesHeightLabel
    | ActivitiesHeightTitle
    | ActivitiesMuacHelp
    | ActivitiesMuacLabel
    | ActivitiesMuacTitle
    | ActivitiesToComplete Int
    | ActivitiesNutritionSignsAbdominalDisortionLabel
    | ActivitiesNutritionSignsApathyLabel
    | ActivitiesNutritionSignsBrittleHairLabel
    | ActivitiesNutritionSignsDrySkinLabel
    | ActivitiesNutritionSignsEdemaLabel
    | ActivitiesNutritionSignsHelp
    | ActivitiesNutritionSignsLabel
    | ActivitiesNutritionSignsNoneLabel
    | ActivitiesNutritionSignsPoorAppetiteLabel
    | ActivitiesNutritionSignsTitle
    | ActivitiesPhotoHelp
    | ActivitiesPhotoTitle
    | ActivitiesWeightHelp
    | ActivitiesWeightLabel
    | ActivitiesWeightTitle
    | Age Int Int
    | AgeDays Int
    | AgeSingleMonth Int Int
    | AgeSingleDay Int Int
    | Baby
    | BabyName String
    | CentimeterShorthand
    | Children
    | Connected
    | Dashboard
    | DropzoneDefaultMessage
    | ErrorBadUrl
    | ErrorBadPayload
    | ErrorBadStatus
    | ErrorCheckLocalConfig
    | ErrorConfigurationError
    | ErrorNetworkError
    | ErrorTimeout
    | KilogramShorthand
    | LinkToMother
    | Login
    | Logout
    | MeasurementNoChange
    | MeasurementGained Float
    | MeasurementLost Float
    | Mother
    | MotherName String
    | Mothers
    | MyAccount
    | NoActiveIncidents
    | NoChildrenRegisteredInTheSystem
    | NoParticipantsFound
    | NotConnected
    | Page404
    | PageNotFoundMsg
    | Password
    | Participants
    | PlaceholderTextGroupDate
    | PlaceholderTextJoined
    | PreviousFloatMeasurement Float
    | ReportRemaining Int
    | ReloadParticipant
    | Retake
    | Retry
    | Save
    | SaveError
    | SearchByName
    | TitleHealthAssessment
    | Username
    | WelcomeUser String


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

                ActivitiesFamilyPlanningSignsCondomsLabel ->
                    { english = "Condoms" }

                ActivitiesFamilyPlanningSignsHelp ->
                    { english = "Every mother should be asked about her family planing method(s) each month. If a mother needs family planning, refer her to a clinic." }

                ActivitiesFamilyPlanningSignsIUDLabel ->
                    { english = "IUD" }

                ActivitiesFamilyPlanningSignsInjectionLabel ->
                    { english = "Injection" }

                ActivitiesFamilyPlanningSignsLabel ->
                    { english = "Which, if any, of the following methods do you use?" }

                ActivitiesFamilyPlanningSignsNecklaceLabel ->
                    { english = "Necklace" }

                ActivitiesFamilyPlanningSignsNoneLabel ->
                    { english = "None of these" }

                ActivitiesFamilyPlanningSignsTitle ->
                    { english = "Family Planning:" }

                ActivitiesFamilyPlanningSignsPillLabel ->
                    { english = "Pill" }

                ActivitiesHeightHelp ->
                    { english = "Ask the mother to hold the baby’s head at the end of the measuring board. Move the slider to the baby’s heel and pull their leg straight." }

                ActivitiesHeightLabel ->
                    { english = "Height:" }

                ActivitiesHeightTitle ->
                    { english = "Height:" }

                ActivitiesMuacHelp ->
                    { english = "Make sure to measure at the center of the baby’s upper arm." }

                ActivitiesMuacLabel ->
                    { english = "MUAC:" }

                ActivitiesMuacTitle ->
                    { english = "Mid Upper Arm Circumference (MUAC):" }

                ActivitiesNutritionSignsAbdominalDisortionLabel ->
                    { english = "Abdominal Disortion" }

                ActivitiesNutritionSignsApathyLabel ->
                    { english = "Apathy" }

                ActivitiesNutritionSignsBrittleHairLabel ->
                    { english = "Brittle Hair" }

                ActivitiesNutritionSignsDrySkinLabel ->
                    { english = "Dry Skin" }

                ActivitiesNutritionSignsEdemaLabel ->
                    { english = "Edema" }

                ActivitiesNutritionSignsHelp ->
                    { english = "Explain to the mother how to check the malnutrition signs for their own child." }

                ActivitiesNutritionSignsLabel ->
                    { english = "Select all signs that are present:" }

                ActivitiesNutritionSignsNoneLabel ->
                    { english = "None of these" }

                ActivitiesNutritionSignsPoorAppetiteLabel ->
                    { english = "Poor Appetite" }

                ActivitiesNutritionSignsTitle ->
                    { english = "Nutrition:" }

                ActivitiesPhotoHelp ->
                    { english = "Take each baby's picture at each health assesment. Then you and the mother will see the how the baby has grown!" }

                ActivitiesPhotoTitle ->
                    { english = "Photo:" }

                ActivitiesWeightHelp ->
                    { english = "Calibrate the scale before taking the first baby's weight.  Place baby in harness with no clothes on." }

                ActivitiesToComplete count ->
                    { english = "To Do (" ++ toString count ++ ")" }

                ActivitiesWeightLabel ->
                    { english = "Weight:" }

                ActivitiesWeightTitle ->
                    { english = "Weight:" }

                Age months days ->
                    { english = toString months ++ " months and " ++ toString days ++ " days" }

                AgeDays days ->
                    { english = toString days ++ " days" }

                AgeSingleMonth months days ->
                    { english = toString months ++ " month and " ++ toString days ++ " days" }

                AgeSingleDay months days ->
                    { english = toString months ++ " months and " ++ toString days ++ " day" }

                Baby ->
                    { english = "Baby" }

                BabyName name ->
                    { english = "Baby: " ++ name }

                CentimeterShorthand ->
                    { english = "cm" }

                Children ->
                    { english = "Children" }

                Connected ->
                    { english = "Connected" }

                Dashboard ->
                    { english = "Dashboard" }

                DropzoneDefaultMessage ->
                    { english = "Touch here to take a photo, or drop a photo file here." }

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

                KilogramShorthand ->
                    { english = "kg" }

                LinkToMother ->
                    { english = "Link to mother" }

                Login ->
                    { english = "Login" }

                Logout ->
                    { english = "Logout" }

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

                MyAccount ->
                    { english = "My Account" }

                NoActiveIncidents ->
                    { english = "No active incidents!" }

                NoChildrenRegisteredInTheSystem ->
                    { english = "No children registered in the system" }

                NoParticipantsFound ->
                    { english = "No participants found" }

                NotConnected ->
                    { english = "Not Connected" }

                Page404 ->
                    { english = "404 page" }

                PageNotFoundMsg ->
                    { english = "Sorry, nothing found in this URL." }

                Password ->
                    { english = "Password" }

                Participants ->
                    { english = "Participants" }

                PlaceholderTextGroupDate ->
                    { english = "Group Date" }

                PlaceholderTextJoined ->
                    { english = "Joined in June 2017" }

                PreviousFloatMeasurement value ->
                    { english = "Previous measurement: " ++ (toString value) }

                ReportRemaining remaining ->
                    { english = toString remaining ++ " remaning" }

                ReloadParticipant ->
                    { english = "Re-load Participant" }

                Retake ->
                    { english = "Retake" }

                Retry ->
                    { english = "Retry" }

                Save ->
                    { english = "Save" }

                SaveError ->
                    { english = "Save error" }

                SearchByName ->
                    { english = "Search by Name" }

                TitleHealthAssessment ->
                    { english = "2017 July Health Assessment" }

                Username ->
                    { english = "Username" }

                WelcomeUser name ->
                    { english = "Welcome " ++ name }
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
