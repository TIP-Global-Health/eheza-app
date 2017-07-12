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
    | ActivitiesCompleted
    | ActivitiesToComplete
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
    | ActivitiesWeightHelp
    | ActivitiesWeightLabel
    | ActivitiesWeightTitle
    | Children
    | Connected
    | Dashboard
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
    | Mother
    | Mothers
    | MyAccount
    | NoActiveIncidents
    | NoChildrenRegisteredInTheSystem
    | NoPatientsFound
    | NotConnected
    | Page404
    | PageNotFoundMsg
    | Password
    | Patients
    | ReportRemaining Int
    | ReloadPatient
    | Retry
    | Save
    | SearchByName
    | SignOut
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

                ActivitiesCompleted ->
                    { english = "ActivitiesCompleted" }

                ActivitiesToComplete ->
                    { english = "ActivitiesToComplete" }

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

                ActivitiesWeightHelp ->
                    { english = "Calibrate the scale before taking the first baby's weight.  Place baby in harness with no clothes on." }

                ActivitiesWeightLabel ->
                    { english = "Weight:" }

                ActivitiesWeightTitle ->
                    { english = "Weight:" }

                Children ->
                    { english = "Children" }

                Connected ->
                    { english = "Connected" }

                Dashboard ->
                    { english = "Dashboard" }

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

                Mother ->
                    { english = "Mother" }

                Mothers ->
                    { english = "Mothers" }

                MyAccount ->
                    { english = "My Account" }

                NoActiveIncidents ->
                    { english = "No active incidents!" }

                NoChildrenRegisteredInTheSystem ->
                    { english = "No children registered in the system" }

                NoPatientsFound ->
                    { english = "No patients found" }

                NotConnected ->
                    { english = "Not Connected" }

                Page404 ->
                    { english = "404 page" }

                PageNotFoundMsg ->
                    { english = "Sorry, nothing found in this URL." }

                Password ->
                    { english = "Password" }

                Patients ->
                    { english = "Patients" }

                ReportRemaining remaining ->
                    { english = toString remaining ++ " remaning" }

                ReloadPatient ->
                    { english = "Re-load Patient" }

                Retry ->
                    { english = "Retry" }

                Save ->
                    { english = "Save" }

                SearchByName ->
                    { english = "Search by Name" }

                SignOut ->
                    { english = "Sign Out" }

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
