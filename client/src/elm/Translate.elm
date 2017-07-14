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
    | ActivitiesHeightHelp
    | ActivitiesHeightLabel
    | ActivitiesHeightTitle
    | ActivitiesMuacHelp
    | ActivitiesMuacLabel
    | ActivitiesMuacTitle
    | ActivitiesToComplete
    | ActivitiesWeightHelp
    | ActivitiesWeightLabel
    | ActivitiesWeightTitle
    | CentimeterShorthand
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

                ActivitiesWeightHelp ->
                    { english = "Calibrate the scale before taking the first baby's weight.  Place baby in harness with no clothes on." }

                ActivitiesToComplete ->
                    { english = "ActivitiesToComplete" }

                ActivitiesWeightLabel ->
                    { english = "Weight:" }

                ActivitiesWeightTitle ->
                    { english = "Weight:" }

                CentimeterShorthand ->
                    { english = "cm" }

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
