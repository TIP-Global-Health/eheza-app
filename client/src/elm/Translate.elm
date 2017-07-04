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
    | Children
    | Dashboard
    | LinkToMother
    | Login
    | Logout
    | Mothers
    | MyAccount
    | NoActiveIncidents
    | NoChildrenRegisteredInTheSystem
    | NoPatientsFound
    | Page404
    | PageNotFoundMsg
    | Patients
    | ReloadPatient
    | Retry
    | SignOut


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

                Children ->
                    { english = "Children" }

                Dashboard ->
                    { english = "Dashboard" }

                LinkToMother ->
                    { english = "Link to mother" }

                Login ->
                    { english = "Login" }

                Logout ->
                    { english = "Logout" }

                Mothers ->
                    { english = "Mothers" }

                MyAccount ->
                    { english = "My Account" }

                NoActiveIncidents ->
                    { english = "No active incidents" }

                NoChildrenRegisteredInTheSystem ->
                    { english = "No children registered in the system" }

                NoPatientsFound ->
                    { english = "No patients found" }

                Page404 ->
                    { english = "404 page" }

                PageNotFoundMsg ->
                    { english = "Sorry, nothing found in this URL." }

                Patients ->
                    { english = "Patients" }

                ReloadPatient ->
                    { english = "Re-load Patient" }

                Retry ->
                    { english = "Retry" }

                SignOut ->
                    { english = "Sign Out" }

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
