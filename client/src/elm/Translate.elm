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
    | Dashboard
    | LinkToMother
    | Login
    | Logout
    | MyAccount
    | NoActiveIncidents
    | NoChildrenRegisteredInTheSystem
    | Page404
    | PageNotFoundMsg
    | Patients
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

                Dashboard ->
                    { english = "Dashboard" }

                LinkToMother ->
                    { english = "Link to mother" }

                Login ->
                    { english = "Login" }

                Logout ->
                    { english = "Logout" }

                MyAccount ->
                    { english = "My Account" }

                NoActiveIncidents ->
                    { english = "No active incidents" }

                NoChildrenRegisteredInTheSystem ->
                    { english = "No children registered in the system" }

                Page404 ->
                    { english = "404 page" }

                PageNotFoundMsg ->
                    { english = "Sorry, nothing found in this URL." }

                Patients ->
                    { english = "Patients" }

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
