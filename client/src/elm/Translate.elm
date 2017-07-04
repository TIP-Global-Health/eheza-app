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
    | Login
    | Logout
    | MyAccount
    | Page404
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

                Login ->
                    { english = "Login" }

                Logout ->
                    { english = "Logout" }

                MyAccount ->
                    { english = "My Account" }

                Page404 ->
                    { english = "404 page" }

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
