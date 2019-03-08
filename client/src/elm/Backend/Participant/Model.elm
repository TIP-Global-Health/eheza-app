module Backend.Participant.Model exposing (Gender(..), Ubudehe(..), allUbudehes)

{-| This module contains common types for Backend.Child and Backend.Mother modules.
-}


type Gender
    = Female
    | Male


type Ubudehe
    = Ubudehe1
    | Ubudehe2
    | Ubudehe3
    | Ubudehe4


allUbudehes : List Ubudehe
allUbudehes =
    [ Ubudehe1
    , Ubudehe2
    , Ubudehe3
    , Ubudehe4
    ]
