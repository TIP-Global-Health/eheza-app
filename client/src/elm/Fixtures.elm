module Fixtures exposing (..)

import Backend.Child.Model exposing (Child, Gender(..))
import Backend.Mother.Model exposing (EducationLevel(..), Mother)
import EverySet
import Time.Date exposing (date)
import User.Model exposing (User)


{-| } An example access token.
-}
exampleAccessToken : String
exampleAccessToken =
    "some-access-token"


{-| } An example backend URL.
-}
exampleBackendUrl : String
exampleBackendUrl =
    "https://example.com"


{-| } An example user.
-}
exampleUser : User
exampleUser =
    { id = 35
    , name = "aya"
    , avatarUrl = "http://example.com/avatar.jpg"
    , clinics = []
    , roles = EverySet.empty
    }


{-| An example child.
-}
exampleChildA : Child
exampleChildA =
    { name = "Michelle Kelly"
    , avatarUrl = Just "http://lorempixel.com/output/people-q-c-640-480-8.jpg"
    , motherId = Nothing
    , siblingId = Nothing
    , birthDate = date 2016 8 28
    , gender = Male
    }


{-| Another example child.
-}
exampleChildB : Child
exampleChildB =
    { name = "Habimana Hakizimana"
    , avatarUrl = Just "http://lorempixel.com/output/people-q-c-640-480-8.jpg"
    , motherId = Nothing
    , siblingId = Nothing
    , birthDate = date 2016 11 17
    , gender = Female
    }


{-| An example mother.
-}
exampleMother : Mother
exampleMother =
    { name = "Sebabive Gahiji"
    , avatarUrl = Just "http://lorempixel.com/output/people-q-c-640-480-8.jpg"
    , children = []
    , birthDate = date 2016 8 28
    , ubudehe = Just "1"
    , educationLevel = NoSchooling
    }
