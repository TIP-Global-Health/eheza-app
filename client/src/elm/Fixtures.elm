module Fixtures exposing (..)

import Backend.Mother.Model exposing (Mother)
import Backend.Child.Model exposing (Child, Gender(..))
import Date
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
    }


{-| An example child.
-}
exampleChildA : Child
exampleChildA =
    { name = "Michelle Kelly"
    , image = "http://lorempixel.com/output/people-q-c-640-480-8.jpg"
    , motherId = Nothing
    , siblingId = Nothing
    , examinations = []
    , birthDate = Date.fromTime 1472373589000
    , gender = Male
    }


{-| Another example child.
-}
exampleChildB : Child
exampleChildB =
    { name = "Habimana Hakizimana"
    , image = "http://lorempixel.com/output/people-q-c-640-480-8.jpg"
    , motherId = Nothing
    , siblingId = Nothing
    , examinations = []
    , birthDate = Date.fromTime 1479373589000
    , gender = Female
    }


{-| An example mother.
-}
exampleMother : Mother
exampleMother =
    { name = "Sebabive Gahiji"
    , image = "http://lorempixel.com/output/people-q-c-640-480-8.jpg"
    , children = []
    , examinations = []
    , birthDate = Date.fromTime 1472373589000
    }
