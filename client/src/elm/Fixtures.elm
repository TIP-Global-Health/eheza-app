module Fixtures exposing (..)

import Activity.Model exposing (emptyChildActivityDates)
import Child.Model exposing (Child)
import User.Model exposing (User)


{-| } An example access token.
-}
exampleAccessToken : String
exampleAccessToken =
    "pR3ZFF_NmC$ji5NF8TP32dASM*m0TVGEOL2bLfU_"


{-| } An example backend URL.
-}
exampleBackendUrl : String
exampleBackendUrl =
    "https://example.com/api"


{-| } An example user.
-}
exampleUser : User
exampleUser =
    { id = 35
    , name = "moo"
    , avatarUrl = "http://lorempixel.com/output/people-q-c-640-480-8.jpg"
    }


{-| An example child.
-}
exampleChild : Child
exampleChild =
    { name = "Michelle Kelly"
    , image = "http://lorempixel.com/output/people-q-c-640-480-8.jpg"
    , motherId = Nothing
    , lastExamination = Nothing
    , activityDates = emptyChildActivityDates
    }
