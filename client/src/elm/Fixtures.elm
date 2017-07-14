module Fixtures exposing (..)

import Activity.Model exposing (emptyChildActivityDates)
import Child.Model exposing (Child)
import User.Model exposing (User)


{-| An example user.
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
