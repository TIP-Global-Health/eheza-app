module Fixtures exposing (exampleAccessToken, exampleBackendUrl, exampleChildA, exampleChildB, exampleMother, exampleUser)

import Backend.Measurement.Model exposing (Gender(..))
import Backend.Person.Model exposing (EducationLevel(..), Person, Ubudehe(..))
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
exampleChildA : Person
exampleChildA =
    { name = "Michelle Kelly"
    , firstName = "Michelle"
    , secondName = "Kelly"
    , nationalIdNumber = Nothing
    , hmisNumber = Just "01"
    , avatarUrl = Just "http://lorempixel.com/output/people-q-c-640-480-8.jpg"
    , birthDate = Just <| date 2016 8 28
    , educationLevel = Nothing
    , maritalStatus = Nothing
    , hivStatus = Nothing
    , numberOfChildren = Nothing
    , modeOfDelivery = Nothing
    , isDateOfBirthEstimated = False
    , gender = Male
    , ubudehe = Nothing
    , province = Nothing
    , district = Nothing
    , sector = Nothing
    , cell = Nothing
    , village = Nothing
    , telephoneNumber = Nothing
    , healthCenterId = Nothing
    }


{-| Another example child.
-}
exampleChildB : Person
exampleChildB =
    { name = "Habimana Hakizimana"
    , firstName = "Habimana"
    , secondName = "Hakizimana"
    , nationalIdNumber = Nothing
    , hmisNumber = Just "02"
    , avatarUrl = Just "http://lorempixel.com/output/people-q-c-640-480-8.jpg"
    , birthDate = Just <| date 2016 11 17
    , isDateOfBirthEstimated = True
    , educationLevel = Nothing
    , maritalStatus = Nothing
    , hivStatus = Nothing
    , numberOfChildren = Nothing
    , modeOfDelivery = Nothing
    , gender = Female
    , ubudehe = Nothing
    , province = Nothing
    , district = Nothing
    , sector = Nothing
    , cell = Nothing
    , village = Nothing
    , telephoneNumber = Nothing
    , healthCenterId = Nothing
    }


{-| An example mother.
-}
exampleMother : Person
exampleMother =
    { name = "Sebabive Gahiji"
    , firstName = "Sebabive"
    , secondName = "Gahiji"
    , nationalIdNumber = Just "192324232"
    , hmisNumber = Nothing
    , avatarUrl = Just "http://lorempixel.com/output/people-q-c-640-480-8.jpg"
    , birthDate = Just <| date 2016 8 28
    , isDateOfBirthEstimated = False
    , gender = Female
    , hivStatus = Nothing
    , numberOfChildren = Nothing
    , modeOfDelivery = Nothing
    , ubudehe = Just Ubudehe1
    , educationLevel = Just NoSchooling
    , maritalStatus = Nothing
    , province = Nothing
    , district = Nothing
    , sector = Nothing
    , cell = Nothing
    , village = Nothing
    , telephoneNumber = Nothing
    , healthCenterId = Nothing
    }
