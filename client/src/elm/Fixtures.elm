module Fixtures exposing (exampleAccessToken, exampleBackendUrl, exampleChildA, exampleChildB, exampleMother, exampleUser)

import Backend.Child.Model exposing (Child)
import Backend.Mother.Model exposing (ChildrenRelationType(..), Mother)
import Backend.Person.Model exposing (EducationLevel(..), Gender(..), Ubudehe(..))
import EverySet
import Restful.Endpoint exposing (toEntityId)
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
    , firstName = "Michelle"
    , middleName = Nothing
    , secondName = "Kelly"
    , nationalIdNumber = Just "324324232"
    , avatarUrl = Just "http://lorempixel.com/output/people-q-c-640-480-8.jpg"
    , motherId = Nothing
    , birthDate = date 2016 8 28
    , isDateOfBirthEstimated = False
    , gender = Male
    , modeOfDelivery = Nothing
    , ubudehe = Nothing
    , motherName = Nothing
    , motherNationalId = Nothing
    , fatherName = Nothing
    , fatherNationalId = Nothing
    , caregiverName = Nothing
    , caregiverNationalId = Nothing
    , province = Nothing
    , district = Nothing
    , sector = Nothing
    , cell = Nothing
    , village = Nothing
    , telephoneNumber = Nothing
    , healthCenter = Nothing
    }


{-| Another example child.
-}
exampleChildB : Child
exampleChildB =
    { name = "Habimana Hakizimana"
    , firstName = "Habimana"
    , middleName = Nothing
    , secondName = "Hakizimana"
    , nationalIdNumber = Just "232324232"
    , avatarUrl = Just "http://lorempixel.com/output/people-q-c-640-480-8.jpg"
    , motherId = Nothing
    , birthDate = date 2016 11 17
    , isDateOfBirthEstimated = True
    , gender = Female
    , modeOfDelivery = Nothing
    , ubudehe = Nothing
    , motherName = Nothing
    , motherNationalId = Nothing
    , fatherName = Nothing
    , fatherNationalId = Nothing
    , caregiverName = Nothing
    , caregiverNationalId = Nothing
    , province = Nothing
    , district = Nothing
    , sector = Nothing
    , cell = Nothing
    , village = Nothing
    , telephoneNumber = Nothing
    , healthCenter = Nothing
    }


{-| An example mother.
-}
exampleMother : Mother
exampleMother =
    { name = "Sebabive Gahiji"
    , firstName = "Sebabive"
    , middleName = Nothing
    , secondName = "Gahiji"
    , nationalIdNumber = Just "192324232"
    , avatarUrl = Just "http://lorempixel.com/output/people-q-c-640-480-8.jpg"
    , birthDate = Just <| date 2016 8 28
    , isDateOfBirthEstimated = False
    , gender = Female
    , relation = MotherRelation
    , ubudehe = Just Ubudehe1
    , educationLevel = Just NoSchooling
    , profession = Nothing
    , maritalStatus = Nothing
    , hivStatus = Nothing
    , householdSize = Nothing
    , numberOfChildren = Nothing
    , province = Nothing
    , district = Nothing
    , sector = Nothing
    , cell = Nothing
    , village = Nothing
    , telephoneNumber = Nothing
    , clinic = Nothing
    , healthCenter = Nothing
    }
