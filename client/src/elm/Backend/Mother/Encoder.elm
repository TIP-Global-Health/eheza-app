module Backend.Mother.Encoder exposing (encodeEducationLevel, encodeMother)

import Backend.Mother.Model exposing (..)
import Backend.Patient.Encoder exposing (encodeUbudehe)
import Gizra.NominalDate exposing (encodeYYYYMMDD)
import Json.Encode exposing (..)
import Json.Encode.Extra exposing (maybe)
import Restful.Endpoint exposing (encodeEntityId)


encodeMother : Mother -> List ( String, Value )
encodeMother mother =
    [ ( "label", string mother.name )
    , ( "avatar", maybe string mother.avatarUrl )
    , ( "children", list (List.map encodeEntityId mother.children) )
    , ( "date_birth", encodeYYYYMMDD mother.birthDate )
    , ( "ubudehe", maybe encodeUbudehe mother.ubudehe )
    , ( "education_level", maybe encodeEducationLevel mother.educationLevel )
    ]


encodeEducationLevel : EducationLevel -> Value
encodeEducationLevel educationLevel =
    case educationLevel of
        NoSchooling ->
            int 0

        PrimarySchool ->
            int 1

        VocationalTrainingSchool ->
            int 2

        SecondarySchool ->
            int 3

        DiplomaProgram ->
            int 4

        HigherEducation ->
            int 5

        AdvancedDiploma ->
            int 6
