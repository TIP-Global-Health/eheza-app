module Backend.Mother.Encoder exposing (..)

import Backend.Mother.Model exposing (..)
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
    , ( "ubudehe", maybe string mother.ubudehe )
    , ( "education_level", maybe encodeEducationLevel mother.educationLevel )
    ]


encodeEducationLevel : EducationLevel -> Value
encodeEducationLevel educationLevel =
    case educationLevel of
        NoSchooling ->
            string "0"

        PrimarySchool ->
            string "1"

        VocationalTrainingSchool ->
            string "2"

        SecondarySchool ->
            string "3"

        DiplomaProgram ->
            string "4"

        HigherEducation ->
            string "5"

        AdvancedDiploma ->
            string "6"
