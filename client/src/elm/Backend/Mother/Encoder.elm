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
    , ( "date_birth", maybe encodeYYYYMMDD mother.birthDate )
    , ( "relation", encodeChildrenRelation mother.relation )
    ]


encodeChildrenRelation : ChildrenRelationType -> Value
encodeChildrenRelation relation =
    case relation of
        MotherRelation ->
            string "mother"

        CaregiverRelation ->
            string "caregiver"
