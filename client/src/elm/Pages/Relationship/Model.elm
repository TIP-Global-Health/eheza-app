module Pages.Relationship.Model exposing (Model, Msg(..), emptyModel)

import Backend.Entities exposing (..)
import Backend.Relationship.Model exposing (..)
import Pages.Page exposing (Page)


type alias Model =
    { relatedBy : Maybe MyRelatedBy
    , assignToGroup : Maybe ClinicId
    }


emptyModel : Model
emptyModel =
    { relatedBy = Nothing
    , assignToGroup = Nothing
    }


type Msg
    = AssignToClinicId String
    | RelationshipSelected MyRelatedBy
    | Save (Maybe MyRelatedBy) (Maybe ClinicId)
    | Reset
    | SetActivePage Page
