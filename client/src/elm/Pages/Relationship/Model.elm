module Pages.Relationship.Model exposing (Model, Msg(..), emptyModel)

import Backend.Relationship.Model exposing (..)
import Pages.Page exposing (Page)


type alias Model =
    Maybe MyRelationship


emptyModel : Model
emptyModel =
    Nothing


type Msg
    = SetActivePage Page
