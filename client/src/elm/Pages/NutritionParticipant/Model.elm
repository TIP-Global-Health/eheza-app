module Pages.NutritionParticipant.Model exposing (Model, Msg(..), emptyModel)

import Backend.Entities exposing (..)
import Date exposing (Date)
import Pages.Page exposing (Page)


type alias Model =
    {}


type Msg
    = SetActivePage Page


emptyModel : Model
emptyModel =
    {}
