module Pages.Activity.Model exposing (..)

import Activity.Model exposing (ActivityType(..), ChildActivityType(..))
import App.PageType exposing (Page)


type alias Model =
    { selectedActivity : ActivityType
    }


type Msg
    = SetRedirectPage Page


emptyModel : Model
emptyModel =
    { selectedActivity = Child Height
    }
