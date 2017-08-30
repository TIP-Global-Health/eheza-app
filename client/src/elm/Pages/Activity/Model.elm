module Pages.Activity.Model exposing (..)

import Activity.Model exposing (ActivityType(..), ChildActivityType(..))
import App.PageType exposing (Page)


type alias Model =
    { selectedActivity : ActivityType
    , selectedTab : Tab
    }


type Msg
    = SetRedirectPage Page
    | SetSelectedTab Tab


type Tab
    = Completed
    | Pending


emptyModel : Model
emptyModel =
    { selectedActivity = Child Height
    , selectedTab = Pending
    }
