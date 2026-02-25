module Pages.FamilyNutrition.ProgressReport.Model exposing (..)

import Backend.Entities exposing (..)
import Pages.FamilyNutrition.Encounter.Model exposing (FamilyMember(..))
import Pages.Page exposing (Page)


type alias Model =
    { selectedFamilyMember : FamilyMember
    }


emptyModel : Model
emptyModel =
    { selectedFamilyMember = FamilyMemberMother
    }


type Msg
    = SetActivePage Page
    | SetSelectedFamilyMember FamilyMember
