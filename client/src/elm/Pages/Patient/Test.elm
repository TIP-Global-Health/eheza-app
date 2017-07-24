module Pages.Patient.Test exposing (all)

import Activity.Model exposing (ActivityType(..), ChildActivityType(..))
import Fixtures exposing (exampleAccessToken, exampleBackendUrl, exampleChild, exampleUser)
import Measurement.Model exposing (..)
import Measurement.View exposing (..)
import Test exposing (Test, describe, test)
import Test.Html.Query as Query
import Test.Html.Selector as Selector exposing (class, classes, id, tag, text)
import Translate exposing (..)


viewPatientTest : Test
viewPatientTest =
    describe "The pateint page" <|
        [ test "Then a height form should be displayed when selected" <|
            \() ->
                viewChild exampleBackendUrl exampleAccessToken exampleUser English ( 5, exampleChild ) (Just <| Child Height) emptyModel
                    |> Query.fromHtml
                    |> Query.find [ Selector.class "height" ]
                    |> Query.find [ tag "h3" ]
                    |> Query.has [ text "Height:" ]
        ]


all : Test
all =
    describe "pateint pages : form tests"
        [ viewPatientTest
        ]
