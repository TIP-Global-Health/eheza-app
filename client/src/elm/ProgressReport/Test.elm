module ProgressReport.Test exposing (all)

import Date
import Fixtures exposing (exampleAccessToken, exampleBackendUrl, exampleChild, exampleUser)
import Html exposing (div)
import Html.Attributes as Attr
import Pages.Participant.Model exposing (Tab(..), emptyModel)
import Pages.Participant.View exposing (viewChild)
import RemoteData exposing (RemoteData(NotAsked))
import Test exposing (Test, describe, test)
import Test.Html.Query as Query
import Test.Html.Selector as Selector exposing (class, classes, id, tag, text, className)
import Translate exposing (..)


viewProgressReportTest : Test
viewProgressReportTest =
    let
        viewChildWithTab selectedTab =
            div []
                (viewChild
                    exampleBackendUrl
                    exampleAccessToken
                    exampleUser
                    English
                    (Date.fromTime 1504185446000)
                    NotAsked
                    ( 5, exampleChild )
                    { emptyModel | selectedTab = selectedTab }
                )
    in
        describe "A nurse visits the assesment of a Child at the Todo tab" <|
            [ test "Then a Progress Report tab is present, but not active, if Completes is the selected one" <|
                \() ->
                    viewChildWithTab Completed
                        |> Query.fromHtml
                        |> Query.find [ id "progressreport-tab" ]
                        |> Query.hasNot [ class "active" ]
            , test "Then a Progress Report tab is present, but active, if this is the selected one" <|
                \() ->
                    viewChildWithTab ProgressReport
                        |> Query.fromHtml
                        |> Query.find [ id "progressreport-tab" ]
                        |> Query.has [ class "active" ]
            ]


all : Test
all =
    describe "Progress Report of the Children"
        [ viewProgressReportTest
        ]
