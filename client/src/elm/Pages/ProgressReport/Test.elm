module Pages.ProgressReport.Test exposing (all)

import Expect
import Fixtures exposing (exampleAccessToken, exampleBackendUrl, exampleChildB, exampleMother, exampleUser)
import Html exposing (div)
import Html.Attributes as Attr
import Pages.Participant.Model exposing (Tab(..), emptyModel)
import Pages.Participant.View exposing (viewChild, viewMother)
import RemoteData exposing (RemoteData(NotAsked))
import Restful.Endpoint exposing (toEntityUuid)
import Test exposing (Test, describe, test)
import Test.Html.Query as Query
import Test.Html.Selector as Selector exposing (class, classes, id, tag, text)
import Translate exposing (..)


viewChildProgressReportTest : Test
viewChildProgressReportTest =
    test "redo viewChildProgressReportTest" <|
        always Expect.pass



{-
   let
       viewChildWithTab selectedTab =
           div []
               (viewChild
                   English
                   (Date.fromTime 1504185446000)
                   NotAsked
                   ( toEntityUuid 5, exampleChildB )
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
-}


viewMotherProgressReportTest : Test
viewMotherProgressReportTest =
    test "redo viewMotherProgressReportTest" <|
        always Expect.pass



{-
   let
       viewMotherWithTab selectedTab =
           div []
               (viewMother
                   English
                   (toEntityUuid 5)
                   exampleMother
                   []
                   { emptyModel | selectedTab = selectedTab }
               )
   in
       describe "A nurse visits the assesment of a Mother at the Todo tab" <|
           [ test "Then a Progress Report tab must not be here" <|
               \() ->
                   viewMotherWithTab Completed
                       |> Query.fromHtml
                       |> Query.findAll [ id "progressreport-tab" ]
                       |> Query.count (Expect.equal 0)
           ]
-}


all : Test
all =
    describe "Progress Report of the Children"
        [ viewChildProgressReportTest
        , viewMotherProgressReportTest
        ]
