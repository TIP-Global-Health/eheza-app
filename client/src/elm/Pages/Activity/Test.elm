module Pages.Activity.Test exposing (all)

import Activity.Model exposing (ChildActivity(..), CompletedAndPending)
import AssocList as Dict exposing (Dict)
import Backend.Entities exposing (PersonId)
import Backend.Person.Model exposing (Person)
import Expect
import Pages.Activity.Model exposing (Model, Tab(..), emptyModel)
import Pages.Activity.Utils exposing (resolveChildToLoad)
import Restful.Endpoint exposing (toEntityUuid)
import Test exposing (Test, describe, test)
import TestFixtures exposing (testPerson)


{-| Testing auto form switch functionality for activity screen.

Can be extended meaningfully with more cases when we don't
rely on the timestamps anymore for the completed/not completed status.

-}
nextParticipantTest : Test
nextParticipantTest =
    test "redo nextParticipantTest" <|
        always Expect.pass



-- FIXTURES
--
-- The activity page lists children in two tabs, Pending and Completed. When
-- nobody is tapped (or the tapped child is not in the current tab), the page
-- shows the first child of the current tab, after the name filter. These
-- tests check that `resolveChildToLoad` names that same child, so the group
-- session fetch loads their individual Child Nutrition history, which the
-- malnutrition assessment on save (Nutrition) and the questionnaire (Child
-- Scorecard) read.


alice : PersonId
alice =
    toEntityUuid "alice"


bob : PersonId
bob =
    toEntityUuid "bob"


claire : PersonId
claire =
    toEntityUuid "claire"


diane : PersonId
diane =
    toEntityUuid "diane"


eric : PersonId
eric =
    toEntityUuid "eric"


childNamed : String -> Person
childNamed name =
    { testPerson | name = name }


{-| Pending: Alice, Bob Habimana, Claire Habimana (in that order).
Completed: Diane, Eric Habimana. The name filter "habimana" keeps Bob and
Claire on Pending, so the first child of the filtered Pending list (Bob)
differs from the first child of the unfiltered one (Alice).
-}
lists : CompletedAndPending (Dict PersonId Person)
lists =
    { pending =
        Dict.fromList
            [ ( alice, childNamed "Alice Uwase" )
            , ( bob, childNamed "Bob Habimana" )
            , ( claire, childNamed "Claire Habimana" )
            ]
    , completed =
        Dict.fromList
            [ ( diane, childNamed "Diane Mukamana" )
            , ( eric, childNamed "Eric Habimana" )
            ]
    }


{-| The same session after Alice's save on the activity: Alice has moved from
Pending to Completed.
-}
listsAfterAliceSaved : CompletedAndPending (Dict PersonId Person)
listsAfterAliceSaved =
    { pending =
        Dict.fromList
            [ ( bob, childNamed "Bob Habimana" )
            , ( claire, childNamed "Claire Habimana" )
            ]
    , completed = Dict.insert alice (childNamed "Alice Uwase") lists.completed
    }


{-| The same session after Bob's save: Bob, from the middle of Pending, has
moved to Completed.
-}
listsAfterBobSaved : CompletedAndPending (Dict PersonId Person)
listsAfterBobSaved =
    { pending =
        Dict.fromList
            [ ( alice, childNamed "Alice Uwase" )
            , ( claire, childNamed "Claire Habimana" )
            ]
    , completed = Dict.insert bob (childNamed "Bob Habimana") lists.completed
    }


emptyLists : CompletedAndPending (Dict PersonId Person)
emptyLists =
    { pending = Dict.empty, completed = Dict.empty }


withTab : Tab -> Model PersonId
withTab tab =
    { emptyModel | selectedTab = tab }


withFilter : String -> Model PersonId
withFilter filter =
    { emptyModel | filter = filter }


tappedOn : Tab -> PersonId -> Model PersonId
tappedOn tab childId =
    { emptyModel | selectedTab = tab, selectedParticipant = Just childId }



-- CRITERIA


{-| Criterion: On the Nutrition activity of a group session, saving for the
child the page shows without a tap assesses that child with their earlier
individual Child Nutrition weights, as it does for a tapped child.

Nobody tapped, Pending tab, no filter: the page shows Alice, so her individual
data must be loaded.

-}
nutritionShownWithoutTap : Test
nutritionShownWithoutTap =
    describe "Nutrition: the child the page shows without a tap"
        [ test "On the Nutrition activity, the child the page shows without a tap has their individual data loaded" <|
            \_ ->
                resolveChildToLoad NutritionSigns emptyModel lists
                    |> Expect.equal (Just alice)

        -- Boundary: a single pending child is the one shown.
        , test "On Nutrition with one pending child, that child is loaded" <|
            \_ ->
                resolveChildToLoad NutritionSigns emptyModel { lists | pending = Dict.fromList [ ( claire, childNamed "Claire Habimana" ) ] }
                    |> Expect.equal (Just claire)

        -- Boundary: nobody pending means the page shows no child on the
        -- Pending tab, so there is no one to load (even though children are
        -- on Completed).
        , test "On Nutrition with no pending child, no one is loaded" <|
            \_ ->
                resolveChildToLoad NutritionSigns emptyModel { lists | pending = Dict.empty }
                    |> Expect.equal Nothing

        -- Boundary: the empty session.
        , test "On Nutrition with no children at all, no one is loaded" <|
            \_ ->
                resolveChildToLoad NutritionSigns emptyModel emptyLists
                    |> Expect.equal Nothing
        ]


{-| Criterion: This holds after the app was reloaded since the child's other
measurements were taken.

After a reload nothing is in memory: the activity page starts from
`emptyModel` and the child's individual history is not loaded unless the
fetch asks for it. Alice already has Weight and MUAC (so she is Completed on
those activities) but is still Pending on Nutrition. Opening Nutrition, the
page shows her by itself, so she must be the child to load.

-}
nutritionAfterReload : Test
nutritionAfterReload =
    let
        -- Alice's Weight was taken before the reload; Bob's was not.
        weightLists =
            { pending = Dict.fromList [ ( bob, childNamed "Bob Habimana" ) ]
            , completed = Dict.fromList [ ( alice, childNamed "Alice Uwase" ) ]
            }

        -- Nutrition was not done for anyone yet.
        nutritionLists =
            { pending =
                Dict.fromList
                    [ ( alice, childNamed "Alice Uwase" )
                    , ( bob, childNamed "Bob Habimana" )
                    ]
            , completed = Dict.empty
            }
    in
    describe "Nutrition after a reload"
        [ test "After a reload since the child's other measurements were taken, the Nutrition page's default child has their individual data loaded" <|
            \_ ->
                resolveChildToLoad NutritionSigns emptyModel nutritionLists
                    |> Expect.equal (Just alice)

        -- Same session, on Weight, where Alice is done: the page shows Bob by
        -- default, and Bob is the one loaded, as today.
        , test "After a reload, the Weight page's default child is the first one still pending there" <|
            \_ ->
                resolveChildToLoad Weight emptyModel weightLists
                    |> Expect.equal (Just bob)
        ]


{-| Criterion: It holds for the next child the page shows by itself after a
save.

Alice was tapped and saved; she moves to Completed, but the page state still
names her as the selected participant. She is no longer on the Pending tab,
so the page shows the first pending child, Bob.

-}
nutritionNextChildAfterSave : Test
nutritionNextChildAfterSave =
    describe "Nutrition: the next child after a save"
        [ test "On Nutrition, the next child the page shows by itself after a save has their individual data loaded" <|
            \_ ->
                resolveChildToLoad NutritionSigns (tappedOn Pending alice) listsAfterAliceSaved
                    |> Expect.equal (Just bob)

        -- Boundary: the saved child was tapped in the middle of the list. The
        -- page then shows the first pending child (Alice), not the one after
        -- the saved child.
        , test "On Nutrition, after saving a child tapped mid-list, the first pending child is loaded" <|
            \_ ->
                resolveChildToLoad NutritionSigns (tappedOn Pending bob) listsAfterBobSaved
                    |> Expect.equal (Just alice)

        -- Boundary: the saved child was the last pending one. The page shows
        -- no child on the Pending tab.
        , test "On Nutrition, after saving the last pending child, no one is loaded" <|
            \_ ->
                resolveChildToLoad NutritionSigns
                    (tappedOn Pending alice)
                    { pending = Dict.empty
                    , completed = Dict.insert alice (childNamed "Alice Uwase") lists.completed
                    }
                    |> Expect.equal Nothing

        -- Boundary: the next child after a save, with a filter still typed.
        -- Bob (filtered) was saved; the first remaining filtered child is
        -- Claire, even though Alice heads the unfiltered list.
        , test "On Nutrition, after a save with a filter typed, the next child is the first of the filtered list" <|
            \_ ->
                resolveChildToLoad NutritionSigns { emptyModel | filter = "habimana", selectedParticipant = Just bob } listsAfterBobSaved
                    |> Expect.equal (Just claire)
        ]


{-| Criterion: With a name filter typed, it holds for the first child of the
filtered list.

`children` arrives unfiltered. With "habimana" typed, the page lists Bob and
Claire and shows Bob.

-}
nutritionWithNameFilter : Test
nutritionWithNameFilter =
    describe "Nutrition with a name filter typed"
        [ test "On Nutrition with a name filter typed, the first child of the filtered list has their individual data loaded" <|
            \_ ->
                resolveChildToLoad NutritionSigns (withFilter "habimana") lists
                    |> Expect.equal (Just bob)

        -- Boundary: the page normalises the filter (lower case, trimmed), so
        -- "  HABIMANA " lists the same children.
        , test "On Nutrition, a filter typed with capitals and spaces selects the same first child" <|
            \_ ->
                resolveChildToLoad NutritionSigns (withFilter "  HABIMANA ") lists
                    |> Expect.equal (Just bob)

        -- Boundary: a filter that matches the middle of a name.
        , test "On Nutrition, a filter matching the middle of a name selects that child" <|
            \_ ->
                resolveChildToLoad NutritionSigns (withFilter "aire") lists
                    |> Expect.equal (Just claire)

        -- Boundary: a filter of spaces only is no filter; the page shows
        -- Alice.
        , test "On Nutrition, a filter of spaces only selects the first unfiltered child" <|
            \_ ->
                resolveChildToLoad NutritionSigns (withFilter "   ") lists
                    |> Expect.equal (Just alice)

        -- Boundary: a filter matching nobody pending (only Diane, on
        -- Completed). The page shows no child on the Pending tab.
        , test "On Nutrition, a filter matching no pending child loads no one" <|
            \_ ->
                resolveChildToLoad NutritionSigns (withFilter "mukamana") lists
                    |> Expect.equal Nothing

        -- Boundary: the filter applies on the Completed tab as well. Diane
        -- heads Completed, but only Eric matches.
        , test "On Nutrition's Completed tab with a filter typed, the first filtered completed child is loaded" <|
            \_ ->
                resolveChildToLoad NutritionSigns { emptyModel | selectedTab = Completed, filter = "habimana" } lists
                    |> Expect.equal (Just eric)
        ]


{-| Criterion: On the Completed tab, it holds for the child shown there by
default.

On the Completed tab, with nobody tapped, the page shows Diane.

-}
nutritionCompletedTab : Test
nutritionCompletedTab =
    describe "Nutrition: the Completed tab"
        [ test "On Nutrition's Completed tab, the child shown there by default has their individual data loaded" <|
            \_ ->
                resolveChildToLoad NutritionSigns (withTab Completed) lists
                    |> Expect.equal (Just diane)

        -- Boundary: a child tapped on the Pending tab, then the tab switched.
        -- The tapped child is not on Completed, so the page shows Diane.
        , test "On Nutrition's Completed tab, a child tapped on Pending is not the one shown" <|
            \_ ->
                resolveChildToLoad NutritionSigns (tappedOn Completed bob) lists
                    |> Expect.equal (Just diane)

        -- Boundary: nobody completed. The page shows no child on Completed,
        -- even though children are pending.
        , test "On Nutrition's Completed tab with no completed child, no one is loaded" <|
            \_ ->
                resolveChildToLoad NutritionSigns (withTab Completed) { lists | completed = Dict.empty }
                    |> Expect.equal Nothing
        ]


{-| Criterion: On the Child Scorecard activity, the child shown without a tap
gets the same questionnaire as the same child tapped.

Child Scorecard is the `NCDA` child activity. The questionnaire depends on
the child's individual history, so the child shown by default must have it
loaded just as a tapped child does.

-}
childScorecardShownWithoutTap : Test
childScorecardShownWithoutTap =
    describe "Child Scorecard: the child the page shows without a tap"
        [ test "On Child Scorecard, the child shown without a tap has their individual data loaded" <|
            \_ ->
                resolveChildToLoad NCDA emptyModel lists
                    |> Expect.equal (Just alice)

        -- Boundary: the next child after a save on Child Scorecard.
        , test "On Child Scorecard, the next child shown by itself after a save has their individual data loaded" <|
            \_ ->
                resolveChildToLoad NCDA (tappedOn Pending alice) listsAfterAliceSaved
                    |> Expect.equal (Just bob)

        -- Boundary: the Completed tab on Child Scorecard.
        , test "On Child Scorecard's Completed tab, the child shown by default has their individual data loaded" <|
            \_ ->
                resolveChildToLoad NCDA (withTab Completed) lists
                    |> Expect.equal (Just diane)

        -- Boundary: a filter typed on Child Scorecard.
        , test "On Child Scorecard with a name filter typed, the first filtered child has their individual data loaded" <|
            \_ ->
                resolveChildToLoad NCDA (withFilter "habimana") lists
                    |> Expect.equal (Just bob)

        -- Boundary: nobody pending on Child Scorecard.
        , test "On Child Scorecard with no pending child, no one is loaded" <|
            \_ ->
                resolveChildToLoad NCDA emptyModel { lists | pending = Dict.empty }
                    |> Expect.equal Nothing
        ]


{-| Criterion: Height, MUAC and Weight keep behaving as today, for tapped
children and for children shown by default.

Shown by default: today the fetch loads the first pending child on these
three activities. That must stay.

Tapped: today a tap loads the tapped child's data by itself (outside this
function) and the fetch keeps loading the first pending child. The criterion
does not say which of the two this function should name for a tapped child,
so the tapped tests accept either, and only forbid naming no one or some
third child.

-}
measurementsKeepBehaviour : Test
measurementsKeepBehaviour =
    let
        testsFor ( activity, name ) =
            [ test ("On " ++ name ++ ", the child shown by default has their individual data loaded, as today") <|
                \_ ->
                    resolveChildToLoad activity emptyModel lists
                        |> Expect.equal (Just alice)
            , test ("On " ++ name ++ ", a tapped child keeps having their individual data loaded, as today") <|
                \_ ->
                    List.member (resolveChildToLoad activity (tappedOn Pending claire) lists) [ Just claire, Just alice ]
                        |> Expect.equal True
                        |> Expect.onFail "expected the tapped child (claire) or the first pending child (alice)"
            , test ("On " ++ name ++ " with no pending child, no one is loaded, as today") <|
                \_ ->
                    resolveChildToLoad activity emptyModel { lists | pending = Dict.empty }
                        |> Expect.equal Nothing
            ]
    in
    describe "Height, MUAC and Weight keep behaving as today"
        (List.concatMap testsFor
            [ ( Height, "Height" )
            , ( Muac, "MUAC" )
            , ( Weight, "Weight" )
            ]
        )


{-| Criterion: It holds for a CHW's group session as well as a nurse's.

`resolveChildToLoad` takes no nurse/CHW input: whether the user is a CHW
only changes which children the session lists for an activity, and that is
computed before this function sees `children`. For Nutrition the lists are
the same for both. These tests run Nutrition on a CHW session's lists (CHWs
do not get Child Scorecard) and expect the same answers as for a nurse.

-}
chwSession : Test
chwSession =
    let
        -- A CHW group session: Alice is done on Nutrition, Bob is pending.
        chwNutritionLists =
            { pending = Dict.fromList [ ( bob, childNamed "Bob Habimana" ) ]
            , completed = Dict.fromList [ ( alice, childNamed "Alice Uwase" ) ]
            }
    in
    describe "CHW group session"
        [ test "In a CHW's group session, the Nutrition page's default child has their individual data loaded" <|
            \_ ->
                resolveChildToLoad NutritionSigns emptyModel chwNutritionLists
                    |> Expect.equal (Just bob)

        -- The next child after Alice's save, in a CHW's session.
        , test "In a CHW's group session, the Nutrition page's next child after a save has their individual data loaded" <|
            \_ ->
                resolveChildToLoad NutritionSigns (tappedOn Pending alice) chwNutritionLists
                    |> Expect.equal (Just bob)
        ]


all : Test
all =
    describe "Activity page"
        [ describe "Next participant flow"
            [ nextParticipantTest
            ]
        , describe "resolveChildToLoad"
            [ nutritionShownWithoutTap
            , nutritionAfterReload
            , nutritionNextChildAfterSave
            , nutritionWithNameFilter
            , nutritionCompletedTab
            , childScorecardShownWithoutTap
            , measurementsKeepBehaviour
            , chwSession
            ]
        ]
