module Backend.Session.Test exposing (all)

import Activity.Model exposing (ChildActivity(..), MotherActivity(..))
import AssocList as Dict
import Backend.Entities exposing (HeightId, PersonId)
import Backend.Session.Model exposing (Model, emptyModel)
import Backend.Session.Utils exposing (childActivitySaveRequest, isSaveAllowed, motherActivitySaveRequest)
import Expect
import Http
import RemoteData exposing (RemoteData(..), WebData)
import Restful.Endpoint exposing (toEntityUuid)
import Test exposing (Test, describe, test)



-- SCENARIO DATA


{-| One group session activity: how a test records the state of its latest save
for a person, and how the app reads that state back.
-}
type alias ActivityCase =
    { label : String
    , read : PersonId -> Model -> WebData ()
    , write : PersonId -> WebData () -> Model -> Model
    }


childCase : String -> ChildActivity -> (PersonId -> WebData () -> Model -> Model) -> ActivityCase
childCase label activity write =
    { label = label
    , read = childActivitySaveRequest activity
    , write = write
    }


motherCase : String -> MotherActivity -> (PersonId -> WebData () -> Model -> Model) -> ActivityCase
motherCase label activity write =
    { label = label
    , read = motherActivitySaveRequest activity
    , write = write
    }


childFbf : ActivityCase
childFbf =
    childCase "FBF (child)"
        ChildFbf
        (\person state model -> { model | saveFbfRequest = Dict.insert person state model.saveFbfRequest })


childPicture : ActivityCase
childPicture =
    childCase "Photo"
        ChildPicture
        (\person state model -> { model | savePhotoRequest = Dict.insert person state model.savePhotoRequest })


contributingFactors : ActivityCase
contributingFactors =
    childCase "Contributing Factors"
        ContributingFactors
        (\person state model -> { model | saveContributingFactorsRequest = Dict.insert person state model.saveContributingFactorsRequest })


followUp : ActivityCase
followUp =
    childCase "Follow Up"
        FollowUp
        (\person state model -> { model | saveFollowUpRequest = Dict.insert person state model.saveFollowUpRequest })


healthEducation : ActivityCase
healthEducation =
    childCase "Health Education"
        HealthEducation
        (\person state model -> { model | saveHealthEducationRequest = Dict.insert person state model.saveHealthEducationRequest })


height : ActivityCase
height =
    childCase "Height"
        Height
        (\person state model -> { model | saveHeightRequest = Dict.insert person state model.saveHeightRequest })


muac : ActivityCase
muac =
    childCase "MUAC"
        Muac
        (\person state model -> { model | saveMuacRequest = Dict.insert person state model.saveMuacRequest })


ncda : ActivityCase
ncda =
    childCase "NCDA"
        NCDA
        (\person state model -> { model | saveNCDARequest = Dict.insert person state model.saveNCDARequest })


nutritionSigns : ActivityCase
nutritionSigns =
    childCase "Nutrition"
        NutritionSigns
        (\person state model -> { model | saveNutritionRequest = Dict.insert person state model.saveNutritionRequest })


sendToHC : ActivityCase
sendToHC =
    childCase "Send to HC"
        SendToHC
        (\person state model -> { model | saveSendToHCRequest = Dict.insert person state model.saveSendToHCRequest })


weight : ActivityCase
weight =
    childCase "Weight"
        Weight
        (\person state model -> { model | saveWeightRequest = Dict.insert person state model.saveWeightRequest })


familyPlanning : ActivityCase
familyPlanning =
    motherCase "Family Planning"
        FamilyPlanning
        (\person state model -> { model | saveFamilyPlanningRequest = Dict.insert person state model.saveFamilyPlanningRequest })


lactation : ActivityCase
lactation =
    motherCase "Lactation"
        Lactation
        (\person state model -> { model | saveLactationRequest = Dict.insert person state model.saveLactationRequest })


motherFbf : ActivityCase
motherFbf =
    motherCase "FBF (mother)"
        MotherFbf
        (\person state model -> { model | saveFbfRequest = Dict.insert person state model.saveFbfRequest })


participantConsent : ActivityCase
participantConsent =
    motherCase "Participant Consent"
        ParticipantConsent
        (\person state model -> { model | saveParticipantConsentRequest = Dict.insert person state model.saveParticipantConsentRequest })


{-| Every activity of a child.
-}
childActivities : List ActivityCase
childActivities =
    [ childFbf
    , childPicture
    , contributingFactors
    , followUp
    , healthEducation
    , height
    , muac
    , ncda
    , nutritionSigns
    , sendToHC
    , weight
    ]


{-| Every activity of a mother.
-}
motherActivities : List ActivityCase
motherActivities =
    [ familyPlanning
    , lactation
    , motherFbf
    , participantConsent
    ]


{-| The activities the issue names. FBF exists for both a child and a mother,
and both are covered.
-}
namedActivities : List ActivityCase
namedActivities =
    [ childFbf
    , childPicture
    , familyPlanning
    , height
    , lactation
    , motherFbf
    , muac
    , nutritionSigns
    , weight
    ]


personA : PersonId
personA =
    toEntityUuid "person-a"


personB : PersonId
personB =
    toEntityUuid "person-b"


{-| The device holds no saved record for this person and activity.
-}
noRecord : Maybe HeightId
noRecord =
    Nothing


{-| The device holds the saved record's ID.
-}
savedRecord : Maybe HeightId
savedRecord =
    Just (toEntityUuid "height-1")


failed : WebData ()
failed =
    Failure Http.NetworkError


timedOut : WebData ()
timedOut =
    Failure Http.Timeout


{-| The app's check for a tap on Save, for a person who has no saved record of
this activity on the device yet.
-}
firstSaveAllowed : ActivityCase -> PersonId -> Model -> Bool
firstSaveAllowed activity person model =
    isSaveAllowed noRecord (activity.read person model)



-- ONE TEST PER CRITERION


{-| A second tap arrives in one of two windows: while the first save is still
running, or after it has succeeded but before the device has the saved record's
ID. Both have to be refused, or the second tap stores a second record.
-}
tappingSaveTwiceQuicklyStoresOneRecord : Test
tappingSaveTwiceQuicklyStoresOneRecord =
    describe "Tapping Save twice quickly stores one record"
        (List.map
            (\activity ->
                describe activity.label
                    [ test "the first tap goes ahead" <|
                        \_ ->
                            firstSaveAllowed activity personA emptyModel
                                |> Expect.equal True
                    , test "a second tap while the first save is in progress is refused" <|
                        \_ ->
                            activity.write personA Loading emptyModel
                                |> firstSaveAllowed activity personA
                                |> Expect.equal False
                    , test "a second tap after the save succeeded, before the saved record's ID reaches the device, is refused" <|
                        \_ ->
                            activity.write personA (Success ()) emptyModel
                                |> firstSaveAllowed activity personA
                                |> Expect.equal False
                    ]
            )
            namedActivities
        )


{-| The button reads the request state, so an in-progress save has to be
readable for the activity being saved, whether it creates a record or updates
one.
-}
whileASaveIsInProgress : Test
whileASaveIsInProgress =
    describe "While a save is in progress, the button shows that and ignores further taps"
        [ test "further taps are ignored when the save is creating a record" <|
            \_ ->
                isSaveAllowed noRecord Loading
                    |> Expect.equal False
        , test "further taps are ignored when the save is updating a saved record" <|
            \_ ->
                isSaveAllowed savedRecord Loading
                    |> Expect.equal False
        , describe "the in-progress state is readable for every activity"
            (List.map
                (\activity ->
                    test activity.label <|
                        \_ ->
                            activity.write personA Loading emptyModel
                                |> activity.read personA
                                |> Expect.equal Loading
                )
                (childActivities ++ motherActivities)
            )
        ]


onceSavedTheValueCanBeChanged : Test
onceSavedTheValueCanBeChanged =
    describe "Once saved, the value can be changed, and Save updates that same record"
        [ test "Save goes ahead once the device has the saved record's ID" <|
            \_ ->
                isSaveAllowed savedRecord (Success ())
                    |> Expect.equal True

        -- The request state lives in memory only. After the app is reopened it
        -- is empty while the saved record is still on the device.
        , test "Save goes ahead for a saved record when no save has been made since the app opened" <|
            \_ ->
                isSaveAllowed savedRecord NotAsked
                    |> Expect.equal True
        , test "a second edit goes ahead after the first edit succeeded" <|
            \_ ->
                height.write personA (Success ()) emptyModel
                    |> height.read personA
                    |> isSaveAllowed savedRecord
                    |> Expect.equal True
        ]


{-| Save state is kept per person and per activity. Each scenario below starts
from a save that has just run, including the moment after it succeeded and
before the device has the record's ID, which is when a shared state would block
the next nurse action.
-}
savingForOnePersonNeverBlocks : Test
savingForOnePersonNeverBlocks =
    let
        recentSaves =
            [ ( "in progress", Loading )
            , ( "just succeeded", Success () )
            , ( "just failed", failed )
            ]

        forEachRecentSave scenario =
            List.map (\( label, state ) -> test label <| \_ -> scenario state) recentSaves

        forEachActivityPair activities scenario =
            List.concatMap
                (\saved ->
                    List.filterMap
                        (\next ->
                            if saved.label == next.label then
                                Nothing

                            else
                                Just <|
                                    describe (saved.label ++ " saved, then " ++ next.label)
                                        (forEachRecentSave (scenario saved next))
                        )
                        activities
                )
                activities
    in
    describe "Saving for one person never blocks Save for the next person or the next activity, even immediately afterwards"
        [ describe "the next person, same activity"
            (List.map
                (\activity ->
                    describe activity.label
                        (forEachRecentSave
                            (\state ->
                                activity.write personA state emptyModel
                                    |> firstSaveAllowed activity personB
                                    |> Expect.equal True
                            )
                        )
                )
                (childActivities ++ motherActivities)
            )
        , describe "the next activity of the same child"
            (forEachActivityPair childActivities
                (\saved next state ->
                    saved.write personA state emptyModel
                        |> firstSaveAllowed next personA
                        |> Expect.equal True
                )
            )
        , describe "the next activity of the same mother"
            (forEachActivityPair motherActivities
                (\saved next state ->
                    saved.write personA state emptyModel
                        |> firstSaveAllowed next personA
                        |> Expect.equal True
                )
            )

        -- The child's FBF and the mother's FBF are different activities whose
        -- saves are tracked together, per person. A mother and her child are
        -- different people, so neither may hold the other up.
        , describe "a mother's FBF does not block her child's FBF"
            (forEachRecentSave
                (\state ->
                    motherFbf.write personA state emptyModel
                        |> firstSaveAllowed childFbf personB
                        |> Expect.equal True
                )
            )
        , describe "a child's FBF does not block the mother's FBF"
            (forEachRecentSave
                (\state ->
                    childFbf.write personA state emptyModel
                        |> firstSaveAllowed motherFbf personB
                        |> Expect.equal True
                )
            )
        , test "the first person's save is still refused while the next person's goes ahead" <|
            \_ ->
                let
                    model =
                        height.write personA Loading emptyModel
                in
                ( firstSaveAllowed height personA model, firstSaveAllowed height personB model )
                    |> Expect.equal ( False, True )
        ]


ifASaveFails : Test
ifASaveFails =
    describe "If a save fails, the error shows and Save works again"
        [ test "Save works again after a failed first save" <|
            \_ ->
                isSaveAllowed noRecord failed
                    |> Expect.equal True
        , test "Save works again after a failed update of a saved record" <|
            \_ ->
                isSaveAllowed savedRecord failed
                    |> Expect.equal True
        , test "Save works again whatever the kind of failure" <|
            \_ ->
                isSaveAllowed noRecord timedOut
                    |> Expect.equal True
        , describe "the failure is readable for every activity, so the error can show"
            (List.map
                (\activity ->
                    test activity.label <|
                        \_ ->
                            activity.write personA failed emptyModel
                                |> activity.read personA
                                |> Expect.equal failed
                )
                (childActivities ++ motherActivities)
            )
        , describe "Save works again through the request state of every named activity"
            (List.map
                (\activity ->
                    test activity.label <|
                        \_ ->
                            activity.write personA failed emptyModel
                                |> firstSaveAllowed activity personA
                                |> Expect.equal True
                )
                namedActivities
            )
        ]



-- BOUNDARY CASES


{-| Reads have to come from the activity's own state, for the person asked
about, and report "nothing happened yet" when no save has been made.
-}
readingTheStateOfTheLatestSave : Test
readingTheStateOfTheLatestSave =
    describe "Reading the state of the latest save"
        [ describe "nothing saved yet, on a model with no save state at all"
            (List.map
                (\activity ->
                    test activity.label <|
                        \_ ->
                            activity.read personA emptyModel
                                |> Expect.equal NotAsked
                )
                (childActivities ++ motherActivities)
            )
        , describe "the state belongs to the person it was recorded for"
            (List.map
                (\activity ->
                    test activity.label <|
                        \_ ->
                            activity.write personA (Success ()) emptyModel
                                |> activity.read personB
                                |> Expect.equal NotAsked
                )
                (childActivities ++ motherActivities)
            )
        , describe "every state is returned as recorded"
            (List.map
                (\activity ->
                    describe activity.label
                        [ test "not asked" <|
                            \_ ->
                                activity.write personA NotAsked emptyModel
                                    |> activity.read personA
                                    |> Expect.equal NotAsked
                        , test "succeeded" <|
                            \_ ->
                                activity.write personA (Success ()) emptyModel
                                    |> activity.read personA
                                    |> Expect.equal (Success ())
                        ]
                )
                (childActivities ++ motherActivities)
            )
        ]


isSaveAllowedForEveryCombination : Test
isSaveAllowedForEveryCombination =
    describe "isSaveAllowed for each saved-record and save-state combination"
        [ test "no record, no save yet: first Save goes ahead" <|
            \_ ->
                isSaveAllowed noRecord NotAsked
                    |> Expect.equal True
        , test "no record, save in progress: refused" <|
            \_ ->
                isSaveAllowed noRecord Loading
                    |> Expect.equal False
        , test "no record, save succeeded: refused until the ID arrives" <|
            \_ ->
                isSaveAllowed noRecord (Success ())
                    |> Expect.equal False
        , test "no record, save failed: Save works again" <|
            \_ ->
                isSaveAllowed noRecord failed
                    |> Expect.equal True
        , test "saved record, no save yet: Save goes ahead" <|
            \_ ->
                isSaveAllowed savedRecord NotAsked
                    |> Expect.equal True
        , test "saved record, save in progress: refused" <|
            \_ ->
                isSaveAllowed savedRecord Loading
                    |> Expect.equal False
        , test "saved record, save succeeded: Save goes ahead" <|
            \_ ->
                isSaveAllowed savedRecord (Success ())
                    |> Expect.equal True
        , test "saved record, save failed: Save works again" <|
            \_ ->
                isSaveAllowed savedRecord failed
                    |> Expect.equal True
        ]


all : Test
all =
    describe "Group session Save button"
        [ tappingSaveTwiceQuicklyStoresOneRecord
        , whileASaveIsInProgress
        , onceSavedTheValueCanBeChanged
        , savingForOnePersonNeverBlocks
        , ifASaveFails
        , readingTheStateOfTheLatestSave
        , isSaveAllowedForEveryCombination
        ]
