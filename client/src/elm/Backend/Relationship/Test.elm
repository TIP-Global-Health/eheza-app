module Backend.Relationship.Test exposing (all)

import Backend.Relationship.Model exposing (MyRelatedBy(..))
import Backend.Relationship.Utils exposing (isChildRelation)
import Expect
import Test exposing (Test, describe, test)


isChildRelationTest : Test
isChildRelationTest =
    -- Both a person's own child and a child they are a caregiver for count as
    -- children of the family: the Family Nutrition fetch and its assembler both
    -- rely on this, so a MyCaregiven child is fetched and shown, not dropped.
    describe "isChildRelation"
        [ test "MyChild is a child relation" <|
            \_ -> isChildRelation MyChild |> Expect.equal True
        , test "MyCaregiven (cared-for child) is a child relation" <|
            \_ -> isChildRelation MyCaregiven |> Expect.equal True
        , test "MyParent is not a child relation" <|
            \_ -> isChildRelation MyParent |> Expect.equal False
        , test "MyCaregiver is not a child relation" <|
            \_ -> isChildRelation MyCaregiver |> Expect.equal False
        ]


all : Test
all =
    describe "Backend.Relationship" [ isChildRelationTest ]
