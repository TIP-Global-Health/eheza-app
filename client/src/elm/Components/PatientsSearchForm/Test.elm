module Components.PatientsSearchForm.Test exposing (all)

import Components.PatientsSearchForm.Model exposing (Model, Msg(..), PatientsSearchFormMode(..), emptyModel)
import Components.PatientsSearchForm.Update exposing (update)
import Expect
import Test exposing (Test, describe, test)


{-| The search form in national ID mode, after typing each input in turn.
-}
typedByNationalId : List String -> Model
typedByNationalId inputs =
    List.foldl (\input model -> update (SetInput input) model |> Tuple.first)
        { emptyModel | mode = ModeSearchByNationalId }
        inputs


all : Test
all =
    describe "patient search by national ID"
        [ test "an ID holding '/' is kept as typed" <|
            \_ ->
                typedByNationalId [ "12", "12/", "12/3" ]
                    |> .input
                    |> Expect.equal "12/3"
        , test "an ID holding letters and '-' is kept as typed" <|
            \_ ->
                typedByNationalId [ "AB-1" ]
                    |> .input
                    |> Expect.equal "AB-1"
        , test "an emptied field searches for nothing" <|
            \_ ->
                typedByNationalId [ "12/3" ]
                    |> update (SetSearch "12/3")
                    |> Tuple.first
                    |> update (SetInput "")
                    |> Tuple.first
                    |> .search
                    |> Expect.equal Nothing
        ]
