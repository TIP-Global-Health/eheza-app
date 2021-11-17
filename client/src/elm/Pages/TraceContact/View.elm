module Pages.TraceContact.View exposing (view)

import AssocList as Dict exposing (Dict)
import Backend.Entities exposing (..)
import Backend.Measurement.Model exposing (ContactTraceEntry)
import Backend.Model exposing (ModelIndexedDb)
import Backend.Person.Model exposing (Person)
import Backend.Person.Utils exposing (generateFullName)
import Gizra.Html exposing (emptyNode)
import Gizra.NominalDate exposing (NominalDate)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Maybe.Extra exposing (isJust)
import Pages.Page exposing (Page(..), UserPage(..))
import Pages.TraceContact.Model exposing (..)
import Pages.WellChildEncounter.View exposing (thumbnailDimensions, viewPersonDetails)
import RemoteData exposing (RemoteData)
import Translate exposing (Language, TranslationId, translate)
import Utils.Html exposing (thumbnailImage)


view : Language -> NominalDate -> AcuteIllnessTraceContactId -> ModelIndexedDb -> Model -> Html Msg
view language currentDate id db model =
    let
        traceContact =
            Dict.get id db.traceContacts
                |> Maybe.andThen RemoteData.toMaybe
                |> Maybe.map .value

        tracePerson =
            Maybe.andThen
                (\contact ->
                    Dict.get contact.personId db.people
                        |> Maybe.andThen RemoteData.toMaybe
                )
                traceContact

        personDetails =
            Maybe.map (viewPersonDetails language currentDate)
                tracePerson
                |> Maybe.withDefault (viewContactDetails language currentDate traceContact)

        content =
            personDetails
    in
    div [ class "page-encounter well-child" ] <|
        viewHeader language
            :: content


viewHeader : Language -> Html Msg
viewHeader language =
    div
        [ class "ui basic segment head" ]
        [ h1
            [ class "ui header" ]
            [ text <| translate language Translate.CovidContactTracing ]
        , span
            [ class "link-back"
            , onClick <| SetActivePage <| UserPage GlobalCaseManagementPage
            ]
            [ span [ class "icon-back" ] []
            , span [] []
            ]
        ]


viewContactDetails : Language -> NominalDate -> Maybe ContactTraceEntry -> List (Html any)
viewContactDetails language currentDate traceContact =
    Maybe.map
        (\contact ->
            let
                name =
                    generateFullName contact.firstName contact.secondName

                -- genderEntry =
                --     viewEntry Translate.GenderLabel (translate language <| Translate.Gender person.gender)
                --
                -- villageEntry =
                --     Maybe.map (viewEntry Translate.Village) person.village
                --         |> Maybe.withDefault emptyNode
                viewEntry labelTransId content =
                    p []
                        [ span [ class "label" ] [ text <| translate language labelTransId ++ ": " ]
                        , span [] [ text content ]
                        ]
            in
            [ div [ class "ui image" ]
                [ thumbnailImage "mother" Nothing name thumbnailDimensions.height thumbnailDimensions.width ]
            , div [ class "details" ]
                [ h2 [ class "ui header" ]
                    [ text name ]

                -- , genderEntry
                -- , villageEntry
                ]
            ]
        )
        traceContact
        |> Maybe.withDefault []
