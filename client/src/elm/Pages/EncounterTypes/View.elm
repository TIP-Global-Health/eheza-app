module Pages.EncounterTypes.View exposing (view)

import App.Model exposing (Msg(SetActivePage))
import Backend.Entities exposing (..)
import Backend.PrenatalParticipant.Model exposing (EncounterType(..))
import Gizra.Html exposing (emptyNode)
import Gizra.NominalDate exposing (NominalDate)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick)
import Pages.Page exposing (Page(..), UserPage(..))
import Translate exposing (Language, translate)


view : Language -> NominalDate -> PersonId -> Html App.Model.Msg
view language currentDate personId =
    div
        [ class "wrap wrap-alt-2 page-encounter-types" ]
        [ viewHeader language
        , viewContent language currentDate personId
            |> div [ class "ui full segment" ]
        ]


viewHeader : Language -> Html Msg
viewHeader language =
    div [ class "ui basic head segment" ]
        [ h1
            [ class "ui header" ]
            [ text <| translate language Translate.BegingNewEncounter ]
        , a
            [ class "link-back"
            , onClick <| SetActivePage <| UserPage PrenatalParticipantsPage
            ]
            [ span [ class "icon-back" ] []
            , span [] []
            ]
        ]


viewContent : Language -> NominalDate -> PersonId -> List (Html App.Model.Msg)
viewContent language currentDate personId =
    let
        antenatalButton =
            EveryDict.get relationId db.people
                |> Maybe.withDefault NotAsked
                |> RemoteData.map
                    (\person ->
                        if isPersonAFertileWoman currentDate person then
                            button
                                [ class "ui primary button encounter-type"
                                , onClick <| SetActivePage <| UserPage <| PrenatalParticipantPage personId
                                ]
                                [ span [ class "text" ] [ text <| translate language <| Translate.EncounterType AntenatalEncounter ]
                                , span [ class "icon-back" ] []
                                ]

                        else
                            emptyNode
                    )
                |> RemoteData.withDefault (fromEntityUuid relationId)
                |> (\name -> translate language (Translate.AddFamilyMemberFor name))
    in
    [ p [] [ text <| translate language Translate.SelectEncounterType ++ ":" ]
    , antenatalButton
    ]
