module Pages.Home_ exposing (Model, Msg, page)

import Gen.Params.Home_ exposing (Params)
import Gen.Route as Route exposing (Route)
import Html exposing (..)
import Html.Attributes
    exposing
        ( class
        , href
        , target
        , value
        )
import Html.Events exposing (onClick)
import Page
import Request
import Shared
import Sudoku.Cell as Cell exposing (Cell)
import Sudoku.Grid as Grid exposing (Grid)
import UI
import View exposing (View)


page : Shared.Model -> Request.With Params -> Page.With Model Msg
page shared req =
    Page.element
        { init = init shared.currentPuzzle
        , update = update
        , view = view
        , subscriptions = subscriptions
        }



-- INIT


type alias Model =
    { currentPuzzle : Maybe Grid }


init : Maybe Grid -> ( Model, Cmd Msg )
init puzzle =
    ( { currentPuzzle = puzzle }
    , Cmd.none
    )


type Msg
    = ClickedNewPuzzle


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    ( model, Cmd.none )


view : Model -> View Msg
view model =
    { title = "Sudoku Trainer"
    , body =
        UI.layout
            [ case model.currentPuzzle of
                Nothing ->
                    viewNewPuzzle

                Just grid ->
                    div [ class "grid grid-cols-2 gap-8" ]
                        [ div []
                            [ Grid.preview grid
                            , div [ class "text-center" ]
                                [ a
                                    [ class "btn mt-4 inline-block"
                                    , href (Route.toHref Route.Solve)
                                    ]
                                    [ text "Resume Puzzle" ]
                                ]
                            ]
                        , viewNewPuzzle
                        ]
            ]
    }


viewNewPuzzle : Html Msg
viewNewPuzzle =
    div []
        [ Grid.preview Grid.empty
        , div [ class "text-center" ]
            [ a
                [ class "btn mt-4 inline-block"
                , href (Route.toHref Route.New)
                ]
                [ text "Start New Puzzle" ]
            ]
        ]


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none
