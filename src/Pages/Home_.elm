module Pages.Home_ exposing (Model, Msg, page)

import Gen.Params.Home_ exposing (Params)
import Gen.Route as Route
import Html exposing (..)
import Html.Attributes exposing (class, href)
import Page
import Request
import Shared
import Solve.Puzzle exposing (Puzzle)
import UI
import View exposing (View)


page : Shared.Model -> Request.With Params -> Page.With Model Msg
page shared _ =
    Page.element
        { init = init shared.currentPuzzle
        , update = update
        , view = view
        , subscriptions = subscriptions
        }



-- INIT


type alias Model =
    { currentPuzzle : Maybe Puzzle }


init : Maybe Puzzle -> ( Model, Cmd Msg )
init puzzle =
    ( { currentPuzzle = puzzle }
    , Cmd.none
    )


type Msg
    = ClickedNewPuzzle


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        ClickedNewPuzzle ->
            ( model, Cmd.none )


view : Model -> View Msg
view model =
    { title = "Sudoku Trainer"
    , body =
        UI.layout
            [ case model.currentPuzzle of
                Nothing ->
                    viewNewPuzzle

                Just _ ->
                    div [ class "grid grid-cols-2 gap-8" ]
                        [ div []
                            [ div [ class "text-center" ]
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
        [ div [ class "text-center" ]
            [ a
                [ class "btn mt-4 inline-block"
                , href (Route.toHref Route.New)
                ]
                [ text "Start New Puzzle" ]
            ]
        ]


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.none
