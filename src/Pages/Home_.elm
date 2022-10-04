module Pages.Home_ exposing (Model, Msg, page)

import Gen.Params.Home_ exposing (Params)
import Gen.Route as Route
import Html exposing (..)
import Html.Attributes exposing (class, href)
import Page
import Request
import Shared
import Sudoku.Puzzle as Puzzle exposing (Puzzle)
import UI
import View exposing (View)


page : Shared.Model a -> Request.With Params -> Page.With (Model a) Msg
page shared _ =
    Page.element
        { init = init shared.currentPuzzle
        , update = update
        , view = view
        , subscriptions = subscriptions
        }



-- INIT


type alias Model a =
    { currentPuzzle : Maybe (Puzzle a) }


init : Maybe (Puzzle a) -> ( Model a, Cmd Msg )
init puzzle =
    ( { currentPuzzle = puzzle }
    , Cmd.none
    )


type Msg
    = ClickedNewPuzzle


update : Msg -> Model a -> ( Model b, Cmd Msg )
update msg model =
    case msg of
        ClickedNewPuzzle ->
            ( model, Cmd.none )


view : Model a -> View Msg
view model =
    { title = "Sudoku Trainer"
    , body =
        UI.layout
            [ case model.currentPuzzle of
                Nothing ->
                    viewNewPuzzle

                Just puzzle ->
                    div [ class "grid grid-cols-2 gap-8" ]
                        [ div []
                            [ UI.previewPuzzle puzzle
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
        [ UI.previewPuzzle Puzzle.empty
        , div [ class "text-center" ]
            [ a
                [ class "btn mt-4 inline-block"
                , href (Route.toHref Route.New)
                ]
                [ text "Start New Puzzle" ]
            ]
        ]


subscriptions : Model a -> Sub Msg
subscriptions _ =
    Sub.none
