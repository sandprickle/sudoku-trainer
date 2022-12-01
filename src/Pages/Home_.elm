module Pages.Home_ exposing (Model, Msg, page)

import Gen.Params.Home_ exposing (Params)
import Gen.Route as Route
import Html exposing (..)
import Html.Attributes exposing (class, href)
import Page
import Request
import Shared
import Sudoku.SolveGrid as SolveGrid exposing (SolveGrid)
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
    { currentPuzzle : Maybe SolveGrid }


init : Maybe SolveGrid -> ( Model, Cmd Msg )
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

                Just grid ->
                    div [ class "grid grid-cols-2 gap-8" ]
                        [ div []
                            [ SolveGrid.preview grid
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
        [ SolveGrid.preview SolveGrid.empty
        , div [ class "text-center" ]
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
