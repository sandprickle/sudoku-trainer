module Pages.New exposing (Model, Msg, page)

import Gen.Params.New exposing (Params)
import Gen.Route as Route
import Html exposing (button, div, h2, p, text, textarea)
import Html.Attributes
    exposing
        ( autocomplete
        , autofocus
        , class
        , spellcheck
        , value
        )
import Html.Events exposing (onClick, onInput)
import Page
import Request
import Shared
import Sudoku.Grid as Grid
import UI
import View exposing (View)


page : Shared.Model -> Request.With Params -> Page.With Model Msg
page _ req =
    Page.element
        { init = init
        , update = update req
        , view = view
        , subscriptions = subscriptions
        }



-- INIT


type alias Model =
    { input : String
    }


init : ( Model, Cmd Msg )
init =
    ( { input = "" }, Cmd.none )



-- UPDATE


type Msg
    = PuzzleInput String
    | ClickedStart


update : Request.With Params -> Msg -> Model -> ( Model, Cmd Msg )
update req msg model =
    case msg of
        PuzzleInput str ->
            ( { model | input = str }, Cmd.none )

        ClickedStart ->
            ( model
            , Cmd.batch
                [ Grid.save (Grid.fromString model.input)
                , Request.pushRoute Route.Solve req
                ]
            )



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.none



-- VIEW


view : Model -> View Msg
view model =
    { title = "New Puzzle | Sudoku Trainer"
    , body =
        UI.layout
            [ div []
                [ h2 [ class "font-bold text-2xl text-primary" ] [ text "New Puzzle" ]
                , div [ class "flex justify-between my-4" ]
                    [ p [] [ text "Input (1-9 = number, dot = empty)" ]
                    , p [] [ text "Preview" ]
                    ]
                , div [ class "grid grid-cols-2 gap-8" ]
                    [ textarea
                        [ autocomplete False
                        , autofocus True
                        , spellcheck False
                        , value model.input
                        , onInput PuzzleInput
                        , class "bg-slate-800 tracking-huge leading-8 font-mono"
                        ]
                        []
                    , Grid.preview (Grid.fromString model.input)
                    ]
                , div [ class "flex justify-center mt-4" ]
                    [ button
                        [ class "btn", onClick ClickedStart ]
                        [ text "Start Puzzle" ]
                    ]
                ]
            ]
    }
