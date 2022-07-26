module Pages.Solve exposing (Model, Msg, page)

import Gen.Params.Solve exposing (Params)
import Gen.Route as Route
import Html exposing (Html, div, table, td, text, tr)
import Html.Attributes exposing (class)
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
        { init = init req shared.currentPuzzle
        , update = update
        , view = view
        , subscriptions = subscriptions
        }



-- INIT


type alias Model =
    { puzzle : Grid }


init : Request.With Params -> Maybe Grid -> ( Model, Cmd Msg )
init req puzzle =
    case puzzle of
        Nothing ->
            ( { puzzle = Grid.empty }
            , Request.pushRoute Route.Home_ req
            )

        Just grid ->
            ( { puzzle = grid }
            , Cmd.none
            )



-- UPDATE


type Msg
    = ReplaceMe


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        ReplaceMe ->
            ( model, Cmd.none )



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none



-- VIEW


view : Model -> View Msg
view model =
    let
        viewCell index cell =
            td [ class "border border-zinc-700" ]
                [ div
                    [ class "h-14 w-14 flex justify-center items-center text-3xl" ]
                    [ text (Cell.numberToString cell) ]
                ]

        viewRow index row =
            tr [] (List.indexedMap viewCell row)
    in
    { title = "Solve"
    , body =
        UI.layout
            [ table [ class "puzzle border-2 border-zinc-500" ]
                (List.indexedMap viewRow (Grid.toRows model.puzzle))
            ]
    }
