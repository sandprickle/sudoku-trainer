module Pages.Solve exposing (Model, Msg, page)

import Gen.Params.Solve exposing (Params)
import Gen.Route as Route
import Html exposing (Html, div, table, td, text, tr)
import Html.Attributes exposing (class, classList)
import Html.Events exposing (onClick)
import Html.Lazy exposing (lazy2)
import Page
import Request
import Shared
import Sudoku.Cell as Cell exposing (Cell)
import Sudoku.Grid as Grid exposing (Coord, Grid)
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
    { puzzle : Grid
    , selectedCell : Maybe Coord
    }


init : Request.With Params -> Maybe Grid -> ( Model, Cmd Msg )
init req puzzle =
    case puzzle of
        Nothing ->
            ( { puzzle = Grid.empty, selectedCell = Nothing }
            , Request.pushRoute Route.Home_ req
            )

        Just grid ->
            ( { puzzle = grid, selectedCell = Nothing }
            , Cmd.none
            )



-- UPDATE


type Msg
    = ClickedCell Coord


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        ClickedCell coord ->
            ( { model | selectedCell = Just coord }
            , Cmd.none
            )



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none



-- VIEW


view : Model -> View Msg
view model =
    let
        viewCell :
            { currentCoord : Coord
            , selectedCoord : Maybe Coord
            }
            -> Cell
            -> Html Msg
        viewCell { currentCoord, selectedCoord } cell =
            let
                { x, y } =
                    currentCoord

                selected =
                    case selectedCoord of
                        Just coord ->
                            x == coord.x && y == coord.y

                        Nothing ->
                            False

                twClasses =
                    "h-14 w-14 flex justify-center items-center text-3xl"
            in
            td [ class "border border-zinc-700" ]
                [ div
                    [ classList
                        [ ( twClasses ++ " selected", selected )
                        , ( twClasses, not selected )
                        ]
                    , onClick <| ClickedCell currentCoord
                    ]
                    [ text (Cell.numberToString cell) ]
                ]

        viewRow : Int -> List Cell -> Html Msg
        viewRow y row =
            tr [] <|
                List.indexedMap
                    (\x ->
                        lazy2 viewCell
                            { currentCoord = { x = x, y = y }
                            , selectedCoord = model.selectedCell
                            }
                    )
                    row
    in
    { title = "Solve"
    , body =
        UI.layout
            [ table [ class "puzzle border-2 border-zinc-500" ]
                (List.indexedMap viewRow (Grid.toRows model.puzzle))
            ]
    }
