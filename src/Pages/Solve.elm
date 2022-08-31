module Pages.Solve exposing (Model, Msg, page)

import Gen.Params.Solve exposing (Params)
import Gen.Route as Route
import Html exposing (Html, div, table, td, text, tr)
import Html.Attributes exposing (class, classList)
import Html.Events exposing (onClick)
import Html.Lazy exposing (lazy3)
import Keyboard exposing (RawKey)
import Page
import Request
import Set exposing (Set)
import Shared
import Sudoku.Cell as Cell exposing (Cell(..))
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
    | KeyDown RawKey


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        ClickedCell coord ->
            ( { model | selectedCell = Just coord }
            , Cmd.none
            )

        KeyDown rawKey ->
            let
                keyStr =
                    case Keyboard.characterKeyOriginal rawKey of
                        Just (Keyboard.Character str) ->
                            str

                        _ ->
                            ""
            in
            Debug.todo "handle keyboard input"



-- valueKeys : Set String
-- valueKeys =
--     Set.fromList [ "0", "1", "2", "3", "4", "5", "6", "7", "8", "9" ]
-- navKeys : Set String
-- navKeys =
--     Set.fromList [ "h", "j", "k", "l" ]
-- moveSelectionLeft : Coord -> Coord
-- moveSelectionLeft { x, y } =
--     if x == 0 then
--         { x = 8, y = y }
--     else
--         { x = x - 1, y = y }
-- moveSelectionRight : Coord -> Coord
-- moveSelectionRight { x, y } =
--     if x == 8 then
--         { x = 0, y = y }
--     else
--         { x = x + 1, y = y }
-- moveSelectionUp : Coord -> Coord
-- moveSelectionUp { x, y } =
--     if y == 0 then
--         { x = x, y = 8 }
--     else
--         { x = x, y = y - 1 }
-- moveSelectionDown : Coord -> Coord
-- moveSelectionDown { x, y } =
--     if y == 8 then
--         { x = x, y = 0 }
--     else
--         { x = x, y = y + 1 }
-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions _ =
    Keyboard.downs KeyDown



-- VIEW


view : Model -> View Msg
view model =
    let
        viewCell : Coord -> Bool -> Cell -> Html Msg
        viewCell currentCoord selected cell =
            let
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
                        let
                            currentCoord =
                                { x = x, y = y }

                            selected =
                                case model.selectedCell of
                                    Just coord ->
                                        coord == currentCoord

                                    Nothing ->
                                        False
                        in
                        lazy3 viewCell currentCoord selected
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
