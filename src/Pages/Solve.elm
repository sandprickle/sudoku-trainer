module Pages.Solve exposing (Model, Msg, page)

import Gen.Params.Solve exposing (Params)
import Gen.Route as Route
import Html exposing (Html, div, table, td, text, tr)
import Html.Attributes exposing (class, classList)
import Html.Events exposing (onClick)
import Html.Lazy exposing (lazy2)
import Keyboard exposing (RawKey)
import Page
import Request
import Set
import Shared
import Sudoku.Cell as Cell exposing (Cell(..))
import Sudoku.Grid as Grid exposing (Coord, Grid)
import Sudoku.Number as Number exposing (Number)
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
    , problemCells : List Coord
    }


init : Request.With Params -> Maybe Grid -> ( Model, Cmd Msg )
init req puzzle =
    case puzzle of
        Nothing ->
            ( { puzzle = Grid.empty
              , selectedCell = Nothing
              , problemCells = []
              }
            , Request.pushRoute Route.Home_ req
            )

        Just grid ->
            ( { puzzle = grid
              , selectedCell = Nothing
              , problemCells = []
              }
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
            ( { model | selectedCell = Just coord }, Cmd.none )

        KeyDown rawKey ->
            let
                keyStr =
                    case Keyboard.characterKeyOriginal rawKey of
                        Just (Keyboard.Character str) ->
                            str

                        _ ->
                            ""
            in
            case parseAction keyStr of
                InsertNumber num ->
                    case insertNumber num model.selectedCell model.puzzle of
                        Ok puzzle ->
                            ( { model | puzzle = puzzle }, Cmd.none )

                        Err ( puzzle, coord ) ->
                            ( { model
                                | puzzle = puzzle
                                , problemCells = coord :: model.problemCells
                              }
                            , Cmd.none
                            )

                ClearNumber ->
                    ( { model
                        | puzzle = clearNumber model.selectedCell model.puzzle
                      }
                    , Cmd.none
                    )

                UpdateSelection action ->
                    ( { model
                        | selectedCell =
                            updateSelection action model.selectedCell
                      }
                    , Cmd.none
                    )

                None ->
                    ( model, Cmd.none )


type Action
    = InsertNumber Number
    | ClearNumber
    | UpdateSelection SelectionAction
    | None


parseAction : String -> Action
parseAction str =
    let
        valueKeys =
            Set.fromList [ "0", "1", "2", "3", "4", "5", "6", "7", "8", "9" ]
    in
    if Set.member str valueKeys then
        case Number.fromString str of
            Just num ->
                InsertNumber num

            Nothing ->
                ClearNumber

    else
        case str of
            "h" ->
                UpdateSelection MoveLeft

            "j" ->
                UpdateSelection MoveDown

            "k" ->
                UpdateSelection MoveUp

            "l" ->
                UpdateSelection MoveRight

            _ ->
                None


insertNumber : Number -> Maybe Coord -> Grid -> Result ( Grid, Coord ) Grid
insertNumber num selectedCell grid =
    case selectedCell of
        Just coord ->
            let
                cell =
                    Grid.getByCoord coord grid
            in
            case cell of
                Given _ ->
                    Ok grid

                Fixed _ _ ->
                    Ok grid

                Possible _ notes ->
                    let
                        newGrid =
                            Grid.setByCoord coord grid (Fixed num notes)
                    in
                    if Grid.isLegal newGrid then
                        Ok (Grid.pruneAll newGrid)

                    else
                        Err ( newGrid, coord )

        Nothing ->
            Ok grid


clearNumber : Maybe Coord -> Grid -> Grid
clearNumber selectedCell grid =
    case selectedCell of
        Just coord ->
            let
                cell =
                    Grid.getByCoord coord grid
            in
            case cell of
                Given _ ->
                    grid

                Possible _ _ ->
                    grid

                Fixed _ notes ->
                    Possible Number.setAll notes
                        |> Grid.setByCoord coord grid
                        |> Grid.resetPossible
                        |> Grid.pruneAll

        Nothing ->
            grid


type SelectionAction
    = MoveDown
    | MoveUp
    | MoveLeft
    | MoveRight


updateSelection : SelectionAction -> Maybe Coord -> Maybe Coord
updateSelection action selectedCell =
    let
        moveSelectionLeft { x, y } =
            if x == 0 then
                { x = 8, y = y }

            else
                { x = x - 1, y = y }

        moveSelectionRight { x, y } =
            if x == 8 then
                { x = 0, y = y }

            else
                { x = x + 1, y = y }

        moveSelectionUp { x, y } =
            if y == 0 then
                { x = x, y = 8 }

            else
                { x = x, y = y - 1 }

        moveSelectionDown { x, y } =
            if y == 8 then
                { x = x, y = 0 }

            else
                { x = x, y = y + 1 }
    in
    case selectedCell of
        Nothing ->
            Just { x = 4, y = 4 }

        Just coord ->
            case action of
                MoveLeft ->
                    Just (moveSelectionLeft coord)

                MoveDown ->
                    Just (moveSelectionDown coord)

                MoveUp ->
                    Just (moveSelectionUp coord)

                MoveRight ->
                    Just (moveSelectionRight coord)



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions _ =
    Keyboard.downs KeyDown



-- VIEW


view : Model -> View Msg
view model =
    let
        viewCell :
            { coord : Coord
            , selected : Bool
            , problem : Bool
            }
            -> Cell
            -> Html Msg
        viewCell { coord, selected, problem } cell =
            td [ class "border border-zinc-700" ]
                [ div
                    [ classList
                        [ ( "selected", selected )
                        , ( "problem", problem )
                        ]
                    , class
                        "h-14 w-14 flex justify-center items-center text-3xl"
                    , onClick <| ClickedCell coord
                    ]
                    [ text (Cell.numberToString cell) ]
                ]

        viewRow : Int -> List Cell -> Html Msg
        viewRow y row =
            row
                |> List.indexedMap
                    (\x ->
                        let
                            currentCoord =
                                { x = x, y = y }
                        in
                        lazy2 viewCell
                            { coord = currentCoord
                            , selected =
                                case model.selectedCell of
                                    Just coord ->
                                        coord == currentCoord

                                    Nothing ->
                                        False
                            , problem =
                                List.member currentCoord model.problemCells
                            }
                    )
                |> tr []
    in
    { title = "Solve"
    , body =
        UI.layout
            [ table [ class "puzzle border-2 border-zinc-500" ]
                (List.indexedMap viewRow (Grid.toRows model.puzzle))
            ]
    }
