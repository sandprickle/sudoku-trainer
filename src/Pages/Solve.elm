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
import Sudoku.Puzzle as Puzzle exposing (Puzzle)
import UI
import View exposing (View)


page : Shared.Model a -> Request.With Params -> Page.With (Model a) Msg
page shared req =
    Page.element
        { init = init req shared.currentPuzzle
        , update = update
        , view = view
        , subscriptions = subscriptions
        }



-- INIT


type alias Model a =
    { puzzle : Puzzle a
    , selectedCell : Maybe Coord
    , problemCell : Maybe Coord
    }


init : Request.With Params -> Maybe (Puzzle a) -> ( Model a, Cmd Msg )
init req puzzle =
    case puzzle of
        Nothing ->
            ( { puzzle = Puzzle.empty
              , selectedCell = Nothing
              , problemCell = Nothing
              }
            , Request.pushRoute Route.Home_ req
            )

        Just grid ->
            ( { puzzle = grid
              , selectedCell = Nothing
              , problemCell = Nothing
              }
            , Cmd.none
            )



-- UPDATE


type Msg
    = ClickedCell Coord
    | KeyDown RawKey


update : Msg -> Model a -> ( Model a, Cmd Msg )
update msg model =
    let
        saveCmd =
            case model.problemCell of
                Just _ ->
                    Cmd.none

                Nothing ->
                    Puzzle.save model.puzzle
    in
    case msg of
        ClickedCell coord ->
            ( { model | selectedCell = Just coord }, saveCmd )

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
                    if model.problemCell == Nothing then
                        case model.selectedCell of
                            Just selectedCell ->
                                case Puzzle.insertNumber num selectedCell model.puzzle of
                                    Ok puzzle ->
                                        ( { model | puzzle = puzzle }
                                        , saveCmd
                                        )

                                    Err ( puzzle, coord ) ->
                                        ( { model
                                            | puzzle = puzzle
                                            , problemCell = Just coord
                                          }
                                        , Cmd.none
                                        )

                            Nothing ->
                                ( model, Cmd.none )

                    else
                        ( model, Cmd.none )

                ClearNumber ->
                    let
                        puzzle =
                            Puzzle.clearNumber model.selectedCell model.puzzle

                        problemCell =
                            if model.problemCell == model.selectedCell then
                                Nothing

                            else
                                model.problemCell
                    in
                    ( { model | puzzle = puzzle, problemCell = problemCell }
                    , saveCmd
                    )

                UpdateSelection action ->
                    ( { model
                        | selectedCell =
                            updateSelection action model.selectedCell
                      }
                    , saveCmd
                    )

                None ->
                    ( model, saveCmd )


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


subscriptions : Model a -> Sub Msg
subscriptions _ =
    Keyboard.downs KeyDown



-- VIEW


view : Model a -> View Msg
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
            let
                given =
                    case cell of
                        Given _ ->
                            True

                        Fixed _ _ ->
                            False

                        Possible _ _ ->
                            False
            in
            td [ class "border border-zinc-700" ]
                [ div
                    [ classList
                        [ ( "selected", selected )
                        , ( "problem", problem )
                        , ( "given", given )
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
                            , problem = model.problemCell == Just currentCoord
                            }
                    )
                |> tr []
    in
    { title = "Solve"
    , body =
        UI.layout
            [ table [ class "puzzle border-2 border-zinc-500" ]
                (List.indexedMap viewRow (Puzzle.rows model.puzzle))
            ]
    }
