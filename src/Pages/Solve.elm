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
import Solve.Cell as Cell exposing (Cell(..))
import Solve.Hint as Hint exposing (Hint(..))
import Solve.Puzzle as Puzzle exposing (Coord, Puzzle)
import Sudoku.Number as Number exposing (NumSet, Number)
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
    { puzzle : Puzzle
    , selectedCell : Maybe Coord
    , problemCell : Maybe Coord
    , insertMode : InsertMode
    , hintMessage : String
    }


init : Request.With Params -> Maybe Puzzle -> ( Model, Cmd Msg )
init req savedPuzzle =
    let
        ( puzzle, cmd ) =
            case savedPuzzle of
                Nothing ->
                    ( Puzzle.empty, Request.pushRoute Route.Home_ req )

                Just grid ->
                    ( grid, Cmd.none )
    in
    ( { puzzle = puzzle
      , hintMessage = "_"
      , insertMode = Number
      , selectedCell = Nothing
      , problemCell = Nothing
      }
    , cmd
    )



-- UPDATE


type Msg
    = ClickedCell Coord
    | KeyDown RawKey
    | ClickedChangeMode
    | RequestedHint Hint


update : Msg -> Model -> ( Model, Cmd Msg )
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

        ClickedChangeMode ->
            ( changeMode model, Cmd.none )

        RequestedHint hint ->
            ( { model
                | hintMessage = Hint.generateHint hint model.puzzle
              }
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
            case parseKeyboardAction keyStr of
                InsertNumber num ->
                    if model.problemCell == Nothing then
                        case
                            insertNumber
                                num
                                model.selectedCell
                                model.insertMode
                                model.puzzle
                        of
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

                    else
                        ( model, Cmd.none )

                ClearNumber ->
                    let
                        puzzle =
                            clearNumber model.selectedCell model.puzzle

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

                ChangeMode ->
                    ( changeMode model, Cmd.none )

                None ->
                    ( model, saveCmd )


type KeyboardAction
    = InsertNumber Number
    | ClearNumber
    | UpdateSelection SelectionAction
    | ChangeMode
    | None


parseKeyboardAction : String -> KeyboardAction
parseKeyboardAction str =
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

            "m" ->
                ChangeMode

            "i" ->
                ChangeMode

            _ ->
                None


type InsertMode
    = Number
    | PrimaryNotes
    | SecondaryNotes


changeMode : { a | insertMode : InsertMode } -> { a | insertMode : InsertMode }
changeMode model =
    { model
        | insertMode =
            case model.insertMode of
                Number ->
                    PrimaryNotes

                PrimaryNotes ->
                    SecondaryNotes

                SecondaryNotes ->
                    Number
    }


insertNumber :
    Number
    -> Maybe Coord
    -> InsertMode
    -> Puzzle
    -> Result ( Puzzle, Coord ) Puzzle
insertNumber newNumber selectedCell mode grid =
    case selectedCell of
        Nothing ->
            Ok grid

        Just coord ->
            case Puzzle.getByCoord coord grid of
                Given _ ->
                    Ok grid

                Fixed _ _ ->
                    Ok grid

                Possible possible notes ->
                    case mode of
                        Number ->
                            let
                                newGrid =
                                    Puzzle.setByCoord coord grid <|
                                        Fixed newNumber notes
                            in
                            if Puzzle.isLegal newGrid then
                                Ok (Puzzle.pruneAll newGrid)

                            else
                                Err ( newGrid, coord )

                        PrimaryNotes ->
                            Ok <|
                                Puzzle.setByCoord coord grid <|
                                    Possible possible
                                        { primary = toggleNote newNumber notes.primary
                                        , secondary = notes.secondary
                                        }

                        SecondaryNotes ->
                            Ok <|
                                Puzzle.setByCoord coord grid <|
                                    Possible possible
                                        { primary = notes.primary
                                        , secondary = toggleNote newNumber notes.secondary
                                        }


toggleNote : Number -> NumSet -> NumSet
toggleNote number notes =
    if Number.setMember number notes then
        Number.setRemove number notes

    else
        Number.setInsert number notes


clearNumber : Maybe Coord -> Puzzle -> Puzzle
clearNumber selectedCell grid =
    case selectedCell of
        Just coord ->
            let
                cell =
                    Puzzle.getByCoord coord grid
            in
            case cell of
                Given _ ->
                    grid

                Possible _ _ ->
                    grid

                Fixed _ notes ->
                    Possible Number.fullSet notes
                        |> Puzzle.setByCoord coord grid
                        |> Puzzle.resetPossible
                        |> Puzzle.pruneAll

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
            [ div [ class "grid grid-cols-2" ]
                [ -- Solve Puzzle
                  div []
                    [ div
                        [ class "my-4" ]
                        [ Html.p
                            []
                            [ text "Input Mode" ]
                        , Html.button
                            [ class "rounded border border-gray-700 flex justify-evenly items-center block"
                            , onClick ClickedChangeMode
                            ]
                            ([ { contents = text "Number"
                               , selected = model.insertMode == Number
                               }
                             , { contents = text "Primary Notes"
                               , selected = model.insertMode == PrimaryNotes
                               }
                             , { contents = text "Secondary Notes"
                               , selected = model.insertMode == SecondaryNotes
                               }
                             ]
                                |> List.map
                                    (\{ contents, selected } ->
                                        Html.p
                                            [ class "p-2 border-r border-gray-700"
                                            , classList [ ( "bg-primary", selected ) ]
                                            ]
                                            [ contents ]
                                    )
                            )
                        ]
                    , table
                        [ class "puzzle border-2 border-zinc-500" ]
                        (List.indexedMap viewRow (Puzzle.toRows model.puzzle))
                    ]

                -- Hints Area
                , div
                    []
                    [ text "Hints"
                    , viewHintTool model.hintMessage
                    ]
                ]
            ]
    }


viewHintTool : String -> Html Msg
viewHintTool message =
    let
        btnClass =
            "rounded py-2 px-4 bg-gray-800"
    in
    div []
        [ Html.p
            [ class "bg-black py-2 px-4 my-4 font-mono" ]
            [ text message ]
        , div
            [ class "flex gap-2" ]
            [ Html.button
                [ class btnClass
                , onClick (RequestedHint NakedSingle)
                ]
                [ text "Naked Single" ]
            , Html.button
                [ class btnClass
                , onClick (RequestedHint NakedPair)
                ]
                [ text "Naked Pair" ]
            , Html.button
                [ class btnClass
                , onClick (RequestedHint HiddenPair)
                ]
                [ text "Hidden Pair" ]
            , Html.button
                [ class btnClass
                , onClick (RequestedHint NotImplemented)
                ]
                [ text "X Wing" ]
            , Html.button
                [ class btnClass
                , onClick (RequestedHint NotImplemented)
                ]
                [ text "Y Wing" ]
            , Html.button
                [ class btnClass
                , onClick (RequestedHint NotImplemented)
                ]
                [ text "Swordfish" ]
            ]
        ]


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
    td
        [ class "border border-zinc-700 h-16 w-16" ]
        [ div
            [ class
                "h-full w-full flex justify-center items-center text-3xl"
            , classList
                [ ( "selected", selected )
                , ( "problem", problem )
                , ( "given", given )
                ]
            , onClick <| ClickedCell coord
            ]
            [ case cell of
                Given _ ->
                    text (Cell.numberToString cell)

                Fixed _ _ ->
                    text (Cell.numberToString cell)

                Possible _ notes ->
                    viewNotes notes
            ]
        ]


viewNotes : Cell.Notes -> Html Msg
viewNotes { primary, secondary } =
    div
        [ class "h-full w-full text-gray-500 grid grid-cols-3 grid-rows-3 text-sm" ]
        (Number.setToList Number.fullSet
            |> List.map
                (\number ->
                    if Number.setMember number primary then
                        div
                            [ class
                                "text-cyan-300 font-bold  flex items-center justify-center"
                            ]
                            [ text (Number.toString number) ]

                    else if Number.setMember number secondary then
                        div
                            [ class "flex items-center justify-center" ]
                            [ text (Number.toString number) ]

                    else
                        div [] []
                )
        )
