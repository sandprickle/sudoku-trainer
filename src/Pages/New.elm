module Pages.New exposing (Model, Msg, page)

import Gen.Params.New exposing (Params)
import Html exposing (button, div, h2, table, td, text, tr)
import Html.Attributes
    exposing
        ( class
        , classList
        )
import Html.Events exposing (onClick)
import Html.Lazy
import Keyboard exposing (Key, RawKey)
import Page
import Request
import Shared
import Sudoku.Grid as Grid exposing (Grid)
import Sudoku.Number as Number exposing (Number)
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
    { currentIndex : Int
    , grid : Grid Cell
    }


type Cell
    = Number Number
    | Blank


cellToString : Cell -> String
cellToString cell =
    case cell of
        Number num ->
            Number.toString num

        Blank ->
            ""


init : ( Model, Cmd Msg )
init =
    ( { currentIndex = 0
      , grid = Grid.init Blank
      }
    , Cmd.none
    )



-- UPDATE


type Msg
    = ClickedStart
    | KeyDown RawKey


type EditAction
    = Append Cell
    | Delete


update : Request.With Params -> Msg -> Model -> ( Model, Cmd Msg )
update _ msg model =
    case msg of
        KeyDown rawKey ->
            let
                editAction =
                    Keyboard.oneOf
                        [ Keyboard.whitespaceKey
                        , Keyboard.characterKeyOriginal
                        , Keyboard.editingKey
                        ]
                        rawKey
                        |> Maybe.andThen parseEditAction
            in
            case editAction of
                Just action ->
                    ( processEditAction model action, Cmd.none )

                Nothing ->
                    ( model, Cmd.none )

        ClickedStart ->
            ( model, Cmd.none )


processEditAction : Model -> EditAction -> Model
processEditAction model action =
    case action of
        Append cell ->
            { model
                | grid =
                    Grid.setByIndex
                        model.currentIndex
                        model.grid
                        cell
                , currentIndex = model.currentIndex + 1
            }

        Delete ->
            { model
                | grid =
                    Grid.setByIndex
                        model.currentIndex
                        model.grid
                        Blank
                , currentIndex = model.currentIndex - 1
            }


parseEditAction : Key -> Maybe EditAction
parseEditAction key =
    case key of
        Keyboard.Backspace ->
            Just Delete

        Keyboard.Spacebar ->
            Just (Append Blank)

        Keyboard.Character "0" ->
            Just (Append Blank)

        Keyboard.Character "1" ->
            Just (Append (Number Number.one))

        Keyboard.Character "2" ->
            Just (Append (Number Number.two))

        Keyboard.Character "3" ->
            Just (Append (Number Number.three))

        Keyboard.Character "4" ->
            Just (Append (Number Number.four))

        Keyboard.Character "5" ->
            Just (Append (Number Number.five))

        Keyboard.Character "6" ->
            Just (Append (Number Number.six))

        Keyboard.Character "7" ->
            Just (Append (Number Number.seven))

        Keyboard.Character "8" ->
            Just (Append (Number Number.eight))

        Keyboard.Character "9" ->
            Just (Append (Number Number.nine))

        _ ->
            Nothing



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions _ =
    Keyboard.downs KeyDown



-- VIEW


view : Model -> View Msg
view model =
    let
        legal =
            isLegal model.grid

        viewCell cell =
            td [ class "border border-zinc-700" ]
                [ div
                    [ class " h-14 w-14 flex justify-center items-center text-3xl"
                    , classList [ ( "problem", not legal ) ]
                    ]
                    [ text (cellToString cell) ]
                ]

        viewRow row =
            tr [] <|
                List.map
                    (Html.Lazy.lazy viewCell)
                    row
    in
    { title = "New Puzzle | Sudoku Trainer"
    , body =
        UI.layout
            [ div []
                [ h2
                    [ class "font-bold text-2xl text-primary " ]
                    [ text "New Puzzle" ]
                , table
                    [ class "puzzle border-2 border-zinc-500 my-8" ]
                  <|
                    List.map viewRow (Grid.toRows model.grid)
                , div
                    [ class "flex justify-center mt-4" ]
                    [ button
                        [ class "btn", onClick ClickedStart ]
                        [ text "Start Puzzle" ]
                    ]
                ]
            ]
    }


getNumber : Cell -> Maybe Number
getNumber cell =
    case cell of
        Number num ->
            Just num

        Blank ->
            Nothing


isLegal : Grid Cell -> Bool
isLegal grid =
    Grid.isLegal getNumber grid


isSolvable : Grid Cell -> Bool
isSolvable grid =
    Grid.isSolvable getNumber grid
