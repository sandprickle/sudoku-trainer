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
import Keyboard exposing (Key, RawKey)
import Page
import Request
import Set
import Shared
import Sudoku.Grid as Grid
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
    { input : String
    }


init : ( Model, Cmd Msg )
init =
    ( { input = "" }, Cmd.none )



-- UPDATE


type Msg
    = Edit EditAction
    | ClickedStart
    | KeyDown RawKey


type EditAction
    = Append String
    | Delete


update : Request.With Params -> Msg -> Model -> ( Model, Cmd Msg )
update req msg model =
    case msg of
        ClickedStart ->
            ( model
            , Cmd.batch
                [ Grid.save (Grid.fromString model.input)
                , Request.pushRoute Route.Solve req
                ]
            )

        KeyDown rawKey ->
            case parseRawKey rawKey of
                Just action ->
                    case action of
                        Append str ->
                            ( { model | input = model.input ++ str }
                            , Cmd.none
                            )

                        Delete ->
                            ( { model | input = String.dropRight 1 model.input }
                            , Cmd.none
                            )

                Nothing ->
                    ( model, Cmd.none )

        _ ->
            ( model, Cmd.none )


parseRawKey : RawKey -> Maybe EditAction
parseRawKey rawKey =
    let
        validKeys =
            Set.fromList
                [ "0", "1", "2", "3", "4", "5", "6", "7", "8", "9", "Backspace" ]

        parseKey : RawKey -> Maybe String
        parseKey input =
            let
                keyString =
                    Keyboard.rawValue input
            in
            if Set.member keyString validKeys then
                Just keyString

            else
                Nothing

        numberKeys =
            Set.fromList [ "1", "2", "3", "4", "5", "6", "7", "8", "9" ]

        deleteKeys =
            Set.fromList [ "0", "Backspace" ]
    in
    case parseKey rawKey of
        Just keyString ->
            if Set.member keyString numberKeys then
                Just (Append keyString)

            else if Set.member keyString deleteKeys then
                Just Delete

            else
                Nothing

        Nothing ->
            Nothing



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions _ =
    Keyboard.downs KeyDown



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
