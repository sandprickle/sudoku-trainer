module Pages.New exposing (Model, Msg, page)

import Html.Styled as Html exposing (..)
import Html.Styled.Attributes as Attr exposing (css)
import Html.Styled.Events exposing (onInput)
import Shared
import Sudoku.Cell as Cell
import Sudoku.Grid as Grid exposing (Grid)
import Tailwind.Utilities as Tw
import UI
import View exposing (View)


page : Shared.Model -> Request.With Params -> Page.With Model Msg
page shared req =
    Page.element
        { init = init
        , update = update
        , view = view
        , subscriptions = subscriptions
        }



-- INIT


type alias Model =
    { puzzle : Grid
    }


init : ( Model, Cmd Msg )
init =
    ( { puzzle = Grid.fromString ""
      }
    , Cmd.none
    )



-- UPDATE


type Msg
    = ClickedSave
    | InputChanged String


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        ClickedSave ->
            ( model, Cmd.none )

        InputChanged newValue ->
            ( { model | puzzle = Grid.fromString newValue }
            , Cmd.none
            )



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none



-- VIEW


view : Model -> View Msg
view model =
    { title = "New Puzzle"
    , body =
        UI.centeredBox
            [ div [ css [ Tw.flex, Tw.flex_col, Tw.items_center, Tw.gap_8 ] ]
                [ h2 [] [ text "Input your puzzle here" ]
                , div [ css [ Tw.flex, Tw.justify_evenly, Tw.gap_8 ] ]
                    [ textarea
                        [ css
                            [ Tw.bg_gray_800
                            , Tw.font_mono
                            , Tw.tracking_widest
                            ]
                        , Attr.rows 9
                        , Attr.cols 9
                        , onInput InputChanged
                        ]
                        []
                    , viewPuzzlePreview model.puzzle
                    ]
                , button [ css [ UI.btnStyles ] ] [ text "Save" ]
                ]
            ]
    }


viewPuzzlePreview : Grid -> Html msg
viewPuzzlePreview grid =
    let
        rows =
            Grid.toRows grid

        viewCell index cell =
            td
                [ css
                    [ Tw.h_6
                    , Tw.w_6
                    , Tw.border_r_2
                    , Tw.text_sm
                    , if remainderBy 3 (index + 1) == 0 then
                        Tw.border_gray_400

                      else
                        Tw.border_none
                    ]
                ]
                [ div
                    [ css
                        [ Tw.flex
                        , Tw.items_center
                        , Tw.justify_center
                        ]
                    ]
                    [ text (Cell.numberToString cell) ]
                ]

        viewRow index row =
            tr
                [ css
                    [ Tw.border_b_2
                    , if remainderBy 3 (index + 1) == 0 then
                        Tw.border_gray_400

                      else
                        Tw.border_none
                    ]
                ]
                (List.indexedMap viewCell row)
    in
    table [ css [ Tw.border_2, Tw.border_gray_400 ] ] (List.indexedMap viewRow rows)
