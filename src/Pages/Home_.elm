module Pages.Home_ exposing (Model, Msg, page)

import Gen.Params.Home_ exposing (Params)
import Html.Styled as Html exposing (..)
import Html.Styled.Attributes exposing (css, href)
import Page
import Request
import Shared
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
    {}


init : ( Model, Cmd Msg )
init =
    ( {}, Cmd.none )



-- UPDATE


type Msg
    = NoOp


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NoOp ->
            ( model, Cmd.none )



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none



-- VIEW


view : Model -> View Msg
view model =
    { title = "Sudoku Trainer"
    , body =
        UI.centeredBox
            [ div
                [ css
                    [ Tw.flex
                    , Tw.flex_col
                    , Tw.items_center
                    , Tw.gap_8
                    ]
                ]
                [ h2 [] [ text "Hi, welcome to Bryce's Sudoku Trainer!" ]
                , a [ css [ UI.btnStyles ], href "/new" ]
                    [ text "New Puzzle" ]
                ]
            ]
    }
