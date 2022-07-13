module UI exposing (..)

import Css exposing (Style)
import Html.Styled as Html exposing (..)
import Html.Styled.Attributes exposing (css)
import Tailwind.Utilities as Tw


btnStyles : Style
btnStyles =
    Css.batch
        [ Tw.rounded_full
        , Tw.bg_pink_800
        , Tw.px_4
        , Tw.py_2
        , Tw.no_underline
        , Css.hover [ Tw.bg_pink_700 ]
        , Css.focus [ Tw.bg_pink_700 ]
        ]


centeredBox : List (Html msg) -> List (Html msg)
centeredBox children =
    [ div
        [ css
            [ Tw.flex
            , Tw.items_center
            , Tw.justify_center
            , Tw.h_full
            ]
        ]
        [ div
            [ css
                [ Tw.p_8
                , Tw.rounded
                , Tw.bg_gray_700
                ]
            ]
            children
        ]
    ]
