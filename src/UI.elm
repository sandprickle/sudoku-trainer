module UI exposing (layout)

import Gen.Route as Route
import Html exposing (..)
import Html.Attributes exposing (class, href, target)
import Svg exposing (path, svg)
import Svg.Attributes as SvgAttr
import UI.Icon as Icon


layout : List (Html msg) -> List (Html msg)
layout children =
    [ div [ class "px-8 grid grid-cols-layout grid-rows-layout h-full" ]
        [ header [ class "flex items-center justify-between py-4 row-start-1 row-end-2" ]
            [ h1
                [ class "text-3xl text-primary font-bold" ]
                [ a [ href (Route.toHref Route.Home_) ] [ text "Sudoku Trainer" ] ]
            , a
                [ href "https://github.com/sandprickle/sudoku-trainer"
                , target "_blank"
                , class "text-primary"
                ]
                [ Icon.github ]
            ]
        , main_
            [ class
                "h-full row-start-2 row-end-3"
            ]
            children
        ]
    ]
