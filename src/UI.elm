module UI exposing (layout, theme)

import Gen.Route as Route
import Html exposing (..)
import Html.Attributes exposing (class, href, target)
import Svg exposing (path, svg)
import Svg.Attributes as SvgAttr


layout : List (Html msg) -> List (Html msg)
layout children =
    let
        githubIcon =
            svg
                [ SvgAttr.width "24"
                , SvgAttr.height "24"
                , SvgAttr.viewBox "0 0 24 24"
                , SvgAttr.fill "none"
                , SvgAttr.stroke "currentColor"
                , SvgAttr.strokeWidth "2"
                , SvgAttr.strokeLinecap "round"
                , SvgAttr.strokeLinejoin "round"
                , SvgAttr.class "feather feather-github"
                ]
                [ path
                    [ SvgAttr.d
                        "M9 19c-5 1.5-5-2.5-7-3m14 6v-3.87a3.37 3.37 0 0 0-.94-2.61c3.14-.35 6.44-1.54 6.44-7A5.44 5.44 0 0 0 20 4.77 5.07 5.07 0 0 0 19.91 1S18.73.65 16 2.48a13.38 13.38 0 0 0-7 0C6.27.65 5.09 1 5.09 1A5.07 5.07 0 0 0 5 4.77a5.44 5.44 0 0 0-1.5 3.78c0 5.42 3.3 6.61 6.44 7A3.37 3.37 0 0 0 9 18.13V22"
                    ]
                    []
                ]
    in
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
                [ githubIcon ]
            ]
        , main_
            [ class
                "h-full row-start-2 row-end-3 flex items-center justify-center"
            ]
            children
        ]
    ]


theme =
    { cell =
        "border border-zinc-700 h-14 w-14 flex justify-center items-center text-3xl"
    , puzzleBorder = "border-2 border-zinc-500"
    }
