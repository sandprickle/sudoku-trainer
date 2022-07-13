module View exposing (View, map, none, placeholder, toBrowserDocument)

import Browser
import Css.Global
import Html.Styled exposing (toUnstyled)
import Html.Styled.Attributes exposing (css)
import Tailwind.Utilities as Tw exposing (globalStyles)


type alias View msg =
    { title : String
    , body : List (Html.Styled.Html msg)
    }


placeholder : String -> View msg
placeholder str =
    { title = str
    , body = [ Html.Styled.text str ]
    }


none : View msg
none =
    placeholder ""


map : (a -> b) -> View a -> View b
map fn view =
    { title = view.title
    , body = List.map (Html.Styled.map fn) view.body
    }


toBrowserDocument : View msg -> Browser.Document msg
toBrowserDocument view =
    { title = view.title
    , body =
        [ toUnstyled <|
            Html.Styled.div
                [ css
                    [ Tw.h_full
                    , Tw.w_full
                    , Tw.overflow_scroll
                    , Tw.bg_gray_900
                    , Tw.text_gray_300
                    ]
                ]
                (Css.Global.global globalStyles :: view.body)
        ]
    }
