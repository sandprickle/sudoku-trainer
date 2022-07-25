module Sudoku.Number exposing
    ( Number
    , all
    , decoder
    , eight
    , encode
    , five
    , four
    , fromChar
    , nine
    , one
    , seven
    , six
    , three
    , toString
    , two
    )

import Json.Decode as Decode exposing (Decoder)
import Json.Encode as Encode


type Number
    = Number Int


encode : Number -> Encode.Value
encode (Number num) =
    Encode.int num


decoder : Decoder Number
decoder =
    Decode.int
        |> Decode.andThen
            (\num ->
                if num >= 1 && num <= 9 then
                    Decode.succeed (Number num)

                else
                    Decode.fail "Number must be between 1 and 9"
            )


toString : Number -> String
toString (Number int) =
    String.fromInt int


fromChar : Char -> Maybe Number
fromChar char =
    case char of
        '1' ->
            Just (Number 1)

        '2' ->
            Just (Number 2)

        '3' ->
            Just (Number 3)

        '4' ->
            Just (Number 4)

        '5' ->
            Just (Number 5)

        '6' ->
            Just (Number 6)

        '7' ->
            Just (Number 7)

        '8' ->
            Just (Number 8)

        '9' ->
            Just (Number 9)

        _ ->
            Nothing



-- Hardcoded Numbers


one : Number
one =
    Number 1


two : Number
two =
    Number 2


three : Number
three =
    Number 3


four : Number
four =
    Number 4


five : Number
five =
    Number 5


six : Number
six =
    Number 6


seven : Number
seven =
    Number 7


eight : Number
eight =
    Number 8


nine : Number
nine =
    Number 9


all : List Number
all =
    [ one, two, three, four, five, six, seven, eight, nine ]
