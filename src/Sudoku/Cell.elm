module Sudoku.Cell exposing
    ( Cell
    , Notes
    , decoder
    , default
    , encode
    , getNotes
    , getNumber
    , getPossible
    , initFromChar
    , isFilled
    , isGiven
    , numberToString
    , setPossible
    )

import Json.Decode as Decode exposing (Decoder)
import Json.Encode as Encode
import Sudoku.Number as Number exposing (Number)


type Cell
    = Given Number
    | Fixed Number Notes
    | Possible (List Number) Notes


initFromChar : Char -> Cell
initFromChar char =
    case Number.fromChar char of
        Just number ->
            Given number

        Nothing ->
            Possible Number.all { primary = [], secondary = [] }


numberToString : Cell -> String
numberToString cell =
    case cell of
        Given number ->
            Number.toString number

        Fixed number _ ->
            Number.toString number

        Possible _ _ ->
            ""


isFilled : Cell -> Bool
isFilled cell =
    case cell of
        Given _ ->
            True

        Fixed _ _ ->
            True

        Possible _ _ ->
            False


isGiven : Cell -> Bool
isGiven cell =
    case cell of
        Given _ ->
            True

        Fixed _ _ ->
            False

        Possible _ _ ->
            False


getNumber : Cell -> Maybe Number
getNumber cell =
    case cell of
        Given number ->
            Just number

        Fixed number _ ->
            Just number

        Possible _ _ ->
            Nothing


getPossible : Cell -> Maybe (List Number)
getPossible cell =
    case cell of
        Given _ ->
            Nothing

        Fixed _ _ ->
            Nothing

        Possible possible _ ->
            Just possible


setPossible : List Number -> Cell -> Cell
setPossible possible cell =
    case cell of
        Given _ ->
            cell

        Fixed _ _ ->
            cell

        Possible _ notes ->
            Possible possible notes


getNotes : Cell -> Maybe Notes
getNotes cell =
    case cell of
        Given _ ->
            Nothing

        Fixed _ notes ->
            Just notes

        Possible _ notes ->
            Just notes


default : Cell
default =
    Possible [] { primary = [], secondary = [] }



-- Encode/Decode


encode : Cell -> Encode.Value
encode cell =
    case cell of
        Given num ->
            Encode.object
                [ ( "type", Encode.string "given" )
                , ( "number", Number.encode num )
                ]

        Fixed num notes ->
            Encode.object
                [ ( "type", Encode.string "fixed" )
                , ( "number", Number.encode num )
                , ( "notes", encodeNotes notes )
                ]

        Possible nums notes ->
            Encode.object
                [ ( "type", Encode.string "possible" )
                , ( "possibleNums", Encode.list Number.encode nums )
                , ( "notes", encodeNotes notes )
                ]


decoder : Decoder Cell
decoder =
    Decode.field "type" Decode.string
        |> Decode.andThen
            (\str ->
                case str of
                    "given" ->
                        decodeGiven

                    "fixed" ->
                        decodeFixed

                    "possible" ->
                        decodePossible

                    _ ->
                        Decode.fail "Invalid cell type"
            )


decodeGiven : Decoder Cell
decodeGiven =
    Decode.map Given (Decode.field "number" Number.decoder)


decodeFixed : Decoder Cell
decodeFixed =
    Decode.map2 Fixed
        (Decode.field "number" Number.decoder)
        (Decode.field "notes" notesDecoder)


decodePossible : Decoder Cell
decodePossible =
    Decode.map2 Possible
        (Decode.field "possibleNums" (Decode.list Number.decoder))
        (Decode.field "notes" notesDecoder)



-- Notes Type


type alias Notes =
    { primary : List Number
    , secondary : List Number
    }


encodeNotes : Notes -> Encode.Value
encodeNotes notes =
    Encode.object
        [ ( "primary", Encode.list Number.encode notes.primary )
        , ( "secondary", Encode.list Number.encode notes.secondary )
        ]


notesDecoder : Decoder Notes
notesDecoder =
    Decode.map2 Notes
        (Decode.field "primary" (Decode.list Number.decoder))
        (Decode.field "secondary" (Decode.list Number.decoder))
