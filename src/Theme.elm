module Theme exposing (theme)

import Bitwise exposing (..)
import Css exposing (Color, rgb)


mkColor : Int -> Color
mkColor n =
    let
        red =
            and n 0x00FF0000 |> shiftRightBy 16

        green =
            and n 0xFF00 |> shiftRightBy 8

        blue =
            and n 0xFF
    in
    rgb red green blue


theme =
    { darkPrimary = mkColor 0x00455A64
    , lightPrimary = mkColor 0x00CFD8DC
    , primary = mkColor 0x00607D8B
    , textOrIcons = mkColor 0x00FFFFFF
    , accent = mkColor 0x9688
    , primaryText = mkColor 0x00212121
    , secondaryText = mkColor 0x00757575
    , equals = mkColor 0x00FF9800
    }
