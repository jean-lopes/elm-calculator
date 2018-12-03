module Calculator.Tree exposing (createEqualsNode, tree, treeAsHtml)

import Calculator.AST exposing (..)
import Calculator.Eval exposing (eval)
import Css as CSS exposing (Color, ColorValue)
import Html.Styled exposing (fromUnstyled)
import String as String
import Svg as Svg exposing (Svg)
import Svg.Attributes as Svg
import Theme exposing (theme)
import TreeDiagram exposing (..)
import TreeDiagram.Svg exposing (draw)


numberToStr : Number -> String
numberToStr n =
    case n of
        Integer a ->
            String.fromInt a

        Double b ->
            String.fromFloat b


tree : Expression -> Tree String
tree expr =
    case expr of
        Value n ->
            node (numberToStr n) []

        Neg e ->
            node "-" [ tree e ]

        Abs e ->
            node "abs" [ tree e ]

        Nested e ->
            tree e

        Add x y ->
            node "+" [ tree x, tree y ]

        Sub x y ->
            node "-" [ tree x, tree y ]

        Mul x y ->
            node "*" [ tree x, tree y ]

        Div x y ->
            node "/" [ tree x, tree y ]

        Exp x y ->
            node "^" [ tree x, tree y ]


createEqualsNode : Expression -> Maybe (Tree String)
createEqualsNode expr =
    Just <|
        TreeDiagram.node "="
            [ tree expr
            , TreeDiagram.node (numberToStr <| eval expr) []
            ]


drawLine : ( Float, Float ) -> Svg msg
drawLine ( targetX, targetY ) =
    Svg.line
        [ Svg.x1 <| String.fromFloat 0
        , Svg.y1 <| String.fromFloat 5
        , Svg.x2 <| String.fromFloat targetX
        , Svg.y2 <| String.fromFloat (targetY - 40)
        , Svg.stroke "#212121"
        ]
        []


drawNode : String -> Svg msg
drawNode str =
    Svg.text_
        [ Svg.textAnchor "middle"
        , Svg.fontSize "3rem"
        , if String.any Char.isDigit str then
            Svg.fill "#009688"

          else
            Svg.fill "#455A64"
        ]
        [ Svg.text str ]


layout n =
    { orientation = topToBottom
    , levelHeight = 100
    , siblingDistance = 40 + n * 20
    , subtreeDistance = 80
    , padding = 40
    }


maxLength : Tree String -> Int
maxLength target =
    flatten target
        |> List.map String.length
        |> List.maximum
        |> Maybe.withDefault 0


treeAsHtml : Tree String -> Html.Styled.Html msg
treeAsHtml target =
    let
        length =
            maxLength target
    in
    fromUnstyled <|
        draw (layout length) drawNode drawLine target
