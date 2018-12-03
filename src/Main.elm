module Main exposing (main)

import Browser exposing (Document)
import Calculator.AST exposing (..)
import Calculator.Eval exposing (..)
import Calculator.Parser exposing (..)
import Calculator.Tree exposing (..)
import Css as CSS exposing (..)
import Css.Global as CSS
import Custom exposing (..)
import Html as Native
import Html.Styled as Styled exposing (text, toUnstyled)
import Html.Styled.Attributes as Styled exposing (..)
import Html.Styled.Events as Styled exposing (..)
import String as String
import Svg.Attributes as Svg
import Theme exposing (theme)
import TreeDiagram as TreeDiagram exposing (..)


type alias Model =
    Maybe (Tree String)


type Msg
    = ExpressionChanged String


handleExpressionChange : String -> Model -> Model
handleExpressionChange str model =
    if String.isEmpty str then
        Nothing

    else
        case parse str of
            Err _ ->
                model

            Ok expr ->
                createEqualsNode expr


update : Msg -> Model -> Model
update msg model =
    case msg of
        ExpressionChanged str ->
            handleExpressionChange str model


syntaxTree : Model -> Styled.Html Msg
syntaxTree model =
    Maybe.withDefault
        (div [])
        (Maybe.andThen (\m -> Just <| treeAsHtml m) model)


view : Model -> Native.Html Msg
view model =
    toUnstyled <|
        div
            [ header
                [ h6
                    [ text "elm-calculator" ]
                ]
            , div
                [ input
                    [ onInput ExpressionChanged
                    , autocomplete False
                    , autofocus True
                    , placeholder "Type your math expression here"
                    ]
                    []
                , syntaxTree model
                ]
            ]


pageCSS : Native.Html msg
pageCSS =
    toUnstyled <|
        CSS.global
            [ CSS.each [ CSS.body, CSS.header, CSS.h6, CSS.div, CSS.input ]
                [ margin auto
                , marginBottom <| px 6
                , padding zero
                , CSS.width <| pct 100
                , CSS.height <| pct 100
                , textAlign center
                , fontFamily monospace
                , backgroundColor theme.lightPrimary
                ]
            , CSS.h6
                [ fontSize <| px 48
                , backgroundColor theme.darkPrimary
                , color theme.textOrIcons
                ]
            , CSS.input
                [ fontSize <| px 40
                , CSS.width <| pct 98
                , backgroundColor theme.textOrIcons
                , color theme.primaryText
                , CSS.outline CSS.zero
                ]
            , CSS.svg
                [ textAlign center
                , CSS.descendants
                    [ CSS.selector "g"
                        [ textAlign center
                        ]
                    ]
                ]
            ]


document : Model -> Document Msg
document model =
    { title = "elm-calculator"
    , body =
        [ pageCSS
        , view model
        ]
    }

main : Program () Model Msg
main =
    Browser.document
        { init = \_ -> ( Nothing, Cmd.none )
        , view = document
        , update = \msg model -> ( update msg model, Cmd.none )
        , subscriptions = \_ -> Sub.none
        }
