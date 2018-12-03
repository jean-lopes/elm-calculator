module Calculator.Parser exposing (parse)

import Calculator.AST exposing (..)
import Parser as Parser exposing ((|.), (|=), DeadEnd, Parser, Problem(..))
import Parser.Expression as Parser exposing (Assoc(..), Operator, OperatorTable)
import Parser.Extras as Parser
import String exposing (..)


lexeme : Parser a -> Parser a
lexeme p =
    p |. Parser.spaces


symbol : String -> Parser String
symbol str =
    lexeme (Parser.succeed str |. Parser.symbol str)


void : Parser a -> Parser ()
void p =
    Parser.succeed () |. p


integer : Parser Number
integer =
    lexeme <|
        Parser.number
            { int = Just Integer
            , hex = Just Integer
            , octal = Just Integer
            , binary = Just Integer
            , float = Nothing
            }


double : Parser Number
double =
    lexeme <|
        Parser.number
            { int = Nothing
            , hex = Nothing
            , octal = Nothing
            , binary = Nothing
            , float = Just Double
            }


number : Parser Number
number =
    Parser.oneOf [ Parser.backtrackable integer, double ]


value : Parser Expression
value =
    Parser.succeed Value |= number


type alias UnaryExpressionOp =
    Expression -> Expression


type alias BinaryExpressionOp =
    Expression -> Expression -> Expression


unaryOp : UnaryExpressionOp -> String -> Operator Expression
unaryOp constructor str =
    Parser.prefixOperator constructor (symbol str |> void)


binaryOp : BinaryExpressionOp -> String -> Operator Expression
binaryOp constructor str =
    Parser.infixOperator constructor (symbol str |> void) AssocLeft


operators : OperatorTable Expression
operators =
    [ [ unaryOp Neg "-", unaryOp identity "+" ]
    , [ binaryOp Exp "^" ]
    , [ binaryOp Mul "*", binaryOp Div "/" ]
    , [ binaryOp Add "+", binaryOp Sub "-" ]
    ]


closed : String -> Parser p -> String -> Parser p
closed open p close =
    Parser.between (symbol open) (symbol close) <| Parser.lazy (\_ -> p)


term : Parser Expression
term =
    Parser.oneOf
        [ Parser.succeed Nested |= closed "(" expression ")"
        , Parser.succeed Abs |= closed "|" expression "|"
        , value
        ]


expression : Parser Expression
expression =
    Parser.buildExpressionParser operators (Parser.lazy <| \_ -> term)


parse : String -> Result String Expression
parse str =
    case Parser.run (expression |. Parser.end) str of
        Err errors ->
            Err ":("

        Ok expr ->
            Ok expr
