module Calculator.AST exposing (Expr(..), Expression, Number(..))


type Number
    = Integer Int
    | Double Float


type Expr a
    = Value a
    | Neg (Expr a)
    | Abs (Expr a)
    | Add (Expr a) (Expr a)
    | Sub (Expr a) (Expr a)
    | Mul (Expr a) (Expr a)
    | Div (Expr a) (Expr a)
    | Exp (Expr a) (Expr a)
    | Nested (Expr a)


type alias Expression =
    Expr Number
