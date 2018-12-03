module Calculator.Eval exposing (eval)

import Calculator.AST exposing (..)


eval : Expression -> Number
eval expr =
    case expr of
        Value x ->
            x

        Neg x ->
            case eval x of
                Integer a ->
                    Integer (negate a)

                Double b ->
                    Double (negate b)

        Abs x ->
            case eval x of
                Integer a ->
                    Integer (abs a)

                Double b ->
                    Double (abs b)

        Add x y ->
            case ( eval x, eval y ) of
                ( Integer a, Integer b ) ->
                    Integer (a + b)

                ( Integer a, Double b ) ->
                    Double (toFloat a + b)

                ( Double a, Integer b ) ->
                    Double (a + toFloat b)

                ( Double a, Double b ) ->
                    Double (a + b)

        Sub x y ->
            case ( eval x, eval y ) of
                ( Integer a, Integer b ) ->
                    Integer (a - b)

                ( Integer a, Double b ) ->
                    Double (toFloat a - b)

                ( Double a, Integer b ) ->
                    Double (a - toFloat b)

                ( Double a, Double b ) ->
                    Double (a - b)

        Mul x y ->
            case ( eval x, eval y ) of
                ( Integer a, Integer b ) ->
                    Integer (a * b)

                ( Integer a, Double b ) ->
                    Double (toFloat a * b)

                ( Double a, Integer b ) ->
                    Double (a * toFloat b)

                ( Double a, Double b ) ->
                    Double (a * b)

        Div x y ->
            case ( eval x, eval y ) of
                ( Integer a, Integer b ) ->
                    Double (toFloat a / toFloat b)

                ( Integer a, Double b ) ->
                    Double (toFloat a / b)

                ( Double a, Integer b ) ->
                    Double (a / toFloat b)

                ( Double a, Double b ) ->
                    Double (a / b)

        Exp x y ->
            case ( eval x, eval y ) of
                ( Integer a, Integer b ) ->
                    Integer (a ^ b)

                ( Integer a, Double b ) ->
                    Double (toFloat a ^ b)

                ( Double a, Integer b ) ->
                    Double (a ^ toFloat b)

                ( Double a, Double b ) ->
                    Double (a ^ b)

        Nested e ->
            eval e
