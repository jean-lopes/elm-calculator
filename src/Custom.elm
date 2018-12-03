module Custom exposing (div, h6, header, input)

import Css as CSS
import Html.Styled as Styled
import Html.Styled.Attributes as Attribute
import Html.Styled.Events as Events


type alias Attribute msg =
    Styled.Attribute msg


type alias Attributes msg =
    List (Attribute msg)


type alias Element msg =
    Styled.Html msg


type alias Elements msg =
    List (Element msg)


type alias Children msg =
    Elements msg


type alias Constructor msg =
    Attributes msg -> Children msg -> Element msg


custom : Constructor msg -> ( Attributes msg, Children msg ) -> Element msg
custom constructor ( attributes, children ) =
    constructor attributes children


div : Children msg -> Element msg
div children =
    custom Styled.div ( [], children )


header : Children msg -> Element msg
header children =
    custom Styled.header ( [], children )


h6 : Children msg -> Element msg
h6 children =
    custom Styled.h6 ( [], children )


input : Attributes msg -> Children msg -> Element msg
input attributes children =
    custom Styled.input ( attributes, children )
