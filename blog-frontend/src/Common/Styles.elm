module Common.Styles exposing (..)

import Element
    exposing
        ( padding
        , spacingXY
        )
import Element.Font as Font


linkStyle : List (Element.Attribute msg)
linkStyle =
    [ Font.underline ]


blogViewPadding : Element.Attribute msg
blogViewPadding =
    padding 20


paraSpacing : Element.Attribute msg
paraSpacing =
    spacingXY 0 20
