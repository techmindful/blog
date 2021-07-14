module Common.Styles exposing (..)

import Element
    exposing
        ( padding
        , spacingXY
        )
import Element.Border as Border
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


roundedBorder : List (Element.Attribute msg)
roundedBorder =
    [ Border.width 2
    , Border.rounded 6
    , padding 5
    ]


squareBorder : Int -> List (Element.Attribute msg)
squareBorder paddingPx =
    [ Border.width 2
    , padding paddingPx
    ]


edges =
    { top = 0, bottom = 0, left = 0, right = 0 }
