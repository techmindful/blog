module Blogs.Common.Styles exposing
    ( commentStyle
    , infoStyle
    , noticeStyle
    , warningStyle
    )

import Common.Colors
    exposing
        ( lightBlue
        , lightYellow
        , white
        )
import Element
    exposing
        ( fill
        , padding
        , spacing
        , width
        )
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font


noticeStyle : Element.Color -> List (Element.Attribute msg)
noticeStyle bgColor =
    [ width fill
    , Border.width 2
    , padding 15
    , Background.color bgColor
    , Font.size 18
    ]


warningStyle =
    noticeStyle lightYellow


commentStyle =
    noticeStyle lightBlue


infoStyle =
    noticeStyle white
