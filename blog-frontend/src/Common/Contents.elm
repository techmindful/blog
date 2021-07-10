module Common.Contents exposing
    ( borderedButton
    , codeBlock
    , codeBlock_
    , codeBlock__
    , inlineCode
    , plainPara
    , underlinedLink
    , underlinedLink_
    , underlinedNewTabLink
    , underlinedNewTabLink_
    )

import Common.Colors exposing (..)
import Common.Styles exposing (roundedBorder)
import Element
    exposing
        ( Element
        , column
        , el
        , link
        , newTabLink
        , padding
        , paddingXY
        , paragraph
        , rgb255
        , text
        )
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font
import Element.Input as Input exposing (button)
import Html
import Html.Attributes as HtmlAttr
import String.Extra as String


plainPara : String -> Element msg
plainPara str =
    paragraph [] [ text str ]


underlinedLink : String -> String -> Element msg
underlinedLink url txt =
    link
        [ Font.underline ]
        { url = url
        , label = Element.text txt
        }


underlinedLink_ : String -> Element msg
underlinedLink_ url =
    link
        [ Font.underline ]
        { url = url
        , label = text url
        }


underlinedNewTabLink : String -> String -> Element msg
underlinedNewTabLink url txt =
    newTabLink
        [ Font.underline ]
        { url = url
        , label = Element.text txt
        }


underlinedNewTabLink_ : String -> Element msg
underlinedNewTabLink_ url =
    newTabLink
        [ Font.underline ]
        { url = url
        , label = text url
        }


borderedButton : msg_ -> String -> Element msg_
borderedButton msg labelStr =
    button
        roundedBorder
        { onPress = Just msg
        , label = Element.text labelStr
        }


inlineCode : String -> Element msg
inlineCode str =
    el
        [ Background.color codeGray ]
        (text str)


mkCodeBlock : String -> Element msg
mkCodeBlock str =
    el
        [ Border.width 2
        , padding 20
        , Background.color codeGray
        ]
    <|
        Element.html <|
            Html.code
                []
                [ Html.text str ]


{-| Code block, not trimming any whitespace.
-}
codeBlock =
    mkCodeBlock


{-| Code block, trimming initial whitespaces
-}
codeBlock_ =
    mkCodeBlock << String.trimLeft


{-| Code block, trimming whitespaces on both ends.
-}
codeBlock__ =
    mkCodeBlock << String.trim
