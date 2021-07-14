module Common.Contents exposing
    ( borderedButton
    , codeBlock
    , codeBlock_
    , codeBlock__
    , inlineCode
    , plainImage
    , plainPara
    , sizedText
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
        , fill
        , image
        , link
        , newTabLink
        , padding
        , paddingXY
        , paragraph
        , rgb255
        , text
        , width
        )
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font
import Element.Input as Input exposing (button)
import Html
import Html.Attributes as HtmlAttr exposing (style)
import String.Extra as String


sizedText : Int -> String -> Element msg
sizedText fontSize str =
    el
        [ Font.size fontSize ]
        (text str)


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


mkCodeBlock : Bool -> String -> Element msg
mkCodeBlock widthFill str =
    el
        ([ Border.width 2
         , padding 20
         , Background.color codeGray
         ]
            ++ (if widthFill then
                    [ width fill ]

                else
                    []
               )
        )
    <|
        Element.html <|
            Html.code
                [ style "white-space" "pre-wrap"
                , style "word-wrap" "break-word"
                ]
                [ Html.text str ]


{-| Code block, not trimming any whitespace.
-}
codeBlock =
    mkCodeBlock


{-| Code block, trimming initial whitespaces
-}
codeBlock_ widthFill =
    mkCodeBlock widthFill << String.trimLeft


{-| Code block, trimming whitespaces on both ends.
-}
codeBlock__ widthFill =
    mkCodeBlock widthFill << String.trim


plainImage : String -> String -> Element msg
plainImage src description =
    image
        []
        { src = src
        , description = description
        }
