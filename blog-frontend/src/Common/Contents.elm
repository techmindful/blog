module Common.Contents exposing
    ( boldText
    , borderedButton
    , codeBlock
    , codeBlock_
    , codeBlock__
    , httpErrorView
    , inlineCode
    , italicText
    , limitedLengthInput
    , limitedLengthMultiline
    , plainImage
    , plainPara
    , sizedText
    , underlinedLink
    , underlinedLink_
    , underlinedNewTabLink
    , underlinedNewTabLink_
    , underlinedText
    , wordBreakPara
    )

import Common.Colors exposing (..)
import Common.Styles exposing (roundedBorder, squareBorder)
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
        , spacing
        , text
        , width
        )
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font
import Element.Input as Input exposing (button)
import Html
import Html.Attributes as HtmlAttr exposing (style)
import Http
import String.Extra as String
import Utils.Networking exposing (httpErrorToStr)


sizedText : Int -> String -> Element msg
sizedText fontSize str =
    el [ Font.size fontSize ] (text str)


boldText : String -> Element msg
boldText str =
    el [ Font.bold ] (text str)


italicText : String -> Element msg
italicText str =
    el
        [ Font.italic ]
        (text str)


underlinedText : String -> Element msg
underlinedText str =
    el
        [ Font.underline ]
        (text str)


plainPara : String -> Element msg
plainPara str =
    paragraph [] [ text str ]


wordBreakPara : String -> Element msg
wordBreakPara str =
    paragraph
        [ Element.htmlAttribute <| style "overflow-wrap" "anywhere" ]
        [ text str ]


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


borderedButton : Maybe msg_ -> String -> Element msg_
borderedButton maybeMsg labelStr =
    button
        roundedBorder
        { onPress = maybeMsg
        , label = Element.text labelStr
        }


inlineCode : String -> Element msg
inlineCode str =
    el
        [ Background.color codeGray
        , Border.rounded 2
        , paddingXY 4 1
        , Font.family [ Font.monospace ]
        ]
        (sizedText 18 str)


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


limitedLengthInput :
    Int
    -> List (Element.Attribute msg)
    ->
        { onChange : String -> msg
        , text : String
        , placeholder : Maybe (Input.Placeholder msg)
        , label : Input.Label msg
        }
    -> Element msg
limitedLengthInput maxLength attrs r =
    column
        [ width fill
        , spacing 10
        ]
        [ Input.text attrs r
        , if String.length r.text > maxLength then
            paragraph
                [ Font.color red ]
                [ text <|
                    "Input length is limited at "
                        ++ String.fromInt maxLength
                        ++ " characters."
                ]

          else
            Element.none
        ]


limitedLengthMultiline :
    Int
    -> List (Element.Attribute msg)
    ->
        { onChange : String -> msg
        , text : String
        , placeholder : Maybe (Input.Placeholder msg)
        , label : Input.Label msg
        , spellcheck : Bool
        }
    -> Element msg
limitedLengthMultiline maxLength attrs r =
    column
        [ width fill
        , spacing 10
        ]
        [ Input.multiline attrs r
        , if String.length r.text > maxLength then
            paragraph
                [ Font.color red ]
                [ text <|
                    "Input length is limited at "
                        ++ String.fromInt maxLength
                        ++ " characters."
                ]

          else
            Element.none
        ]


httpErrorView : Http.Error -> Element msg
httpErrorView error =
    paragraph
        (squareBorder 10 ++ [ Font.color red ])
        [ text <| "HTTP Error: " ++ httpErrorToStr error ]
