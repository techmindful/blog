module Elm.Compiler exposing
    ( Error
    , errorView
    )

import Common.Colors exposing (red)
import Common.Contents
    exposing
        ( plainPara
        , sizedText
        )
import Element
    exposing
        ( Element
        , column
        , el
        , fill
        , padding
        , paddingEach
        , paragraph
        , spacing
        , text
        , width
        )
import Element.Border as Border
import Element.Font as Font
import Elm.Shared exposing (title)
import Html
import Html.Attributes as HtmlAttr
import Json.Decode
    exposing
        ( Decoder
        , andThen
        , bool
        , field
        , list
        , map
        , map2
        , null
        , oneOf
        , string
        , succeed
        )
import List.Extra as List
import Parser
    exposing
        ( (|.)
        , (|=)
        , Parser
        , chompIf
        , chompUntil
        , chompUntilEndOr
        , chompWhile
        , getChompedString
        )
import String.Extra as String


type alias Error =
    { errorType : String
    , errorMsg : String
    }


errorView : String -> Element msg
errorView compilerErrorStr =
    column
        [ width fill
        , spacing 15
        , Font.family [ Font.monospace ]
        ]
        [ el
            [ Border.widthEach { bottom = 2, top = 0, left = 0, right = 0 }
            , paddingEach { bottom = 5, top = 0, left = 0, right = 0 }
            , width fill
            ]
            (el
                [ Font.color red, Font.size 24 ]
                (text "Compiler Error")
            )
        , case Parser.run compilerErrorParser compilerErrorStr of
            Err _ ->
                plainPara "Error: Can't parse compiler error."

            Ok compilerError ->
                column
                    [ spacing 20 ]
                    [ text compilerError.errorType
                    , Element.html <|
                        Html.p
                            [ HtmlAttr.style "white-space" "pre-wrap"
                            , HtmlAttr.style "overflow-wrap" "anywhere"
                            ]
                            [ Html.text compilerError.errorMsg ]
                    ]
        ]


compilerErrorParser : Parser Error
compilerErrorParser =
    Parser.succeed Error
        |. Parser.symbol "-- "
        |= (Parser.map (String.toTitleCase << String.toLower) <|
                getChompedString <|
                    chompUntil " -"
           )
        |. chompUntil ".elm"
        |. Parser.symbol ".elm"
        |. Parser.spaces
        |= (getChompedString <| chompWhile (\_ -> True))
