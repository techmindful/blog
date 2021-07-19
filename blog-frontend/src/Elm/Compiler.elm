module Elm.Compiler exposing
    ( Error
    , errorView
    )

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
        ]
        [ title "Compiler Error"
        , case Parser.run compilerErrorParser compilerErrorStr of
            Err _ ->
                plainPara "Error: Can't parse compiler error."

            Ok compilerError ->
                column
                    [ spacing 20 ]
                    [ text compilerError.errorType
                    , column
                        [ width fill
                        , spacing 10
                        ]
                      <|
                        List.map
                            (\str ->
                                paragraph
                                    [ Element.htmlAttribute <|
                                        HtmlAttr.style "white-space" "pre-wrap"
                                    , Element.htmlAttribute <|
                                        HtmlAttr.style "overflow-wrap" "anywhere"
                                    ]
                                    [ text str ]
                            )
                            (String.split "\n" compilerError.errorMsg)
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
