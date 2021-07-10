module BackendElmResp exposing
    ( ElmTestResp(..)
    , elmTestRespDecoder
    , elmTestRespView
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
        , spacing
        , text
        , width
        )
import Element.Border as Border
import Json.Decode
    exposing
        ( Decoder
        , andThen
        , bool
        , field
        , map
        , null
        , oneOf
        , string
        , succeed
        )


type ElmTestResp
    = CompilerError String
    | TestFailure String
    | Pass
    | InternalServerError


elmTestRespView : Maybe ElmTestResp -> String -> Element msg
elmTestRespView maybeResp expected =
    let
        title : String -> Element msg
        title str =
            el
                [ Border.widthEach { bottom = 2, top = 0, left = 0, right = 0 }
                , paddingEach { bottom = 5, top = 0, left = 0, right = 0 }
                , width fill
                ]
                (sizedText 24 str)
    in
    el
        [ padding 10
        , width fill
        , Border.width 2
        ]
    <|
        case maybeResp of
            Nothing ->
                text "Run the code and result will be displayed."

            Just resp ->
                case resp of
                    CompilerError compilerMsg ->
                        column
                            [ width fill
                            , spacing 15
                            ]
                            [ title "Compiler Error"
                            , plainPara compilerMsg
                            ]

                    TestFailure actual ->
                        column
                            [ width fill ]
                            [ title "Incorrect Result"
                            , column
                                [ paddingEach { top = 15, bottom = 0, left = 0, right = 0 }
                                , spacing 10
                                ]
                                [ plainPara <| "Expected: " ++ expected
                                , plainPara <| "Actual: " ++ actual
                                ]
                            ]

                    Pass ->
                        column
                            [ width fill ]
                            [ title "Correct result!"
                            , column
                                [ paddingEach { top = 15, bottom = 0, left = 0, right = 0 }
                                , spacing 10
                                ]
                                [ plainPara <| "Expected: " ++ expected
                                , plainPara <| "Actual: " ++ expected
                                ]
                            ]

                    InternalServerError ->
                        text "Error: Server encountered an error."


elmTestRespDecoder : Decoder ElmTestResp
elmTestRespDecoder =
    oneOf
        [ map CompilerError <| field "compilerError" string
        , field "pass" bool
            |> andThen
                (\pass ->
                    if pass then
                        succeed Pass

                    else
                        map TestFailure <| field "actual" string
                )
        , null InternalServerError
        ]
