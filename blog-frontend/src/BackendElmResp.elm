module BackendElmResp exposing
    ( ElmTestResp(..)
    , elmTestRespDecoder
    , elmTestRespToTitle
    )

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


elmTestRespToTitle : ElmTestResp -> String
elmTestRespToTitle resp =
    case resp of
        CompilerError _ ->
            "Compiler Error"

        TestFailure _ ->
            "Test Failure"

        Pass ->
            "Passed"

        InternalServerError ->
            "Internal Server Error"


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
