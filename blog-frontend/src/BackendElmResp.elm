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
        , list
        , map
        , map2
        , null
        , oneOf
        , string
        , succeed
        )
import List.Extra as List


type ElmTestResp
    = CompilerError String
    | Results (List ElmTestResult)
    | InternalServerError


type ElmTestResult
    = Fail Failure
    | Pass


type alias Failure =
    { expected : String
    , actual : String
    }


elmTestRespDecoder : Decoder ElmTestResp
elmTestRespDecoder =
    let
        elmTestResultDecoder =
            field "pass" bool
                |> andThen
                    (\pass ->
                        if pass then
                            succeed Pass

                        else
                            map Fail <|
                                map2 Failure
                                    (field "expected" string)
                                    (field "actual" string)
                    )
    in
    oneOf
        [ map CompilerError <| field "compilerError" string
        , map Results <| list elmTestResultDecoder
        , null InternalServerError
        ]


elmTestRespView : Maybe ElmTestResp -> List String -> Element msg
elmTestRespView maybeResp allExpected =
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

                    Results results ->
                        let
                            failedCaseView : Failure -> Element msg
                            failedCaseView failure =
                                column
                                    [ spacing 10 ]
                                    [ plainPara <| "Expected: " ++ failure.expected
                                    , plainPara <| "Actual: " ++ failure.actual
                                    ]

                            failedCasesView =
                                column
                                    [ spacing 20 ]
                                <|
                                    List.map failedCaseView <|
                                        getFailedCases results

                            passedCaseView : String -> Element msg
                            passedCaseView expected =
                                plainPara <| "Passed: " ++ expected

                            passedCasesView =
                                column
                                    [ spacing 10 ]
                                <|
                                    List.map passedCaseView <|
                                        getPassedCases results allExpected
                        in
                        column
                            [ width fill ]
                            [ title <|
                                if areAllResultsPassed results then
                                    "Test Passed"

                                else
                                    "Test Failed"
                            , column
                                [ paddingEach { top = 15, bottom = 0, left = 0, right = 0 }
                                , spacing 20
                                ]
                                [ failedCasesView
                                , passedCasesView
                                ]
                            ]

                    InternalServerError ->
                        text "Error: Server encountered an error."


areAllResultsPassed : List ElmTestResult -> Bool
areAllResultsPassed results =
    List.all (\result -> result == Pass) results


getFailedCases : List ElmTestResult -> List Failure
getFailedCases results =
    List.filterMap
        (\result ->
            case result of
                Pass ->
                    Nothing

                Fail failure ->
                    Just { expected = failure.expected, actual = failure.actual }
        )
        results


{-| Get expected values of test cases that passed from all ElmTestResult's.

    Doing these gymnastics because
    elm-test doesn't output expected/actual
    for passed cases.

-}
getPassedCases : List ElmTestResult -> List String -> List String
getPassedCases results allExpected =
    let
        allFailedExpected =
            List.map
                .expected
                (getFailedCases results)
    in
    List.filterNot
        (\expected -> List.member expected allFailedExpected)
        allExpected
