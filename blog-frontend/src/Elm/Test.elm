module Elm.Test exposing
    ( ElmTestResp(..)
    , ElmTestResult(..)
    , elmTestRespDecoder
    , elmTestRespView
    , elmTestResultsView
    , internalServerErrorView
    , noRespView_
    )

import Common.Colors
    exposing
        ( green
        , red
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
import Element.Font as Font
import Elm.Compiler
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


type ElmTestResp
    = CompilerError_ String
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
        [ map CompilerError_ <| field "compilerError" string
        , map Results <| list elmTestResultDecoder
        , null InternalServerError
        ]


elmTestResultsView :
    List ElmTestResult
    -> List String
    -> (Failure -> Element msg)
    -> (String -> Element msg)
    -> Element msg
elmTestResultsView results allExpected failedCaseView passedCaseView =
    let
        failedCasesView =
            column
                [ spacing 20
                , width fill
                ]
            <|
                List.map failedCaseView <|
                    getFailedCases results

        passedCasesView =
            column
                [ spacing 10
                , width fill
                ]
            <|
                List.map passedCaseView <|
                    getPassedCases results allExpected
    in
    column
        [ width fill ]
        [ el
            [ Border.widthEach { bottom = 2, top = 0, left = 0, right = 0 }
            , paddingEach { bottom = 5, top = 0, left = 0, right = 0 }
            , width fill
            ]
          <|
            if areAllResultsPassed results then
                el [ Font.color green ] <| sizedText 24 "Test Passed"

            else
                el [ Font.color red ] <| sizedText 24 "Test Failed"
        , column
            [ paddingEach { top = 15, bottom = 0, left = 0, right = 0 }
            , spacing 20
            , width fill
            ]
            [ failedCasesView
            , passedCasesView
            ]
        ]


{-| A default view for elm-test results
-}
elmTestResultsView_ : List ElmTestResult -> List String -> Element msg
elmTestResultsView_ results allExpected =
    elmTestResultsView results allExpected failedCaseView_ passedCaseView_


{-| A default view for elm-test failed case.
-}
failedCaseView_ : Failure -> Element msg
failedCaseView_ failure =
    column
        [ spacing 10 ]
        [ plainPara <| "Expected: " ++ failure.expected
        , plainPara <| "Actual: " ++ failure.actual
        ]


{-| A default view for elm-test passed case.
-}
passedCaseView_ : String -> Element msg
passedCaseView_ expected =
    plainPara <| "Passed: " ++ expected


internalServerErrorView : Element msg
internalServerErrorView =
    text "Error: Server encountered an error."


noRespView_ : Element msg
noRespView_ =
    text "Run the code and result will be displayed."


{-| A default view for ElmTestResp.
-}
elmTestRespView : Maybe ElmTestResp -> List String -> Element msg
elmTestRespView maybeResp allExpected =
    el
        [ padding 10
        , width fill
        , Border.width 2
        ]
    <|
        case maybeResp of
            Nothing ->
                noRespView_

            Just resp ->
                case resp of
                    CompilerError_ compilerErrorStr ->
                        Elm.Compiler.errorView compilerErrorStr

                    Results results ->
                        elmTestResultsView_ results allExpected

                    InternalServerError ->
                        internalServerErrorView


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
