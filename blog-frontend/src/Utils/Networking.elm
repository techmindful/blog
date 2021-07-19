module Utils.Networking exposing
    ( httpErrorToStr
    , plainPutReq
    , utf8StringBody
    )

import Common.Colors exposing (red)
import Common.Styles exposing (squareBorder)
import Element
    exposing
        ( Element
        , paragraph
        , text
        )
import Element.Font as Font
import Http


{-| A PUT request with no headers, no timeout, no tracking.
-}
plainPutReq : String -> Http.Body -> Http.Expect msg -> Cmd msg
plainPutReq url body expect =
    Http.request
        { method = "PUT"
        , headers = []
        , url = url
        , body = body
        , expect = expect
        , timeout = Nothing
        , tracker = Nothing
        }


utf8StringBody : String -> Http.Body
utf8StringBody payload =
    Http.stringBody "text/plain;charset=utf-8" payload


httpErrorToStr : Http.Error -> String
httpErrorToStr err =
    case err of
        Http.BadUrl str ->
            "Bad URL " ++ str

        Http.Timeout ->
            "Timeout"

        Http.NetworkError ->
            """
            Can't reach server. Either your network has a problem, or server is down.
            """

        Http.BadStatus code ->
            "Bad status code: " ++ String.fromInt code

        Http.BadBody str ->
            "Bad response body: " ++ str
