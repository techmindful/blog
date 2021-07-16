module Utils.Networking exposing
    ( plainPutReq
    , utf8StringBody
    )

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
