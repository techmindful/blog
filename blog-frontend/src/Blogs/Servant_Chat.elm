module Blogs.Servant_Chat exposing
    ( Model
    , Msg
    , init
    , titleStr
    , view
    )

import Blogs.Common.Contents exposing (title)
import Common.Contents
    exposing
        ( codeBlock
        , codeBlock__
        , inlineCode
        , italicText
        , plainImage
        , plainPara
        , underlinedNewTabLink
        )
import Common.Styles
    exposing
        ( blogViewPadding
        , paraSpacing
        )
import Element
    exposing
        ( Element
        , column
        , el
        , fill
        , paragraph
        , text
        , width
        )


type Msg
    = Default


type alias Model =
    { a : Int }


titleStr =
    "WebSockets in Servant"


init : Model
init =
    { a = 3 }


view : Model -> Element Msg
view model =
    column
        [ width fill
        , blogViewPadding
        , paraSpacing
        ]
        [ title titleStr
        , paragraph
            []
            [ text "I needed to select a server framework, for buliding the chat server of "
            , underlinedNewTabLink "https://github.com/techmindful/hideout" "Hideout"
            , text
                """, a private chat service. Haskell Servant stood out, for its strong type safety at levels as high as the API description. Modern chat applications use WebSockets, instead of REST API. This is because when the server receives a message from one client, it should relay the message to other clients right away. With a REST API, the server only has the opportunity to do so when the client initiates a request, typically from polling. In contrast, with an established websocket conneciton, the server has the freedom to send the message to the client immediately."""
            ]
        , paragraph
            []
            [ text "However, I struggled to find any instruction about using websockets with Servant. "
            , underlinedNewTabLink "https://docs.servant.dev/en/stable/" "The official docs"
            , text
                """ were fairly comprehensive on many other topics, but a section for websockets is missing. From search engine, I could only find two open-source projects as examples, and scarce forum discussions. Hence, I'm motivated to write a reasonably detailed guide on the topic. Those who aren't too familiar with Servant yet, or those who are fast-paced and skipped imperative languages when learning web dev like I did, may find this guide helpful."""
            ]
        , paragraph
            []
            [ text "I'm using the handy "
            , underlinedNewTabLink "https://docs.haskellstack.org/en/stable/README/" "Haskell Stack"
            , text " as the build tool. You are free to use others. After "
            , inlineCode "stack new <project-name>"
            , text ", Let's first put some basic packages in "
            , inlineCode "package.yaml"
            , text ", under dependencies:"
            ]
        , codeBlock__ False
            """
- base >= 4.7 && < 5
- bytestring
- containers
- errors
- mtl
- servant
- servant-server
- text
- wai
- warp
- websockets
            """
        , paragraph
            []
            [ text "Let's then feed "
            , inlineCode "app/Main.hs"
            , text " a bunch of language extensions."
            ]
        , codeBlock__ False
            """
{-# language DataKinds #-}
{-# language DeriveGeneric #-}
{-# language DuplicateRecordFields #-}
{-# language FlexibleContexts #-}
{-# language LambdaCase #-}
{-# language OverloadedLabels #-}
{-# language OverloadedStrings #-}
{-# language ScopedTypeVariables #-}
{-# language TemplateHaskell #-}
{-# language TypeOperators #-}
            """
        , paragraph
            []
            [ text "Let's define the API now. But what is Servant's type for serving a websocket API endpoint? Turns out we need such a type from "
            , inlineCode "servant-websockets"
            , text " ("
            , underlinedNewTabLink "https://github.com/moesenle/servant-websockets" "credits"
            , text "), which isn't a default package from Servant. You can find the type documented "
            , underlinedNewTabLink "https://hackage.haskell.org/package/servant-websockets-2.0.0/docs/Servant-API-WebSocket.html#t:WebSocket" "here"
            , text ". So let's first put "
            , inlineCode "servant-websockets >= 2.0.0"
            , text " in "
            , inlineCode "package.yaml"
            , text "."
            ]
        , plainPara "To import the API endpoint type, we do:"
        , codeBlock__ False
            """
import           Servant.API.WebSocket ( WebSocket )
            """
        , plainPara "To use it, we do:"
        , codeBlock__ False
            """
type API = "ws" :> WebSocket
            """
        , plainPara "That wasn't so obvious, was it? Now let's fill in the remaining Servant boilerplate:"
        , codeBlock__ True
            """
server :: Servant.Server API
server = wsHandler


api :: Servant.Proxy API
api = Servant.Proxy


mkApp :: Servant.Application
mkApp =
  Servant.serve api server


main :: IO ()
main = do
  Warp.run 9100 mkApp


wsHandler :: ???
wsHandler = ???
            """
        , paragraph
            []
            [ text
                """
                Until the moment I'm writing these words, I still haven't fully understood what the various functions here do, let alone what's going on behind the scene. But there is absolutely no need to worry. Turns out mere mimicking/pattern-matching with the examples from
                """
            , underlinedNewTabLink "https://docs.servant.dev/en/stable/" "the official docs"
            , text " is enough for one to go very far!"
            ]
        , paragraph
            []
            [ text "Let's write out "
            , inlineCode "wsHandler"
            , text ". So to define a websocket endpoint in our "
            , inlineCode "API"
            , text " type, we used "
            , inlineCode "servant-websockets"
            , text ". Now to actually \"do something\" through the websocket connection, we need the "
            , inlineCode "websockets"
            , text " package ("
            , underlinedNewTabLink "https://github.com/jaspervdj/websockets" "credits"
            , text ")."
            ]
        , paragraph
            []
            [ text "Having included the package name in our "
            , inlineCode "package.yaml"
            , text ", let's import its relevant modules:"
            ]
        , codeBlock__ False
            """
import qualified Network.WebSockets as NetWS
            """
        , paragraph
            []
            [ text "The type of "
            , inlineCode "wsHandler"
            , text " is:"
            ]
        , codeBlock__ False
            """
wsHandler :: NetWS.Connection -> Servant.Handler ()
            """
        , paragraph
            []
            [ text "This means "
            , inlineCode "wsHandler"
            , text
                " is a Servant handler that takes in a websocket connection. The handler doesn't return anything, "
            , italicText "unlike"
            , text
                " its REST counterparts, but it's free to send and receive data, as long as the connection is alive."
            ]
        , paragraph
            []
            [ text "Let's make "
            , inlineCode "wsHandler"
            , text " send some text data!"
            ]
        , codeBlock__ True
            """
wsHandler :: NetWS.Connection -> Servant.Handler ()
wsHandler conn = do
  liftIO $ NetWS.sendTextData conn ( "Test ws msg!!!" :: Text )
            """
        , paragraph
            []
            [ text "Which turns out to be pretty easy, with the "
            , inlineCode "sendTextData"
            , text " function from "
            , underlinedNewTabLink
                "https://jaspervdj.be/websockets/reference/Network-WebSockets.html#v:sendTextData"
                "here"
            , text "."
            ]
        , paragraph
            []
            [ text "I can't wait to test our Servant websocket handler! A good way to test websockets is with a little html and javascript:"
            ]
        , codeBlock__ True
            """
<!DOCTYPE html>
<html>
<head>
  <meta charset="UTF-8">
</head>
<body>
  <script>
    ws = new WebSocket("ws://localhost:9100/ws");
    ws.onopen = function (event) {
      console.log("ws is open.");
    };
    ws.onmessage = function (e) {
      console.log(e.data);
    };
  </script>
</body>
            """
        , paragraph
            []
            [ text "Now if we do "
            , inlineCode "stack install"
            , text ", run the built program, and open the html in a browser, we can see from the console:"
            ]
        , codeBlock__ False
            """
ws is open.
Test ws msg!!!
            """
        , plainImage "/static/blogs/servant-chat/console.png" "A screenshot of the browser console"
        , plainPara "That is a crucial first step toward a chat server! I'm still pondering whether, and how to write the second part. But for now, I may have just covered enough of the obscure bits, like which packages to install, which functions to use, where to look at, for you to figure out the rest of the whole implementation, which is pretty fun to try on your own!"
        ]
