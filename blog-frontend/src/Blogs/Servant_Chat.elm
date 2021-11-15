module Blogs.Servant_Chat exposing
    ( Model
    , Msg
    , init
    , titleStr
    , view
    )

import Blogs.Common.Contents exposing (title)
import Blogs.Common.Styles exposing (commentStyle, infoStyle, warningStyle)
import Common.Colors exposing (lightYellow)
import Common.Contents
    exposing
        ( boldText
        , codeBlock
        , codeBlock__
        , inlineCode
        , italicText
        , plainImage
        , plainPara
        , underlinedNewTabLink
        , underlinedText
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
        , padding
        , paragraph
        , spacing
        , text
        , width
        )
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font


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
            infoStyle
            [ text "Note: This guide may be expanded into a series about building a chat server." ]
        , paragraph
            []
            [ text "I needed to select a server framework, for buliding the chat server of "
            , underlinedNewTabLink "https://github.com/techmindful/hideout" "Hideout"
            , text
                """, a private chat service. Haskell Servant stood out, for its strong type safety at levels as high as the API description. Modern chat applications use WebSockets, instead of REST API. This is because when the server receives a message from one client, it should relay the message to other clients right away. With a REST API, the server only has the opportunity to do so when the client initiates a request, typically from polling. In contrast, with an established websocket conneciton, the server has the freedom to send the message to the client proactively."""
            ]
        , paragraph
            []
            [ text "However, I struggled to find any instruction about using websockets with Servant. "
            , underlinedNewTabLink "https://docs.servant.dev/en/stable/" "The official docs"
            , text
                """ were fairly comprehensive on many other topics, but a section for websockets is missing. From search engine, I could only find two open-source projects as examples ("""
            , underlinedNewTabLink
                "https://github.com/realli/chatqy"
                "realli/chatqy"
            , text " and "
            , underlinedNewTabLink
                "https://github.com/farnoy/chat"
                "farnoy/chat"
            , text """), and scarce forum discussions. Hence, I'm motivated to write a reasonably detailed guide on the topic. Those who aren't too familiar with Servant yet, or those who are fast-paced and skipped imperative languages when learning web dev like I did, may find this guide helpful."""
            ]
        , column
            (warningStyle ++ [ spacing 10 ])
            [ plainPara "Prerequisites:"
            , paragraph
                []
                [ text "  * You need to know how to build a normal REST API with Servant first. This is documented very well in "
                , underlinedNewTabLink "https://docs.servant.dev/en/stable/" "the official docs"
                , text ", so this guide will skip that part."
                ]
            , paragraph
                []
                [ text "* You do "
                , underlinedText "not"
                , text " need to be a type expert."
                ]
            ]
        , paragraph
            []
            [ text "I'm using the handy "
            , underlinedNewTabLink "https://docs.haskellstack.org/en/stable/README/" "Haskell Stack"
            , text
                " as the build tool. You are free to use others. These packages are relevant to a Servant server:"
            ]
        , codeBlock__ False
            """
- servant
- servant-server
- wai
- warp
            """
        , paragraph
            infoStyle
            [ text "Version bounds are omitted, as they may be outdated at the time of your reading. Other basic packages are omitted." ]
        , paragraph
            warningStyle
            [ text "At the time of writing, Stack reported an error when including those packages, and recommended adding some dependencies in "
            , inlineCode "extra-deps"
            , text " in "
            , inlineCode "stack.yaml"
            , text ". Following the recommendation solved the problem."
            ]
        , paragraph
            []
            [ text "A few language extensions are needed:" ]
        , codeBlock__ False
            """
{-# language DataKinds #-}
{-# language OverloadedStrings #-}
{-# language ScopedTypeVariables #-}
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
            , inlineCode "servant-websockets"
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
        , plainPara "Simple enough. Though it wasn't so obvious, was it? Now let's fill in the remaining Servant boilerplate."
        , plainPara "The import list to start with:"
        , codeBlock__ False
            """
import           Control.Monad.IO.Class (liftIO)
import qualified Data.ByteString.Lazy as LazyByteStr
import           Data.Text ( Text )
import qualified Network.Wai.Handler.Warp as Warp
import qualified Servant
import           Servant ( (:>) )
import           Servant.API.WebSocket ( WebSocket )
            """
        , plainPara "The rest of the barebone structure of Servant:"
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
            , text " should be:"
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
                " is a Servant handler that takes in a websocket connection. The handler doesn't return anything right away, "
            , italicText "unlike"
            , text
                " its REST counterparts, but it's free to send and receive data through the websocket connection, as long as it's alive."
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
                "https://www.stackage.org/haddock/lts-18.16/websockets-0.12.7.3/Network-WebSockets.html#v:sendTextData"
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

    // Print a message when ws is open.
    ws.onopen = function (event) {
      console.log("ws is open.");
    };

    // When a msg from server arrives, print it.
    ws.onmessage = function (event) {
      console.log(event.data);
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
        , plainImage
            "/static/blogs/servant-chat/console-server-send.png"
            "A screenshot of the browser console, after the server sends a message through the websocket."
        , paragraph
            []
            [ text "Our server can actively send a message to the client (browser) now! How about receiving a message from the client? For that, we need the "
            , underlinedNewTabLink
                "https://www.stackage.org/haddock/lts-18.16/websockets-0.12.7.3/Network-WebSockets.html#v:receiveDataMessage"
                "receiveDataMessage"
            , text " function. "
            ]
        , paragraph
            []
            [ boldText
                """One very, very important caveat about this function is that, it blocks. So whenever execution reaches this function, it doesn't move on, until the server receives a message from client through the websocket. Keeping this in mind helps us plan our code for now, and will become critical later on when we write the logic for chatting, where the
                """
            , el
                [ Font.bold
                , Font.underline
                ]
                (text "order")
            , boldText " of these IO code absolutely matters."
            ]
        , paragraph
            []
            [ boldText "These IO code, and the blocking nature of this function in particular, had caused me major headache. And it's not mentioned at all." ]
        , paragraph
            []
            [ text "To demonstrate both sending and receiving through the websocket, let's modify "
            , inlineCode "wsHandler"
            , text " to echo the message from client back to the client itself. It's a simple three-step process: Receive message from the client itself; get the lazy ByteString from the message; send the lazy ByteString back to the client."
            ]
        , codeBlock__ True
            """
wsHandler :: NetWS.Connection -> Servant.Handler ()
wsHandler conn = do

  -- *Wait* and receive the message from client.
  -- Execution will *not* continue, until a message is received.
  msgFromClient :: NetWS.DataMessage <-
    liftIO $ NetWS.receiveDataMessage conn

  let byteStrFromClient :: LazyByteStr.ByteString
      byteStrFromClient =
        case msgFromClient of
          NetWS.Text byteStr _ -> byteStr
          NetWS.Binary byteStr -> byteStr

  -- Echo the message back to the client.
  liftIO $ NetWS.sendTextData conn byteStrFromClient
            """
        , paragraph
            []
            [ text "The second step where we get the lazy ByteString from the received message is a little more than expected. It's simply because what's received from "
            , underlinedNewTabLink
                "https://www.stackage.org/haddock/lts-18.16/websockets-0.12.7.3/Network-WebSockets.html#v:receiveDataMessage"
                "receiveDataMessage"
            , text " is of type "
            , underlinedNewTabLink
                "https://www.stackage.org/haddock/lts-18.16/websockets-0.12.7.3/Network-WebSockets.html#t:DataMessage"
                "DataMessage"
            , text ". We get the lazy ByteString based on its two constructors."
            ]
        , paragraph
            []
            [ text "Time for testing again! Let's modify the HTML a little, for sending a message to the server, through the websocket:"
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
      // Send a msg, after ws is open.
      ws.send("Hello, world!");
    };

    // When a msg from server arrives, print it.
    ws.onmessage = function (event) {
      console.log(event.data);
    };
  </script>
</body>
            """
        , plainPara "After opening the HTML, the browser sends \"Hello, world!\" to the server. The server echoes it back, which we can see is then printed in the console."
        , plainImage
            "/static/blogs/servant-chat/console-echo.png"
            "A screenshot of the browser console, after the server echoes back the message."
        , plainPara "We can test by changing the \"Hello, world!\" string to others, and see that server will echo it back correspondingly."
        , plainPara "We have explored sending and receiving messages through websockets with Servant. That is a crucial first step toward a chat server! I'm still pondering whether, and how to write the second part. But for now, I may have just covered enough of the obscure bits, like which packages to install, which functions to use, where to look at, for you to figure out the rest of the whole implementation, which is pretty fun to try on your own!"
        ]
