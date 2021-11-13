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
        ( codeBlock__
        , inlineCode
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
    "Building a Chat Server with Haskell Servant and WebSockets"


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
            , text "I'm using the handy "
            , underlinedNewTabLink "https://docs.haskellstack.org/en/stable/README/" "Haskell Stack"
            , text " as the build tool. You are free to use others. After "
            , inlineCode "stack new <project-name>"
            , text ", I put the following packages in "
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
- servant-websockets >= 2.0.0
- text
- wai
- warp
- wai-cors
- websockets
            """
        , paragraph
            []
            [ text "In the "
            , inlineCode "app/Main.hs"
            , text ", I first laid out the language extensions and imports:"
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

module Main where

import           Control.Monad.IO.Class (liftIO)
import           Control.Monad.Reader ( ReaderT, ask, runReaderT )
import           Data.Text ( Text )
import qualified Network.Wai as Wai
import qualified Network.Wai.Handler.Warp as Warp
import           Network.Wai.Middleware.Cors ( cors, CorsResourcePolicy(..) )
import qualified Network.WebSockets as NetWS
import qualified Servant
import           Servant
    ( (:>)
    , (:<|>)(..)
    , Capture
    , Get
    , NoContent(..)
    , Put
    , ReqBody
    )
import           Servant.API.WebSocket ( WebSocket )
            """
        ]
