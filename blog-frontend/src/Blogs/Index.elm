module Blogs.Index exposing
    ( findByTitle
    , index
    )

import AssocList
import Blogs.Emojis_In_Elm
import Blogs.Inc
import Blogs.Servant_Chat
import Blogs.Types exposing (Blog)
import Element exposing (Element)
import List.Extra as List


index : Blogs.Types.Models -> List Blog
index models =
    [ Blog (processTitle Blogs.Emojis_In_Elm.titleStr) <|
        Element.map
            Blogs.Types.Emojis_In_Elm_Msg
        <|
            Blogs.Emojis_In_Elm.view models.emojis_In_Elm_Model
    , Blog (processTitle Blogs.Servant_Chat.titleStr) <|
        Element.map
            Blogs.Types.Servant_Chat_Msg
        <|
            Blogs.Servant_Chat.view models.servant_Chat_Model
    , Blog (processTitle Blogs.Inc.title) <|
        Element.map
            Blogs.Types.Inc_Msg
        <|
            Blogs.Inc.view models.inc_Model
    ]


findByTitle : Blogs.Types.Models -> String -> Maybe Blog
findByTitle models title =
    List.find
        (\blog -> blog.title == title)
        (index models)


processTitle : String -> String
processTitle title =
    title
        |> String.replace " " "-"
        |> String.map Char.toLower
