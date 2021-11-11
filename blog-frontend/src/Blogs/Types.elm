module Blogs.Types exposing
    ( Blog
    , Models
    , Msg(..)
    , init
    , update
    )

import Blogs.Emojis_In_Elm as Emojis_In_Elm
import Blogs.Inc as Inc
import Blogs.Servant_Chat as Servant_Chat
import Element exposing (Element)


type alias Blog =
    { title : String
    , view : Element Msg
    }


type alias Models =
    { emojis_In_Elm_Model : Emojis_In_Elm.Model
    , servant_Chat_Model : Servant_Chat.Model
    , inc_Model : Inc.Model
    }


type Msg
    = Emojis_In_Elm_Msg Emojis_In_Elm.Msg
    | Servant_Chat_Msg Servant_Chat.Msg
    | Inc_Msg Inc.Msg
    | Other_Msg


init : Models
init =
    { emojis_In_Elm_Model = Emojis_In_Elm.init
    , servant_Chat_Model = Servant_Chat.init
    , inc_Model = Inc.init
    }


update : Msg -> Models -> ( Models, Cmd Msg )
update msg_ models =
    case msg_ of
        Emojis_In_Elm_Msg msg ->
            let
                ( model, cmd ) =
                    Emojis_In_Elm.update msg models.emojis_In_Elm_Model
            in
            ( { models | emojis_In_Elm_Model = model }
            , Cmd.map Emojis_In_Elm_Msg cmd
            )

        Inc_Msg msg ->
            let
                ( model, cmd ) =
                    Inc.update msg models.inc_Model
            in
            ( { models | inc_Model = model }
            , cmd
            )

        _ ->
            ( models, Cmd.none )
