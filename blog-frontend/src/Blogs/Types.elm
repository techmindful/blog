module Blogs.Types exposing
    ( Blog
    , Models
    , Msg(..)
    , init
    , update
    )

import Blogs.Emojis_Elm as Emojis_Elm
import Blogs.Inc as Inc
import Element exposing (Element)


type alias Blog =
    { title : String
    , view : Element Msg
    }


type alias Models =
    { emojis_Elm_Model : Emojis_Elm.Model
    , inc_Model : Inc.Model
    }


type Msg
    = Emojis_Elm_Msg Emojis_Elm.Msg
    | Inc_Msg Inc.Msg
    | Other_Msg


init : Models
init =
    { emojis_Elm_Model = Emojis_Elm.init
    , inc_Model = Inc.init
    }


update : Msg -> Models -> ( Models, Cmd msg__ )
update msg_ models =
    case msg_ of
        Emojis_Elm_Msg msg ->
            let
                ( model, cmd ) =
                    Emojis_Elm.update msg models.emojis_Elm_Model
            in
            ( { models | emojis_Elm_Model = model }
            , cmd
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
