module Common.Urls exposing (blogApisRoot)

import Url.Builder
    exposing
        ( absolute
        , relative
        )


blogApisRoot : String
blogApisRoot =
    absolute [ "blog-apis" ] []
