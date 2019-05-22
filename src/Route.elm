module Route exposing (Route(..), parse)

import Url exposing (Url)
import Url.Parser exposing ((</>), Parser, map, oneOf, parse, string, top)


type Route
    = Top


parse : Url -> Maybe Route
parse url =
    Url.Parser.parse parser url


parser : Parser (Route -> a) a
parser =
    oneOf
        [ map Top top
        ]
