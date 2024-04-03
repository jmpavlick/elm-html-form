module VirtualDom exposing (..)

import Html


type alias Event msg =
    msg -> Html.Attribute msg
