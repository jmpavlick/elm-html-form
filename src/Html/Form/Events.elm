module Html.Form.Events exposing (..)

import Html
import Html.Events
import VirtualDom


type alias Event msg =
    VirtualDom.Event msg


onInput : (String -> msg) -> Event msg
onInput =
    Html.Events.onInput
