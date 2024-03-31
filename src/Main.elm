module Main exposing (..)

import Browser
import Html exposing (Html)
import Signup
import Ui.Form


main : Program () Model Msg
main =
    Browser.element
        { init = always init
        , view = view
        , update = update
        , subscriptions = always Sub.none
        }


type alias Model =
    {}


type Msg
    = Msg


init : ( Model, Cmd Msg )
init =
    ( {}, Cmd.none )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    ( model, Cmd.none )


view : Model -> Html Msg
view model =
    Debug.toString model |> Html.text



{-
   element :
       { init : flags -> ( model, Cmd msg )
       , view : model -> Html msg
       , update : msg -> model -> ( model, Cmd msg )
       , subscriptions : model -> Sub msg
       }
       -> Program flags model msg
-}
