module Main exposing (..)

import Browser
import Html exposing (Html)
import Html.Events
import Signup
import Ui.Form
import Ui.Form2


main : Program () Model Msg
main =
    Browser.element
        { init = always init
        , view = view
        , update = update
        , subscriptions = always Sub.none
        }


type alias Model =
    { signupForm : Ui.Form.Model Signup.Editor }


type Msg
    = GotSignupMsg (Ui.Form.Msg Signup.Editor)
    | GotSignupOnSubmit Signup.Record


signupModule : Ui.Form.Module Signup.Editor Model Msg
signupModule =
    Signup.form { toMsg = GotSignupMsg, onSubmit = GotSignupOnSubmit }


init : ( Model, Cmd Msg )
init =
    signupModule.init
        ( \sf -> { signupForm = sf }, Cmd.none )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        GotSignupMsg signupMsg ->
            signupModule.update signupMsg model

        GotSignupOnSubmit record ->
            let
                _ =
                    Debug.log "SUBMITTED" record
            in
            ( model, Cmd.none )


view : Model -> Html Msg
view model =
    Html.div []
        [ Html.div []
            [ Html.div [] <| signupModule.elements.fields model
            , Html.button [ Html.Events.onClick signupModule.elements.submitMsg ] [ Html.text "Submit" ]
            , Html.hr [] []
            , Debug.toString model |> Html.text
            ]
        ]



{-
   element :
       { init : flags -> ( model, Cmd msg )
       , view : model -> Html msg
       , update : msg -> model -> ( model, Cmd msg )
       , subscriptions : model -> Sub msg
       }
       -> Program flags model msg
-}


type alias Msg_ =
    ( Model -> Model, Cmd (Model -> Model) )


update_ : Msg_ -> Model -> ( Model, Cmd Msg_ )
update_ ( msg, cmd ) model =
    ( msg model
    , Cmd.map (\c -> ( c, Cmd.none )) cmd
    )


mapMsg : Msg -> Msg_
mapMsg classic =
    ( \model ->
        update classic model
            |> Tuple.first
    , Cmd.none
    )


withoutIo : Msg -> (Model -> Model)
withoutIo msg =
    \model ->
        update msg model
            |> Tuple.first


mapMsg3 : Msg -> (Model -> ( Model, Cmd (Model -> Model) ))
mapMsg3 msg =
    \model ->
        update msg model
            |> (\( updatedModel, cmdMsg ) ->
                    ( updatedModel
                    , Cmd.map withoutIo cmdMsg
                    )
               )


etadpu : Msg -> ( Msg_, Cmd Msg_ )
etadpu classic =
    Debug.todo ""
