module Main exposing (..)

import Browser
import Form
import Html exposing (Html)
import Html.Attributes as Attr
import Html.Events
import Signup


main : Program () Model Msg
main =
    Browser.element
        { init = always init
        , view = view
        , update = update
        , subscriptions = always Sub.none
        }


type alias Model =
    { signupForm : Form.Model Signup.Editor }


type Msg
    = GotSignupMsg (Form.Msg Signup.Editor)
    | GotSignupOnSubmit Signup.Record
    | GotSideloadedCallback String


signupModule : Form.Module Signup.Editor Model (Signup.Fieldset Msg) Msg
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

        GotSideloadedCallback value ->
            let
                _ =
                    Debug.log "GotSideloadedCallback" value
            in
            ( model, Cmd.none )


view : Model -> Html Msg
view model =
    let
        fieldset =
            signupModule.fieldset model

        withLabel l field =
            Html.div [ Attr.style "margin" "12px" ]
                [ Html.label [] [ Html.text l ]
                , field [ Attr.style "margin" "4px" ] |> withCallback
                ]

        withCallback field =
            Html.div [ Html.Events.onInput GotSideloadedCallback ] [ field ]
    in
    Html.div []
        [ Html.div []
            [ Html.div []
                [ fieldset.name |> withLabel "Name"
                , fieldset.age |> withLabel "Age"
                , fieldset.emailAddress |> withLabel "Email address"
                , fieldset.subscribe |> withLabel "Subscribe"
                ]
            , Html.button [ Html.Events.onClick signupModule.submitMsg ] [ Html.text "Submit" ]
            , Html.hr [] []
            , Debug.toString model |> Html.text
            ]
        ]
