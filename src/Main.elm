module Main exposing (..)

import Browser
import Html exposing (Html)
import Html.Attributes
import Html.Events
import Html.Form
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
    { signupForm : Html.Form.Model Signup.Editor }


type Msg
    = GotSignupMsg (Html.Form.Msg Signup.Editor)
    | GotSignupOnSubmit Signup.Record
    | GotSideloadedCallback String


signupModule : Html.Form.Module String Signup.Editor Model (Signup.Fieldset Msg) Msg
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
            Html.div [ Html.Attributes.style "margin" "12px" ]
                [ Html.label [] [ Html.text l ]
                , field [ Html.Attributes.style "margin" "4px" ] |> withCallback
                ]

        withCallback field =
            Html.div [ Html.Events.onInput GotSideloadedCallback ] [ field ]
    in
    Html.div []
        [ Html.div []
            [ Html.div []
                [ Html.div []
                    [ fieldset.name.element |> withLabel "Name"
                    , List.map
                        (\e ->
                            Html.div [] [ Html.hr [] [], Html.text e ]
                        )
                        fieldset.name.errors
                        |> Html.div []
                    ]
                , fieldset.age.element |> withLabel "Age"
                , fieldset.emailAddress.element |> withLabel "Email address"
                , fieldset.subscribe.element |> withLabel "Subscribe"
                ]
            , Html.button [ Html.Events.onClick signupModule.submitMsg ] [ Html.text "Submit" ]
            , Html.hr [] []
            , Debug.toString model |> Html.text
            , Html.hr [] []
            , Debug.toString (signupModule.errors model) |> Html.text
            , Html.hr [] []
            , Debug.toString (signupModule.errors model |> Signup.fromErrors) |> Html.text
            , Html.div []
                [ Html.input [ Html.Attributes.attribute "list" "0" ] []
                , Html.datalist [ Html.Attributes.id "0" ]
                    [ Html.option [ Html.Attributes.value "lorem" ] [ Html.text "lorem" ]
                    , Html.option [ Html.Attributes.value "ipsum" ] [ Html.text "ipsum" ]
                    , Html.option [ Html.Attributes.value "dolor" ] [ Html.text "dolor" ]
                    ]
                ]
            ]
        ]
