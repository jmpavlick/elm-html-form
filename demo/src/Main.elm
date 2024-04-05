module Main exposing (Model, Msg(..), main)

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
    | GotPropagatedEvent String


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
                    Debug.log "GotSignupOnSubmit" record
            in
            ( model, Cmd.none )

        GotPropagatedEvent value ->
            let
                _ =
                    Debug.log "GotPropagatedEvent" value
            in
            ( model, Cmd.none )


view : Model -> Html Msg
view model =
    let
        fieldset : Signup.Fieldset Msg
        fieldset =
            signupModule.fieldset model

        margin : Int -> Html.Attribute msg
        margin i =
            String.fromInt i
                ++ "px"
                |> Html.Attributes.style "margin"

        withLabel l field =
            Html.div [ margin 12 ]
                [ Html.label [ margin 4 ]
                    [ Html.text l
                    , field
                    ]
                ]

        withCallback field =
            Html.div [ Html.Events.onInput GotPropagatedEvent ] [ field ]

        withErrors field attrs =
            let
                showErrors =
                    not <| List.isEmpty field.errors
            in
            Html.div []
                [ field.element <|
                    if showErrors then
                        Html.Attributes.style "text-decoration-style" "wavy"
                            :: Html.Attributes.style "text-decoration-line" "underline"
                            :: Html.Attributes.style "text-decoration-color" "red"
                            :: attrs

                    else
                        attrs
                , if showErrors then
                    Html.div [] [ Html.ul [] <| Html.h4 [] [ Html.text "Errors" ] :: List.map (\err -> Html.li [] [ Html.text err ]) field.errors ]

                  else
                    Html.text ""
                ]
    in
    Html.div []
        [ Html.div []
            [ Html.div []
                [ withErrors fieldset.name [ margin 12 ] |> withLabel "Name"
                , Html.input (fieldset.age.toAttrs [ margin 12 ]) [] |> withLabel "Age"
                , withErrors fieldset.emailAddress [ margin 12 ] |> withLabel "Email Address"
                , fieldset.subscribe.element [ Html.Attributes.style "margin" "4px" ] |> withLabel "Subscribe"
                ]
            , Html.button [ Html.Events.onClick signupModule.submitMsg ] [ Html.text "Submit" ]
            , Html.hr [] []
            , Debug.toString model |> Html.text
            , Html.hr [] []
            , Debug.toString (signupModule.errors model) |> Html.text
            , Html.hr [] []
            , Debug.toString (signupModule.errors model |> Signup.fromErrors) |> Html.text
            ]
        ]
