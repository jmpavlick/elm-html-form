# elm-html-form

A typesafe form package for Elm that lets you really _use the platform_.

⚠️ This is alpha, severely under-documented, and has zero tests - so please proceed with caution, for now. ⚠️

But with all that said - this is the form package that you want. The form package that you didn't realize could exist.


## Features 

The `Html.Form.Module` instance gives you access to a record with named fields that allows you to access:

- The form input element
- The form input element's attributes (i.e., events and value attribute) as a record with named fields
- The form input element's attributes as a function `List (Html.Attribute msg) -> List (Html.Attribute msg)
- The form input element's errors

You can implement this in an existing application by applying the element's attributes to whatever inputs you have already - or you can build up
elements yourself and hoist the form events into them - or you can simply take the element outputs and style them however you want. It's up to you, really.

It also gives you access to a list of your whole form's errors, tupled with their "editor" value - which you can easily restructure into a record with named properties,
if you'd like.

Oh, and none of this is using functions in the `Model`, or any black magic. At time of writing, the only dependencies are on packages from the `elm` Github org.


## Sample Implementation

More documentation to come soon, but here's a taste of the API - this is all copied from this repo's [`demo/src/Main.elm`](https://github.com/jmpavlick/elm-html-form/blob/master/demo/src/Main.elm) and [`demo/src/Signup.elm`](https://github.com/jmpavlick/elm-html-form/blob/master/demo/src/Signup.elm) modules.


### Callsites for form fields

Forgive this; I'm not very good at HTML. But at least you can get an idea of what your callsites could look like! (I'm going to clean this up later.)

```
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

```


### Form creation
This is how you initialize a `Html.Form.Module`.

```
import Html
import Html.Form
import Html.Form.Validation

{-| First, you create a type to represent a single field's editing state.
-}
type Editor
    = Name (Maybe String)
    | Age (Maybe Int)
    | EmailAddress (Maybe String)
    | Subscribe (Maybe Bool)


{-| Then, you create a type to represent a validated, submitted value.
-}
type alias Record =
    { name : String
    , age : Int
    , emailAddress : String
    , subscribe : Bool
    }

{-| You can use custom types for errors, if you want to!
-}
type alias Error =
    String

{-| Then, you create a type to represent your output, from the form module.
-}
type alias Fieldset msg =
    { name : Html.Form.Field Error msg
    , age : Html.Form.Field Error msg
    , emailAddress : Html.Form.Field Error msg
    , subscribe : Html.Form.Field Error msg
    }

{-| Now, you provide a map from a `List` of your `Editor` type, to a `Maybe Record`.

(Note: Don't put your validation in here - that's not where it goes!)
-}
toRecord : List Editor -> Maybe Record
toRecord editors =
    List.foldl
        (\step acc ->
            case step of
                Name name ->
                    { acc | name = name }

                Age age ->
                    { acc | age = age }

                EmailAddress emailAddress ->
                    { acc | emailAddress = emailAddress }

                Subscribe subscribe ->
                    { acc | subscribe = subscribe }
        )
        { name = Nothing
        , age = Nothing
        , emailAddress = Nothing
        , subscribe = Nothing
        }
        editors
        |> (\e -> Maybe.map4 Record e.name e.age e.emailAddress e.subscribe)

{-| Now, you can initialize the module. (In this sample, I'm defining my form in one module and actually
hosting it in another, so `toMsg` and `onSubmit` are parameterized.)
-}
form :
    { toMsg : Html.Form.Msg Editor -> msg, onSubmit : Record -> msg }
    -> Html.Form.Module String Editor { model | signupForm : Html.Form.Model Editor } (Fieldset msg) msg
form { toMsg, onSubmit } =
    {-
      Mapping the fields to the `Fieldset msg` works similiarly to `Json.Decode.Pipeline`, where you use a constructor function
      (usually the type alias constructor from a record type alias) to kick off an applicative. 
    -}
    Html.Form.init Fieldset
        { toModel = \m formModel -> { m | signupForm = formModel }
        , fromModel = .signupForm
        , toMsg = toMsg
        , toRecord = toRecord
        , onSubmit = onSubmit
        }
        -- You can then add as many fields as you have parameters to your constructor function.
        |> Html.Form.withField Name
            (Html.Form.input
                {-
                  You can allow propagation on inputs so that the event bubbles; you can use this
                  to fire events in a host module when something happens in your form - per field!
                -}
                |> Html.Form.withStopPropagation False
                {-
                  You can add validation!
                -}
                |> Html.Form.withValidation
                    (Html.Form.Validation.when.blurredAfterEdit
                        (\args ->
                            if args.self == Name (Just "John") then
                                Err "I'm sorry, John; you can't do that."

                            else
                                Ok args.self
                        )
                    )
            )
        |> Html.Form.withField (Maybe.andThen String.toInt >> Age) Html.Form.input
        |> Html.Form.withField EmailAddress
            (Html.Form.input
                |> Html.Form.withInitialValue (Just "john@pavlick.dev")
                |> Html.Form.withValidation
                    (Html.Form.Validation.when.editingOrBlurred
                        (\args ->
                            case args.self of
                                EmailAddress (Just v) ->
                                    if v == "" then
                                        Err "Email address must not be empty"

                                    else
                                        Ok args.self

                                _ ->
                                    Err "Email address must not be empty"
                        )
                        |> Html.Form.Validation.andThen
                            (\args ->
                                case args.self of
                                    EmailAddress (Just v) ->
                                        if not <| String.contains "@" v then
                                            Err "Email addresses must contain an @ symbol"

                                        else
                                            Ok args.self

                                    _ ->
                                        Ok args.self
                            )
                    )
            )
        |> Html.Form.withField Subscribe (Html.Form.checkbox |> Html.Form.withStopPropagation False)
        |> Html.Form.build
```