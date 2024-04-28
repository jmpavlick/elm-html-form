module Html.Form exposing
    ( init
    , Module
    , build
    , Config(..)
    , withField
    , Model
    , Msg
    , Field
    , FieldConfig
    , Fieldset(..)
    , Element
    , input
    , checkbox
    , custom
    , withInitialValue
    , Validation
    , withValidation
    , withStopPropagation
    , withPreventDefault
    )

{-|


# Overview

This package is, in essence, a state management library that happens to be tightly-scoped to the problem space of "forms in Elm".

Forms in Elm are hard! Forms are hard, regardless of which language they're implemented in; but they're particularly hard in Elm, because all state in an Elm application has to be stored in the `Model` type that's registered in the `main` function - so even if a value can be considered "throwaway" (i.e., unparsed / unvalidated user input), it has to be persisted _somewhere_; and as such, it must become a part of the application's domain. Any state changes require careful handling and trips through the runtime.

Since "trips through the runtime" are only possible in the `update` function, this means that the majority of the code that defines the behavior of a given part of an application gets shoved into a branch on `update`, which means that it can become difficult to generalize over behavior for a given form input without also making it a full-blown "nested TEA" implementation, which requires storing and accessing a "sub-`model`" value. And having all of your application's behavior shoved into `update` creates distance between the the definition of a behavior, and the location of the code that's dependent on and / or providing an user interface for that behavior.

In short, it can be remarkably difficult to create generic, reusable forms in an Elm application that that aren't directly dependent on both state and IO.

This package provides better ergonomics for handling this complexity. Here's how it works, at 30,000 feet (9,144 metres):

  - You create a "module instace" of a `Html.Form` by parameterizing it with a rudimentary "lens" that defines accessing the form's persistence from a host `Model` type, and a map that turns messages from a form into messages on the "ohst page"
  - TODO: continue


# Module instance

This package uses a pattern that I've been referring to as ["the Module Pattern"](https://dev.to/jmpavlick/for-lack-of-a-better-name-im-calling-it-the-module-pattern-5dfi).
To use it, you initialize it via `init` with its depenencies, then add fields with the `withField` function. When you've added all of your fields, you can apply the `build` function;
its output will be a value of type `Module error editor model fieldset msg`.

To summarize the article - the core conceit is that an instance of a `Module` can conceal its internal state while providing functions that allow other code to inter-operate with
that state. Since the `Module` is constructed as a function from an instance of its own `Model` (proxied via a function `model -> Model`, parameterized into `init`), the functions
that it creates can close over the state of the `model`.

This lets us create references from some given state of the `Model`, and close over them as we define the functions - which is the mechanism for having a pre-defined relationship between
the editing state of the fields on a form, and the functions used to update that state.

A `Module` value is a record that exposes fields that contain functions that allow you to:

  - Initialize the module's internal state
  - Allow for the module's internal state to be attached to and synchronized to a "host" / parent
  - Map the module's `update` motion and any commands that it sends to the runtime through to the host
  - Expose functions that update the module's internal state
  - Expose data from the module

So that instead of exposing types and functions `view, update, init, Model, Msg` from a module, you expose its `Module`, `Msg`, and any functions needed to instantiate and configure the `Module` (such as `init`).

@docs init
@docs Module
@docs build
@docs Config
@docs withField
@docs Model
@docs Msg


# Field configuration

@docs Field
@docs FieldConfig
@docs Fieldset
@docs Element


# Fields

@docs input
@docs checkbox
@docs custom


# Field builders

@docs withInitialValue
@docs Validation
@docs withValidation
@docs withStopPropagation
@docs withPreventDefault

-}

import Dict
import Html
import Html.Attributes
import Html.Events
import Internals
import Json.Decode



-- Module instance


{-| Module initialization.

This function creates a value of type `Config error editor record fieldset model msg`. The `Config` defines your form's fields and
behaviors.

`Config`'s type parameters are satisfied as follows:

  - `error`: The type that you will use to represent errors or invalid states within a form. You can use any type; `String` is a good place to start - you can use a different type if you need to.
  - `editor`: The type whose constructors represent editing state for a single given field on your form. For instance, a "signup" form containing fields for a username, an email address, and a user's age could be written as follows:
    type Editor
    = Name (Maybe String)
    | Email (Maybe String)
    | Age (Maybe Int)
  - `record`:

-}
init :
    fieldset
    ->
        { toModel : model -> Model editor -> model
        , fromModel : model -> Model editor
        , toMsg : Msg editor -> msg
        , toRecord : List editor -> Maybe record
        , onSubmit : Result (List error) record -> msg
        }
    -> Config error editor record fieldset model msg
init fieldset { toModel, fromModel, toMsg, toRecord, onSubmit } =
    Config
        { toModel = toModel
        , fromModel = fromModel
        , toMsg = toMsg
        , toRecord = toRecord
        , index = 0
        , initModel = identity
        , onSubmit = onSubmit
        , fieldset = Fieldset (always fieldset)
        , errors = always identity
        }


{-| -}
type alias Module error editor model fieldset msg =
    { init : ( Model editor -> model, Cmd msg ) -> ( model, Cmd msg )
    , submitMsg : msg
    , update : Msg editor -> model -> ( model, Cmd msg )
    , fieldset : model -> fieldset
    , errors : model -> List { editor : editor, errors : List error }
    }


{-| -}
build : Config error editor record fieldset model msg -> Module error editor model fieldset msg
build (Config config) =
    { init =
        \( toModel, cmdMsg ) ->
            ( toModel <|
                config.initModel <|
                    Internals.Model
                        { editors = Dict.empty
                        , focusEvents = []
                        }
            , cmdMsg
            )
    , submitMsg = config.toMsg Internals.UserClickedSubmit
    , update =
        \msg model ->
            Internals.update
                { errors = config.errors model [] |> List.map .errors |> List.concat
                , onSubmit = config.onSubmit
                , toRecord = config.toRecord
                }
                msg
                (config.fromModel model)
                |> Tuple.mapFirst
                    (config.toModel model)
    , fieldset =
        \model ->
            (\(Fieldset fs) -> fs model) config.fieldset
    , errors =
        \model ->
            config.errors model []
    }


{-| -}
type Config error editor record fieldset model msg
    = Config
        { toModel : model -> Model editor -> model
        , fromModel : model -> Model editor
        , toMsg : Msg editor -> msg
        , toRecord : List editor -> Maybe record
        , onSubmit : Result (List error) record -> msg
        , index : Int
        , initModel : Model editor -> Model editor
        , fieldset : Fieldset model fieldset
        , errors : model -> (List { editor : editor, errors : List error } -> List { editor : editor, errors : List error })
        }


{-| -}
withField :
    (Maybe value -> editor)
    -> FieldConfig error value editor msg
    -> Config error editor record (Field error msg -> fieldset) model msg
    -> Config error editor record fieldset model msg
withField wrap (Internals.FieldConfig fieldConfig) (Config config) =
    let
        toFormMsg : value -> msg
        toFormMsg =
            Just >> wrap >> Internals.UserUpdatedField config.index >> config.toMsg

        eventHandler : Html.Attribute msg
        eventHandler =
            Html.Events.custom
                fieldConfig.eventName
                (Json.Decode.map toFormMsg fieldConfig.decoder
                    |> Json.Decode.map
                        (\msg ->
                            { message = msg
                            , stopPropagation = fieldConfig.stopPropagation
                            , preventDefault = fieldConfig.preventDefault
                            }
                        )
                )

        initialEditor : editor
        initialEditor =
            wrap fieldConfig.initialValue

        editor : model -> editor
        editor model =
            let
                (Internals.Model internals) =
                    config.fromModel model
            in
            Dict.get config.index internals.editors
                |> Maybe.withDefault initialEditor

        withEventAttrs : model -> List (Html.Attribute msg) -> List (Html.Attribute msg)
        withEventAttrs model attrs =
            let
                { onFocus, onBlur, onEvent, value } =
                    attrsRecord model
            in
            onFocus :: onBlur :: onEvent :: value :: attrs

        attrsRecord :
            model
            ->
                { onFocus : Html.Attribute msg
                , onBlur : Html.Attribute msg
                , onEvent : Html.Attribute msg
                , value : Html.Attribute msg
                }
        attrsRecord model =
            let
                mapFocusBlur : Internals.FocusEvent -> msg
                mapFocusBlur =
                    Internals.UserGeneratedFocusEvent >> config.toMsg
            in
            { onFocus = Html.Events.onFocus (mapFocusBlur <| Internals.Focused config.index)
            , onBlur = Html.Events.onBlur (mapFocusBlur <| Internals.Blurred config.index)
            , onEvent = eventHandler
            , value = fieldConfig.valueAttr { wrap = wrap, initialValue = fieldConfig.initialValue } (editor model)
            }

        element : model -> List (Html.Attribute msg) -> Html.Html msg
        element model attrs =
            fieldConfig.element
                (withEventAttrs model attrs)
                []

        invalidateWhens : List Internals.FocusEvent -> List Internals.InvalidateWhen
        invalidateWhens list =
            List.concat
                [ [ Internals.Always ]
                , if fieldConfig.initialValue /= Nothing && List.isEmpty list then
                    [ Internals.EditingOrBlurred, Internals.BlurredAfterEdit ]

                  else
                    case Debug.log "invalidateWhens input" list of
                        [] ->
                            []

                        x :: xs ->
                            if x == Internals.Focused config.index then
                                [ Internals.EditingOrBlurred ]

                            else if x == Internals.Blurred config.index then
                                [ Internals.EditingOrBlurred, Internals.BlurredAfterEdit ]

                            else if List.member (Internals.Blurred config.index) list then
                                [ Internals.BlurredAfterEdit ]

                            else
                                []
                ]

        allErrors : model -> List { error : error, shouldBeRaised : Bool }
        allErrors model =
            let
                (Internals.Model internals) =
                    config.fromModel model

                editor_ : editor
                editor_ =
                    editor model

                editors : List editor
                editors =
                    Dict.values internals.editors

                foldEditors : Internals.Validation error value editor -> Result error editor
                foldEditors (Internals.Validation v) =
                    List.foldl
                        (\step acc ->
                            case acc of
                                Err _ ->
                                    acc

                                Ok _ ->
                                    v.func { self = editor_, other = step }
                        )
                        (Ok editor_)
                        editors

                invalidateWhens_ : List Internals.InvalidateWhen
                invalidateWhens_ =
                    internals.focusEvents
                        |> List.filterMap
                            (\fe ->
                                if
                                    config.index
                                        == (case fe of
                                                Internals.Blurred i ->
                                                    i

                                                Internals.Focused i ->
                                                    i
                                           )
                                then
                                    Just fe

                                else
                                    Nothing
                            )
                        |> invalidateWhens

                validations : List { error : Result error editor, shouldBeRaised : Bool }
                validations =
                    List.map
                        (\(Internals.Validation v) ->
                            { error =
                                foldEditors (Internals.Validation v)
                            , shouldBeRaised =
                                List.member v.when invalidateWhens_
                            }
                        )
                        fieldConfig.validations
            in
            List.foldl
                (\(Internals.Validation v) acc ->
                    case foldEditors (Internals.Validation v) of
                        Ok _ ->
                            acc

                        Err e ->
                            { error = e
                            , shouldBeRaised =
                                List.member v.when invalidateWhens_
                            }
                                :: acc
                )
                []
                fieldConfig.validations

        fieldErrors : model -> List error
        fieldErrors =
            allErrors
                >> List.filterMap
                    (\{ error, shouldBeRaised } ->
                        if shouldBeRaised then
                            Just error

                        else
                            Nothing
                    )

        field : model -> Field error msg
        field model =
            { element = element model
            , errors = fieldErrors model
            , toAttrs = withEventAttrs model
            , attrs = attrsRecord model
            }
    in
    Config
        { index = config.index + 1
        , initModel =
            config.initModel
                >> (\(Internals.Model m) -> Internals.Model { m | editors = Dict.insert config.index initialEditor m.editors })
        , toModel = config.toModel
        , fromModel = config.fromModel
        , toMsg = config.toMsg
        , toRecord = config.toRecord
        , onSubmit = config.onSubmit
        , fieldset = Fieldset (\model -> (\(Fieldset fs) -> fs model (field model)) config.fieldset)
        , errors =
            \model nextErrors ->
                allErrors model
                    |> List.map .error
                    |> (\es ->
                            if List.isEmpty es then
                                nextErrors

                            else
                                { editor = editor model, errors = es } :: nextErrors
                       )
                    |> config.errors model
        }


{-| -}
type alias Model editor =
    Internals.Model editor


{-| -}
type alias Msg editor =
    Internals.Msg editor



-- Field configuration


{-| -}
type alias Field error msg =
    { element : List (Html.Attribute msg) -> Html.Html msg
    , errors : List error
    , toAttrs : List (Html.Attribute msg) -> List (Html.Attribute msg)
    , attrs :
        { onFocus : Html.Attribute msg
        , onBlur : Html.Attribute msg
        , onEvent : Html.Attribute msg
        , value : Html.Attribute msg
        }
    }


{-| -}
type alias FieldConfig error value editor msg =
    Internals.FieldConfig error value editor msg


{-| -}
type Fieldset model fieldset
    = Fieldset (model -> fieldset)


{-| -}
type alias Element msg =
    Internals.Element msg



-- Fields


{-| -}
input : FieldConfig error String editor msg
input =
    custom
        { eventName = "input"
        , decoder = Html.Events.targetValue
        , element = Html.input
        , valueAttr =
            \{ wrap, initialValue } ->
                \editor ->
                    if wrap initialValue == editor then
                        Maybe.map Html.Attributes.value initialValue
                            |> Maybe.withDefault (Html.Attributes.class "")

                    else
                        Html.Attributes.class ""
        }


{-| -}
checkbox : FieldConfig error Bool editor msg
checkbox =
    custom
        { eventName = "input"
        , decoder = Html.Events.targetChecked
        , element =
            \attrs elems ->
                Html.input (Html.Attributes.type_ "checkbox" :: attrs) elems
        , valueAttr =
            \{ wrap } ->
                \editor ->
                    if editor == wrap (Just True) then
                        Html.Attributes.checked True

                    else
                        Html.Attributes.class ""
        }
        |> withInitialValue (Just False)


{-| -}
custom :
    { eventName : String
    , decoder : Json.Decode.Decoder value
    , element : Element msg
    , valueAttr :
        { wrap : Maybe value -> editor, initialValue : Maybe value }
        -> (editor -> Html.Attribute msg)
    }
    -> FieldConfig error value editor msg
custom { eventName, decoder, element, valueAttr } =
    Internals.FieldConfig
        { initialValue = Nothing
        , eventName = eventName
        , stopPropagation = True
        , preventDefault = True
        , decoder = decoder
        , element = element
        , valueAttr = valueAttr
        , validations = []
        }



-- Field builders


{-| -}
withInitialValue : Maybe value -> FieldConfig error value editor msg -> FieldConfig error value editor msg
withInitialValue value (Internals.FieldConfig field) =
    Internals.FieldConfig
        { field | initialValue = value }


{-| -}
withStopPropagation : Bool -> FieldConfig error value editor msg -> FieldConfig error value editor msg
withStopPropagation value (Internals.FieldConfig field) =
    Internals.FieldConfig { field | stopPropagation = value }


{-| -}
withPreventDefault : Bool -> FieldConfig error value editor msg -> FieldConfig error value editor msg
withPreventDefault value (Internals.FieldConfig field) =
    Internals.FieldConfig { field | preventDefault = value }


{-| -}
type alias Validation error value editor =
    Internals.Validation error value editor


{-| -}
withValidation : Validation error value editor -> FieldConfig error value editor msg -> FieldConfig error value editor msg
withValidation value (Internals.FieldConfig field) =
    Internals.FieldConfig { field | validations = value :: field.validations }
