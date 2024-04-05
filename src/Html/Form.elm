module Html.Form exposing
    ( Module
    , Config(..)
    , init
    , withField
    , build
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

{-| Form


# Module instance

@docs Module
@docs Config
@docs init
@docs withField
@docs build
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
import Html.Form.Validation
import Internals
import Json.Decode



-- Module instance


{-| -}
type alias Module error editor model fieldset msg =
    { init : ( Model editor -> model, Cmd msg ) -> ( model, Cmd msg )
    , submitMsg : msg
    , update : Msg editor -> model -> ( model, Cmd msg )
    , fieldset : model -> fieldset
    , errors : model -> List { editor : editor, errors : List error }
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
                    case list of
                        [] ->
                            []

                        x :: xs ->
                            if x == Internals.Focused config.index then
                                [ Internals.EditingOrBlurred ]

                            else if x == Internals.Blurred config.index then
                                [ Internals.EditingOrBlurred ]

                            else if List.member (Internals.Blurred config.index) xs then
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
                    invalidateWhens internals.focusEvents

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
