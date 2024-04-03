module Form exposing (..)

import Dict exposing (Dict)
import Html exposing (Html)
import Html.Attributes as Attr
import Html.Events
import Json.Decode
import Task



-- TEA


type Model editor
    = Model (Internals editor)


type alias Internals editor =
    { editors : Dict Int editor
    , focusedIndex : Maybe Int
    }


type Msg editor
    = UserUpdatedField Int editor
    | UserFocusedField Int
    | UserBlurredField
    | UserClickedSubmit


update :
    { onSubmit : record -> msg, toMsg : Msg editor -> msg, toRecord : List editor -> Maybe record }
    -> Msg editor
    -> Model editor
    -> ( Model editor, Cmd msg )
update { onSubmit, toMsg, toRecord } msg (Model model) =
    case msg of
        UserUpdatedField index editor ->
            ( Model { model | editors = Dict.insert index editor model.editors }
            , Cmd.none
            )

        UserFocusedField index ->
            ( Model { model | focusedIndex = Just index }
            , Cmd.none
            )

        UserBlurredField ->
            ( Model { model | focusedIndex = Nothing }
            , Cmd.none
            )

        UserClickedSubmit ->
            ( Model model
            , let
                call : msg -> Cmd msg
                call m =
                    Task.perform
                        (always m)
                        (Task.succeed identity)
              in
              Maybe.map
                (onSubmit >> call)
                (Dict.values model.editors |> toRecord)
                |> Maybe.withDefault Cmd.none
            )



-- module


type alias Module editor model fieldset msg =
    { init : ( Model editor -> model, Cmd msg ) -> ( model, Cmd msg )
    , submitMsg : msg
    , update : Msg editor -> model -> ( model, Cmd msg )
    , fieldset : model -> fieldset
    }


type Init editor record fieldset model msg
    = Init
        { toModel : model -> Model editor -> model
        , fromModel : model -> Model editor
        , toMsg : Msg editor -> msg
        , toRecord : List editor -> Maybe record
        , onSubmit : record -> msg
        , nextIndex : Int
        , initModel : Model editor -> Model editor
        , fieldset : Fieldset model fieldset
        }


init :
    fieldset
    ->
        { toModel : model -> Model editor -> model
        , fromModel : model -> Model editor
        , toMsg : Msg editor -> msg
        , toRecord : List editor -> Maybe record
        , onSubmit : record -> msg
        }
    -> Init editor record fieldset model msg
init fieldset { toModel, fromModel, toMsg, toRecord, onSubmit } =
    Init
        { toModel = toModel
        , fromModel = fromModel
        , toMsg = toMsg
        , toRecord = toRecord
        , nextIndex = 0
        , initModel = identity
        , onSubmit = onSubmit
        , fieldset = Fieldset (always fieldset)
        }


build : Init editor record fieldset model msg -> Module editor model fieldset msg
build (Init init_) =
    { init =
        \( toModel, cmdMsg ) ->
            ( toModel <|
                init_.initModel <|
                    Model
                        { editors = Dict.empty
                        , focusedIndex = Nothing
                        }
            , cmdMsg
            )
    , submitMsg = init_.toMsg UserClickedSubmit
    , update =
        \msg model ->
            update
                { onSubmit = init_.onSubmit
                , toRecord = init_.toRecord
                , toMsg = init_.toMsg
                }
                msg
                (init_.fromModel model)
                |> Tuple.mapFirst
                    (init_.toModel model)
    , fieldset =
        \model ->
            (\(Fieldset fs) -> fs model) init_.fieldset
    }


type Fieldset model fieldset
    = Fieldset (model -> fieldset)



-- fields


type alias FieldEl msg =
    List (Html.Attribute msg) -> Html msg


type Field value editor msg
    = Field
        { initialValue : Maybe value
        , element : List (Html.Attribute msg) -> List (Html msg) -> Html msg
        , withValueAttr :
            { wrap : Maybe value -> editor
            , initialValue : Maybe value
            }
            -> editor
            -> List (Html.Attribute msg)
            -> List (Html.Attribute msg)
        , withEventHandler : { wrap : Maybe value -> editor, index : Int, toMsg : Msg editor -> msg } -> List (Html.Attribute msg) -> List (Html.Attribute msg)
        }


withField :
    (Maybe value -> editor)
    -> Field value editor msg
    -> Init editor record ((List (Html.Attribute msg) -> Html msg) -> fieldset) model msg
    -> Init editor record fieldset model msg
withField wrap (Field field) (Init init_) =
    let
        internals : model -> Internals editor
        internals =
            init_.fromModel >> (\(Model m) -> m)

        initEditor : editor
        initEditor =
            wrap field.initialValue

        withValueAttr : model -> List (Html.Attribute msg) -> List (Html.Attribute msg)
        withValueAttr model attrs_ =
            Maybe.map
                (\value ->
                    field.withValueAttr { wrap = wrap, initialValue = field.initialValue }
                        value
                        attrs_
                )
                -- syntax crimes
                ((internals model).editors |> Dict.get init_.nextIndex)
                |> Maybe.withDefault attrs_

        withEvents : List (Html.Attribute msg) -> List (Html.Attribute msg)
        withEvents attrs_ =
            Html.Events.onFocus (UserFocusedField init_.nextIndex |> init_.toMsg)
                :: Html.Events.onBlur (init_.toMsg UserBlurredField)
                :: attrs_
                |> field.withEventHandler { wrap = wrap, index = init_.nextIndex, toMsg = init_.toMsg }

        element : model -> List (Html.Attribute msg) -> Html msg
        element model attrs =
            field.element
                (attrs |> withValueAttr model |> withEvents)
                []
    in
    Init
        { nextIndex = init_.nextIndex + 1
        , initModel =
            init_.initModel
                >> (\(Model m) -> Model { m | editors = Dict.insert init_.nextIndex initEditor m.editors })
        , toModel = init_.toModel
        , fromModel = init_.fromModel
        , toMsg = init_.toMsg
        , toRecord = init_.toRecord
        , onSubmit = init_.onSubmit
        , fieldset = Fieldset (\model -> (\(Fieldset fs) -> fs model (element model)) init_.fieldset)
        }



-- inputs


custom :
    { element : List (Html.Attribute b) -> List (Html b) -> Html b
    , eventHandler : (String -> b) -> Html.Attribute b
    }
    -> Field String editor b
custom { element, eventHandler } =
    Field
        { initialValue = Nothing
        , element = element
        , withValueAttr =
            \{ wrap, initialValue } editor attrs ->
                (if wrap initialValue == editor then
                    initialValue

                 else
                    Nothing
                )
                    |> Maybe.map (\v -> Attr.value v :: attrs)
                    |> Maybe.withDefault attrs
        , withEventHandler =
            \{ wrap, index, toMsg } ->
                (::)
                    (eventHandler
                        (\value ->
                            Just value
                                |> wrap
                                |> UserUpdatedField index
                                |> toMsg
                        )
                    )
        }


toFormMsg : { wrap : Maybe a -> editor, index : Int, toMsg : Msg editor -> msg } -> a -> msg
toFormMsg { wrap, index, toMsg } =
    Just >> wrap >> UserUpdatedField index >> toMsg


customEvent :
    String
    ->
        { stopPropagation : Bool
        , preventDefault : Bool
        }
    -> Json.Decode.Decoder a
    ->
        { wrap : Maybe a -> editor
        , index : Int
        , toMsg : Msg editor -> msg
        }
    -> Html.Attribute msg
customEvent event { stopPropagation, preventDefault } decoder params =
    Json.Decode.map (toFormMsg params) decoder
        |> Json.Decode.map
            (\msg ->
                { message = msg
                , stopPropagation = stopPropagation
                , preventDefault = preventDefault
                }
            )
        |> Html.Events.custom event


custom_ :
    String
    ->
        { stopPropagation : Bool
        , preventDefault : Bool
        }
    -> Json.Decode.Decoder a
    ->
        { element : List (Html.Attribute msg) -> List (Html msg) -> Html msg
        , toValueAttr : a -> Maybe (Html.Attribute msg)
        }
    -> Field a editor msg
custom_ event params decoder { element, toValueAttr } =
    Field
        { initialValue = Nothing
        , element = element
        , withValueAttr =
            \{ wrap, initialValue } editor attrs ->
                (if wrap initialValue == editor then
                    initialValue

                 else
                    Nothing
                )
                    |> Maybe.andThen toValueAttr
                    |> Maybe.map (\v -> v :: attrs)
                    |> Maybe.withDefault attrs
        , withEventHandler =
            \({ wrap, index, toMsg } as params_) ->
                (::)
                    (customEvent event params decoder params_)
        }


{-| the WIP here is that we want to be able to "sidechain" events, so we need to rewrite the `Field` type
so that we can defunctionalize the commands to create an `input` so that we can define all of our "field"
functions in terms of the current constructor of `custom_`, so that we can expose an API like this:

    Form.input
        |> Form.withStopPropagation

and so that we can add a motion like this:

    Form.input
        |> withSubscription someMsg

that sort of >>s the element inside of `div` tags that also emit bubbled events, with the outermost element being responsible for stopping propagation.

-}
input : Field String editor msg
input =
    custom_
        "input"
        { stopPropagation = True
        , preventDefault = True
        }
        Html.Events.targetValue
        { element = Html.input
        , toValueAttr = Attr.value >> Just
        }


inputShort : Field String editor msg
inputShort =
    custom { element = Html.input, eventHandler = Html.Events.onInput }


input_ : Field String editor msg
input_ =
    Field
        { initialValue = Nothing
        , element = Html.input
        , withValueAttr =
            \{ wrap, initialValue } editor attrs ->
                (if wrap initialValue == editor then
                    initialValue

                 else
                    Nothing
                )
                    |> Maybe.map (\v -> Attr.value v :: attrs)
                    |> Maybe.withDefault attrs
        , withEventHandler =
            \{ wrap, index, toMsg } ->
                (::) (Html.Events.onInput (Just >> wrap >> UserUpdatedField index >> toMsg))
        }


checkbox : Field Bool editor msg
checkbox =
    Field
        { initialValue = Nothing
        , element =
            \attrs elems ->
                Html.input (Attr.type_ "checkbox" :: attrs) elems
        , withValueAttr =
            \{ wrap, initialValue } editor attrs ->
                (if wrap initialValue == editor then
                    initialValue

                 else
                    Nothing
                )
                    |> Maybe.map (\v -> Attr.checked v :: attrs)
                    |> Maybe.withDefault attrs
        , withEventHandler =
            \{ wrap, index, toMsg } ->
                (::) (Html.Events.onCheck (Just >> wrap >> UserUpdatedField index >> toMsg))
        }



-- input builders


withInitialValue : Maybe value -> Field value editor msg -> Field value editor msg
withInitialValue value (Field field) =
    Field { field | initialValue = value }
