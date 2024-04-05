module Internals exposing (..)

import Dict
import Html
import Json.Decode
import Task



-- toplevel types


type alias Element msg =
    List (Html.Attribute msg) -> List (Html.Html msg) -> Html.Html msg


type FieldConfig error value editor msg
    = FieldConfig
        { initialValue : Maybe value
        , eventName : String
        , stopPropagation : Bool
        , preventDefault : Bool
        , decoder : Json.Decode.Decoder value
        , element : Element msg
        , withValueAttr :
            { wrap : Maybe value -> editor, initialValue : Maybe value }
            -> (editor -> List (Html.Attribute msg) -> List (Html.Attribute msg))
        , validations : List (Validation error value editor)
        }


type InvalidateWhen
    = EditingOrBlurred
    | BlurredAfterEdit
    | Always


type Validation error value editor
    = Validation
        { func : { self : editor, other : editor } -> Result error editor
        , when : InvalidateWhen
        }



-- TEA


type Model editor
    = Model (Internals editor)


type alias Internals editor =
    { editors : Dict.Dict Int editor
    , focusEvents : List FocusEvent
    }


type FocusEvent
    = Focused Int
    | Blurred Int


type Msg editor
    = UserUpdatedField Int editor
    | UserGeneratedFocusEvent FocusEvent
    | UserClickedSubmit


update :
    { onSubmit : record -> msg, toRecord : List editor -> Maybe record }
    -> Msg editor
    -> Model editor
    -> ( Model editor, Cmd msg )
update { onSubmit, toRecord } msg (Model model) =
    case msg of
        UserUpdatedField index editor ->
            ( Model { model | editors = Dict.insert index editor model.editors }
            , Cmd.none
            )

        UserGeneratedFocusEvent focusEvent ->
            ( Model { model | focusEvents = focusEvent :: model.focusEvents }
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
