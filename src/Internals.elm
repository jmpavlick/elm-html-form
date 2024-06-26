module Internals exposing (Element, FieldConfig(..), FocusEvent(..), Internals, InvalidateWhen(..), Model(..), Msg(..), Validation(..), update)

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
        , valueAttr :
            { wrap : Maybe value -> editor, initialValue : Maybe value }
            -> (editor -> Html.Attribute msg)
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
    { errors : List error
    , onSubmit : Result (List error) record -> msg
    , toRecord : List editor -> Maybe record
    }
    -> Msg editor
    -> Model editor
    -> ( Model editor, Cmd msg )
update { errors, onSubmit, toRecord } msg (Model model) =
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
              if List.isEmpty errors then
                Maybe.map
                    (Ok >> onSubmit >> call)
                    (toRecord <| Dict.values model.editors)
                    |> Maybe.withDefault Cmd.none

              else
                Err errors
                    |> onSubmit
                    |> call
            )
