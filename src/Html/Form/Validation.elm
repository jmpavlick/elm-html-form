module Html.Form.Validation exposing (Validation, When, when)

import Internals


type alias Validation error value editor =
    Internals.Validation error value editor


type alias When =
    Internals.InvalidateWhen


when :
    { editingOrBlurred : ({ self : editor, other : editor } -> Result error editor) -> Validation error value editor
    , blurredAfterEdit : ({ self : editor, other : editor } -> Result error editor) -> Validation error value editor
    , always : ({ self : editor, other : editor } -> Result error editor) -> Validation error value editor
    }
when =
    { editingOrBlurred = init Internals.EditingOrBlurred
    , blurredAfterEdit = init Internals.BlurredAfterEdit
    , always = init Internals.Always
    }


init : When -> ({ self : editor, other : editor } -> Result error editor) -> Validation error value editor
init when_ func =
    Internals.Validation { func = func, when = when_ }


andThen : ({ self : editor, other : editor } -> Result error editor) -> Validation error value editor -> Validation error value editor
andThen func (Internals.Validation v) =
    Internals.Validation
        { func =
            \params ->
                v.func params
                    |> Result.andThen (always <| func params)
        , when = v.when
        }
