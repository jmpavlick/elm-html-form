module Html.Form.Validation exposing
    ( When
    , when
    , andThen
    )

{-| Validation

@docs When

@docs when

@docs andThen

-}

import Internals


{-| Declare how you want to validate a given field, and when you'd like it for it to be validated.
-}
type alias Validation error value editor =
    Internals.Validation error value editor


{-| Describes when a validation should occur.
-}
type alias When =
    Internals.InvalidateWhen


{-| Constructor record; you can use the dot operator to select a `When` value, which returns a function
that takes a function that validates the current `Field`.

In the callback, `self` represents the current `editor`, and `other` represents _any other_ editor. If you don't
want to validate a given field against another field, you can ignore this value in your callback.

-}
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


{-| Provide additional validations sequentially.

For instance: if you only want to show an error that says that user input is bad _after_ the
field has been edited - but if you want to show an error that says that the field must be non-empty - you could add the "field must be non-empty"
callback with `when`, and "chain" the "the field must match a given pattern" value with `andThen`.

-}
andThen : ({ self : editor, other : editor } -> Result error editor) -> Validation error value editor -> Validation error value editor
andThen func (Internals.Validation v) =
    Internals.Validation
        { func =
            \params ->
                v.func params
                    |> Result.andThen (always <| func params)
        , when = v.when
        }
