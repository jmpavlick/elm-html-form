module Signup exposing (..)

import Html
import Html.Form
import Html.Form.Validation


type Editor
    = Name (Maybe String)
    | Age (Maybe Int)
    | EmailAddress (Maybe String)
    | Subscribe (Maybe Bool)


type alias Record =
    { name : String
    , age : Int
    , emailAddress : String
    , subscribe : Bool
    }


type alias Error =
    String


type alias Fieldset msg =
    { name : Html.Form.Field Error msg
    , age : Html.Form.Field Error msg
    , emailAddress : Html.Form.Field Error msg
    , subscribe : Html.Form.Field Error msg
    }


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


form :
    { toMsg : Html.Form.Msg Editor -> msg, onSubmit : Record -> msg }
    -> Html.Form.Module String Editor { model | signupForm : Html.Form.Model Editor } (Fieldset msg) msg
form { toMsg, onSubmit } =
    Html.Form.init Fieldset
        { toModel = \m formModel -> { m | signupForm = formModel }
        , fromModel = .signupForm
        , toMsg = toMsg
        , toRecord = toRecord
        , onSubmit = onSubmit
        }
        |> Html.Form.withField Name
            (Html.Form.input
                |> Html.Form.withStopPropagation False
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
        |> Html.Form.withField EmailAddress (Html.Form.input |> Html.Form.withInitialValue (Just "john@pavlick.dev"))
        |> Html.Form.withField Subscribe (Html.Form.checkbox |> Html.Form.withStopPropagation False)
        |> Html.Form.build



-- optionally, if you're really snapped, you can restructure your errors, too


type alias Errors =
    { name : Maybe { editor : Editor, errors : List Error }
    , age : Maybe { editor : Editor, errors : List Error }
    , emailAddress : Maybe { editor : Editor, errors : List Error }
    , subscribe : Maybe { editor : Editor, errors : List Error }
    }


fromErrors : List { editor : Editor, errors : List Error } -> Errors
fromErrors =
    List.foldl
        (\step acc ->
            case step.editor of
                Name _ ->
                    { acc | name = Just step }

                Age _ ->
                    { acc | age = Just step }

                EmailAddress _ ->
                    { acc | emailAddress = Just step }

                Subscribe _ ->
                    { acc | subscribe = Just step }
        )
        { name = Nothing
        , age = Nothing
        , emailAddress = Nothing
        , subscribe = Nothing
        }
