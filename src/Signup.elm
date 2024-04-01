module Signup exposing (..)

import Form


type Editor
    = Name (Maybe String)
    | Age (Maybe Int)
    | EmailAddress (Maybe String)


type alias Record =
    { name : String
    , age : Int
    , emailAddress : String
    }


type alias Fieldset msg =
    { name : Form.FieldEl msg
    , age : Form.FieldEl msg
    , emailAddress : Form.FieldEl msg
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
        )
        { name = Nothing
        , age = Nothing
        , emailAddress = Nothing
        }
        editors
        |> (\e -> Maybe.map3 Record e.name e.age e.emailAddress)


form :
    { toMsg : Form.Msg Editor -> msg, onSubmit : Record -> msg }
    -> Form.Module Editor { model | signupForm : Form.Model Editor } (Fieldset msg) msg
form { toMsg, onSubmit } =
    Form.init Fieldset
        { toModel = \m formModel -> { m | signupForm = formModel }
        , fromModel = .signupForm
        , toMsg = toMsg
        , toRecord = toRecord
        , onSubmit = onSubmit
        }
        |> Form.withField Name Form.input
        |> Form.withField (Maybe.andThen String.toInt >> Age) Form.input
        |> Form.withField EmailAddress (Form.input |> Form.withInitialValue (Just "john@pavlick.dev"))
        |> Form.build
