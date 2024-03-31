module Signup exposing (..)

import Ui.Form


type Editor
    = Name (Maybe String)
    | Age (Maybe Int)
    | EmailAddress (Maybe String)


type alias Record =
    { name : String
    , age : Int
    , emailAddress : String
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


form : { toMsg : Ui.Form.Msg Editor -> msg, onSubmit : Record -> msg } -> Ui.Form.Module Editor { model | signupForm : Ui.Form.Model Editor } msg
form { toMsg, onSubmit } =
    Ui.Form.init
        { toModel = \m formModel -> { m | signupForm = formModel }
        , fromModel = .signupForm
        , toMsg = toMsg
        , toRecord = toRecord
        , onSubmit = onSubmit
        }
        |> Ui.Form.withInput
            { wrap = Name
            , initialValue = Nothing
            , attrs = []
            }
        |> Ui.Form.withInput
            { wrap = Maybe.andThen String.toInt >> Age
            , initialValue = Nothing
            , attrs = []
            }
        |> Ui.Form.withInput
            { wrap = EmailAddress
            , initialValue = Nothing
            , attrs = []
            }
        |> Ui.Form.build
