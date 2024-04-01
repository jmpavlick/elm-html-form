module Signup2 exposing (..)

import Html exposing (Html)
import Ui.Form2


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
    { name : Html msg
    , age : Html msg
    , emailAddress : Html msg
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


form : { toMsg : Ui.Form2.Msg Editor -> msg, onSubmit : Record -> msg } -> Ui.Form2.Module Editor { model | signupForm : Ui.Form2.Model Editor } (Fieldset msg) msg
form { toMsg, onSubmit } =
    Ui.Form2.init Fieldset
        { toModel = \m formModel -> { m | signupForm = formModel }
        , fromModel = .signupForm
        , toMsg = toMsg
        , toRecord = toRecord
        , onSubmit = onSubmit
        }
        |> Ui.Form2.withInput
            { wrap = Name
            , initialValue = Nothing
            , attrs = []
            }
        |> Ui.Form2.withInput
            { wrap = Maybe.andThen String.toInt >> Age
            , initialValue = Nothing
            , attrs = []
            }
        |> Ui.Form2.withInput
            { wrap = EmailAddress
            , initialValue = Nothing
            , attrs = []
            }
        |> Ui.Form2.build
