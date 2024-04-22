module Signup exposing (Editor(..), Error, Errors, Fieldset, Record, TrafficLight, form, fromErrors, trafficLightDecoder, trafficLightValues)

import Html
import Html.Form
import Html.Form.Validation
import Json.Decode
import Json.Encode


type Editor
    = Name (Maybe String)
    | Age (Maybe Int)
    | EmailAddress (Maybe String)
    | Subscribe (Maybe Bool)
    | TrafficLight (Maybe TrafficLight)


type alias Record =
    { name : String
    , age : Int
    , emailAddress : String
    , subscribe : Bool
    , trafficLight : TrafficLight
    }


type alias Error =
    String


type alias Fieldset msg =
    { name : Html.Form.Field Error msg
    , age : Html.Form.Field Error msg
    , emailAddress : Html.Form.Field Error msg
    , subscribe : Html.Form.Field Error msg
    , trafficLight : Html.Form.Field Error msg
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

                TrafficLight trafficLight ->
                    { acc | trafficLight = trafficLight }
        )
        { name = Nothing
        , age = Nothing
        , emailAddress = Nothing
        , subscribe = Nothing
        , trafficLight = Nothing
        }
        editors
        |> (\e -> Maybe.map5 Record e.name e.age e.emailAddress e.subscribe e.trafficLight)


type TrafficLight
    = Red
    | Yellow
    | Green


trafficLightValues : List TrafficLight
trafficLightValues =
    [ Red
    , Yellow
    , Green
    ]


trafficLightDecoder : Json.Decode.Decoder TrafficLight
trafficLightDecoder =
    Json.Decode.andThen
        (\str ->
            case Debug.log "trafficLightDecoder" str of
                "red" ->
                    Json.Decode.succeed Red

                "yellow" ->
                    Json.Decode.succeed Yellow

                "green" ->
                    Json.Decode.succeed Green

                _ ->
                    Json.Decode.fail <| str ++ " is not a valid value for TrafficLight"
        )
        Json.Decode.string


trafficLightEncoder : TrafficLight -> Json.Encode.Value
trafficLightEncoder value =
    Json.Encode.string <|
        case value of
            Red ->
                "red"

            Yellow ->
                "yellow"

            Green ->
                "green"


form :
    { toMsg : Html.Form.Msg Editor -> msg, onSubmit : Result (List Error) Record -> msg }
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
        |> Html.Form.withField EmailAddress
            (Html.Form.input
                |> Html.Form.withValidation
                    (Html.Form.Validation.when.editingOrBlurred
                        (\args ->
                            case args.self of
                                EmailAddress (Just v) ->
                                    if v == "" then
                                        Err "Email address must not be empty"

                                    else
                                        Ok args.self

                                _ ->
                                    Err "Email address must not be empty"
                        )
                        |> Html.Form.Validation.andThen
                            (\args ->
                                case args.self of
                                    EmailAddress (Just v) ->
                                        if not <| String.contains "@" v then
                                            Err "Email addresses must contain an @ symbol"

                                        else
                                            Ok args.self

                                    _ ->
                                        Ok args.self
                            )
                    )
            )
        |> Html.Form.withField Subscribe (Html.Form.checkbox |> Html.Form.withStopPropagation False)
        |> Html.Form.withField TrafficLight
            (Html.Form.dropdown
                { decoder = trafficLightDecoder
                , encoder = trafficLightEncoder
                , toOption =
                    { label = Debug.toString >> Html.text
                    , element = Html.option
                    }
                }
                trafficLightValues
            )
        |> Html.Form.build



-- optionally, if you're really snapped, you can restructure your errors, too


type alias Errors =
    { name : Maybe { editor : Editor, errors : List Error }
    , age : Maybe { editor : Editor, errors : List Error }
    , emailAddress : Maybe { editor : Editor, errors : List Error }
    , subscribe : Maybe { editor : Editor, errors : List Error }
    , trafficLight : Maybe { editor : Editor, errors : List Error }
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

                TrafficLight _ ->
                    { acc | trafficLight = Just step }
        )
        { name = Nothing
        , age = Nothing
        , emailAddress = Nothing
        , subscribe = Nothing
        , trafficLight = Nothing
        }
