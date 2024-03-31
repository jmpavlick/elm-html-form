module Ui.Form exposing (..)

import Dict exposing (Dict)
import Html exposing (Html)
import Html.Attributes as Attr
import Html.Events


type Model editor
    = Model (InternalModel editor)


type alias InternalModel editor =
    { editors : Dict Int editor }


type Msg editor
    = UserUpdatedField Int editor


update : Msg editor -> Model editor -> ( Model editor, Cmd (Msg editor) )
update msg (Model model) =
    case msg of
        UserUpdatedField index editor ->
            ( Model { model | editors = Dict.insert index editor model.editors }
            , Cmd.none
            )


type Init editor record model msg
    = Init
        { toModel : model -> Model editor -> model
        , fromModel : model -> Model editor
        , toMsg : Msg editor -> msg
        , toRecord : editor -> Maybe record
        , fields : Dict Int (model -> Html msg)
        , initModel : Model editor -> Model editor
        }


init :
    { toModel : model -> Model editor -> model
    , fromModel : model -> Model editor
    , toMsg : Msg editor -> msg
    , toRecord : editor -> Maybe record
    }
    -> Init editor record model msg
init { toModel, fromModel, toMsg, toRecord } =
    Init
        { toModel = toModel
        , fromModel = fromModel
        , toMsg = toMsg
        , toRecord = toRecord
        , fields = Dict.empty
        , initModel = identity
        }


withInput :
    { wrap : Maybe String -> editor
    , initialValue : Maybe String
    , attrs : List (Html.Attribute msg)
    }
    -> Init editor record model msg
    -> Init editor record model msg
withInput { wrap, initialValue, attrs } (Init init_) =
    let
        internalModel : model -> InternalModel editor
        internalModel =
            init_.fromModel >> (\(Model m) -> m)

        initEditor : editor
        initEditor =
            wrap initialValue

        nextIndex : Int
        nextIndex =
            Dict.size init_.fields

        withValueAttr : model -> List (Html.Attribute msg) -> List (Html.Attribute msg)
        withValueAttr model attrs_ =
            Maybe.andThen
                (\value ->
                    if value == initEditor then
                        Maybe.map (\v -> Attr.value v :: attrs_) initialValue

                    else
                        Nothing
                )
                ((internalModel model).editors |> Dict.get nextIndex)
                |> Maybe.withDefault attrs

        withOnInput : List (Html.Attribute msg) -> List (Html.Attribute msg)
        withOnInput attrs_ =
            Html.Events.onInput (Just >> wrap >> UserUpdatedField nextIndex >> init_.toMsg)
                :: attrs_

        field =
            \model ->
                Html.input
                    (attrs |> withValueAttr model |> withOnInput)
                    []
    in
    Init
        { init_
            | fields = Dict.insert nextIndex field init_.fields
            , initModel =
                init_.initModel
                    >> (\(Model m) -> Model { m | editors = Dict.insert nextIndex initEditor m.editors })
        }


type alias Module editor model msg =
    { init : ( model, Cmd msg ) -> ( model, Cmd msg )
    , elements : { fields : List (Html msg), submitMsg : msg }
    , update : Msg editor -> model -> ( model, Cmd msg )
    }


build : Init editor record model msg -> Module editor model msg
build (Init init_) =
    { init = Debug.todo ""
    , elements = Debug.todo ""
    , update = Debug.todo ""
    }
