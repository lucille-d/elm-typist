module Main exposing (..)

import Html exposing (..)
import Keyboard exposing (..)
import Debug exposing (..)
import Char exposing (toCode, fromCode)


main =
    Html.program
        { init = init
        , update = update
        , view = view
        , subscriptions = subscriptions
        }


type alias Model =
    { textToType : String }


init : ( Model, Cmd Msg )
init =
    ( model, Cmd.none )


model : Model
model =
    { textToType = "cornichon" }


type Msg
    = KeyMsg Keyboard.KeyCode


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        KeyMsg code ->
            let
                typedLetter =
                    String.fromChar (Char.fromCode code)

                firstLetter =
                    String.left 1 model.textToType
            in
                if String.toUpper typedLetter == String.toUpper firstLetter then
                    let
                        remainingLetters =
                            String.dropLeft 1 model.textToType
                    in
                        if String.length remainingLetters == 0 then
                            ( { model | textToType = "FINI" }, Cmd.none )
                        else
                            ( { model | textToType = remainingLetters }, Cmd.none )
                else
                    ( model, Cmd.none )


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ Keyboard.downs KeyMsg ]


view : Model -> Html Msg
view model =
    div [] [ text model.textToType ]
