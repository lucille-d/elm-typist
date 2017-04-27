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
    { textToType : String, remainingLetters : String, score : Int }


init : ( Model, Cmd Msg )
init =
    ( model, Cmd.none )


model : Model
model =
    { textToType = "cornichon", remainingLetters = "cornichon", score = 0 }


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
                    String.left 1 model.remainingLetters
            in
                if String.toUpper typedLetter == String.toUpper firstLetter then
                    let
                        newRemainingLetters =
                            String.dropLeft 1 model.remainingLetters
                    in
                        if String.length newRemainingLetters == 0 then
                            ( { model | remainingLetters = model.textToType, score = model.score + 10 }, Cmd.none )
                        else
                            ( { model | remainingLetters = newRemainingLetters, score = model.score + 1 }, Cmd.none )
                else
                    ( { model | score = model.score - 3 }, Cmd.none )


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ Keyboard.downs KeyMsg ]


view : Model -> Html Msg
view model =
    div []
        [ div [] [ text model.remainingLetters ]
        , div [] [ text (toString model.score) ]
        ]
