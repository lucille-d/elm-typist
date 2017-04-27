module Main exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)
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
    { textToType : String, typedLetters : String, score : Int }


init : ( Model, Cmd Msg )
init =
    ( model, Cmd.none )


model : Model
model =
    { textToType = "cornichon", typedLetters = "", score = 0 }


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
                            ( { model
                                | typedLetters = model.typedLetters ++ firstLetter
                                , textToType = remainingLetters
                                , score = model.score + 10
                              }
                            , Cmd.none
                            )
                        else
                            ( { model
                                | typedLetters = model.typedLetters ++ firstLetter
                                , textToType = remainingLetters
                                , score = model.score + 1
                              }
                            , Cmd.none
                            )
                else
                    ( { model | score = model.score - 3 }, Cmd.none )


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ Keyboard.downs KeyMsg ]


view : Model -> Html Msg
view model =
    div [ class "app" ]
        [ div [ class "score" ] [ text (toString model.score) ]
        , div [ class "text" ]
            [ span [ class "typed" ] [ text model.typedLetters ]
            , span [ class "remaining" ] [ text model.textToType ]
            ]
        , Html.node "link" [ Html.Attributes.rel "stylesheet", Html.Attributes.href "style.css" ] []
        ]
