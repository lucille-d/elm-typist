module Main exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Keyboard exposing (..)
import Time exposing (..)
import Char exposing (toCode, fromCode)


type alias WordList =
    List String


defaultWords : WordList
defaultWords =
    [ "toto", "cornichon", "machin" ]


alphabet : WordList
alphabet =
    [ "a", "b", "c", "d", "e", "f", "g", "h", "i", "j", "k", "l", "m", "n", "o", "p", "q", "r", "s", "t", "u", "v", "w", "x", "y", "z" ]


main =
    Html.program
        { init = init
        , update = update
        , view = view
        , subscriptions = subscriptions
        }


type alias Model =
    { words : WordList
    , currentWordIndex : Int
    , textToType : String
    , typedLetters : String
    , score : Int
    , currentMode : String
    }


init : ( Model, Cmd Msg )
init =
    ( model, Cmd.none )


model : Model
model =
    { words = defaultWords
    , currentWordIndex = 0
    , textToType = ""
    , typedLetters = ""
    , score = 0
    , currentMode = ""
    }


type Msg
    = KeyMsg Keyboard.KeyCode
    | Start String


reinitWordList : Model -> Model
reinitWordList model =
    if model.currentMode == "words" then
        { model | words = defaultWords }
    else
        { model | words = alphabet }


loadNextWord : Model -> Model
loadNextWord model =
    if List.length model.words == 0 then
        { model | currentMode = "" }
    else
        { model
            | typedLetters = ""
            , textToType = Maybe.withDefault "truc" (List.head model.words)
            , words = List.drop 1 model.words
        }


score : Int -> Model -> Model
score points model =
    { model
        | score = model.score + points
    }


processTypedLetter : Char.KeyCode -> Model -> Model
processTypedLetter code model =
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
                    loadNextWord model
                        |> score 10
                else
                    score 1
                        { model
                            | typedLetters = model.typedLetters ++ firstLetter
                            , textToType = remainingLetters
                        }
        else
            score -3 model


changeMode : String -> Model -> Model
changeMode mode model =
    { model | currentMode = mode }


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Start mode ->
            ( changeMode mode model
                |> reinitWordList
                |> loadNextWord
            , Cmd.none
            )

        KeyMsg code ->
            ( processTypedLetter code model
            , Cmd.none
            )


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ Keyboard.downs KeyMsg ]


view : Model -> Html Msg
view model =
    div [ class "app" ]
        [ menuButtons model
        , scoreDiv model
        , gameDiv model
        , styleSheet model
        ]


menuButtons : Model -> Html Msg
menuButtons model =
    if model.currentMode == "" then
        div []
            [ button [ onClick (Start "words") ] [ text "Start Words" ]
            , button [ onClick (Start "alphabet") ] [ text "Start Alphabet" ]
            ]
    else
        div [] []


scoreDiv : Model -> Html Msg
scoreDiv model =
    if model.currentMode /= "" then
        div [ class "score" ] [ text (toString model.score) ]
    else
        div [] []


gameDiv : Model -> Html Msg
gameDiv model =
    if model.currentMode /= "" then
        div [ class "text" ]
            [ span [ class "typed" ] [ text model.typedLetters ]
            , span [ class "remaining" ] [ text model.textToType ]
            ]
    else
        div [] []


styleSheet : Model -> Html Msg
styleSheet model =
    Html.node "link" [ Html.Attributes.rel "stylesheet", Html.Attributes.href "style.css" ] []
