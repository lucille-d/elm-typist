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



-- alphabet : WordList
-- alphabet =
--     [ "a", "b", "c", "d", "e", "f", "g", "h", "i", "j", "k", "l", "m", "n", "o", "p", "q", "r", "s", "t", "u", "v", "w", "x", "y", "z" ]


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
    , lastAction : String
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
    , lastAction = ""
    }


type Msg
    = KeyMsg Keyboard.KeyCode
    | Start
    | Tick Time


reinitWordList : Model -> Model
reinitWordList model =
    { model | words = defaultWords, lastAction = "" }


loadNexttWord : Model -> Model
loadNexttWord model =
    if List.length model.words == 0 then
        loadNexttWord (reinitWordList model)
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
                    loadNexttWord { model | lastAction = "success" }
                        |> score 10
                else
                    score 1
                        { model
                            | typedLetters = model.typedLetters ++ firstLetter
                            , textToType = remainingLetters
                            , lastAction = "success"
                        }
        else
            score -3 { model | lastAction = "fail" }


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Start ->
            ( loadNexttWord (reinitWordList model)
            , Cmd.none
            )

        KeyMsg code ->
            ( processTypedLetter code model
            , Cmd.none
            )

        Tick newTime ->
            ( { model | lastAction = "" }, Cmd.none )


subscriptions : Model -> Sub Msg
subscriptions model =
    if model.lastAction /= "" then
        Sub.batch
            [ Keyboard.downs KeyMsg, Time.every (300 * millisecond) Tick ]
    else
        Sub.batch
            [ Keyboard.downs KeyMsg ]


view : Model -> Html Msg
view model =
    div [ class "app" ]
        [ div [ class "score" ] [ text (toString model.score) ]
        , div [ class "text" ]
            [ span [ class "typed" ] [ text model.typedLetters ]
            , span [ class "remaining", class model.lastAction ] [ text model.textToType ]
            ]
        , button [ onClick Start ] [ text "START" ]
        , Html.node "link" [ Html.Attributes.rel "stylesheet", Html.Attributes.href "style.css" ] []
        ]
