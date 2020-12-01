module Main exposing (..)

import Browser
import File exposing (File)
import File.Select as Select
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Json.Decode as D
import Task
import Json.Decode exposing (Decoder, map2, field, string, int)
import Http


-- MAIN


main =
  Browser.element
    { init = init
    , view = view
    , update = update
    , subscriptions = subscriptions
    }



-- MODEL

type alias WordCount =
  { word : String
  , count : Int
  }

type alias Model =
  { previews : List String
  , text : String
  , files : List File
  , wordsCount : List WordCount
  }


init : () -> (Model, Cmd Msg)
init _ =
  (Model [] "" [] [], Cmd.none)



-- UPDATE


type Msg
  = Pick
  | GotFiles File (List File)
  | Text String
  | SubmitInformation
  | GotWords (Result Http.Error (List WordCount))


update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    Pick ->
      ( model
      , Select.files ["text/plain"] GotFiles
      )

    GotFiles file files ->
      ( { model | files = file::files }
      , Cmd.none
      )

    Text text ->
      ( { model | text = text }
      , Cmd.none
      )

    SubmitInformation ->
        ( model
        , postWords model)

    GotWords result ->
        case result of
            Ok wordsCount ->
                (model, Cmd.none)
            Err _ ->
                (model, Cmd.none)

                    
postWords : Model -> Cmd Msg
postWords model =
  Http.post
    { url = "http://localhost:3000/words/count?sortBy=asc"
    , body = Http.multipartBody ([ Http.stringPart "text" (.text model)] ++ (List.map  (\file_ -> Http.filePart "wordsFile" file_) (.files model)))
    , expect = Http.expectJson GotWords (D.list wordCountDecoder)
    }

-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
  Sub.none



-- VIEW


view : Model -> Html Msg
view model =
  div
    [ style "background" "white"
    ]
    [ button [ onClick Pick ] [ text "Upload File" ]
    , div
        [ style "display" "flex"
        , style "align-items" "center"
        , style "height" "60px"
        , style "padding" "20px"
        ]
        [text (Debug.toString model)]
    , textarea
        [ cols 40,
          rows 10,
          placeholder "Add the text so we can count its words",
          onInput Text
        ] []
    , button [ onClick SubmitInformation ] [text "Count words"]
    ]


-- EXTRA

wordCountDecoder : Decoder WordCount
wordCountDecoder =
  map2 WordCount
      (field "word" string)
      (field "count" int)
