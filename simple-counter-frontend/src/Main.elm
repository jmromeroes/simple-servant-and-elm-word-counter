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
import String
import Svg
--Visualization

import LineChart
import LineChart.Axis as Axis
import LineChart.Junk as Junk
import LineChart.Legends as Legends
import LineChart.Line as Line
import LineChart.Events as Events
import LineChart.Grid as Grid
import LineChart.Area as Area
import LineChart.Interpolation as Interpolation
import LineChart.Axis.Intersection as Intersection
import LineChart.Dots as Dots
import LineChart.Colors as Colors
import LineChart.Container as Container
import LineChart.Axis.Range as Range
import LineChart.Axis.Line as AxisLine
import LineChart.Axis.Ticks as Ticks
import LineChart.Axis.Tick as Tick
import LineChart.Axis.Title as Title

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
                ( { model | wordsCount = wordsCount }, Cmd.none)
            Err _ ->
                (model, Cmd.none)

                    
postWords : Model -> Cmd Msg
postWords model =
  Http.post
    { url = "http://localhost:3000/words/count?sortBy=desc"
    , body = Http.multipartBody ([ Http.stringPart "text" (.text model)] ++ (List.map  (\file_ -> Http.filePart "wordsFile" file_) (.files model)))
    , expect = Http.expectJson GotWords (D.list wordCountDecoder)
    }

-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
  Sub.none

-- VIEW

formatX : (Int, WordCount) -> String
formatX (_, wc) = .word wc

formatY : (Int, WordCount) -> String
formatY (_, wc) = String.fromInt (.count wc)

tickWord : ( Int, WordCount ) -> Tick.Config msg
tickWord ( value, wc ) =
    Tick.custom
      { position = toFloat value
      , color = Colors.gray
      , width = 1
      , length = 5
      , grid = True
      , direction = Tick.negative
      , label = Just (tickLabel (.word wc))
      }
        
tickLabel : String -> Svg.Svg msg
tickLabel =
    Junk.label Colors.black
        
xAxisConfig : Model -> Axis.Config (Int, WordCount) Msg
xAxisConfig model =
    Axis.custom
      { title = Title.default "Word"
      , variable = Just << toFloat << .count << Tuple.second
      , pixels = 1270
      , range = Range.padded 1 1
      , axisLine = AxisLine.none
      , ticks = Ticks.custom <| \dataRange axisRange ->
           List.indexedMap (\i w -> (tickWord (i, w))) (.wordsCount model)
      }
        
chartConfig : Model -> LineChart.Config (Int, WordCount) Msg
chartConfig model =
    { y = Axis.default 450 "word" (toFloat << Tuple.first)
    , x = xAxisConfig model
    , junk = Junk.default
    , interpolation = Interpolation.monotone
    , intersection = Intersection.default
    , legends = Legends.default
    , events = Events.custom []
    , grid = Grid.dots 1 Colors.gray
    , area = Area.stacked 0.5
    , line = Line.default
    , dots = Dots.custom (Dots.empty 5 1)
    , container = containerConfig             
    }
    
containerConfig : Container.Config Msg
containerConfig =
    Container.custom
      { attributesHtml = []
      , attributesSvg = []
      , size = Container.relative
      , margin = Container.Margin 30 100 30 70
      , id = "line-chart-area"
      }
        
chart : Model -> Html msg                                             
chart model =
   LineChart.view1 Tuple.first Tuple.second
    (List.indexedMap (\index w -> Tuple.pair (toFloat index) (toFloat (.count w))) (.wordsCount model))


mapWordCountToDiv : WordCount -> Html msg
mapWordCountToDiv wordCount =
    div []
      [
        span
            [
              style "font-weight" "bold"
            , style "min-width" "30px"
            ]
            [
             text ((.word wordCount) ++ ": ")
            ]
      , span
            [
            ]
            [
             text (String.repeat (.count wordCount) "*")
            ]
      ]

view : Model -> Html Msg
view model =
  div
    [ style "background" "white"
    , style "display" "flex"
    , style "flex-direction" "column"
    , style "margin" "1rem auto"
    , style "width" "480px"
    , style "justify-content" "center"
    , style "align-items" "center"
    , style "padding" "1rem"
    ]
    [ div
      []
      [
       button
       [
         style "background" "#f98e01"
       , style "color" "white"
       , style "font-weight" "500"
       , style "font-size" "20px"
       , style "margin" "1rem"
       , style "padding" ".5rem 2rem"
       , onClick Pick
       ]
       [
        text "Upload File"
       ]
      ]
    , div [] [
       textarea
        [ cols 100,
          rows 10,
          placeholder "Add the text so we can count its words",
          onInput Text
        ] []
      ]
    , div []
      [
        button
         [
           style "background" "#f98e01"
         , style "color" "white"
         , style "font-weight" "500"
         , style "font-size" "15px"
         , style "margin" "1rem"
         , style "padding" ".5rem 2rem"
         , onClick SubmitInformation
       ] [text "Count words"]]
    , div [] (List.map mapWordCountToDiv (.wordsCount model))
    , chart model
    ]


-- EXTRA

wordCountDecoder : Decoder WordCount
wordCountDecoder =
  map2 WordCount
      (field "word" string)
      (field "count" int)
