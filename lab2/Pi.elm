module Pi exposing (main)

-- Add/modify imports if you'd like. ---------------------------------

import Random exposing (Generator, Seed)
import Time exposing (..)
import Html exposing (..)
import Color exposing (..)
import Collage exposing (Shape,Form)
import Text exposing (fromString,Style,style,Text)
import Element exposing (toHtml)
import Platform.Sub exposing (batch)
import Window exposing (Size,resizes)
import Task


----------------------------------------------------------------------

main : Program Never Model Msg
main =
  Html.program
    { init = init
    , view = view
    , update = update
    , subscriptions = subscriptions }

type alias Point =
  { x : Float
  , y : Float }

type alias Model =
  { hits : List Point
  , misses : List Point
  , hitCount : Int
  , missCount : Int
  , seed : Seed
  , window : Window.Size}

type Msg =
  Tick
  | SizeUpdated Size

init : (Model, Cmd Msg)
init =
  (initialModel, initialSize)

initialSize : Cmd Msg
initialSize =
  Window.size
  |> Task.perform SizeUpdated

initialModel : Model
initialModel =
  { hits = []
  , misses = []
  , hitCount = 0
  , missCount = 0
  , seed = Random.initialSeed 4308
  , window = Size 0 0 }

subscriptions : Model -> Sub Msg
subscriptions model =
  batch
    [ every millisecond (\_ -> Tick)
    , resizes SizeUpdated ]

randomFloat : Generator Float
randomFloat =
  Random.float -1 1

point : Float -> Float -> Point
point f1 f2 =
  {x = f1, y = f2}

pointGenerator : Generator Point
pointGenerator =
  Random.map2 point randomFloat randomFloat

distance : Point -> Float
distance point =
  sqrt (point.x^2 + point.y^2)

insideUnitCircle : Point -> Bool
insideUnitCircle point = distance point < 1

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    SizeUpdated newSize ->
      {model | window = newSize} ! []
    Tick ->
      let (point, newSeed) =
        Random.step pointGenerator model.seed
      in
        if insideUnitCircle point then
          { model
          | hits = point :: model.hits
          , hitCount = model.hitCount + 1
          , seed = newSeed } ! []
        else
          { model
          | misses = point :: model.misses
          , missCount = model.missCount + 1
          , seed = newSeed } ! []


format : Text -> Text
format = Text.height 40 >> Text.monospace

genPi : Model -> Form
genPi model =
  let numerator = toFloat model.hitCount in
  let denominator = toFloat (model.hitCount + model.missCount) in
  let pie = (numerator / denominator) * (toFloat 4) |> toString in
    pie |> fromString |> format |> Collage.text |> Collage.move (0.0,0.0)


pointsToCircles : Float -> Float -> Color -> List Point -> List Form
pointsToCircles sX sY c points =
  let circles = Collage.circle 2 in
    List.map (\point -> Collage.filled c circles
      |> Collage.move (sX * point.x, sY * point.y)) points



view : Model -> Html Msg
view model =
  div []
    [
      let w = model.window.width |> toFloat in
      let h = model.window.height |> toFloat in
      let scaleX = w/2 in
      let scaleY = h/2 in
      let piTxT = genPi model in
        pointsToCircles scaleX scaleY red model.hits
          |> List.append (pointsToCircles scaleX scaleY green model.misses)
          |> List.append [ piTxT ]
          |> Collage.collage (round w) (round h)
          |> toHtml
    ]
