module Pi exposing (main)

-- Add/modify imports if you'd like. ---------------------------------

import Random exposing (Generator, Seed)
import Time exposing (..)
import Html exposing (..)
import Color exposing (..)
import Collage exposing (Shape,Form)
import Element exposing (toHtml)
import Platform.Sub exposing (batch)


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
  , seed : Seed }

type Msg =
  Tick

init : (Model, Cmd Msg)
init =
  (initialModel, Cmd.none)

initialModel : Model
initialModel =
  { hits = []
  , misses = []
  , hitCount = 0
  , missCount = 0
  , seed = Random.initialSeed 4308 }

subscriptions : Model -> Sub Msg
subscriptions model =
  batch
    [ every millisecond (\_ -> Tick) ]

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

genPi : Model -> String
genPi model =
  let numerator = toFloat model.hitCount in
    let denominator = toFloat (model.hitCount + model.missCount) in
      let pie = (numerator / denominator) * (toFloat 4) in
        toString pie


pointsToCircles : Float -> Color -> List Point -> List Form
pointsToCircles s c points =
  let circles = Collage.circle 2 in
    List.map (\point -> Collage.filled c circles
      |> Collage.move (s * point.x, s * point.y)) points



view : Model -> Html Msg
view model =
  div []
    [
      let l = 500 in
        pointsToCircles (l/2) red model.hits
          |> List.append (pointsToCircles (l/2) green model.misses)
          |> Collage.collage l l
          |> toHtml
      , genPi model |> text
    ]
