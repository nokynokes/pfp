module Pi exposing (main)

-- Add/modify imports if you'd like. ---------------------------------

import Random exposing (Generator, Seed)
import Time
import Html exposing (..)


----------------------------------------------------------------------

main : Program Never Model Msg
main =
  Html.program
    { init = init
    , view = view
    , update = update
    , subscriptions = subscriptions
    }

type alias Point = { x:Float, y:Float }

type alias Model =
  { hits : List Point
  , misses : List Point
  , hitCount : Int
  , missCount : Int
  , seed : Seed
  }

type Msg = Tick

init : (Model, Cmd Msg)
init = (initialModel, Cmd.none)

initialModel : Model
initialModel =
  { hits = []
  , misses = []
  , hitCount = 0
  , missCount = 0
  , seed = Random.initialSeed 4308
  }



subscriptions : Model -> Sub Msg
subscriptions model =
  Time.every Time.millisecond (\_ -> Tick)

randomFloat : Generator Float
randomFloat = Random.float 0 1

point : Float -> Float -> Point
point f1 f2 = {x = f1, y = f2}

pointGenerator : Generator Point
pointGenerator =
  Random.map2 point randomFloat randomFloat

distance : Point -> Float
distance point = sqrt (point.x^2 + point.y^2)

insideUnitCircle : Point -> Bool
insideUnitCircle point = if distance point < 1 then True else False

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
          , seed = newSeed
          } ! []
        else
          { model
          | misses = point :: model.misses
          , missCount = model.missCount + 1
          , seed = newSeed
          } ! []

genPi : Model -> String
genPi model =
  let numerator = toFloat model.hitCount in
    let denominator = toFloat (model.hitCount + model.missCount) in
      let pie = (numerator / denominator) * (toFloat 4) in
        toString pie
        
view : Model -> Html Msg
view model =
  div []
    [
      h1 [] [text "Hello World!"]
    , div []
      [
        h2 [] [text "Value of pi is: "]
      , genPi model |> text
      ]
    ]
