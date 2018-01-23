module Pi exposing (main)

-- Add/modify imports if you'd like. ---------------------------------

import Random exposing (Generator, Seed)
import Time
import Html exposing (Html)


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
  Time.every Time.second (\_ -> Tick)

randomFloat : Generator Float
randomFloat = Random.float 0 1

point : Float -> Float -> Point
point f1 f2 = {x = f1, y = f2}

pointGenerator : Generator Point
pointGenerator =
  Random.map2 point randomFloat randomFloat

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    Tick ->
      let (point, newSeed) =
        Random.step pointGenerator model.seed
      in
        Debug.crash "TODO"

view : Model -> Html Msg
view model =
  Debug.crash "TODO"
