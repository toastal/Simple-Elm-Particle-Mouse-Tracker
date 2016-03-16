module Main (..) where

import Color exposing (Color, hsla)
import Graphics.Collage exposing (Shape, collage, filled, move, ngon, rotate, scale)
import Graphics.Element exposing (Element)
import List exposing ((::))
import Mouse
import Random
import Time
import Transducer exposing ((>>>), transduceList)
import Window


-- TYPES


type SpinDirection
  = Clockwise
  | CounterClockwise


type alias Particle =
  { x : Float
  , y : Float
  , hue : Float
  , alpha : Float
  , spin : SpinDirection
  , rotate : Float
  , scale : Float
  , seed : Float
  }


type alias State =
  { particles : List Particle
  , seed : Random.Seed
  , windowDimensions : ( Int, Int )
  }


-- UTILITY


primaryColor : Float -> Float -> Color
primaryColor hue alpha =
  hsla (degrees hue) 0.6 0.7 alpha


makeParticle : ( Float, Float ) -> Float -> Random.Seed -> ( Particle, Random.Seed )
makeParticle ( x, y ) tick seed =
  let
    ( px, seed0 ) =
      Random.generate (Random.float -16 16) seed

    ( py, seed1 ) =
      Random.generate (Random.float -12 12) seed0

    ( alpha, seed2 ) =
      Random.generate (Random.float 0.8 1) seed1

    ( spin, seed3 ) =
      Random.generate
        (Random.map
          (\b ->
            if b then
              Clockwise
            else
              CounterClockwise
          )
          Random.bool
        )
        seed2

    ( rotate, seed4 ) =
      Random.generate (Random.float 0 360) seed3

    ( pseed, seedLast ) =
      Random.generate (Random.float 0 1) seed4

    particle =
      { x = px + x
      , y = py + y
      , hue = tick
      , alpha = alpha
      , spin = spin
      , rotate = rotate
      , scale = 1
      , seed = pseed
      }
  in
    ( particle, seedLast )


-- MODEL


init : State
init =
  { particles = []
  , seed = Random.initialSeed 38
  , windowDimensions = ( 0, 0 )
  }


-- UPDATE


update : ( ( Int, Int ), ( Int, Int ), Float ) -> State -> State
update ( ( w, h ), ( x, y ), tick ) state =
  let
    ( dx, dy ) =
      case ( x, y ) of
        ( 0, 0 ) ->
          ( 0, 0 )

        _ ->
          ( toFloat x - toFloat w / 2, toFloat h / 2 - toFloat y )

    isTransparentish : Particle -> Bool
    isTransparentish =
      .alpha >> (<=) 3.5e-2

    adapt : Particle -> Particle
    adapt particle =
      { particle
        | y = particle.y + 3.25 + (10 * particle.seed)
        , alpha = (particle.alpha - 7.0e-2 * particle.seed) * 0.96
        , rotate =
            case particle.spin of
              Clockwise ->
                particle.rotate + (particle.seed * 0.7)

              _ ->
                particle.rotate - (particle.seed * 0.7)
        , scale = particle.scale * 1.03
      }

    updateParticles : List Particle -> List Particle
    updateParticles =
      transduceList (Transducer.filter isTransparentish >>> Transducer.map adapt)

    ( spawn, seed ) =
      makeParticle ( dx, dy ) tick state.seed
  in
    { state
      | particles = spawn :: (updateParticles state.particles)
      , seed = seed
      , windowDimensions = ( w, h )
    }


-- VIEW


view : State -> Element
view state =
  let
    ( w, h ) =
      state.windowDimensions

    renderParticle particle =
      ngon 6 11
        |> filled (primaryColor particle.hue particle.alpha)
        |> rotate particle.rotate
        |> scale particle.scale
        |> move ( particle.x, particle.y )
  in
    List.map renderParticle state.particles
      |> collage w h


-- GO TIME


main : Signal Element
main =
  Signal.foldp (\_ t -> t + 1) 0 (Time.fps 60)
    |> Signal.map3 (,,) Window.dimensions Mouse.position
    |> Signal.foldp update init
    |> Signal.map view
