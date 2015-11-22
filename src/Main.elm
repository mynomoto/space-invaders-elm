-- See this document for more information on making Pong:
-- http://elm-lang.org/blog/pong
import Color exposing (..)
import Graphics.Collage exposing (..)
import Graphics.Element exposing (..)
import Keyboard
import Text
import Time exposing (..)
import Window


-- MODEL

(gameWidth,gameHeight) = (600,400)
(halfWidth,halfHeight) = (300,200)

(alienRows,alienCols) = (4,8)

bulletSpeed = 10
playerHorizontalSpeed = 10

type State = Play | Pause


type alias Alien =
  { x : Float
  , y : Float
  , vx : Float
  , vy : Float
  , w : Int
  , h : Int
  }


type alias Player =
  { x : Float
  , y : Float
  , vx : Float
  , vy : Float
  , w : Int
  , h : Int
  }

type alias Bullet =
  { x : Float
  , y : Float
  , vx : Float
  , vy : Float
  , w : Int
  , h : Int
  }


type alias Game =
  { state : State
  , aliens : List Alien
  , player : Player
  , lives : Int
  , bullets : List Bullet
  }


initPlayer : Player
initPlayer =
  Player 0 0 0 0 3


initBullet : Bullet
initBullet =
  Bullet 0 0 0 bulletSpeed


initGame : Game
initGame =
  { state = Pause
  , aliens = []--List.concat (List.map (\i -> List.map (\j -> Alien {x=i, y=j, vx=10, vy=0, w=10, h=10}) [1..alienCols]) [1..alienRows])
  , player = initPlayer
  , lives = 3
  , bullets = []
  }

type Direction = Left | Right | Idle

type alias Input =
  { start : Bool
  , shoot : Bool
  , dir : Direction
  , delta : Time
  }

collidedNone objs obj =
  not <| List.any (rectCollision obj) objs


dirToFloat : Direction -> Float
dirToFloat dir =
  case dir of
    Left -> -1.0
    Right -> 1.0
    Idle -> 0.0

-- UPDATE

update : Input -> Game -> Game
update {start,shoot,dir,delta} ({state,aliens,player,lives,bullets} as game) =
  let
    playerAlive =
      collidedNone aliens player

    newBullets =
      (if shoot then {initBullet | x=player.x} :: bullets else bullets)
        |> List.filter (collidedNone aliens)
        --|> List.map (physicsUpdate delta)

    newAliens =
      aliens
        |> List.filter (collidedNone (player :: bullets))
        |> List.map (physicsUpdate delta)

    newPlayer =
      physicsUpdate delta {player | vx = (dirToFloat dir) * playerHorizontalSpeed}

    newLives =
      if collidedNone player aliens then lives else lives-1

    newState =
      if start then
          Play

      else if player.lives <= 0 then
          Pause

      else
          state

  in
    { game |
        state = newState,
        aliens = newAliens,
        player = newPlayer,
        lives = newLives,
        bullets = newBullets
    }


rectCollision r1 r2 =
  let
    distant = r1.x+r1.w<r2.x || r2.x+r2.w<r1.x || r1.y+r1.h<r2.y || r2.y+r2.h<r1.y
  in
    not distant


physicsUpdate delta obj =
  { obj |
      x = obj.x + obj.vx * delta,
      y = obj.y + obj.vy * delta
  }


-- VIEW

view : (Int,Int) -> Game -> Element
view (w,h) game =
  let
    lives =
      txt (Text.height 50) (toString game.lives)
    bullets = game.bullets
                |> List.map (\bullet -> oval 10 10 |> make bullet)
    aliens = game.aliens
                |> List.map (\alien -> rect 12 12 |> make alien)

  in
    container w h middle <|
    collage gameWidth gameHeight
      ([ rect gameWidth gameHeight
          |> filled pongGreen
      , rect 30 30
          |> make game.player
      , toForm lives
          |> move (0, gameHeight/2 - 40)
      , toForm (if game.state == Play then spacer 1 1 else txt identity msg)
          |> move (0, 40 - gameHeight/2)
      ] ++ bullets ++ aliens)


pongGreen =
  rgb 60 100 60


textGreen =
  rgb 160 200 160


txt f string =
  Text.fromString string
    |> Text.color textGreen
    |> Text.monospace
    |> f
    |> leftAligned


msg = "SPACE to start, WS and &uarr;&darr; to move"

make obj shape =
  shape
    |> filled white
    |> move (obj.x, obj.y)


-- SIGNALS

main =
  Signal.map2 view Window.dimensions gameState


gameState : Signal Game
gameState =
  Signal.foldp update initGame input


delta =
  Signal.map inSeconds (fps 35)


input : Signal Input
input =
  Signal.sampleOn delta <|
    Signal.map4 Input
      Keyboard.enter
      Keyboard.space
      (Signal.map .x Keyboard.arrows)
      delta
