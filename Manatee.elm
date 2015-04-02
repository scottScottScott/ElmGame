import Color (..)
import Graphics.Collage (..)
import Graphics.Element (..)
import Keyboard
import Signal
import Time (..)
import Window
import Text
import List

type alias State = {x:Float, y:Float, angle:Float, speed:Float, 
                    titleOpac:Float, jumped:Bool, jumpAngle:Float, 
                    crabGoRight:Bool, crabX:Float, planetRotation:Float, 
                    t:Float, xInit:Float, yInit:Float}

manateeSim : State
manateeSim = { x = 0, y = 0, angle = 0, speed = 0, titleOpac = 1, jumped = False, jumpAngle = 0, crabGoRight = True, crabX = 0, t = 0, xInit = 0, yInit = 0, planetRotation = 0}

update_angle : Int -> State -> State
update_angle x m = if | m.jumped  -> { m | angle <- m.angle - (toFloat x) / 5 }
                      | otherwise -> { m | angle <- m.angle - m.speed * (toFloat x) / 3 }

update_speed : Int -> State -> State
update_speed y m = if | m.jumped                   -> m
                      | y == 1 && (m.speed < 0.35) -> { m | speed <- m.speed + 0.008} 
                      | y == 1 && (m.speed < 0.60) -> { m | speed <- m.speed + 0.004}
                      | y /= 1 && m.speed > 0      -> { m | speed <- m.speed - 0.010}
                      | y /= 1 && m.speed < 0      -> { m | speed <- 0}
                      | otherwise                  -> m

checkVerticalBounds : Float -> State -> State
checkVerticalBounds h m = let lineUp    = 0.7 * h / 10
                              lineDown  = -h - (3 * h / 16)
                              dir       = sin m.angle
                              initSpeed = (sin m.angle) * (m.speed + 0.1) * (sin m.jumpAngle) |> abs
                          in if | lineDown < m.y && m.y < lineUp && m.jumped -> { m | speed <- initSpeed, jumped <- False }
                                | lineDown < m.y && m.y < lineUp || m.jumped -> m
                                | m.y > lineUp && not m.jumped               -> { m | jumped <- True, jumpAngle <- m.angle, t <- 0, xInit <- m.x, yInit <- m.y}
                                | otherwise                                  -> if | dir < 0     -> { m | speed <- 0, y <-lineDown + 75 }
                                                                                   | otherwise   -> m

checkHorizontalBounds : Float -> State -> State
checkHorizontalBounds w m = let leftBound  = -w / 2
                                rightBound = w / 2
                                dir        = cos m.angle
                        in if | m.x < leftBound && dir < 0  -> { m | x <- rightBound}
                              | m.x > rightBound && dir > 0 -> { m | x <- leftBound}
                              | otherwise                   -> m

update_position: Float -> State -> State
update_position t m = if | m.jumped  -> { m | x <- m.speed * (cos m.jumpAngle) * m.t + m.xInit,
                                              y <- (m.speed * (sin m.jumpAngle) * m.t) - (0.0003 * m.t * m.t) + m.yInit,
                                              t <- m.t + t
                                        }
                         | otherwise -> { m | x <- m.x + t * m.speed * cos m.angle,
                                              y <- m.y + t * m.speed * sin m.angle
                                        }

toggleTitleOpacity: State -> State
toggleTitleOpacity m = if | m.titleOpac > 0 -> { m | titleOpac <- m.titleOpac - 0.005 }
                          | otherwise       -> { m | titleOpac <- 0}

moveCrab : Float -> State -> State
moveCrab w m = if | m.crabGoRight && m.crabX > w / 2      -> { m | crabGoRight <- False, crabX <- m.crabX - 3}
                  | not m.crabGoRight && m.crabX < -w / 2 -> { m | crabGoRight <- True, crabX <- m.crabX + 3}
                  | m.crabGoRight                         -> { m | crabX <- m.crabX + 3}
                  | not m.crabGoRight                     -> { m | crabX <- m.crabX - 3}

rotatePlanet : State -> State
rotatePlanet m = { m | planetRotation <- m.planetRotation + 1.0 / 4 }

step : (Float, { x:Int, y:Int}, (Int, Int)) -> State -> State
step (dt, {x, y} , (w', h')) =
    update_angle x 
        >> update_speed y
        >> update_position dt
        >> checkHorizontalBounds (toFloat w')
        >> checkVerticalBounds (toFloat h')
        >> toggleTitleOpacity
        >> moveCrab (toFloat w')
        >> rotatePlanet
  
render : (Int, Int) -> State -> Element
render (w',h') m =
  let (w,h) = (toFloat w', toFloat h')
      src = "manateeRight.png" 
      initDegrees = degrees (-40)
      oceanWave = List.map (\x -> (x, sin (0.05 * x) * 6)) [0..w] 
                      |> path 
                      |> traced { defaultLine | width <- 14, color <- blue} 
                      |> move (-w / 2, -h / 20)
      sandWave  = List.map (\x -> (x, cos (0.03 * x) * 5)) [0..w]
                      |> path 
                      |> traced { defaultLine | width <- 14, color <- (rgb 57 39 27)} 
                      |> move (-w / 2, -3 * h / 8)
      title     = Text.fromString "Manatee Simulator 2015:" 
                      |> Text.height 50 
                      |> Text.color white 
                      |> Text.bold
                      |> Text.typeface ["futura"]
                      |> Text.centered 
                      |> opacity m.titleOpac 
                      |> toForm
                      |> move (0, 40)
      subTitle  = Text.fromString "An Homage to Rawkin's 2006 Dolphin Olympics"       
                      |> Text.height 25 
                      |> Text.color white 
                      |> Text.typeface ["futura"]
                      |> Text.centered
                      |> opacity (m.titleOpac - 0.1)
                      |> toForm
      moon      = toForm (image 160 160 "moon.png") 
                      |> move (400, (7 * h) / 20)
      neptune   = toForm (image 300 300 "Neptune.png") 
                      |> move (-400, (6 * h) / 20)
                      |> rotate m.planetRotation
      iss       = toForm (image 135 150 "ISS.png") 
                      |> move (200, -(3 * h) / 20)
      manatee   = toForm (image 110 110 src) 
                      |> rotate initDegrees 
                      |> rotate m.angle 
      starfish1 = toForm (image 80 80 "starfish1.png") 
                      |> move (100, -3 * h / 8 - h / 16)
      starfish2 = toForm (image 70 70 "starfish2.png") 
                      |> move (-200, -3 * h / 8 - h / 16)
      starfish3 = toForm (image 90 90 "starfish3.png") 
                      |> move (-500, -3 * h / 8 - h / 16)
      crab      = toForm (image 100 70 "crab.png")
                      |> move (m.crabX, -3 * h / 8 - h / 16)
      ocean     = rect w h 
                      |> filled (rgb 40 100 150)
      sky       = rect w h 
                      |> filled (rgb 0 0 0) 
                      |> move (0,  (9 * h) / 20)
      skyAll    = rect w h 
                      |> filled (rgb 0 0 0) 
      ground    = rect w (h / 8) 
                      |> filled (rgb 61 43 31) 
                      |> move (0, -3 * h / 8 - h / 16)
      depthFrnt = Text.fromString "Altitude: "
                      |> Text.color black
      depthEnd  = Text.fromString " ft" 
                      |> Text.color black
      depth     = (m.y - h / 15) / 10
                      |> floor
                      |> toString
                      |> Text.fromString
                      |> Text.color yellow
                      |> Text.append depthFrnt
                      |> (flip Text.append) depthEnd
                      |> Text.height 25
                      |> Text.bold
                      |> Text.typeface ["futura"]
                      |> Text.leftAligned
                      |> toForm
                      |> move( 4 * w / 10, -h / 3)
      depthFrntWhit = Text.fromString "Altitude: "
                      |> Text.color white
      depthEndWhit  = Text.fromString " ft" 
                      |> Text.color white
      depthWhit  = (m.y - h / 15) / 10
                      |> floor
                      |> toString
                      |> Text.fromString
                      |> Text.color yellow
                      |> Text.append depthFrntWhit
                      |> (flip Text.append) depthEndWhit
                      |> Text.height 25
                      |> Text.bold
                      |> Text.typeface ["futura"]
                      |> Text.leftAligned
                      |> toForm
                      |> move( 4 * w / 10, -h / 3)
      speedFrnt = Text.fromString "Speed: "
                      |> Text.color black
      speedEnd  = Text.fromString " mph" 
                      |> Text.color black
      speed     = m.speed * 100
                      |> floor
                      |> toString
                      |> Text.fromString
                      |> Text.color yellow
                      |> Text.append speedFrnt
                      |> (flip Text.append) speedEnd
                      |> Text.height 25
                      |> Text.bold
                      |> Text.typeface ["futura"]
                      |> Text.leftAligned
                      |> toForm
                      |> move( 4 * w / 10, -h / 3 + 40)
      speedFrntWhit = Text.fromString "Speed: "
                      |> Text.color white
      speedEndWhit  = Text.fromString " mph" 
                      |> Text.color white
      speedWhit = m.speed * 100
                      |> floor
                      |> toString
                      |> Text.fromString
                      |> Text.color yellow
                      |> Text.append speedFrntWhit
                      |> (flip Text.append) speedEndWhit
                      |> Text.height 25
                      |> Text.bold
                      |> Text.typeface ["futura"]
                      |> Text.leftAligned
                      |> toForm
                      |> move( 4 * w / 10, -h / 3 + 40)

  in if | m.y >= h / 2 + (3 * h) / 20 ->
           collage w' h'
           [ skyAll
           , neptune
           , iss
           , title
           , subTitle
           , manatee
                 |> move (m.x, m.y - h - 3 * h / 20)
           , depthWhit
           , speedWhit
           ]
       | m.y >= -h / 2 + (3 * h) / 20 ->
           collage w' h'
           [ ocean
           , sky
           , oceanWave
           , moon
           , title
           , subTitle
           , manatee
                 |> move (m.x, m.y - (3 * h) / 20)
           , depth
           , speed
           ]
        | otherwise ->
           collage w' h'
           [ ocean
           , ground
           , sandWave
           , starfish1
           , starfish2
           , starfish3
           , crab
           , title
           , subTitle
           , manatee
                 |> move (m.x, m.y - (3 * h) / 20 + h)
           , depth
           , speed
           ]

input : Signal (Float, { x:Int, y:Int}, (Int, Int))
input = let delta = fps 20
        in Signal.sampleOn delta (Signal.map3 (, ,) delta Keyboard.arrows Window.dimensions)

main : Signal Element
main = Signal.map2 render Window.dimensions (Signal.foldp step manateeSim input)
