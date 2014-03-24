-- The Telengard Dungeon Map

import Keyboard

data Boundary = EMPTY | DOOR | WALL

data Feature = NONE | ELEVATOR | PIT | TELEPORTER | STAIRS | ALTAR | FOUNTAIN | CUBE | THRONE | BOX

data Command = ERROR | NORTH | EAST | SOUTH | WEST | UP | DOWN

type Room = {top : Boundary, left : Boundary, feature : Feature}

type GameState = {x : Int, y : Int, z : Int}

type Coord = Int

xo = 1.6915
yo = 1.4278
zo = 1.2462

frac : Float -> Float
frac r = r - toFloat (floor r)

decodeBoundary : Int -> Boundary
decodeBoundary x = let n = x `mod` 4 in
  if | n == 2 -> DOOR
     | n == 3 -> WALL
     | otherwise -> EMPTY

decodeBoundaryAt : Coord -> Int -> Boundary
decodeBoundaryAt c h = if c == 1 || c == 201 then WALL else decodeBoundary h

decodeFeature : Int -> Feature
decodeFeature n = case n of
  0 -> NONE
  1 -> ELEVATOR
  2 -> PIT
  3 -> TELEPORTER
  4 -> STAIRS
  5 -> ALTAR
  6 -> FOUNTAIN
  7 -> CUBE
  8 -> THRONE
  9 -> BOX

  10 -> ELEVATOR
  11 -> PIT
  12 -> TELEPORTER
  13 -> STAIRS
  14 -> ALTAR
  15 -> FOUNTAIN

codesAt : Coord -> Coord -> Coord -> (Int, Int)
codesAt x y z = let
    fx = toFloat x
    fy = toFloat y
    fz = toFloat z
    q = fx * xo + fy * yo + fz * zo + (fx + xo) * (fy + yo) * (fz + zo)
    h = floor (frac q * 4694)
    hi = h `div` 256
    ft = if hi == 0 || hi > 5 then 0 else floor (frac (10 * q) * 15) + 1
  in
    (h, ft)  

roomAt : Coord -> Coord -> Coord -> Room
roomAt x y z = if x <= 0 || y <= 0 || x >= 202 || y >= 202 || z <= 0 || z > 50 then {top = EMPTY, left = EMPTY, feature = NONE} else
  if x == 201 && y > 0 && y < 201 then {top = EMPTY, left = WALL, feature = NONE} else
  if y == 201 && x > 0 && x < 201 then {top = WALL, left = EMPTY, feature = NONE} else
  if x == 201 && y == 201 then {top = EMPTY, left = EMPTY, feature = NONE} else
  let
    fx = toFloat x
    fy = toFloat y
    fz = toFloat z
    q = fx * xo + fy * yo + fz * zo + (fx + xo) * (fy + yo) * (fz + zo)
    h = floor (frac q * 4694)
    hi = h `div` 256
    ft = if hi == 0 || hi > 5 then 0 else floor (frac (10 * q) * 15) + 1
  in
    {top = decodeBoundaryAt y h, left = decodeBoundaryAt x (h `div` 4), feature = decodeFeature ft}

hdoor = image 56 8 "hdoor.png" |> toForm
hwall = image 56 8 "hwall.png" |> toForm
vdoor = image 8 56 "vdoor.png" |> toForm
vwall = image 8 56 "vwall.png" |> toForm

stairsUp = image 24 21 "stairs.png" |> toForm
stairsDown = image 24 21 "stairsDown.png" |> toForm
stairsBoth = image 24 21 "stairsBoth.png" |> toForm
elevator = image 24 21 "elevator.png" |> toForm
pit = image 24 21 "pit.png" |> toForm
teleport = image 24 21 "teleport.png" |> toForm
altar = image 24 21 "altar.png" |> toForm
fountain = image 24 21 "fountain.png" |> toForm
cube = image 8 8 "grayMistyCube.png" |> toForm
throne = image 24 21 "throne.png" |> toForm
box = image 8 8 "box.png" |> toForm

featureSymbolAt : Int -> Feature -> Feature -> [Form]
featureSymbolAt z f a = case (f, a) of
  (STAIRS, STAIRS) -> [stairsBoth]
  (_, STAIRS) -> [stairsUp]
  (NONE, _) -> []
  (ELEVATOR, _) -> if z <= 1 then [stairsUp] else [elevator]
  (PIT, _) -> if z >= 50 then [elevator] else [pit]
  (TELEPORTER, _) -> [teleport]
  (STAIRS, _) -> if z >= 50 then [] else [stairsDown]
  (ALTAR, _) -> [altar]
  (FOUNTAIN, _) -> [fountain]
  (CUBE, _) -> [cube]
  (THRONE, _) -> [throne]
  (BOX, _) -> [box]

drawRoom : Int -> Room -> Room -> [Form]
drawRoom z r a =
  (case r.top of
    EMPTY -> []
    DOOR -> [hdoor |> moveY 24]
    WALL -> [hwall |> moveY 24])
  ++ (case r.left of
    EMPTY -> []
    DOOR -> [vdoor |> moveX -24]
    WALL -> [vwall |> moveX -24])
  ++ featureSymbolAt z r.feature a.feature

drawRoomAt : Coord -> Coord -> Coord -> (Int, Int) -> [Form]
drawRoomAt x y z (dx, dy) = drawRoom z (roomAt (x + dx) (y + dy) z) (roomAt (x + dx) (y + dy) (z - 1)) |>
  map (move (48 * toFloat dx, -48 * toFloat dy))

offsets = [-7 .. 7]

allOffsets : [(Int, Int)]
allOffsets = concatMap (\y -> map (\ x -> (x, y)) offsets) offsets

drawNeighborhood : Coord -> Coord -> Coord -> [Form]
drawNeighborhood x y z = concatMap (drawRoomAt x y z) allOffsets

drawView : GameState -> Element
drawView {x, y, z} = collage 650 650 (drawNeighborhood x y z) |> color black

bound : Int -> Int
bound x = clamp 1 200 x

vbound : Int -> Int
vbound x = clamp 1 50 x

update : Command -> GameState -> GameState
update c s = case c of
  ERROR -> s
  NORTH -> {s | y <- bound (s.y - 1)}
  EAST -> {s | x <- bound (s.x + 1)}
  SOUTH -> {s | y <- bound (s.y + 1)}
  WEST -> {s | x <- bound (s.x - 1)}
  UP -> {s | z <- vbound (s.z - 1)}
  DOWN -> {s | z <- vbound (s.z + 1)}

keyCodeCommand : Keyboard.KeyCode -> Command
keyCodeCommand n = case n of
  37 -> WEST
  38 -> NORTH
  39 -> EAST
  40 -> SOUTH
  68 -> DOWN
  85 -> UP
  _ -> ERROR

-- update : {x : Int, y : Int} -> GameState -> GameState
-- update o s = {s | x <- bound (s.x + o.x), y <- bound (s.y - o.y)}

initState = {x = 25, y = 13, z = 1}

commandStream : Signal Command
commandStream = keyCodeCommand <~ Keyboard.lastPressed

stateSignal : Signal GameState
stateSignal = foldp update initState commandStream

main : Signal Element
main = drawView <~ stateSignal

--main = asText . show <~ Keyboard.lastPressed

