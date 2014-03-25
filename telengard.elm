-- The Telengard Dungeon Map

import Keyboard

-- Abstract Model : Rooms and the player position

data Boundary = EMPTY | DOOR | WALL

data Feature = NONE | ELEVATOR | PIT | TELEPORTER | STAIRS_UP | STAIRS_DOWN
  | STAIRS_BOTH | ALTAR | FOUNTAIN | CUBE | THRONE | BOX

data Command = ERROR | NORTH | EAST | SOUTH | WEST | UP | DOWN

type Room = {top : Boundary, left : Boundary, feature : Feature}

type Position = {x : Int, y : Int, z : Int}

type GameState = Position

-- Control : Movement commands

keyCodeCommand : Keyboard.KeyCode -> Command
keyCodeCommand n = case n of
  37 -> WEST
  38 -> NORTH
  39 -> EAST
  40 -> SOUTH
  68 -> DOWN
  85 -> UP
  _ -> ERROR

commandStream : Signal Command
commandStream = keyCodeCommand <~ Keyboard.lastPressed

-- State Changes: How the player moves

initState = {x = 25, y = 13, z = 1}

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

stateSignal : Signal GameState
stateSignal = foldp update initState commandStream

-- Concrete Model: A procedurally generated map

decodeBoundary : Int -> Boundary
decodeBoundary x = let n = x `mod` 4 in
  if | n == 2 -> DOOR
     | n == 3 -> WALL
     | otherwise -> EMPTY

decodeBoundaryAt : Int -> Int -> Boundary
decodeBoundaryAt c h = if c == 1 || c == 201 then WALL else decodeBoundary h

decodeFeature : Int -> Feature
decodeFeature n = case n of
  0 -> NONE
  1 -> ELEVATOR
  2 -> PIT
  3 -> TELEPORTER
  4 -> STAIRS_DOWN
  5 -> ALTAR
  6 -> FOUNTAIN
  7 -> CUBE
  8 -> THRONE
  9 -> BOX

  10 -> ELEVATOR
  11 -> PIT
  12 -> TELEPORTER
  13 -> STAIRS_DOWN
  14 -> ALTAR
  15 -> FOUNTAIN

emptyRoom = {top = EMPTY, left = EMPTY, feature = NONE}

onSoutheastCorner : Position -> Bool
onSoutheastCorner {x, y, z} = x == 201 && y == 201

inBounds : Position -> Bool
inBounds {x, y, z} = x >= 1 && x <= 201 && y >= 1 && y <= 201 && z >= 1 && z <= 50

xo = 1.6915
yo = 1.4278
zo = 1.2462

frac : Float -> Float
frac r = r - toFloat (floor r)

rawRoomAt : Position -> Room
rawRoomAt p = if not (inBounds p) || onSoutheastCorner p then emptyRoom else
  if p.x == 201 then {emptyRoom | left <- WALL} else
  if p.y == 201 then {emptyRoom | top <- WALL} else
  let
    fx = toFloat p.x
    fy = toFloat p.y
    fz = toFloat p.z
    q = fx * xo + fy * yo + fz * zo + (fx + xo) * (fy + yo) * (fz + zo)
    h = floor (frac q * 4694)
    hi = h `div` 256
    ft = if hi == 0 || hi > 5 then 0 else floor (frac (10 * q) * 15) + 1
  in
    {top = decodeBoundaryAt p.y h,
     left = decodeBoundaryAt p.x (h `div` 4),
     feature = decodeFeature ft}

mapFeature : Feature -> Feature -> Room -> Room
mapFeature a b r = if r.feature == a then {r | feature <- b} else r

mergeFeature : Feature -> Room -> Room
mergeFeature f r = case f of
  STAIRS_DOWN -> case r.feature of
    STAIRS_DOWN -> {r | feature <- STAIRS_BOTH}
    _ -> {r | feature <- STAIRS_UP}
  _ -> r

roomAt : Position -> Room
roomAt p = let m = mergeFeature (rawRoomAt {p | z <- p.z - 1}).feature (rawRoomAt p) in
  if | p.z == 1 -> mapFeature ELEVATOR STAIRS_UP m
     | p.z == 50 -> (mapFeature PIT ELEVATOR . mapFeature STAIRS_DOWN NONE) m
     | otherwise -> m

-- View: Show the map in the neighborhood of the player and a map legend

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

featureSymbol : Feature -> [Form]
featureSymbol f = case f of
  NONE -> []
  STAIRS_UP -> [stairsUp]
  STAIRS_BOTH -> [stairsBoth]
  STAIRS_DOWN -> [stairsDown]
  ELEVATOR -> [elevator]
  PIT -> [pit]
  TELEPORTER -> [teleport]
  STAIRS_DOWN -> [stairsDown]
  ALTAR -> [altar]
  FOUNTAIN -> [fountain]
  CUBE -> [cube]
  THRONE -> [throne]
  BOX -> [box]

hdoor = image 56 8 "hdoor.png" |> toForm
hwall = image 56 8 "hwall.png" |> toForm
vdoor = image 8 56 "vdoor.png" |> toForm
vwall = image 8 56 "vwall.png" |> toForm

drawRoom : Room -> [Form]
drawRoom r =
  (case r.top of
    EMPTY -> []
    DOOR -> [hdoor |> moveY 24]
    WALL -> [hwall |> moveY 24])
  ++ (case r.left of
    EMPTY -> []
    DOOR -> [vdoor |> moveX -24]
    WALL -> [vwall |> moveX -24])
  ++ featureSymbol r.feature

drawRoomAt : Position -> (Int, Int) -> [Form]
drawRoomAt p (dx, dy) = 
  drawRoom (roomAt {p | x <- p.x + dx, y <- p.y + dy}) |>
  map (move (48 * toFloat dx, -48 * toFloat dy))

offsets = [-7 .. 7]

allOffsets : [(Int, Int)]
allOffsets = concatMap (\y -> map (\ x -> (x, y)) offsets) offsets

drawNeighborhood : Position -> [Form]
drawNeighborhood p = concatMap (drawRoomAt p) allOffsets

drawView : GameState -> Element
drawView p = (collage 632 632 (drawNeighborhood p) |> color black)
  `beside` container 200 632 midTop [markdown|
![](stairs.png) Stairway up

![](stairsBoth.png) Stairway up and down

![](stairsDown.png) Stairway down

![](elevator.png) Elevator

![](pit.png) Pit

![](teleport.png) Teleportal

![](altar.png) Altar

![](fountain.png) Fountain

![](throne.png) Gem encrusted throne

![](grayMistyCube.png) Gray misty cube

![](box.png) Small box with buttons

----

*Arrow Keys*: Pan the map

*U, D*: Go up or down a level

|] |> color charcoal

-- Program

main : Signal Element
main = drawView <~ stateSignal
