-- The Telengard Dungeon

module Dungeon where

import Dict (Dict)
import Dict
import Graphics

data Boundary = EMPTY | DOOR | WALL

data Feature = NONE | ELEVATOR | PIT | TELEPORTER | STAIRS_UP | STAIRS_DOWN
  | STAIRS_BOTH | ALTAR | FOUNTAIN | CUBE | THRONE | BOX

type Room = {top : Boundary, left : Boundary, feature : Feature}

type Position = {x : Int, y : Int, z : Int}

width = 200
height = 200
depth = 50

eastEdge = width + 1
southEdge = height + 1

decodeBoundary : Int -> Boundary
decodeBoundary x = let n = x `mod` 4 in
  if | n == 2 -> DOOR
     | n == 3 -> WALL
     | otherwise -> EMPTY

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
inBounds {x, y, z} = x >= 1 && x <= eastEdge &&
  y >= 1 && y <= southEdge && z >= 1 && z <= depth

xo = 1.6915
yo = 1.4278
zo = 1.2462

frac : Float -> Float
frac r = r - toFloat (floor r)

rawRoomAt : Position -> Room
rawRoomAt p = if not (inBounds p) || onSoutheastCorner p then emptyRoom else
  if p.x == eastEdge then {emptyRoom | left <- WALL} else
  if p.y == southEdge then {emptyRoom | top <- WALL} else
  let
    fx = toFloat p.x
    fy = toFloat p.y
    fz = toFloat p.z
    q = fx * xo + fy * yo + fz * zo + (fx + xo) * (fy + yo) * (fz + zo)
    h = floor (frac q * 4694)
    hi = h `div` 256
    ft = if hi == 0 || hi > 5 then 0 else floor (frac (10 * q) * 15) + 1
  in
    {top = if p.y == 1 then WALL else decodeBoundary h,
     left = if p.x == 1 then WALL else decodeBoundary (h `div` 4),
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
     | p.z == depth -> (mapFeature PIT ELEVATOR . mapFeature STAIRS_DOWN NONE .
        mapFeature STAIRS_BOTH STAIRS_UP) m
     | otherwise -> m

featureSymbol : Feature -> [Form]
featureSymbol f = case f of
  NONE        -> []
  STAIRS_UP   -> [Graphics.spriteForm "stairsUpb.png"   |> Graphics.movePixels ( -2,  21)]
  STAIRS_BOTH -> [Graphics.spriteForm "stairsBothb.png" |> Graphics.movePixels ( -2,  21)]
  STAIRS_DOWN -> [Graphics.spriteForm "stairsDownb.png" |> Graphics.movePixels ( -2,  21)]
  ELEVATOR    -> []
  PIT         -> [Graphics.spriteForm "pitb.png"        |> Graphics.movePixels (  8, -10)]
  TELEPORTER  -> [Graphics.spriteForm "teleportb.png"   |> Graphics.movePixels ( -8,   8)]
  ALTAR       -> [Graphics.spriteForm "altarb.png"      |> Graphics.movePixels (-12,  16)]
  FOUNTAIN    -> [Graphics.spriteForm "fountainb.png"   |> Graphics.movePixels (-12,  16)]
  CUBE        -> [Graphics.charForm   "cubeb.png"       |> Graphics.movePixels ( -9,  10)]
  THRONE      -> [Graphics.spriteForm "throneb.png"     |> Graphics.movePixels ( -8,  16)]
  BOX         -> [Graphics.charForm   "boxb.png"        |> Graphics.movePixels ( -9,   9)]

featureSymbolAt : Position -> [Form]
featureSymbolAt p = featureSymbol (roomAt p).feature

wallLen   = 7 * Graphics.chars
wallWidth = 1 * Graphics.chars
roomWidth = wallLen - wallWidth
halfWidth = toFloat (roomWidth `div` 2)

hdoor = fittedImage wallLen wallWidth "hdoorb.png" |> toForm
hwall = fittedImage wallLen wallWidth "hwallb.png" |> toForm
vdoor = fittedImage wallWidth wallLen "vdoorb.png" |> toForm
vwall = fittedImage wallWidth wallLen "vwallb.png" |> toForm

drawRoom : Room -> [Form]
drawRoom r =
  (case r.top of
    EMPTY -> []
    DOOR  -> [hdoor |> moveY halfWidth]
    WALL  -> [hwall |> moveY halfWidth])
  ++ (case r.left of
    EMPTY -> []
    DOOR  -> [vdoor |> moveX -halfWidth]
    WALL  -> [vwall |> moveX -halfWidth])
  ++ featureSymbol r.feature

offset : (Int, Int) -> Position -> Position
offset (dx, dy) p = {p | x <- p.x + dx, y <- p.y + dy}

drawRoomAt : Position -> (Int, Int) -> [Form]
drawRoomAt p (dx, dy) = 
  (drawRoom (roomAt (offset (dx, dy) p)) |>
  map (move (roomWidth * toFloat dx, -roomWidth * toFloat dy)))

offsets = [-1 .. 2]

allOffsets : [(Int, Int)]
allOffsets = concatMap (\y -> map (\ x -> (x, y)) offsets) offsets

type Neighborhood = Dict (Int, Int) Room

neighborhood : Position -> Neighborhood
neighborhood p = Dict.fromList (map (\o -> (o, roomAt (offset o p))) allOffsets)

nget : (Int, Int) -> Neighborhood -> Room
nget = Dict.findWithDefault emptyRoom

drawTop : Room -> [Form]
drawTop r = case r.top of
  EMPTY -> []
  DOOR  -> [hdoor |> moveY halfWidth]
  WALL  -> [hwall |> moveY halfWidth]

drawLeft : Room -> [Form]
drawLeft r = case r.left of
  EMPTY -> []
  DOOR  -> [vdoor |> moveX -halfWidth]
  WALL  -> [vwall |> moveX -halfWidth]

shift : (Int, Int) -> [Form] -> [Form]
shift (dx, dy) = map (move (roomWidth * toFloat dx, -roomWidth * toFloat dy))

drawNorthQuad : Neighborhood -> [Form]
drawNorthQuad ns = let
    rc = ns |> nget (0, 0)
    rnw = ns |> nget (-1, -1)
    rn = ns |> nget (0, -1)
    rne = ns |> nget (1, -1)
  in
    if rc.top /= EMPTY then drawTop rc
    else (drawTop rn |> shift (0, -1)) ++
      (if rn.left /= EMPTY then drawLeft rn |> shift (0, -1)
        else drawTop rnw |> shift (-1, -1))
      ++ if rne.left /= EMPTY then drawLeft rne |> shift (1, -1)
        else drawTop rne |> shift (1, -1)

drawWestQuad : Neighborhood -> [Form]
drawWestQuad ns = let
    rc = ns |> nget (0, 0)
    rnw = ns |> nget (-1, -1)
    rw = ns |> nget (-1, 0)
    rsw = ns |> nget (-1, 1)
  in
    if rc.left /= EMPTY then drawLeft rc
    else (drawLeft rw |> shift (-1, 0)) ++
      (if rw.top /= EMPTY then drawTop rw |> shift (-1, 0)
        else drawLeft rnw |> shift (-1, -1))
      ++ if rsw.top /= EMPTY then drawTop rsw |> shift (-1, 1)
        else drawLeft rsw |> shift (-1, 1)

drawSouthQuad : Neighborhood -> [Form]
drawSouthQuad ns = let
    rs = ns |> nget (0, 1)
    rse = ns |> nget (1, 1)
    rssw = ns |> nget (-1, 2)
    rss = ns |> nget (0, 2)
    rsse = ns |> nget (1, 2)
  in
    if rs.top /= EMPTY then drawTop rs |> shift (0, 1)
    else (drawTop rss |> shift (0, 2)) ++
      (if rs.left /= EMPTY then drawLeft rs |> shift (0, 1)
        else drawTop rssw |> shift (-1, 2))
      ++ if rse.left /= EMPTY then drawLeft rse |> shift (1, 1)
        else drawTop rsse |> shift (1, 2)

drawEastQuad : Neighborhood -> [Form]
drawEastQuad ns = let
    re = ns |> nget (1, 0)
    rse = ns |> nget (1, 1)
    rnee = ns |> nget (2, -1)
    ree = ns |> nget (2, 0)
    rsee = ns |> nget (2, 1)
  in
    if re.left /= EMPTY then drawLeft re |> shift (1, 0)
    else (drawLeft ree |> shift (2, 0)) ++
      (if re.top /= EMPTY then drawTop re |> shift (1, 0)
        else drawLeft rnee |> shift (2, -1))
      ++ if rse.top /= EMPTY then drawTop rse |> shift (1, 1)
        else drawLeft rsee |> shift (2, 1)

drawNeighborhood : Position -> [Form]
drawNeighborhood p = let ns = neighborhood p in
  drawNorthQuad ns ++ drawWestQuad ns ++ drawSouthQuad ns ++ drawEastQuad ns

wbound : Int -> Int
wbound x = clamp 1 width x

hbound : Int -> Int
hbound x = clamp 1 height x

dbound : Int -> Int
dbound x = clamp 1 depth x

northOf : Position -> Position
northOf p = {p | y <- p.y - 1}

southOf : Position -> Position
southOf p = {p | y <- p.y + 1}

westOf : Position -> Position
westOf p = {p | x <- p.x - 1}

eastOf : Position -> Position
eastOf p = {p | x <- p.x + 1}

topOf : Position -> Position
topOf p = {p | z <- p.z - 1}

bottomOf : Position -> Position
bottomOf p = {p | z <- p.z + 1}

canClimb : Feature -> Bool
canClimb f = f == STAIRS_UP || f == STAIRS_BOTH

canDescend : Feature -> Bool
canDescend f = f == STAIRS_DOWN || f == STAIRS_BOTH || f == PIT
