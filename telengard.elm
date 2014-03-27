-- The Telengard Dungeon Map

import Keyboard
import Text
import Random
import Dict (Dict)
import Dict

-- Abstract Model : Rooms and the player position

data Boundary = EMPTY | DOOR | WALL

data Feature = NONE | ELEVATOR | PIT | TELEPORTER | STAIRS_UP | STAIRS_DOWN
  | STAIRS_BOTH | ALTAR | FOUNTAIN | CUBE | THRONE | BOX

data Spell = AstralWalk

data Command = ERROR | NORTH | EAST | SOUTH | WEST | UP | DOWN | STAY |
  FIGHT | CAST Spell | TICK | ROLL Float | CONTINUE

data MonsterClass = Dwarf

type Monster = {class : MonsterClass, level : Int, hp : Int}

data Action = WAITING | FIGHTING Monster | FALLING Int | RISING Int | TELEPORTING Int | RESTING

data EquipmentClass = Sword | Armor | Shield | ElvenCloak | ElvenBoots | RingOfRegeneration | RingOfProtection

data ConsumableClass = ScrollOfRescue | PotionOfHealing | PotionOfStrength

type Room = {top : Boundary, left : Boundary, feature : Feature}

type Position = {x : Int, y : Int, z : Int}

type Stats = {str : Int, int : Int, wis : Int, con : Int, dex : Int, chr : Int}

type GameState = {action : Action, pos : Position, name : String, level : Int,
  stats : Stats, hp : Int, maxhp : Int, su : Int, maxsu : Int, exp : Int, gold : Int,
  equipmentBonus : Dict Int Int, consumableCount : Dict Int Int,
  msg : String, msgTimer : Int,
  idleTimer : Int, phaseWalkDuration : Int, rnd : Float}

dungeonWidth = 200
dungeonHeight = 200
dungeonDepth = 50

eastEdge = dungeonWidth + 1
southEdge = dungeonHeight + 1

-- Control : Movement commands

keyCodeCommand : Keyboard.KeyCode -> Command
keyCodeCommand n = case n of
  13 -> CONTINUE
  37 -> WEST
  38 -> NORTH
  39 -> EAST
  40 -> SOUTH
  68 -> DOWN
  85 -> UP
  67 -> CAST AstralWalk
  70 -> FIGHT
  83 -> STAY
  _ -> ERROR

tickCommand : Float -> Command
tickCommand t = TICK

randomCommand : Float -> Command
randomCommand x = ROLL x

commandStream : Signal Command
commandStream = (keyCodeCommand <~ Keyboard.lastPressed) `merge`
                (tickCommand <~ (every (second / 10))) `merge`
                (randomCommand <~ Random.float (every (second / 10)))

-- State Changes

initStats = {str = 9, int = 9, wis = 9, con = 9, dex = 9, chr = 9}

initState = {action = WAITING, pos = {x = 25, y = 13, z = 1},
  name = "STABULO", level = 1, stats = initStats, hp = 9, maxhp = 9, su = 1, maxsu = 1, exp = 0, gold = 0,
  equipmentBonus = Dict.empty, consumableCount = Dict.empty,
  msg = "", msgTimer = 0, idleTimer = 50, phaseWalkDuration = 0, rnd = 0.0}

wbound : Int -> Int
wbound x = clamp 1 dungeonWidth x

hbound : Int -> Int
hbound x = clamp 1 dungeonHeight x

dbound : Int -> Int
dbound x = clamp 1 dungeonDepth x

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

travel : (Position -> Position) -> GameState -> GameState
travel f s = {s | pos <- f s.pos}

canMoveNorth : GameState -> Bool
canMoveNorth s = s.action == WAITING && s.pos.y > 1 &&
  (s.phaseWalkDuration > 0 || (roomAt s.pos).top /= WALL)

canMoveWest : GameState -> Bool
canMoveWest s = s.action == WAITING && s.pos.x > 1 &&
  (s.phaseWalkDuration > 0 || (roomAt s.pos).left /= WALL)

canMoveSouth : GameState -> Bool
canMoveSouth s = s.action == WAITING && s.pos.y < 200 &&
  (s.phaseWalkDuration > 0 || (roomAt (southOf s.pos)).top /= WALL)

canMoveEast : GameState -> Bool
canMoveEast s = s.action == WAITING && s.pos.x < 200 &&
  (s.phaseWalkDuration > 0 || (roomAt (eastOf s.pos)).left /= WALL)

canClimb : Feature -> Bool
canClimb f = f == STAIRS_UP || f == STAIRS_BOTH

canDescend : Feature -> Bool
canDescend f = f == STAIRS_DOWN || f == STAIRS_BOTH || f == PIT

canMoveUp : GameState -> Bool
canMoveUp s = s.action == WAITING &&
  canClimb ((roomAt s.pos).feature)

canMoveDown : GameState -> Bool
canMoveDown s = s.action == WAITING && s.pos.z < 50 &&
  canDescend ((roomAt s.pos).feature)

withMessage : String -> GameState -> GameState
withMessage m s = {s | msg <- m, msgTimer <- 10}

wtf : GameState -> GameState
wtf s = s |> withMessage "No"

reduceSpellDurations : GameState -> GameState
reduceSpellDurations s = if s.phaseWalkDuration > 0
  then {s | phaseWalkDuration <- s.phaseWalkDuration - 1}
  else s

resetIdleTimer : GameState -> GameState
resetIdleTimer s = {s | idleTimer <- 50}

checkForTraps : GameState -> GameState
checkForTraps s = case (roomAt s.pos).feature of
  PIT        -> if s.rnd < 0.5 then {s | action <- FALLING 21} else s
  ELEVATOR   -> {s | action <- RISING 21}
  TELEPORTER -> {s | action <- TELEPORTING 10}
  _          -> s

power : MonsterClass -> Int
power c = case c of
  Dwarf -> 11

-- TODO: Check for invisibility, fear, elven cloak effect, more monster classes, light vs undead,
-- monster healing, monster stealing, monster gifting, initiative check, protection from evil

checkForEncounter : GameState -> GameState
checkForEncounter s = if s.rnd < 0.3 then
  let
    c = Dwarf
    l = floor ((frac (s.rnd * 10) ^ 1.5) * (toFloat s.pos.z * 2 + 2) + 1)
    h = floor ((frac (s.rnd * 1000) ^ 0.5) * toFloat l * toFloat (power c) + 1)
  in
  {s | action <- FIGHTING {class = Dwarf, level = l, hp = h}} else checkForTraps s

successfully : GameState -> GameState
successfully = checkForEncounter . resetIdleTimer . reduceSpellDurations

canCast : Spell -> GameState -> Bool
canCast a s = s.pos.z > 0

reduceMessageTimer : GameState -> GameState
reduceMessageTimer s = if s.msgTimer == 1 then {s | msgTimer <- 0, msg <- ""}
  else if s.msgTimer > 1 then {s | msgTimer <- s.msgTimer - 1}
  else s

randomPos : Float -> Position -> Position
randomPos r p = let
    xr = r * dungeonWidth
    yr = frac(xr) * dungeonHeight
  in {x = floor xr + 1, y = floor yr + 1, z = p.z}


idle : GameState -> GameState
idle s = case s.action of
  WAITING       -> if s.pos.z > 0 && s.idleTimer <= 1 then resetIdleTimer s |> withMessage "Stay" |> successfully
                   else {s | idleTimer <- s.idleTimer - 1}
  FIGHTING    _ -> if s.idleTimer <= 1 then resetIdleTimer s |> withMessage "Wait" |> successfully
                   else {s | idleTimer <- s.idleTimer - 1}
  FALLING     n -> if n == 0 then {s | action <- WAITING} |> travel bottomOf |> successfully
                   else {s | action <- FALLING (n - 1)}
  RISING      n -> if n == 0 then {s | action <- WAITING} |> travel topOf |> successfully
                   else {s | action <- RISING (n - 1)}
  TELEPORTING n -> if n == -10 then {s | action <- WAITING} |> successfully
                   else {s | action <- TELEPORTING (n - 1)} |>
                     if n == 0 then travel (randomPos s.rnd) else id
  RESTING       -> s

isFighting : GameState -> Bool
isFighting s = case s.action of
  FIGHTING _ -> True
  _ -> False

setAction : Action -> GameState -> GameState
setAction a s = {s | action <- a}

update : Command -> GameState -> GameState
update c s = case c of
  ERROR  -> wtf s
  NORTH  -> if s |> canMoveNorth
            then travel northOf s |> withMessage "North" |> successfully
            else wtf s
  EAST   -> if s |> canMoveEast
            then travel eastOf s |> withMessage "East" |> successfully
            else wtf s
  SOUTH  -> if s |> canMoveSouth
            then travel southOf s |> withMessage "South" |> successfully
            else wtf s
  WEST   -> if s |> canMoveWest
            then travel westOf s |> withMessage "West" |> successfully
            else wtf s
  UP     -> if s |> canMoveUp
            then travel topOf s |> (if s.pos.z == 1 then setAction RESTING else successfully . withMessage "Up")
            else wtf s
  DOWN   -> if s |> canMoveDown
            then travel bottomOf s |> withMessage "Down" |> successfully
            else wtf s
  STAY   -> s |> withMessage "Stay" |> successfully
  FIGHT  -> if s |> isFighting then s |> setAction WAITING |> checkForTraps |> withMessage "You killed it!" |> resetIdleTimer
            else wtf s
  CAST a -> if s |> canCast a then {s | phaseWalkDuration <- s.phaseWalkDuration + 10}
            else wtf s
  CONTINUE -> if s.action == RESTING then s |> travel bottomOf |> setAction WAITING |> successfully else s
  TICK   -> reduceMessageTimer (idle s)
  ROLL x -> {s | rnd <- x}

stateSignal : Signal GameState
stateSignal = foldp update initState commandStream

-- Concrete Model: A procedurally generated map

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
  y >= 1 && y <= southEdge && z >= 1 && z <= dungeonDepth

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
     | p.z == dungeonDepth -> (mapFeature PIT ELEVATOR . mapFeature STAIRS_DOWN NONE .
        mapFeature STAIRS_BOTH STAIRS_UP) m
     | otherwise -> m

-- View: Show the map in the neighborhood of the player

zoom = 3

spriteForm : String -> Form
spriteForm s = fittedImage (24 * zoom) (21 * zoom) s |> toForm

charForm : String -> Form
charForm s = fittedImage (8 * zoom) (8 * zoom) s |> toForm

hero = spriteForm "hero.png" |> move (9 * zoom, -5 * zoom)

fallingHero n = croppedImage (0, 0) (24 * zoom) (n * zoom) "hero.png"
  |> toForm |> move (9 * zoom, -5 * zoom)

stairsUp = spriteForm "stairsUpb.png" |> move (-2 * zoom, 21 * zoom)
stairsDown = spriteForm "stairsDownb.png" |> move (-2 * zoom, 21 * zoom)
stairsBoth = spriteForm "stairsBothb.png" |> move (-2 * zoom, 21 * zoom)
pit = spriteForm "pitb.png" |> move (8 * zoom, -10 * zoom)
elevator = spriteForm "elevatorb.png" |> move (3 * zoom, -5 * zoom)
teleporter = spriteForm "teleportb.png" |> move (-8 * zoom, 8 * zoom)
altar = spriteForm "altarb.png" |> move (-12 * zoom, 16 * zoom)
fountain = spriteForm "fountainb.png" |> move (-12 * zoom, 16 * zoom)
cube = charForm "cubeb.png" |> move (-9 * zoom, 10 * zoom)
throne = spriteForm "throneb.png" |> move (-8 * zoom, 16 * zoom)
box = charForm "boxb.png" |> move (-9 * zoom, 9 * zoom)

dwarf = spriteForm "dwarf.png" |> move (-8 * zoom, 8 * zoom)

featureSymbol : Feature -> [Form]
featureSymbol f = case f of
  NONE -> []
  STAIRS_UP -> [stairsUp]
  STAIRS_BOTH -> [stairsBoth]
  STAIRS_DOWN -> [stairsDown]
  ELEVATOR -> [elevator]
  PIT -> [pit]
  TELEPORTER -> [teleporter]
  STAIRS_DOWN -> [stairsDown]
  ALTAR -> [altar]
  FOUNTAIN -> [fountain]
  CUBE -> [cube]
  THRONE -> [throne]
  BOX -> [box]

wallLen = 56 * zoom
wallWidth = 8 * zoom
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
    DOOR -> [hdoor |> moveY halfWidth]
    WALL -> [hwall |> moveY halfWidth])
  ++ (case r.left of
    EMPTY -> []
    DOOR -> [vdoor |> moveX -halfWidth]
    WALL -> [vwall |> moveX -halfWidth])
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

drawTopX : Room -> [Form]
drawTopX r = case r.top of
  EMPTY -> []
  DOOR  -> [cube |> moveY halfWidth]
  WALL  -> [cube |> moveY halfWidth]

drawLeftX : Room -> [Form]
drawLeftX r = case r.left of
  EMPTY -> []
  DOOR  -> [cube |> moveX -halfWidth]
  WALL  -> [cube |> moveX -halfWidth]

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

drawMonster : MonsterClass -> Form
drawMonster c = case c of
  Dwarf -> dwarf

drawAction : GameState -> [Form]
drawAction s = case s.action of
  FALLING n -> [fallingHero n |> moveY (toFloat (zoom * (n - 21)) / 2)]
  RISING n -> [elevator |> moveY (toFloat (zoom * (21 - n))),
    hero |> moveY (toFloat (zoom * (21 - n)))]
  TELEPORTING n -> [hero, rect 800 600 |> filled (rgba 0 0 0 ((10 - abs (toFloat n)) / 10))]
  FIGHTING {class, level, hp} -> [drawMonster class, hero]
  _ -> (roomAt s.pos |> .feature |> featureSymbol) ++ [hero]

adjectives = ["SALTY", "BOLD", "LOUD", "OLD", "GOODLY", "WORTHY", "LOFTY", "FINE", "ROCKY", "AGED"]

nouns = ["ROAD", "EYE", "TOOTH", "DRAGON", "MUG", "DEMON", "WHARF", "BRIDGE", "MEADE", "ALE"]

establishments = ["TAVERN", "ALEHOUSE", "CELLAR", "CLUB", "INN", "HOUSE", "INN", "LODGE", "MEADHALL", "RESTHOUSE"]

nth : Int -> [a] -> a
nth n s = drop n s |> head

lowDigit : Int -> Int
lowDigit n = n - (n `div` 10) * 10

innName : Position -> String
innName p = (adjectives |> nth (lowDigit (p.x * p.y))) ++ " " ++
  (nouns |> nth (lowDigit (p.x + p.y))) ++ " " ++
  (establishments |> nth (lowDigit (p.x * 3 + p.y * 7)))

innDesc : Position -> [String]
innDesc p = ["YOU HAVE FOUND THE " ++ innName p,
  "YOU SPEND THE NIGHT",
  "YOU FEEL BETTER",
  "PRESS <RET> TO RETURN TO THE DUNGEON"]

drawInn : GameState -> [Form]
drawInn s = [group (c64s (innDesc s.pos)) |> move (-10 * 24, -10 * 24), hero]

drawGameState : GameState -> [Form]
drawGameState s = if s.pos.z == 0 then drawInn s
  else drawNeighborhood s.pos ++ drawAction s

textify : String -> Element
textify s = toText ("   " ++ s) |> centered

spellEffects : GameState -> String
spellEffects s = if s.phaseWalkDuration > 0 then "Astral Walk" else ""

className : MonsterClass -> String
className c = case c of
  Dwarf -> "DWARF"

actionMessage : GameState -> String
actionMessage s = case s.action of
  FIGHTING m -> "YOU HAVE ENCOUNTERED A LVL " ++ show m.level ++ " " ++ className m.class ++ " (" ++ show m.hp ++ ")"
  FALLING _ -> "YOU SEE A PIT   YOU FALL IN!!"
  RISING _ -> "YOU FEEL HEAVY FOR A MOMENT"
  TELEPORTING _ -> "ZZAP!! YOU'VE BEEN TELEPORTED..."
  RESTING -> "BLAH"
  _ -> "->"

charAt : (Int, Int) -> Form
charAt (x, y) = croppedImage (x * 24, y * 24) 24 24 "charset.png" |> toForm

c64Char : Char -> Form
c64Char c = case c of
  '@' -> charAt (0, 0)
  'A' -> charAt (1, 0)
  'B' -> charAt (2, 0)
  'C' -> charAt (3, 0)
  'D' -> charAt (4, 0)
  'E' -> charAt (5, 0)
  'F' -> charAt (6, 0)
  'G' -> charAt (7, 0)
  'H' -> charAt (8, 0)
  'I' -> charAt (9, 0)
  'J' -> charAt (10, 0)
  'K' -> charAt (11, 0)
  'L' -> charAt (12, 0)
  'M' -> charAt (13, 0)
  'N' -> charAt (14, 0)
  'O' -> charAt (15, 0)
  'P' -> charAt (16, 0)
  'Q' -> charAt (17, 0)
  'R' -> charAt (18, 0)
  'S' -> charAt (19, 0)
  'T' -> charAt (20, 0)
  'U' -> charAt (21, 0)
  'V' -> charAt (22, 0)
  'W' -> charAt (23, 0)
  'X' -> charAt (24, 0)
  'Y' -> charAt (25, 0)
  'Z' -> charAt (26, 0)
  '[' -> charAt (27, 1)
  -- British Pound
  ']' -> charAt (28, 1)
  -- Up arrow
  -- Left arrow
  ' ' -> charAt (0, 1)
  '!' -> charAt (1, 1)
  '"' -> charAt (2, 1)
  '#' -> charAt (3, 1)
  '$' -> charAt (4, 1)
  '%' -> charAt (5, 1)
  '&' -> charAt (6, 1)
  '`' -> charAt (7, 1)
  '(' -> charAt (8, 1)
  ')' -> charAt (9, 1)
  '*' -> charAt (10, 1)
  '+' -> charAt (11, 1)
  ',' -> charAt (12, 1)
  '-' -> charAt (13, 1)
  '.' -> charAt (14, 1)
  '/' -> charAt (15, 1)
  '0' -> charAt (16, 1)
  '1' -> charAt (17, 1)
  '2' -> charAt (18, 1)
  '3' -> charAt (19, 1)
  '4' -> charAt (20, 1)
  '5' -> charAt (21, 1)
  '6' -> charAt (22, 1)
  '7' -> charAt (23, 1)
  '8' -> charAt (24, 1)
  '9' -> charAt (25, 1)
  ':' -> charAt (26, 1)
  ';' -> charAt (27, 1)
  '<' -> charAt (28, 1)
  '=' -> charAt (29, 1)
  '>' -> charAt (30, 1)
  '?' -> charAt (31, 1)

c64 : String -> Form
c64 s = zip (String.toList s) [0 .. String.length s] |> map (\(c, i) -> c64Char c |> move (toFloat i * 24 + 12, -12)) |> group

c64s : [String] -> [Form]
c64s ss = zip ss [0 .. length ss] |> map (\(s, i) -> c64 s |> moveY (toFloat i * -24))

equipment = Dict.fromList [(0, Sword), (1, Armor), (2, Shield), (3, ElvenCloak), (4, ElvenBoots), (5, RingOfRegeneration), (6, RingOfProtection)]

equipAbbr : EquipmentClass -> String
equipAbbr c = case c of
  Sword -> "SWORD"
  Armor -> "ARMOR"
  Shield -> "SHIELD"
  ElvenCloak -> "ELVN CLK"
  ElvenBoots -> "ELVN BTS"
  RingOfRegeneration -> "RING REG"
  RingOfProtection -> "RING PROT"

bonusStr : Int -> String
bonusStr n = if n == 0 then "" else "+" ++ show n

eqBonus : GameState -> Int -> Int
eqBonus s c = Dict.findWithDefault 0 c s.equipmentBonus

isAlwaysEquipped : EquipmentClass -> Bool
isAlwaysEquipped c = c == Sword || c == Armor || c == Shield

equipStr : GameState -> Int -> [String]
equipStr s i = let
    n = eqBonus s i
    c = Dict.findWithDefault Sword i equipment
  in if n == 0 && not (isAlwaysEquipped c) then []
     else [" " ++ equipAbbr c ++ " " ++ bonusStr n]

consumables = Dict.fromList [(0, ScrollOfRescue), (1, PotionOfHealing), (2, PotionOfStrength)]

consumableAbbr : ConsumableClass -> String
consumableAbbr c = case c of
  ScrollOfRescue -> "SCRL RESC"
  PotionOfHealing -> "POT HEAL"
  PotionOfStrength -> "POT STRG"

consCount : GameState -> Int -> Int
consCount s c = Dict.findWithDefault 0 c s.consumableCount

consStr : GameState -> Int -> [String]
consStr s c = let n = consCount s c in if n == 0 then [] else ["  " ++ show n ++ " " ++ consumableAbbr (Dict.findWithDefault ScrollOfRescue c consumables)]

charData : GameState -> [Form]
charData s = let t = s.stats in
  c64s ([s.name ++ " LVL " ++ show s.level,
  "STR " ++ show t.str ++ "   CON " ++ show t.con,
  "INT " ++ show t.int ++ "   DEX " ++ show t.dex,
  "WIS " ++ show t.wis ++ "   CHR " ++ show t.chr,
  "HP " ++ show s.hp ++ "/" ++ show s.maxhp,
  "SU " ++ show s.su ++ "/" ++ show s.maxsu,
  "EX " ++ show s.exp,
  "GD " ++ show s.gold] ++ concatMap (equipStr s) [0..6] ++ concatMap (consStr s) [0..2])

drawView : GameState -> Element
drawView s = ((collage 960 720 [drawGameState s |> group |> move (-200, 110), group (charData s) |> move (100, 14 * 24)] |> color black) `above`
  textify (actionMessage s) `above` textify (spellEffects s) `above` textify s.msg)

-- Program

main : Signal Element
main = drawView <~ stateSignal
