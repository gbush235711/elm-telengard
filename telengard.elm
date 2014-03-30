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

type SpellEffect = {name : String, abbr : String, duration : Int}

type RandomCtx = {clumsinessAroundPits : Float, itemDesirability : Float, sneakiness : Float, monsterLevel : Float, monsterPower : Float, monsterHealth : Float,
  undeadFearOfLight : Float, sexiness : Float, possessiveness : Float, teleporterInstability : Float}

data Command = ERROR | NORTH | EAST | SOUTH | WEST | UP | DOWN | STAY |
  FIGHT | CAST Spell | TICK | ROLL RandomCtx | CONTINUE

type Monster = {name : String, img : Form, power : Int, level : Int, hp : Int}

data Action = WAITING | FIGHTING Monster | FALLING Int | RISING Int | TELEPORTING Int | RESTING | READING Int Form (GameState -> GameState)

data OnRoll = NoChange | ContinueWith (Float -> GameState -> GameState)

--data EquipmentClass = Sword | Armor | Shield | ElvenCloak | ElvenBoots | RingOfRegeneration | RingOfProtection

type Equipment = {name : String, abbr : String, bonus : Int, isAlwaysEquipped : Bool}

data ConsumableClass = ScrollOfRescue | PotionOfHealing | PotionOfStrength

type Room = {top : Boundary, left : Boundary, feature : Feature}

type Position = {x : Int, y : Int, z : Int}

type Stats = {str : Int, int : Int, wis : Int, con : Int, dex : Int, chr : Int}

type GameState = {action : Action, pos : Position, name : String, level : Int,
  stats : Stats, hp : Int, maxhp : Int, su : Int, maxsu : Int, exp : Int, gold : Int,
  equipment : [Equipment],
  consumableCount : ConsumableClass -> Int,
  effects : [SpellEffect],
  msg : String, msgTimer : Int,
  prompt : [String],
  idleTimer : Int,
  rnd : RandomCtx
}

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

type Glom = {n : Int, rnd : RandomCtx}

randomGlom : Float -> Glom -> Glom
randomGlom x {n, rnd} = case n of
  0 -> {n = n + 1, rnd = {rnd | clumsinessAroundPits <- x}}
  1 -> {n = n + 1, rnd = {rnd | itemDesirability <- x}}
  2 -> {n = n + 1, rnd = {rnd | sneakiness <- x}}
  3 -> {n = n + 1, rnd = {rnd | monsterLevel <- x}}
  4 -> {n = n + 1, rnd = {rnd | monsterPower <- x}}
  5 -> {n = n + 1, rnd = {rnd | monsterHealth <- x}}
  6 -> {n = n + 1, rnd = {rnd | undeadFearOfLight <- x}}
  7 -> {n = n + 1, rnd = {rnd | sexiness <- x}}
  8 -> {n = n + 1, rnd = {rnd | possessiveness <- x}}
  9 -> {n = 0, rnd = {rnd | teleporterInstability <- x}}

initRnd = {clumsinessAroundPits = 0.0, itemDesirability = 0.0, sneakiness = 0.0, monsterLevel = 0.0, monsterPower = 0.0, monsterHealth = 0.0,
    undeadFearOfLight = 0.0, sexiness = 0.0, possessiveness = 0.0, teleporterInstability = 0.0}

randomCommand : Glom -> Command
randomCommand {n, rnd} = ROLL rnd

commandStream : Signal Command
commandStream = (keyCodeCommand <~ Keyboard.lastPressed) `merge`
                (tickCommand <~ (every (second / 10))) `merge`
                (randomCommand <~ keepIf (\g -> g.n == 0) {n = 0, rnd = initRnd} (foldp randomGlom {n = 0, rnd = initRnd} (Random.float (every (second / 100)))))

-- State Changes

effect : String -> String -> Int -> SpellEffect
effect n a d = {name = n, abbr = a, duration = d}

equip : String -> String -> Bool -> Int -> Equipment
equip n a c b = {name = n, abbr = a, isAlwaysEquipped = c, bonus = b}

initStats = {str = 9, int = 9, wis = 9, con = 9, dex = 9, chr = 9}

initState = {action = WAITING, pos = {x = 25, y = 13, z = 1},
  name = "STABULO", level = 1, stats = initStats, hp = 9, maxhp = 9, su = 1, maxsu = 1, exp = 0, gold = 0,

  equipment = [equip "SWORD" "SWORD" True 0,
    equip "ARMOR" "ARMOR" True 0,
    equip "SHIELD" "SHIELD" True 0,
    equip "ELVEN CLOAK" "ELVN CLK" False 0,
    equip "ELVEN BOOTS" "ELVN BTS" False 0,
    equip "RING OF REGENERATION" "RING REG" False 0,
    equip "RING OF PROTECTION" "RING PROT" False 0],

  --data EquipmentClass = Sword | Armor | Shield | ElvenCloak | ElvenBoots | RingOfRegeneration | RingOfProtection


  consumableCount = \c -> 0,
  effects = [effect "Strength" "STRG" 0, effect "Detect Traps" "DTRP" 0, effect "Light" "LGHT" 0, effect "Protection from Evil" "PROT" 0,
    effect "Levitation" "LEVT" 0, effect "Invisibility" "INVS" 0, effect "Fear" "FEAR" 0, effect "Astral Walk" "ASTW" 0, effect "Time Stop" "TMST" 0,
    effect "Resurrection" "RESD" 0, effect "Drunk" "DRNK" 0],
  msg = "", msgTimer = 0, prompt = [], idleTimer = 50,
  rnd = initRnd} |> checkForTraps

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

hasEffect : String -> GameState -> Bool
hasEffect n s = s.effects |> any (\sf -> sf.name == n && sf.duration > 0)

canMoveNorth : GameState -> Bool
canMoveNorth s = s.action == WAITING && s.pos.y > 1 &&
  ((s |> hasEffect "Astral Walk") || (roomAt s.pos).top /= WALL)

canMoveWest : GameState -> Bool
canMoveWest s = s.action == WAITING && s.pos.x > 1 &&
  ((s |> hasEffect "Astral Walk") || (roomAt s.pos).left /= WALL)

canMoveSouth : GameState -> Bool
canMoveSouth s = s.action == WAITING && s.pos.y < 200 &&
  ((s |> hasEffect "Astral Walk") || (roomAt (southOf s.pos)).top /= WALL)

canMoveEast : GameState -> Bool
canMoveEast s = s.action == WAITING && s.pos.x < 200 &&
  ((s |> hasEffect "Astral Walk") || (roomAt (eastOf s.pos)).left /= WALL)

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
reduceSpellDurations s = {s | effects <- map (\sf -> {sf | duration <- max 0 (sf.duration - 1)}) s.effects}

findWithDefault : a -> (a -> Bool) -> [a] -> a
findWithDefault d p s = case s of
  [] -> d
  x :: s -> if p x then x else findWithDefault d p s

equipmentBonus : String -> GameState -> Int
equipmentBonus n s = findWithDefault {name = "", abbr = "", isAlwaysEquipped = False, bonus = 0} (\q -> q.name == n) s.equipment |> .bonus

regenerate : GameState -> GameState
regenerate s = {s | hp <- min (s.hp + equipmentBonus "RING OF REGENERATION" s) s.maxhp}

resetIdleTimer : GameState -> GameState
resetIdleTimer s = {s | idleTimer <- 50}

promptStairway : Bool -> Bool -> GameState -> GameState
promptStairway u d s = s |> clearPrompt
  |> prompt "YOU HAVE FOUND A STAIRWAY"
  |> (if u && s.pos.z == 1 then prompt "YOU SEE `LIGHT` ABOVE" else id)
  |> prompt ("DO YOU WANT TO " ++ (if u then "GO `U`P, " else "") ++ (if d then "GO `D`OWN," else ""))
  |> prompt "OR `S`TAY ON THE SAME LEVEL?"

--roll : (Float -> GameState -> GameState) -> GameState -> GameState
--roll f s = {s | onRoll <- ContinueWith (\r s -> f r {s | onRoll <- NoChange})}

checkForTraps : GameState -> GameState
checkForTraps s = case (roomAt s.pos).feature of
  PIT        -> s |> clearPrompt
                  |> prompt "YOU SEE A PIT"
                  |> if s.rnd.clumsinessAroundPits < 0.5 then setAction (FALLING 21) `andThen` prompt "YOU FALL IN!!" else setAction WAITING `andThen` prompt "YOU CAN GO `D`OWN OR TRAVEL ON"
  ELEVATOR   -> {s | action <- RISING 21, prompt <- ["YOU FEEL HEAVY FOR A MOMENT"]}
  TELEPORTER -> {s | action <- TELEPORTING 21, prompt <- ["ZZAP!! YOU'VE BEEN TELEPORTED..."]}
  STAIRS_UP -> s |> promptStairway True False
  STAIRS_BOTH -> s |> promptStairway True True
  STAIRS_DOWN -> s |> promptStairway False True
  _          -> s |> clearPrompt

mc n i p u = {name = n, img = i, power = p, isUndead = u}

monsterClass n = case n of
  1 -> mc "GNOLL" gnoll 1 False 
  2 -> mc "KOBOLD" kobold 2 False
  3 -> mc "SKELETON" skeleton 3 True
  4 -> mc "HOBBIT" hobbit 4 False
  5 -> mc "ZOMBIE" zombie 5 True
  6 -> mc "ORC" orc 6 False
  7 -> mc "FIGHTER" fighter 7 False
  8 -> mc "MUMMY" mummy 8 True
  9 -> mc "ELF" elf 9 False
  10 -> mc "GHOUL" ghoul 10 True
  11 -> mc "DWARF" dwarf 11 False
  12 -> mc "TROLL" troll 12 False
  13 -> mc "WRAITH" wraith 13 True
  14 -> mc "OGRE" ogre 14 False
  15 -> mc "MINOTAUR" minotaur 15 False
  16 -> mc "GIANT" giant 16 False
  17 -> mc "SPECTER" specter 17 True
  18 -> mc "VAMPIRE" vampire 18 True
  19 -> mc "DEMON" demon 19 False
  20 -> mc "DRAGON" dragon 20 False

-- TODO: Check for invisibility, elven cloak effect, more monster classes, time stop
-- monster stealing, monster gifting, initiative check, protection from evil

dn : Int -> Float -> Int
dn n r = floor (frac r * toFloat n + 1)

chance : Float -> Float -> Bool
chance f r = frac r < f

after : Int -> Form -> (GameState -> GameState) -> GameState -> GameState
after t i f = setAction (READING t i f)

fullHeal : GameState -> GameState
fullHeal s = {s | hp <- s.maxhp}

andThen : (a -> b) -> (b -> c) -> a -> c
andThen f g x = g (f x)

--roll9 : (Float -> Float -> Float -> Float -> Float -> Float -> Float -> Float -> Float -> GameState -> GameState) -> GameState -> GameState
--roll9 f s = s |> roll (\r1 -> roll (\r2 -> roll (\r3 -> roll (\r4 -> roll (\r5 -> roll (\r6 -> roll (\r7 -> roll (\r8 -> roll (\r9 -> f r1 r2 r3 r4 r5 r6 r7 r8 r9)))))))))

stuffToSteal : GameState -> [Equipment]
stuffToSteal s = filter (\q -> q.bonus > 0) s.equipment

hasAnythingToSteal : GameState -> Bool
hasAnythingToSteal s = any (\q -> q.bonus > 0) s.equipment

steal : String -> GameState -> GameState
steal n s = {s | equipment <- s.equipment |> map (\q -> if q.name == n then {q | bonus <- 0} else q)}

at : Int -> [a] -> a
at n s = drop n s |> head


stealSomething : Form -> GameState -> GameState
stealSomething f s = s |> let
    ss = stuffToSteal s
    n = floor (s.rnd.itemDesirability * toFloat (length ss))
    q = at n ss
  in prompt ("HE STEALS YOUR " ++ q.name) `andThen` steal q.name `andThen` after 30 f (setAction WAITING `andThen` checkForTraps)

checkForEncounter : GameState -> GameState
checkForEncounter s = s |> if s.rnd.sneakiness < 0.3 then
  let
    c = monsterClass (dn 20 s.rnd.monsterPower)
    l = floor ((s.rnd.monsterLevel ^ 1.5) * (toFloat s.pos.z * 2 + 2) + 1)
    h = floor ((s.rnd.monsterHealth ^ 0.5) * toFloat l * toFloat c.power + 1)
  in
    if (s |> hasEffect "Fear") && c.power < 5 then
      checkForEncounter
    else if (s |> hasEffect "Light") && c.isUndead && chance 0.2 s.rnd.undeadFearOfLight then
      checkForEncounter
    else
      setAction (FIGHTING {name = c.name, img = c.img, power = c.power, level = l, hp = h})
      `andThen` clearPrompt
       `andThen` prompt ("YOU HAVE ENCOUNTERED A LVL " ++ show l ++ " " ++ c.name)
        `andThen` (if (c.name == "ELF" && chance (0.04 * toFloat s.stats.chr) s.rnd.sexiness) || chance 0.02 s.rnd.sexiness then
              fullHeal
              `andThen` prompt ("THE " ++ c.name ++ " LIKES YOUR BODY")
              `andThen` prompt "HE HEALS YOU TO FULL STRENGTH"
              `andThen` after 30 c.img (checkForTraps . setAction WAITING)
            else if (c.name == "HOBBIT" && chance (1.0 - 0.05 * toFloat s.stats.dex) s.rnd.possessiveness) || chance 0.02 s.rnd.possessiveness then
              prompt ("THE " ++ c.name ++ " MAKES A QUICK MOVE") `andThen`
               (if hasAnythingToSteal s then
                  stealSomething c.img
                else prompt "YOU HAVE NOTHING HE WANTS TO STEAL!" `andThen` after 30 c.img (setAction WAITING `andThen` checkForTraps))
            else prompt "`F`IGHT, `C`AST, OR `E`VADE:")
  else setAction WAITING `andThen` checkForTraps

successfully : GameState -> GameState
successfully = checkForEncounter . resetIdleTimer . regenerate . reduceSpellDurations

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
  FIGHTING    _ -> if s.idleTimer <= 1 then resetIdleTimer s |> withMessage "Wait"
                   else {s | idleTimer <- s.idleTimer - 1}
  FALLING     n -> if n == 0 then {s | action <- WAITING, prompt <- []} |> travel bottomOf |> successfully
                   else {s | action <- FALLING (n - 1)}
  RISING      n -> if n == 0 then {s | action <- WAITING, prompt <- []} |> travel topOf |> successfully
                   else {s | action <- RISING (n - 1)}
  TELEPORTING n -> if n == -21 then {s | action <- WAITING, prompt <- []} |> successfully
                   else s |> setAction (TELEPORTING (n - 1))
                          |> if n == 0 then travel (randomPos s.rnd.teleporterInstability) `andThen` setAction (TELEPORTING (n - 1)) else id
  RESTING       -> s |> clearPrompt
  READING n i f -> if n == 0 then f s else {s | action <- READING (n - 1) i f}

isFighting : GameState -> Bool
isFighting s = case s.action of
  FIGHTING _ -> True
  _ -> False

setAction : Action -> GameState -> GameState
setAction a s = {s | action <- a}

increaseDuration : String -> Int -> GameState -> GameState
increaseDuration n t s = {s | effects <- map (\sf -> {sf | duration <- if sf.name == n then sf.duration + t else sf.duration}) s.effects}

prompt : String -> GameState -> GameState
prompt m s = {s | prompt <- s.prompt ++ [m]}

clearPrompt : GameState -> GameState
clearPrompt s = {s | prompt <- []}

--fight : GameState -> GameState
--fight s = s |> clearPrompt

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
  FIGHT  -> if s |> isFighting then s |> clearPrompt |> setAction WAITING |> checkForTraps |> withMessage "You killed it!" |> resetIdleTimer
            else wtf s
  CAST a -> if s |> canCast a then increaseDuration "Astral Walk" 10 s |> withMessage "Cast" |> resetIdleTimer
            else wtf s
  CONTINUE -> if s.action == RESTING then s |> travel bottomOf |> setAction WAITING |> successfully else s
  TICK   -> reduceMessageTimer (idle s)
  ROLL ctx -> {s | rnd <- ctx}

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

gnoll = ghoul
kobold = ghoul
skeleton = ghoul
hobbit = dwarf
zombie = ghoul
orc = dwarf
fighter = dwarf
mummy = ghoul
elf = dwarf
troll = ghoul
wraith = ghoul
ogre = dwarf
minotaur = dwarf
giant = dwarf
specter = ghoul
vampire = ghoul
demon = ghoul
dragon = ghoul
dwarf = spriteForm "dwarf.png" |> move (-8 * zoom, 8 * zoom)
ghoul = spriteForm "ghoul.png" |> move (-10 * zoom, 10 * zoom)

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

drawAction : GameState -> [Form]
drawAction s = case s.action of
  FALLING n -> [fallingHero n |> moveY (toFloat (zoom * (n - 21)) / 2)]
  RISING n -> [elevator |> moveY (toFloat (zoom * (21 - n))),
    hero |> moveY (toFloat (zoom * (21 - n)))]
  TELEPORTING n -> [hero, rect 800 600 |> filled (rgba 0 0 0 ((21 - abs (toFloat n)) / 21))]
  FIGHTING m -> [m.img, hero]
  READING n i f -> [i, hero]
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
drawInn s = [c64s (innDesc s.pos) |> move (-12 * 24 + 12, -10 * 24), hero]

drawGameState : GameState -> [Form]
drawGameState s = if s.pos.z == 0 then drawInn s
  else drawNeighborhood s.pos ++ drawAction s

textify : String -> Element
textify s = toText ("   " ++ s) |> centered

spellEffectsStr : GameState -> String
spellEffectsStr s = map (\sf -> if sf.duration > 0 then sf.abbr ++ " " else "") s.effects |> foldl (++) ""

monsterStatus : GameState -> Form
monsterStatus s = case s.action of
  FIGHTING m -> c64 ("`LVL " ++ show m.level ++ " " ++ m.name) |> move (4 * 24, -5 * 24)
  _ -> empty |> toForm

charAt : (Int, Int) -> Element
charAt (x, y) = croppedImage (x * 24, y * 24) 24 24 "charset.png"

c64Char : Int -> Char -> Element
c64Char off c = case c of
  '@' -> charAt (0, off)
  'A' -> charAt (1, off)
  'B' -> charAt (2, off)
  'C' -> charAt (3, off)
  'D' -> charAt (4, off)
  'E' -> charAt (5, off)
  'F' -> charAt (6, off)
  'G' -> charAt (7, off)
  'H' -> charAt (8, off)
  'I' -> charAt (9, off)
  'J' -> charAt (10, off)
  'K' -> charAt (11, off)
  'L' -> charAt (12, off)
  'M' -> charAt (13, off)
  'N' -> charAt (14, off)
  'O' -> charAt (15, off)
  'P' -> charAt (16, off)
  'Q' -> charAt (17, off)
  'R' -> charAt (18, off)
  'S' -> charAt (19, off)
  'T' -> charAt (20, off)
  'U' -> charAt (21, off)
  'V' -> charAt (22, off)
  'W' -> charAt (23, off)
  'X' -> charAt (24, off)
  'Y' -> charAt (25, off)
  'Z' -> charAt (26, off)
  '[' -> charAt (27, off)
  -- British Pound
  ']' -> charAt (28, off + 1)
  -- Up arrow
  -- Left arrow
  ' ' -> charAt (0, off + 1)
  '!' -> charAt (1, off + 1)
  '"' -> charAt (2, off + 1)
  '#' -> charAt (3, off + 1)
  '$' -> charAt (4, off + 1)
  '%' -> charAt (5, off + 1)
  '&' -> charAt (6, off + 1)
  '\'' -> charAt (7, off + 1)
  '(' -> charAt (8, off + 1)
  ')' -> charAt (9, off + 1)
  '*' -> charAt (10, off + 1)
  '+' -> charAt (11, off + 1)
  ',' -> charAt (12, off + 1)
  '-' -> charAt (13, off + 1)
  '.' -> charAt (14, off + 1)
  '/' -> charAt (15, off + 1)
  '0' -> charAt (16, off + 1)
  '1' -> charAt (17, off + 1)
  '2' -> charAt (18, off + 1)
  '3' -> charAt (19, off + 1)
  '4' -> charAt (20, off + 1)
  '5' -> charAt (21, off + 1)
  '6' -> charAt (22, off + 1)
  '7' -> charAt (23, off + 1)
  '8' -> charAt (24, off + 1)
  '9' -> charAt (25, off + 1)
  ':' -> charAt (26, off + 1)
  ';' -> charAt (27, off + 1)
  '<' -> charAt (28, off + 1)
  '=' -> charAt (29, off + 1)
  '>' -> charAt (30, off + 1)
  '?' -> charAt (31, off + 1)

enchi : String -> (Bool, Element)
enchi s = foldl (\c (h, s) -> if c == '`' then (not h, s) else (h, s `beside` c64Char (if h then 4 else 0) c)) (False, empty) (String.toList s)

c64 : String -> Form
c64 s = let (_, fs) = enchi s in fs |> toForm |> moveX ((widthOf fs |> toFloat) / 2)

c64s : [String] -> Form
c64s ss = zip ss [0 .. length ss] |> map (\(s, i) -> c64 s |> moveY (toFloat i * -24)) |> group

bonusStr : Int -> String
bonusStr n = if n == 0 then "" else "+" ++ show n

equipStr : Equipment -> [String]
equipStr q = let n = q.bonus in
  if n == 0 && not q.isAlwaysEquipped then []
  else [" " ++ q.abbr ++ " " ++ bonusStr n]

consumables = [ScrollOfRescue, PotionOfHealing, PotionOfStrength]

consumableAbbr : ConsumableClass -> String
consumableAbbr c = case c of
  ScrollOfRescue -> "SCRL RESC"
  PotionOfHealing -> "POT HEAL"
  PotionOfStrength -> "POT STRG"

consStr : GameState -> ConsumableClass -> [String]
consStr s c = let n = s.consumableCount c in if n == 0 then [] else ["  " ++ show n ++ " " ++ consumableAbbr c]

charData : GameState -> Form
charData s = let t = s.stats in
  c64s ([s.name ++ " LVL " ++ show s.level,
  "STR " ++ show t.str ++ "   CON " ++ show t.con,
  "INT " ++ show t.int ++ "   DEX " ++ show t.dex,
  "WIS " ++ show t.wis ++ "   CHR " ++ show t.chr,
  "HP " ++ show s.hp ++ "/" ++ show s.maxhp,
  "SU " ++ show s.su ++ "/" ++ show s.maxsu,
  "EX " ++ show s.exp,
  "GD " ++ show s.gold] ++ concatMap equipStr s.equipment ++ concatMap (consStr s) consumables)

showPrompt : GameState -> Form
showPrompt s = c64s s.prompt

drawView : GameState -> Element
drawView s = ((collage 960 720 [drawGameState s |> group |> move (-200, 110),
  charData s |> move (100, 14 * 24),
  showPrompt s |> move (-19 * 24, -6 * 24),
  monsterStatus s] |> color black) `above`
  textify (spellEffectsStr s) `above` textify s.msg)

-- Program

main : Signal Element
main = drawView <~ stateSignal
