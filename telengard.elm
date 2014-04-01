-- A remake of Telengard

import Keyboard
import Text
import Random
import Dict (Dict)
import Dict

-- Abstract Model

data Boundary = EMPTY | DOOR | WALL

data Feature = NONE | ELEVATOR | PIT | TELEPORTER | STAIRS_UP | STAIRS_DOWN
  | STAIRS_BOTH | ALTAR | FOUNTAIN | CUBE | THRONE | BOX

data Spell = AstralWalk

type SpellEffect = {name : String, abbr : String, duration : Int}

data Command = ERROR | NORTH | EAST | SOUTH | WEST | UP | DOWN | STAY |
  FIGHT | CAST Spell | TICK [Float] | CONTINUE

type Monster = {name : String, img : Form, power : Int, level : Int, hp : Int,
  hitBonus : Int, damageMultiplier : Int, isUndead : Bool, isVeryEvil : Bool,
  drainChance : Float, paralyzeChance : Float}

type Treasure = {name : String, img : Form, value : Int, level : Int}

treasure : String -> Form -> Int -> Int -> Treasure
treasure n i v l = {name = n, img = i, value = v, level = l}

treasures = [treasure "REFUSE" refuse 0 1,
             treasure "SILVER" silver 1 1,
             treasure "GOLD" gold 2 1,
             treasure "GEMS" gems 3 1,
             treasure "JEWELS" gems 4 1]

data Action = WAITING | FIGHTING Monster | FALLING Int | RISING Int | TELEPORTING Int | RESTING | BEING_DEAD |
  LOOTING_FREE Treasure Bool | LOOTING_CHEST Int Bool | LOOTING_ITEM Item Int Bool

data Continuation = Same | Next (GameState -> GameState)

type Item = {name : String, abbr : String, bonus : Int, isAlwaysEquipped : Bool, isConsumable : Bool}

type Room = {top : Boundary, left : Boundary, feature : Feature}

type Position = {x : Int, y : Int, z : Int}

type Stats = {str : Int, int : Int, wis : Int, con : Int, dex : Int, chr : Int}

type GameState = {action : Action, pos : Position, name : String, level : Int,
  stats : Stats, hp : Int, maxhp : Int, su : Int, maxsu : Int, exp : Int, gold : Int,
  items : [Item],
  effects : [SpellEffect],
  msg : String, msgTimer : Int,
  prompt : [String],
  idleTimer : Int,
  delayTimer : Int,
  delayContinuation : Continuation,
  rnd : [Float]}

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

commandStream : Signal Command
commandStream = (keyCodeCommand <~ Keyboard.lastPressed) `merge`
                (TICK <~ Random.floatList (always 50 <~ every (second / 10)))

-- State Changes

effect : String -> String -> Int -> SpellEffect
effect n a d = {name = n, abbr = a, duration = d}

item : String -> String -> Bool -> Bool -> Int -> Item
item n a ae ic b = {name = n, abbr = a, isAlwaysEquipped = ae, isConsumable = ic, bonus = b}

initStats = {str = 9, int = 9, wis = 9, con = 9, dex = 9, chr = 9}

initState = {action = WAITING, pos = {x = 25, y = 13, z = 1},
  name = "STABULO", level = 1, stats = initStats, hp = 9, maxhp = 9, su = 1, maxsu = 1, exp = 0, gold = 0,

  items = [
    item "SWORD" "SWORD" True False 99,
    item "ARMOR" "ARMOR" True False 0,
    item "SHIELD" "SHIELD" True False 0,
    item "ELVEN CLOAK" "ELVN CLK" False False 0,
    item "ELVEN BOOTS" "ELVN BTS" False False 0,
    item "RING OF REGENERATION" "RING REG" False False 0,
    item "RING OF PROTECTION" "RING PROT" False False 0,
    item "SCROLL OF RESCUE" "SCRL RESC" False True 0,
    item "POTION OF HEALING" "POT HEAL" False True 0,
    item "POTION OF STRENGTH" "POT STRG" False True 0],

  effects = [
    effect "STRENGTH" "STRG" 0,
    effect "DETECT TRAPS" "DTRP" 0,
    effect "LIGHT" "LGHT" 0,
    effect "PROTECTION FROM EVIL" "PROT" 0,
    effect "LEVITATION" "LEVT" 0,
    effect "INVISIBILITY" "INVS" 0,
    effect "FEAR" "FEAR" 0,
    effect "ASTRAL WALK" "ASTW" 0,
    effect "TIME STOP" "TMST" 0,
    effect "RESURRECTION" "RESD" 0,
    effect "DRUNK" "DRNK" 0],

  msg = "", msgTimer = 0, prompt = [], idleTimer = 50,
  delayTimer = 0, delayContinuation = Same,
  rnd = []} |> checkForTraps

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
  ((s |> hasEffect "ASTRAL WALK") || (roomAt s.pos).top /= WALL)

canMoveWest : GameState -> Bool
canMoveWest s = s.action == WAITING && s.pos.x > 1 &&
  ((s |> hasEffect "ASTRAL WALK") || (roomAt s.pos).left /= WALL)

canMoveSouth : GameState -> Bool
canMoveSouth s = s.action == WAITING && s.pos.y < 200 &&
  ((s |> hasEffect "ASTRAL WALK") || (roomAt (southOf s.pos)).top /= WALL)

canMoveEast : GameState -> Bool
canMoveEast s = s.action == WAITING && s.pos.x < 200 &&
  ((s |> hasEffect "ASTRAL WALK") || (roomAt (eastOf s.pos)).left /= WALL)

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

itemBonus : String -> GameState -> Int
itemBonus n s = findWithDefault {name = "", abbr = "", isAlwaysEquipped = False, isConsumable = False, bonus = 0} (\q -> q.name == n) s.items |> .bonus

regenerate : GameState -> GameState
regenerate s = {s | hp <- min (s.hp + itemBonus "RING OF REGENERATION" s) s.maxhp}

resetIdleTimer : GameState -> GameState
resetIdleTimer s = {s | idleTimer <- 50}

promptStairway : Bool -> Bool -> GameState -> GameState
promptStairway u d s = s |> clearPrompt
  |> prompt "YOU HAVE FOUND A STAIRWAY"
  |> (if u && s.pos.z == 1 then prompt "YOU SEE `LIGHT` ABOVE" else id)
  |> prompt ("DO YOU WANT TO " ++ (if u then "GO `U`P, " else "") ++ (if d then "GO `D`OWN," else ""))
  |> prompt "OR `S`TAY ON THE SAME LEVEL?"

roll : (Float -> GameState -> GameState) -> GameState -> GameState
roll f s = f (head s.rnd) {s | rnd <- tail s.rnd}

checkForTraps : GameState -> GameState
checkForTraps t = let s = t |> setAction WAITING in case (roomAt s.pos).feature of
  PIT        -> s |> clearPrompt
                  |> prompt "YOU SEE A PIT"
                  |> roll (\r -> if r < 0.5 then setAction (FALLING 21) `andThen` prompt "YOU FALL IN!!"
                    else prompt "YOU CAN GO `D`OWN OR TRAVEL ON")
  ELEVATOR   -> {s | action <- RISING 21, prompt <- ["YOU FEEL HEAVY FOR A MOMENT"]}
  TELEPORTER -> {s | action <- TELEPORTING 21, prompt <- ["ZZAP!! YOU'VE BEEN TELEPORTED..."]}
  STAIRS_UP -> s |> promptStairway True False
  STAIRS_BOTH -> s |> promptStairway True True
  STAIRS_DOWN -> s |> promptStairway False True
  _          -> s |> clearPrompt

mc : String -> Form -> Int -> Int -> Int -> Bool -> Bool -> Float -> Float -> Monster
mc n i p h d u v dc pc = {name = n, img = i, power = p, hitBonus = h, damageMultiplier = d,
  isUndead = u, isVeryEvil = v, drainChance = dc, paralyzeChance = pc, level = 0, hp = 0}

monsterClass : Int -> Monster
monsterClass n = case n of
  1 -> mc "GNOLL" gnoll 1 0 1 False False 0 0
  2 -> mc "KOBOLD" kobold 2 0 1 False False 0 0
  3 -> mc "SKELETON" skeleton 3 0 1 True False 0 0
  4 -> mc "HOBBIT" hobbit 4 0 1 False False 0 0
  5 -> mc "ZOMBIE" zombie 5 0 1 True False 0 0
  6 -> mc "ORC" orc 6 0 1 False False 0 0
  7 -> mc "FIGHTER" fighter 7 0 1 False False 0 0
  8 -> mc "MUMMY" mummy 8 0 1 True False 0 0
  9 -> mc "ELF" elf 9 0 1 False False 0 0
  10 -> mc "GHOUL" ghoul 10 0 1 True False 0 0.5
  11 -> mc "DWARF" dwarf 11 0 1 False False 0 0
  12 -> mc "TROLL" troll 12 0 1 False False 0 0
  13 -> mc "WRAITH" wraith 13 0 1 True False 0.1 0
  14 -> mc "OGRE" ogre 14 0 1 False False 0 0
  15 -> mc "MINOTAUR" minotaur 15 0 1 False False 0 0
  16 -> mc "GIANT" giant 16 0 1 False False 0 0
  17 -> mc "SPECTER" specter 17 0 1 True True 0.2 0
  18 -> mc "VAMPIRE" vampire 18 0 1 True True 0.3 0.3
  19 -> mc "DEMON" demon 19 4 5 False True 0 0
  20 -> mc "DRAGON" dragon 20 0 1 False False 0 0

-- TODO: Check for invisibility, elven cloak effect, more monster classes, time stop
-- initiative check

dn : Int -> Float -> Int
dn n r = floor (frac r * toFloat n + 1)

chance : Float -> Float -> Bool
chance f r = frac r < f

after : Int -> (GameState -> GameState) -> GameState -> GameState
after t f s = {s | delayTimer <- t, delayContinuation <- Next f}

fullHeal : GameState -> GameState
fullHeal s = {s | hp <- s.maxhp}

andThen : (a -> b) -> (b -> c) -> a -> c
andThen f g = g . f

stuffToSteal : GameState -> [Item]
stuffToSteal s = filter (\q -> q.bonus > 0) s.items

hasAnythingToSteal : GameState -> Bool
hasAnythingToSteal s = any (\q -> q.bonus > 0) s.items

steal : String -> GameState -> GameState
steal n s = {s | items <- s.items |> map (\q -> if q.name == n then
  {q | bonus <- if q.isConsumable then q.bonus - 1 else 0} else q)}

stealSomething : Form -> GameState -> GameState
stealSomething f s = s |> roll (\r -> let
    ss = stuffToSteal s
    n = floor (r * toFloat (length ss))
    q = ss |> nth n
  in prompt ("HE STEALS YOUR " ++ q.name) `andThen` steal q.name
    `andThen` after 20 checkForTraps)

replaceItem : String -> Int -> GameState -> GameState
replaceItem n b s = {s | items <- map (\q -> if q.name == n then {q | bonus <- b} else q) s.items}

roll10 : (Float -> Float -> Float -> Float -> Float -> Float -> Float -> Float ->
  Float -> Float -> GameState -> GameState) -> GameState -> GameState
roll10 f = roll (\ra -> roll (\rb -> roll (\rc -> roll (\rd -> roll (\re -> roll (\rf ->
  roll (\rg -> roll (\rh -> roll (\ri -> roll (\rj -> f ra rb rc rd re rf rg rh ri rj))))))))))

checkForEncounter : GameState -> GameState
checkForEncounter s = s |> roll10 (\sneakiness monsterPower monsterLevel monsterHealth undeadFearOfLight
  sexiness possessiveness likability typeOfMagicItemsLyingAround monsterGenerosity ->
  if sneakiness < 0.3 then
  let
    c = monsterClass (dn 20 monsterPower)
    l = floor ((monsterLevel ^ 1.5) * (toFloat s.pos.z * 2 + 2) + 1)
    h = floor ((monsterHealth ^ 0.5) * toFloat l * toFloat c.power + 1)
  in
    if (s |> hasEffect "FEAR") && c.power < 5 then
      checkForEncounter
    else if (s |> hasEffect "LIGHT") && c.isUndead && chance 0.2 undeadFearOfLight then
      checkForEncounter
    else
      setAction (FIGHTING {c | level <- l, hp <- h})
      `andThen` clearPrompt
       `andThen` prompt ("YOU HAVE ENCOUNTERED A LVL " ++ show l ++ " " ++ c.name)
        `andThen` (if (c.name == "ELF" && chance (0.04 * toFloat s.stats.chr) sexiness) || chance 0.02 sexiness then
              fullHeal
              `andThen` prompt ("THE " ++ c.name ++ " LIKES YOUR BODY")
              `andThen` prompt "HE HEALS YOU TO FULL STRENGTH"
              `andThen` after 20 (checkForLoot l)
            else if (c.name == "HOBBIT" && chance (1.0 - 0.05 * toFloat s.stats.chr) possessiveness) || chance 0.02 possessiveness then
              prompt ("THE " ++ c.name ++ " MAKES A QUICK MOVE") `andThen`
               (if hasAnythingToSteal s then
                  stealSomething c.img
                else prompt "YOU HAVE NOTHING HE WANTS TO STEAL!" `andThen` after 20 (checkForLoot l))
            else if (c.name == "DRAGON" && dn 30 likability <= s.stats.chr) || chance 0.02 likability then
              let
                itemToReplace = s.items |> nth (floor (typeOfMagicItemsLyingAround * 7))
              in
                if itemToReplace.bonus < l then
                  let
                    maxUpgrade = l - itemToReplace.bonus
                    actualBonus = itemToReplace.bonus + dn maxUpgrade monsterGenerosity
                  in
                    replaceItem itemToReplace.name actualBonus
                    `andThen` prompt ("THE " ++ c.name ++ " LIKES YOU!")
                    `andThen` prompt ("HE GIVES YOU A " ++ itemToReplace.name ++ " +" ++ show actualBonus)
                    `andThen` after 20 (checkForLoot l)
                else
                  prompt "`F`IGHT, `C`AST, OR `E`VADE:"
            else prompt "`F`IGHT, `C`AST, OR `E`VADE:")
  else checkForLoot s.pos.z)

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
                          |> if n == 0 then roll (\r -> travel (randomPos r) `andThen` setAction (TELEPORTING (n - 1)))
                             else id
  RESTING       -> s |> clearPrompt
  _             -> s

setAction : Action -> GameState -> GameState
setAction a s = {s | action <- a}

increaseDuration : String -> Int -> GameState -> GameState
increaseDuration n t s = {s | effects <-
  map (\sf -> {sf | duration <- if sf.name == n then sf.duration + t else sf.duration}) s.effects}

prompt : String -> GameState -> GameState
prompt m s = {s | prompt <- s.prompt ++ [m]}

clearPrompt : GameState -> GameState
clearPrompt s = {s | prompt <- []}

reduceDelayTimer : GameState -> GameState
reduceDelayTimer s = if s.delayTimer > 1 then {s | delayTimer <- s.delayTimer - 1}
  else if s.delayTimer == 1 then case s.delayContinuation of
    Same -> s
    Next f -> f {s | delayTimer <- 0}
  else s

isNotDelayed : GameState -> Bool
isNotDelayed s = s.delayTimer <= 0

gainExp : Int -> GameState -> GameState
gainExp x s = {s | exp <- s.exp + x} |> prompt ("YOU GAIN " ++ show x ++ " EXPERIENCE POINTS")

clearEffect : String -> GameState -> GameState
clearEffect n s = {s | effects <- map (\f -> if f.name == n then {f | duration <- 0}  else f) s.effects}

updateCon : Int -> Stats -> Stats
updateCon n s = {s | con <- n}

when : (GameState -> GameState) -> Bool -> GameState -> GameState
when t c = if c then t else id

checkForItem : Int -> Bool -> GameState -> GameState
checkForItem ofLvl trapped s = s |> roll5 (\stuffLyingAround quality _ _ _ -> let
      q = s.items |> nth (floor (10 * stuffLyingAround))
      lvl = floor (quality ^ 0.5 * (toFloat ofLvl + 1) + 1)
      q2 = {q | bonus <- lvl}
    in
      prompt ("YOU SEE A " ++ showItem q2)
      `andThen` prompt "<RET> TO PICK IT UP:"
      `andThen` setAction (LOOTING_ITEM q2 ofLvl trapped))

setLevel : Int -> Treasure -> Treasure
setLevel n t = {t | level <- n}

checkForLoot : Int -> GameState -> GameState
checkForLoot ofLvl s = s |> clearPrompt |>
  roll10 (\attentionToDetail traptitude looseChange greed premonition piracy _ _ _ _ ->
  if chance 0.2 attentionToDetail then
    let trapped = chance 0.15 traptitude in
    if chance 0.3 looseChange then
      let t = treasures |> nth (floor (5 * greed)) |> setLevel ofLvl in
        prompt ("YOU SEE SOME " ++ t.name)
        `andThen` prompt "PRESS <RET> TO PICK UP:"
        `andThen` (prompt "YOU DETECT TRAPS!" `when` (trapped && hasEffect "DETECT TRAPS" s && chance 0.9 premonition))
        `andThen` setAction (LOOTING_FREE t trapped)
    else if chance 0.5 piracy then
      prompt "YOU HAVE FOUND A TREASURE CHEST!!"
      `andThen` (prompt "YOU DETECT TRAPS!" `when` (trapped && hasEffect "DETECT TRAPS" s && chance 0.9 premonition))
      `andThen` prompt "<RET> TO OPEN IT:"
      `andThen` setAction (LOOTING_CHEST ofLvl trapped)
    else checkForItem ofLvl trapped
  else checkForTraps)

resurrect : GameState -> GameState
resurrect s = let
    s2 = {s | stats <- updateCon (s.stats.con - 1) s.stats}
         |> clearEffect "RESURRECTION" |> clearPrompt |> prompt "RESURRECTION"
  in
    s2 |> roll (\r -> if chance (toFloat s2.stats.con * 0.06) r then
      prompt "IT WORKS!" `andThen` fullHeal `andThen` after 20 (checkForLoot s.pos.z)
    else
      prompt "IT DOESN'T WORK!" `andThen` after 20 (setAction BEING_DEAD))

takeDamage : Int -> (GameState -> GameState) -> GameState -> GameState
takeDamage d f s = let
    h = s.hp - d
    s2 = {s | hp <- s.hp - d}
  in
    if h < 1 then
      s2 |> prompt "YOU DIED!!" `andThen`
        after 20 (if hasEffect "RESURRECTION" s then resurrect else setAction BEING_DEAD)
    else s2 |> f

roll5 : (Float -> Float -> Float -> Float -> Float -> GameState -> GameState) -> GameState -> GameState
roll5 f = roll (\a -> roll (\b -> roll (\c -> roll (\d -> roll (\e -> f a b c d e)))))

opponentAttacks : GameState -> GameState
opponentAttacks s = case s.action of
  FIGHTING m -> s |> roll5 (\monsterScrappiness monsterFocus monsterMuscleMemory monsterThirst monsterVileness -> let
    hitRoll = floor (monsterScrappiness * 20) + m.level - itemBonus "ARMOR" s - itemBonus "SHIELD" s + m.hitBonus
      - if m.isVeryEvil && hasEffect "PROTECTION FROM EVIL" s then 6 else 0
  in
  if hitRoll < 10 then prompt "IT MISSED..." `andThen` after 15 (clearPrompt `andThen` prompt "`F`IGHT, `C`AST, OR `E`VADE:")
  else let
    damageRoll = floor ((monsterFocus * 8 + monsterMuscleMemory * toFloat m.level * 2 + 1) * toFloat m.damageMultiplier)
    drain = chance m.drainChance monsterThirst
    paralyze = chance m.paralyzeChance monsterVileness
  in
    prompt ("IT DOES " ++ show damageRoll ++ " POINTS DAMAGE")
    `andThen` takeDamage damageRoll (after 15 (clearPrompt `andThen` prompt "`F`IGHT, `C`AST, OR `E`VADE:")))
  _ -> s

damageOpponent : Int -> GameState -> GameState
damageOpponent d s = case s.action of
  FIGHTING m -> let
    mh = m.hp - d
    exp = m.level * m.power * 10
    s2 = s |> prompt ("YOU DO " ++ show d ++ " POINTS DAMAGE")
  in
    if mh <= 0 then
      s2 |> prompt "IT DIED..."
         |> gainExp exp
         |> after 20 (clearPrompt `andThen` checkForLoot m.level)
    else
      s2 |> opponentAttacks
  _ -> s

fight : GameState -> GameState
fight s = case s.action of
  FIGHTING m -> s |> clearPrompt |> roll5 (\scrappiness focus muscleMemory _ _ -> let
    hitRoll = floor (scrappiness * 20) + s.level + itemBonus "SWORD" s + s.stats.str `div` 2 +
      if hasEffect "STRENGTH" s then 4 else 0
  in
    if hitRoll < 10 then
      prompt "YOU MISSED..." `andThen` opponentAttacks
    else
      let
        damageRoll = floor (focus * 8 + muscleMemory * toFloat s.level * 2 + toFloat (itemBonus "SWORD" s) +
          (if hasEffect "STRENGTH" s then 5 else 0) + 1)
      in
        damageOpponent damageRoll)
  _ -> s

promptSameLine : String -> GameState -> GameState
promptSameLine m s = let r = reverse s.prompt in {s | prompt <- case r of
  [] -> [m]
  x :: xs -> reverse ((x ++ m) :: xs)}

addGold : Int -> GameState -> GameState
addGold n s = {s | gold <- s.gold + n}

lootTreasure : Treasure -> Bool -> GameState -> GameState
lootTreasure t trapped s = s |> promptSameLine "SNARF IT"
  |> roll (\r -> let damageRoll = dn (3 * t.level) r in prompt "IT'S TRAPPED!"
    `andThen` prompt ("YOU SUFFER " ++ show damageRoll ++ " POINTS DAMAGE")
    `andThen` takeDamage damageRoll id) `when` trapped
  |> roll (\r -> let goldRoll = dn (t.value * t.level * 200) r in
     prompt ("IT'S WORTH " ++ show goldRoll ++ " GOLD") `andThen` addGold goldRoll)
  |> roll (\r -> if t.value == 0 && chance 0.2 r then
     after 20 (clearPrompt `andThen` checkForItem t.level trapped)
     else after 20 checkForTraps)

lootChest : Int -> Bool -> GameState -> GameState
lootChest lvl trapped s = s |> promptSameLine "OPEN IT"
  |> roll (\r -> let damageRoll = dn (10 * s.pos.z) r in
     prompt "CHEST EXPLODES!!!!!" `andThen` prompt ("YOU SUFFER " ++ show damageRoll ++ " POINTS DAMAGE")
     `andThen` takeDamage damageRoll id) `when` trapped
  |> roll (\r1 -> if chance 0.1 r1 then
       prompt "INSIDE, THERE ARE ONLY COBWEBS..." `andThen` after 20 checkForTraps
     else roll (\r2 -> let goldRoll = dn (1000 * lvl^2) r2 in prompt ("INSIDE ARE " ++ show goldRoll ++ " GOLD PIECES!")
       `andThen` addGold goldRoll `andThen` roll (\r3 -> if chance 0.5 r3 then
          after 20 (clearPrompt `andThen` checkForItem lvl False)
          else after 20 checkForTraps)))

takeItem : Item -> GameState -> GameState
takeItem i s = {s | items <- map (\x -> if x.name == i.name then
    if x.isConsumable then {x | bonus <- x.bonus + 1}
    else {x | bonus <- i.bonus}
  else x) s.items}

-- NOTE : I think it is a bug from the original game where a hostile sword only does 1d5 damage no matter
-- the item level.  I suspect in the original code the "I" for the item index (which is always SWORD = 1)
-- should be a "L" for level, but I'm leaving it the same.

lootItem : Item -> Int -> Bool -> GameState -> GameState
lootItem i lvl trapped = if trapped && i.name == "SWORD" then
    roll (\r -> let damageRoll = dn 5 r in
      prompt "IT'S A HOSTILE SWORD!!" `andThen` prompt ("YOU SUFFER " ++ show damageRoll ++ " DAMAGE POINTS")
      `andThen` takeDamage damageRoll (after 20 checkForTraps))
  else
    promptSameLine "IT'S YOURS!" `andThen` takeItem i `andThen` after 20
      (roll (\r -> if chance 0.5 r then clearPrompt `andThen` checkForItem lvl False
        else checkForTraps))

update : Command -> GameState -> GameState
update c s = case c of
  ERROR  -> wtf s
  NORTH  -> if canMoveNorth s && isNotDelayed s
            then travel northOf s |> withMessage "North" |> successfully
            else wtf s
  EAST   -> if canMoveEast s && isNotDelayed s
            then travel eastOf s |> withMessage "East" |> successfully
            else wtf s
  SOUTH  -> if canMoveSouth s && isNotDelayed s
            then travel southOf s |> withMessage "South" |> successfully
            else wtf s
  WEST   -> if canMoveWest s && isNotDelayed s
            then travel westOf s |> withMessage "West" |> successfully
            else wtf s
  UP     -> if canMoveUp s && isNotDelayed s
            then travel topOf s |> (if s.pos.z == 1 then fullHeal `andThen` setAction RESTING
              else successfully . withMessage "Up")
            else wtf s
  DOWN   -> if canMoveDown s && isNotDelayed s
            then travel bottomOf s |> withMessage "Down" |> successfully
            else wtf s
  STAY   -> if s.action == WAITING && isNotDelayed s
            then s |> withMessage "Stay" |> successfully
            else wtf s
  FIGHT  -> if isNotDelayed s
            then fight s |> resetIdleTimer
            else wtf s
  CAST a -> if canCast a s && isNotDelayed s
            then increaseDuration "Astral Walk" 10 s |> withMessage "Cast" |> resetIdleTimer
            else wtf s
  CONTINUE -> case s.action of
    RESTING -> s |> travel bottomOf |> setAction WAITING |> successfully
    LOOTING_FREE t trapped -> s |> lootTreasure t trapped
    LOOTING_CHEST lvl trapped -> s |> lootChest lvl trapped
    LOOTING_ITEM i lvl trapped -> s |> lootItem i lvl trapped
    _ -> s
  TICK xs -> reduceDelayTimer (reduceMessageTimer (idle {s | rnd <- xs}))

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

wideSpriteForm : String -> Form
wideSpriteForm s = fittedImage (48 * zoom) (21 * zoom) s |> toForm

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

refuse = wideSpriteForm "refuse.png" |> move (9 * zoom, 12 * zoom)
silver = wideSpriteForm "silver.png" |> move (9 * zoom, 12 * zoom)
gold = wideSpriteForm "gold.png" |> move (9 * zoom, 12 * zoom)
gems = spriteForm "gems.png" |> move (9 * zoom, 12 * zoom)
chest = spriteForm "chest.png" |> move (9 * zoom, 12 * zoom)

gnoll = kobold
kobold = spriteForm "kobold.png" |> move (-8 * zoom, 8 * zoom)
skeleton = ghoul
hobbit = dwarf
zombie = ghoul
orc = kobold
fighter = dwarf
mummy = ghoul
elf = dwarf
troll = spriteForm "troll.png" |> move (-8 * zoom, 8 * zoom)
wraith = ghoul
ogre = spriteForm "ogre.png" |> move (-8 * zoom, 8 * zoom)
minotaur = dwarf
giant = ogre
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
  LOOTING_FREE t _ -> [t.img, hero]
  LOOTING_CHEST _ _ -> [chest, hero]
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
enchi s = foldl (\c (h, s) -> if c == '`' then (not h, s)
  else (h, s `beside` c64Char (if h then 4 else 0) c)) (False, empty) (String.toList s)

c64 : String -> Form
c64 s = let (_, fs) = enchi s in fs |> toForm |> moveX ((widthOf fs |> toFloat) / 2)

c64s : [String] -> Form
c64s ss = zip ss [0 .. length ss] |> map (\(s, i) -> c64 s |> moveY (toFloat i * -24)) |> group

bonusStr : Int -> String
bonusStr n = if n == 0 then "" else "+" ++ show n

showItem : Item -> String
showItem q = if q.isConsumable then q.name
  else q.name ++ " " ++ bonusStr q.bonus

maybeShowItem : Item -> [String]
maybeShowItem q = let n = q.bonus in
  if n == 0 && not q.isAlwaysEquipped then []
  else if q.isConsumable then ["  " ++ show n ++ " " ++ q.abbr]
  else [" " ++ q.abbr ++ " " ++ bonusStr n]

charData : GameState -> Form
charData s = let t = s.stats in
  c64s ([s.name ++ " LVL " ++ show s.level,
  "STR " ++ show t.str ++ "   CON " ++ show t.con,
  "INT " ++ show t.int ++ "   DEX " ++ show t.dex,
  "WIS " ++ show t.wis ++ "   CHR " ++ show t.chr,
  "HP " ++ show s.hp ++ "/" ++ show s.maxhp,
  "SU " ++ show s.su ++ "/" ++ show s.maxsu,
  "EX " ++ show s.exp,
  "GD " ++ show s.gold] ++ concatMap maybeShowItem s.items)

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
