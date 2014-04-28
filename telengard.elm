-- A remake of Telengard

import Keyboard
import Text
import Random
import Dict (Dict)
import Dict
import Graphics
import Dungeon
import C64

-- Abstract Model

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

type Item = {name : String, abbr : String, bonus : Int, isAlwaysEquipped : Bool, isConsumable : Bool}

type Stats = {str : Int, int : Int, wis : Int, con : Int, dex : Int, chr : Int}

type Transition = GameState -> GameState

data Continuation = Same | Next Transition

type GameState = {action : Action, pos : Dungeon.Position, name : String, level : Int,
  stats : Stats, hp : Int, maxhp : Int, su : Int, maxsu : Int, exp : Int, gold : Int, totalGold : Int,
  items : [Item],
  effects : [SpellEffect],
  msg : String, msgTimer : Int,
  prompt : [String],
  idleTimer : Int,
  delayTimer : Int,
  delayContinuation : Continuation,
  rnd : [Float]}

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
  name = "STABULO", level = 1, stats = initStats, hp = 9, maxhp = 9, su = 1, maxsu = 1, exp = 0, gold = 0, totalGold = 0,

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

travel : (Dungeon.Position -> Dungeon.Position) -> Transition
travel f s = {s | pos <- f s.pos}

hasEffect : String -> GameState -> Bool
hasEffect n s = s.effects |> any (\sf -> sf.name == n && sf.duration > 0)

canMoveNorth : GameState -> Bool
canMoveNorth s = s.action == WAITING && s.pos.y > 1 &&
  ((s |> hasEffect "ASTRAL WALK") || (Dungeon.roomAt s.pos).top /= Dungeon.WALL)

canMoveWest : GameState -> Bool
canMoveWest s = s.action == WAITING && s.pos.x > 1 &&
  ((s |> hasEffect "ASTRAL WALK") || (Dungeon.roomAt s.pos).left /= Dungeon.WALL)

canMoveSouth : GameState -> Bool
canMoveSouth s = s.action == WAITING && s.pos.y < 200 &&
  ((s |> hasEffect "ASTRAL WALK") || (Dungeon.roomAt (Dungeon.southOf s.pos)).top /= Dungeon.WALL)

canMoveEast : GameState -> Bool
canMoveEast s = s.action == WAITING && s.pos.x < 200 &&
  ((s |> hasEffect "ASTRAL WALK") || (Dungeon.roomAt (Dungeon.eastOf s.pos)).left /= Dungeon.WALL)

canMoveUp : GameState -> Bool
canMoveUp s = s.action == WAITING &&
  Dungeon.canClimb (Dungeon.roomAt s.pos).feature

canMoveDown : GameState -> Bool
canMoveDown s = s.action == WAITING && s.pos.z < 50 &&
  Dungeon.canDescend (Dungeon.roomAt s.pos).feature

withMessage : String -> Transition
withMessage m s = {s | msg <- m, msgTimer <- 10}

wtf : Transition
wtf s = s |> withMessage "No"

reduceSpellDurations : Transition
reduceSpellDurations s = {s | effects <- map (\sf -> {sf | duration <- max 0 (sf.duration - 1)}) s.effects}

findWithDefault : a -> (a -> Bool) -> [a] -> a
findWithDefault d p s = case s of
  [] -> d
  x :: s -> if p x then x else findWithDefault d p s

itemBonus : String -> GameState -> Int
itemBonus n s = findWithDefault {name = "", abbr = "", isAlwaysEquipped = False, isConsumable = False, bonus = 0} (\q -> q.name == n) s.items |> .bonus

regenerate : Transition
regenerate s = {s | hp <- min (s.hp + itemBonus "RING OF REGENERATION" s) s.maxhp}

resetIdleTimer : Transition
resetIdleTimer s = {s | idleTimer <- 50}

promptStairway : Bool -> Bool -> Transition
promptStairway u d s = s |> clearPrompt
  |> prompt "YOU HAVE FOUND A STAIRWAY"
  |> (if u && s.pos.z == 1 then prompt "YOU SEE `LIGHT` ABOVE" else id)
  |> prompt ("DO YOU WANT TO " ++ (if u then "GO `U`P, " else "") ++ (if d then "GO `D`OWN," else ""))
  |> prompt "OR `S`TAY ON THE SAME LEVEL?"

roll : (Float -> Transition) -> Transition
roll f s = f (head s.rnd) {s | rnd <- tail s.rnd}

checkForTraps : Transition
checkForTraps t = let s = t |> setAction WAITING in case (Dungeon.roomAt s.pos).feature of
  Dungeon.PIT        -> s |> clearPrompt
                  |> prompt "YOU SEE A PIT"
                  |> roll (\r -> if r < 0.5 then setAction (FALLING 21) `andThen` prompt "YOU FALL IN!!"
                    else prompt "YOU CAN GO `D`OWN OR TRAVEL ON")
  Dungeon.ELEVATOR   -> {s | action <- RISING 21, prompt <- ["YOU FEEL HEAVY FOR A MOMENT"]}
  Dungeon.TELEPORTER -> {s | action <- TELEPORTING 21, prompt <- ["ZZAP!! YOU'VE BEEN TELEPORTED..."]}
  Dungeon.STAIRS_UP -> s |> promptStairway True False
  Dungeon.STAIRS_BOTH -> s |> promptStairway True True
  Dungeon.STAIRS_DOWN -> s |> promptStairway False True
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
dn n r = floor (r * toFloat n + 1)

after : Int -> Transition -> Transition
after t f s = {s | delayTimer <- t, delayContinuation <- Next f}

fullHeal : Transition
fullHeal s = {s | hp <- s.maxhp}

andThen : (a -> b) -> (b -> c) -> a -> c
andThen f g = g . f

stuffToSteal : GameState -> [Item]
stuffToSteal s = filter (\q -> q.bonus > 0) s.items

hasAnythingToSteal : GameState -> Bool
hasAnythingToSteal s = any (\q -> q.bonus > 0) s.items

steal : String -> Transition
steal n s = {s | items <- s.items |> map (\q -> if q.name == n then
  {q | bonus <- if q.isConsumable then q.bonus - 1 else 0} else q)}

stealSomething : Form -> Transition
stealSomething f s = s |> roll (\r -> let
    ss = stuffToSteal s
    n = floor (r * toFloat (length ss))
    q = ss |> nth n
  in prompt ("HE STEALS YOUR " ++ q.name) `andThen` steal q.name
    `andThen` after 20 checkForTraps)

replaceItem : String -> Int -> Transition
replaceItem n b s = {s | items <- map (\q -> if q.name == n then {q | bonus <- b} else q) s.items}

roll10 : (Float -> Float -> Float -> Float -> Float -> Float -> Float -> Float ->
  Float -> Float -> Transition) -> Transition
roll10 f = roll (\ra -> roll (\rb -> roll (\rc -> roll (\rd -> roll (\re -> roll (\rf ->
  roll (\rg -> roll (\rh -> roll (\ri -> roll (\rj -> f ra rb rc rd re rf rg rh ri rj))))))))))

checkForEncounter : Transition
checkForEncounter s = s |> roll (\quiet -> roll10 (\sneakiness monsterPower monsterLevel monsterHealth undeadCourage
  sexiness possessiveness likability typeOfMagicItemsLyingAround monsterGenerosity ->
  if sneakiness < 0.3  && (not (hasEffect "INVISIBILITY" s) || quiet < 0.2) then
  let
    c = monsterClass (dn 20 monsterPower)
    l = floor ((monsterLevel ^ 1.5) * (toFloat s.pos.z * 2 + 2) + 1)
    h = floor ((monsterHealth ^ 0.5) * toFloat l * toFloat c.power + 1)
  in
    if (s |> hasEffect "FEAR") && c.power < 5 then
      checkForEncounter
    else if (s |> hasEffect "LIGHT") && c.isUndead && undeadCourage < 0.2 then
      checkForEncounter
    else
      setAction (FIGHTING {c | level <- l, hp <- h})
      `andThen` clearPrompt
       `andThen` prompt ("YOU HAVE ENCOUNTERED A LVL " ++ show l ++ " " ++ c.name)
        `andThen` (if (c.name == "ELF" && sexiness < 0.04 * toFloat s.stats.chr) || sexiness < 0.02 then
              fullHeal
              `andThen` prompt ("THE " ++ c.name ++ " LIKES YOUR BODY")
              `andThen` prompt "HE HEALS YOU TO FULL STRENGTH"
              `andThen` after 20 (checkForLoot l)
            else if (c.name == "HOBBIT" && possessiveness < 1.0 - 0.05 * toFloat s.stats.chr) || possessiveness < 0.02 then
              prompt ("THE " ++ c.name ++ " MAKES A QUICK MOVE") `andThen`
               (if hasAnythingToSteal s then
                  stealSomething c.img
                else prompt "YOU HAVE NOTHING HE WANTS TO STEAL!" `andThen` after 20 (checkForLoot l))
            else if (c.name == "DRAGON" && dn 30 likability <= s.stats.chr) || likability < 0.02 then
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
  else checkForLoot s.pos.z))

successfully : Transition
successfully = checkForEncounter . resetIdleTimer . regenerate . reduceSpellDurations

canCast : Spell -> GameState -> Bool
canCast a s = s.pos.z > 0

reduceMessageTimer : Transition
reduceMessageTimer s = if s.msgTimer == 1 then {s | msgTimer <- 0, msg <- ""}
  else if s.msgTimer > 1 then {s | msgTimer <- s.msgTimer - 1}
  else s

teleportFrom : Dungeon.Position -> Dungeon.Position
teleportFrom {x, y, z} = let
    zz = case (x + y) `mod` 4 of
      0 -> z - 1
      1 -> z + 0
      2 -> z + 1
      3 -> z + 2
    xx = x + zz * 8 + y * 13
    yy = y + zz * 6 + xx * 17
  in {x = xx `mod` Dungeon.width, y = yy `mod` Dungeon.height, z = Dungeon.dbound zz}

teleport : Transition
teleport s = let ss = {s | pos <- teleportFrom s.pos} in ss |> roll (\r -> if r < 0.2 then teleport else id)

idle : Transition
idle s = case s.action of
  WAITING       -> if s.pos.z > 0 && s.idleTimer <= 1 then resetIdleTimer s |> withMessage "Stay" |> successfully
                   else {s | idleTimer <- s.idleTimer - 1}
  FIGHTING    _ -> if s.idleTimer <= 1 then resetIdleTimer s |> withMessage "Wait"
                   else {s | idleTimer <- s.idleTimer - 1}
  FALLING     n -> if n == 0 then {s | action <- WAITING, prompt <- []} |> travel Dungeon.bottomOf |> successfully
                   else {s | action <- FALLING (n - 1)}
  RISING      n -> if n == 0 then {s | action <- WAITING, prompt <- []} |> travel Dungeon.topOf |> successfully
                   else {s | action <- RISING (n - 1)}
  TELEPORTING n -> if n == -21 then {s | action <- WAITING, prompt <- []} |> successfully
                   else s |> setAction (TELEPORTING (n - 1))
                          |> if n == 0 then setAction (TELEPORTING (n - 1)) `andThen` teleport
                             else id
  _             -> s

setAction : Action -> Transition
setAction a s = {s | action <- a}

increaseDuration : String -> Int -> Transition
increaseDuration n t s = {s | effects <-
  map (\sf -> {sf | duration <- if sf.name == n then sf.duration + t else sf.duration}) s.effects}

prompt : String -> Transition
prompt m s = {s | prompt <- s.prompt ++ [m]}

clearPrompt : Transition
clearPrompt s = {s | prompt <- []}

reduceDelayTimer : Transition
reduceDelayTimer s = if s.delayTimer > 1 then {s | delayTimer <- s.delayTimer - 1}
  else if s.delayTimer == 1 then case s.delayContinuation of
    Same -> s
    Next f -> f {s | delayTimer <- 0}
  else s

isNotDelayed : GameState -> Bool
isNotDelayed s = s.delayTimer <= 0

gainExp : Int -> Transition
gainExp x s = {s | exp <- s.exp + x}

clearEffect : String -> Transition
clearEffect n s = {s | effects <- map (\f -> if f.name == n then {f | duration <- 0}  else f) s.effects}

updateCon : Int -> Stats -> Stats
updateCon n s = {s | con <- n}

when : Transition -> Bool -> Transition
when t c = if c then t else id

checkForItem : Int -> Bool -> Transition
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

checkForLoot : Int -> Transition
checkForLoot ofLvl s = s |> clearPrompt |>
  roll10 (\attentionToDetail traptitude looseChange greed premonition piracy _ _ _ _ ->
  if attentionToDetail < 0.2 then
    let trapped = traptitude < 0.15 in
    if looseChange < 0.3 then
      let t = treasures |> nth (floor (5 * greed)) |> setLevel ofLvl in
        prompt ("YOU SEE SOME " ++ t.name)
        `andThen` prompt "PRESS <RET> TO PICK UP:"
        `andThen` (prompt "YOU DETECT TRAPS!" `when` (trapped && hasEffect "DETECT TRAPS" s && premonition < 0.9))
        `andThen` setAction (LOOTING_FREE t trapped)
    else if piracy < 0.5 then
      prompt "YOU HAVE FOUND A TREASURE CHEST!!"
      `andThen` (prompt "YOU DETECT TRAPS!" `when` (trapped && hasEffect "DETECT TRAPS" s && premonition < 0.9))
      `andThen` prompt "<RET> TO OPEN IT:"
      `andThen` setAction (LOOTING_CHEST ofLvl trapped)
    else checkForItem ofLvl trapped
  else checkForTraps)

resurrect : Transition
resurrect s = let
    s2 = {s | stats <- updateCon (s.stats.con - 1) s.stats}
         |> clearEffect "RESURRECTION" |> clearPrompt |> prompt "RESURRECTION"
  in
    s2 |> roll (\r -> if r < toFloat s2.stats.con * 0.06 then
      prompt "IT WORKS!" `andThen` fullHeal `andThen` after 20 (checkForLoot s.pos.z)
    else
      prompt "IT DOESN'T WORK!" `andThen` after 20 (setAction BEING_DEAD))

takeDamage : Int -> Transition -> Transition
takeDamage d f s = let
    h = s.hp - d
    s2 = {s | hp <- s.hp - d}
  in
    if h < 1 then
      s2 |> prompt "YOU DIED!!" `andThen`
        after 20 (if hasEffect "RESURRECTION" s then resurrect else setAction BEING_DEAD)
    else s2 |> f

roll5 : (Float -> Float -> Float -> Float -> Float -> Transition) -> Transition
roll5 f = roll (\a -> roll (\b -> roll (\c -> roll (\d -> roll (\e -> f a b c d e)))))

opponentAttacks : Transition
opponentAttacks s = case s.action of
  FIGHTING m -> s |> roll5 (\monsterScrappiness monsterFocus monsterMuscleMemory monsterThirst monsterVileness -> let
    hitRoll = floor (monsterScrappiness * 20) + m.level - itemBonus "ARMOR" s - itemBonus "SHIELD" s + m.hitBonus
      - if m.isVeryEvil && hasEffect "PROTECTION FROM EVIL" s then 6 else 0
  in
  if hitRoll < 10 then prompt "IT MISSED..." `andThen` after 15 (clearPrompt `andThen` prompt "`F`IGHT, `C`AST, OR `E`VADE:")
  else let
    damageRoll = floor ((monsterFocus * 8 + monsterMuscleMemory * toFloat m.level * 2 + 1) * toFloat m.damageMultiplier)
    drain = monsterThirst < m.drainChance
    paralyze = monsterVileness < m.paralyzeChance
  in
    prompt ("IT DOES " ++ show damageRoll ++ " POINTS DAMAGE")
    `andThen` takeDamage damageRoll (after 15 (clearPrompt `andThen` prompt "`F`IGHT, `C`AST, OR `E`VADE:")))
  _ -> s

damageOpponent : Int -> Transition
damageOpponent d s = case s.action of
  FIGHTING m -> let
    mh = m.hp - d
    exp = m.level * m.power * 10
    s2 = s |> prompt ("YOU DO " ++ show d ++ " POINTS DAMAGE")
  in
    if mh <= 0 then
      s2 |> prompt "IT DIED..."
         |> gainExp exp |> prompt ("YOU GAIN " ++ show exp ++ " EXPERIENCE POINTS")
         |> checkForLevelChange
         |> after 20 (clearPrompt `andThen` checkForLoot m.level)
    else
      s2 |> opponentAttacks
  _ -> s

fight : Transition
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

promptSameLine : String -> Transition
promptSameLine m s = let r = reverse s.prompt in {s | prompt <- case r of
  [] -> [m]
  x :: xs -> reverse ((x ++ m) :: xs)}

addGold : Int -> Transition
addGold n s = {s | gold <- s.gold + n}

lootTreasure : Treasure -> Bool -> Transition
lootTreasure t trapped s = s |> promptSameLine "SNARF IT"
  |> roll (\r -> let damageRoll = dn (3 * t.level) r in prompt "IT'S TRAPPED!"
    `andThen` prompt ("YOU SUFFER " ++ show damageRoll ++ " POINTS DAMAGE")
    `andThen` takeDamage damageRoll id) `when` trapped
  |> roll (\r -> let goldRoll = dn (t.value * t.level * 200) r in
     prompt ("IT'S WORTH " ++ show goldRoll ++ " GOLD") `andThen` addGold goldRoll)
  |> roll (\r -> if t.value == 0 && r < 0.2 then
     after 20 (clearPrompt `andThen` checkForItem t.level trapped)
     else after 20 checkForTraps)

lootChest : Int -> Bool -> Transition
lootChest lvl trapped s = s |> promptSameLine "OPEN IT"
  |> roll (\r -> let damageRoll = dn (10 * s.pos.z) r in
     prompt "CHEST EXPLODES!!!!!" `andThen` prompt ("YOU SUFFER " ++ show damageRoll ++ " POINTS DAMAGE")
     `andThen` takeDamage damageRoll id) `when` trapped
  |> roll (\r1 -> if r1 < 0.1 then
       prompt "INSIDE, THERE ARE ONLY COBWEBS..." `andThen` after 20 checkForTraps
     else roll (\r2 -> let goldRoll = dn (1000 * lvl^2) r2 in prompt ("INSIDE ARE " ++ show goldRoll ++ " GOLD PIECES!")
       `andThen` addGold goldRoll `andThen` roll (\r3 -> if r3 < 0.5 then
          after 20 (clearPrompt `andThen` checkForItem lvl False)
          else after 20 checkForTraps)))

takeItem : Item -> Transition
takeItem i s = {s | items <- map (\x -> if x.name == i.name then
    if x.isConsumable then {x | bonus <- x.bonus + 1}
    else {x | bonus <- i.bonus}
  else x) s.items}

-- NOTE : I think it is a bug from the original game where a hostile sword only does 1d5 damage no matter
-- the item level.  I suspect in the original code the "I" for the item index (which is always SWORD = 1)
-- should be a "L" for level, but I'm leaving it the same.

lootItem : Item -> Int -> Bool -> Transition
lootItem i lvl trapped = if trapped && i.name == "SWORD" then
    roll (\r -> let damageRoll = dn 5 r in
      prompt "IT'S A HOSTILE SWORD!!" `andThen` prompt ("YOU SUFFER " ++ show damageRoll ++ " DAMAGE POINTS")
      `andThen` takeDamage damageRoll (after 20 checkForTraps))
  else
    promptSameLine "IT'S YOURS!" `andThen` takeItem i `andThen` after 20
      (roll (\r -> if r < 0.5 then clearPrompt `andThen` checkForItem lvl False
        else checkForTraps))

setGold : Int -> Transition
setGold n s = {s | gold <- n}

putGoldInSafe : Transition
putGoldInSafe s = {s | gold <- 0, totalGold <- s.totalGold + s.gold}

gainLevel : Transition
gainLevel s = let lvl = s.level + 1 in {s | level <- lvl, su <- s.su + lvl, maxsu <- s.maxsu + lvl}

gainHP : Int -> Transition
gainHP n s = {s | hp <- s.hp + n, maxhp <- s.maxhp + n}

capExp : Transition
capExp s = let mx = 1000 * 2 ^ s.level in
  if s.exp >= mx then {s | exp <- mx - 1} else s

checkForLevelChange : Transition
checkForLevelChange s = s |> if s.exp >= 1000 * 2 ^ s.level then
    prompt "YOU WENT UP A LEVEL!" `andThen` gainLevel `andThen` capExp `andThen` roll (\r -> let hpRoll = dn s.stats.con r in prompt ("YOU GAIN " ++ show hpRoll ++ " HIT POINTS") `andThen` gainHP hpRoll)
  else if (s.level > 1 && s.exp < 1000 * 2 ^ (s.level - 1)) || s.exp < 0 then
    prompt "YOU GO DOWN A LEVEL!"
  else id

clearSpellEffects : Transition
clearSpellEffects s = {s | effects <- map (\ef -> {ef | duration <- 0}) s.effects}

fullRecharge : Transition
fullRecharge s = {s | su <- s.maxsu}

rest : Transition
rest s = s |> clearPrompt `andThen` setAction RESTING
  `andThen` gainExp s.gold `andThen` putGoldInSafe
  `andThen` prompt ("YOU HAVE FOUND THE " ++ innName s.pos)
  `andThen` prompt "THEY CASH IN YOUR GOLD"
  `andThen` prompt ("YOU HAVE " ++ show s.totalGold ++ " IN THE SAFE")
  `andThen` checkForLevelChange `andThen` fullHeal
  `andThen` clearSpellEffects `andThen` fullRecharge
  `andThen` prompt "YOU SPEND THE NIGHT"
  `andThen` after 20 (clearPrompt `andThen` prompt "YOU FEEL BETTER"
    `andThen` prompt "PRESS <RET> TO RETURN TO THE DUNGEON")

update : Command -> Transition
update c s = case c of
  ERROR  -> case s.action of
    LOOTING_FREE _ _ -> s |> promptSameLine "LEAVE IT" |> after 20 checkForTraps
    LOOTING_CHEST _ _ -> s |> promptSameLine "LEAVE IT" |> after 20 checkForTraps
    LOOTING_ITEM _ lvl _ -> s |> promptSameLine "LEAVE IT" |> after 20 (roll (\r -> if r < 0.5 then clearPrompt `andThen` checkForItem lvl False else checkForTraps))
    _ -> wtf s
  NORTH  -> if canMoveNorth s && isNotDelayed s
            then travel Dungeon.northOf s |> withMessage "North" |> successfully
            else wtf s
  EAST   -> if canMoveEast s && isNotDelayed s
            then travel Dungeon.eastOf s |> withMessage "East" |> successfully
            else wtf s
  SOUTH  -> if canMoveSouth s && isNotDelayed s
            then travel Dungeon.southOf s |> withMessage "South" |> successfully
            else wtf s
  WEST   -> if canMoveWest s && isNotDelayed s
            then travel Dungeon.westOf s |> withMessage "West" |> successfully
            else wtf s
  UP     -> if canMoveUp s && isNotDelayed s
            then travel Dungeon.topOf s |> (if s.pos.z == 1 then fullHeal `andThen` rest
              else successfully . withMessage "Up")
            else wtf s
  DOWN   -> if canMoveDown s && isNotDelayed s
            then travel Dungeon.bottomOf s |> withMessage "Down" |> successfully
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
    RESTING -> s |> travel Dungeon.bottomOf |> setAction WAITING |> successfully
    LOOTING_FREE t trapped -> s |> lootTreasure t trapped
    LOOTING_CHEST lvl trapped -> s |> lootChest lvl trapped
    LOOTING_ITEM i lvl trapped -> s |> lootItem i lvl trapped
    _ -> s
  TICK xs -> reduceDelayTimer (reduceMessageTimer (idle {s | rnd <- xs}))

stateSignal : Signal GameState
stateSignal = foldp update initState commandStream

-- View: Show the map in the neighborhood of the player

hero = Graphics.spriteForm "hero.png" |> Graphics.movePixels (9, -5)

fallingHero n = croppedImage (0, 0) (24 * Graphics.pixels) (n * Graphics.pixels) "hero.png"
  |> toForm |> move (9 * Graphics.pixels, -5 * Graphics.pixels)

refuse = Graphics.wideSpriteForm "refuse.png" |> Graphics.movePixels (9, 12)
silver = Graphics.wideSpriteForm "silver.png" |> Graphics.movePixels (9, 12)
gold   = Graphics.wideSpriteForm "gold.png"   |> Graphics.movePixels (9, 12)

gems   = Graphics.spriteForm "gems.png"       |> Graphics.movePixels (9, 12)
chest  = Graphics.spriteForm "chest.png"      |> Graphics.movePixels (9, 12)

gnoll    = kobold
kobold   = Graphics.spriteForm "kobold.png"   |> Graphics.movePixels (-8, 8)
skeleton = ghoul
hobbit   = dwarf
zombie   = ghoul
orc      = kobold
fighter  = dwarf
mummy    = ghoul
elf      = dwarf
troll    = Graphics.spriteForm "troll.png"    |> Graphics.movePixels (-8, 8)
wraith   = ghoul
ogre     = Graphics.spriteForm "ogre.png"     |> Graphics.movePixels (-8, 8)
minotaur = dwarf
giant    = ogre
specter  = ghoul
vampire  = ghoul
demon    = ghoul
dragon   = ghoul
dwarf    = Graphics.spriteForm "dwarf.png"    |> Graphics.movePixels (-8, 8)
ghoul    = Graphics.spriteForm "ghoul.png"    |> Graphics.movePixels (-10, 10)

elevator = Graphics.spriteForm "elevatorb.png"   |> Graphics.movePixels (  3,  -5)

drawAction : GameState -> [Form]
drawAction s = case s.action of
  FALLING n -> [fallingHero n |> moveY (toFloat ((n - 21) * Graphics.pixels) / 2)]
  RISING n -> [elevator |> moveY (toFloat ((21 - n) * Graphics.pixels)),
    hero |> moveY (toFloat ((21 - n) * Graphics.pixels))]
  TELEPORTING n -> [hero, rect 800 600 |> filled (rgba 0 0 0 ((21 - abs (toFloat n)) / 21))]
  FIGHTING m -> [m.img, hero]
  LOOTING_FREE t _ -> [t.img, hero]
  LOOTING_CHEST _ _ -> [chest, hero]
  _ -> Dungeon.featureSymbolAt s.pos ++ [hero]

adjectives = ["SALTY", "BOLD", "LOUD", "OLD", "GOODLY", "WORTHY", "LOFTY", "FINE", "ROCKY", "AGED"]

nouns = ["ROAD", "EYE", "TOOTH", "DRAGON", "MUG", "DEMON", "WHARF", "BRIDGE", "MEADE", "ALE"]

establishments = ["TAVERN", "ALEHOUSE", "CELLAR", "CLUB", "INN", "HOUSE", "INN", "LODGE", "MEADHALL", "RESTHOUSE"]

nth : Int -> [a] -> a
nth n s = drop n s |> head

lowDigit : Int -> Int
lowDigit n = n - (n `div` 10) * 10

innName : Dungeon.Position -> String
innName p = (adjectives |> nth (lowDigit (p.x * p.y))) ++ " " ++
  (nouns |> nth (lowDigit (p.x + p.y))) ++ " " ++
  (establishments |> nth (lowDigit (p.x * 3 + p.y * 7)))

drawInn : GameState -> [Form]
drawInn s = [hero]

drawGameState : GameState -> [Form]
drawGameState s = if s.pos.z == 0 then drawInn s
  else Dungeon.drawNeighborhood s.pos ++ drawAction s

textify : String -> Element
textify s = toText ("   " ++ s) |> centered

spellEffectsStr : GameState -> String
spellEffectsStr s = map (\sf -> if sf.duration > 0 then sf.abbr ++ " " else "") s.effects |> foldl (++) ""

monsterStatus : GameState -> Form
monsterStatus s = case s.action of
  FIGHTING m -> C64.printLines ["`LVL " ++ show m.level ++ " " ++ m.name] |> C64.at (23, 19)
  _ -> empty |> toForm

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
  C64.printLines ([s.name ++ " LVL " ++ show s.level,
  "STR " ++ show t.str ++ "   CON " ++ show t.con,
  "INT " ++ show t.int ++ "   DEX " ++ show t.dex,
  "WIS " ++ show t.wis ++ "   CHR " ++ show t.chr,
  "HP " ++ show s.hp ++ "/" ++ show s.maxhp,
  "SU " ++ show s.su ++ "/" ++ show s.maxsu,
  "EX " ++ show s.exp,
  "GD " ++ show s.gold] ++ concatMap maybeShowItem s.items) |> C64.at (23, 0)

showPrompt : GameState -> Form
showPrompt s = C64.printLines s.prompt |> C64.at (0, 20)

drawView : GameState -> Element
drawView s = ((collage 960 600 [drawGameState s |> group |> move (-200, 60),
  charData s,
  showPrompt s,
  monsterStatus s] |> color black) `above`
  textify (spellEffectsStr s) `above` textify s.msg)

-- Program

main : Signal Element
main = drawView <~ stateSignal
