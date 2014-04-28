-- An overly simple emulated Commodore 64 display

module C64 (printLine, printLines, at) where

charAt : (Int, Int) -> Element
charAt (x, y) = croppedImage (x * 24, y * 24) 24 24 "charset.png"

char : Int -> Char -> Element
char off c = case c of
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

printLine : String -> Form
printLine s = let
    loop s = foldl (\c (h, s) ->
      if c == '`' then (not h, s)
      else (h, s `beside` char (if h then 4 else 0) c))
      (False, empty) (String.toList s)
    (_, fs) = loop s
  in
    fs |> toForm |> moveX ((toFloat (widthOf fs) - 40 * 24) / 2)

printLines : [String] -> Form
printLines ss = let len = length ss in
  zip ss [0 .. len] |> map (\(str, line) -> printLine str |> moveY (12 * 24 - 24 * toFloat line)) |> group

at : (Int, Int) -> Form -> Form
at (x, y) = move (24 * toFloat x, -24 * toFloat y)
