-- Basic Graphics Utilities

module Graphics where

pixels = 3
chars = 8 * pixels

spriteForm : String -> Form
spriteForm s = s |> fittedImage (24 * pixels) (21 * pixels) |> toForm

wideSpriteForm : String -> Form
wideSpriteForm s = s |> fittedImage (48 * pixels) (21 * pixels) |> toForm

charForm : String -> Form
charForm s = s |> fittedImage (1 * chars) (1 * chars) |> toForm

movePixels : (Int, Int) -> Form -> Form
movePixels (x, y) = move (toFloat x * pixels, toFloat y * pixels)
