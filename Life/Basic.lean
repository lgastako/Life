-- Conway's Game of Life

import Std.Data.HashMap

import Life.ListComp
import Life.Patterns
import Life.Term

open Std

abbrev Position := Int × Int
abbrev Value := Bool
abbrev Board := HashMap Position Value

def clearScreen : IO Unit :=
  IO.print "\u001b[2J\u001b[H"

@[inline] def wrap (a max : Int) : Int :=
  ((a % max) + max) % max

def renderCell (board : Board) (y x : Int) : String :=
  match board[(x, y)]? with
  | some true  => on
  | some false => off
  | none       => off
  where
    on  := "□"
    off := " "

def intRange (n : Int) : List Int :=
  (List.range ∘ Int.toNat $ n).map Int.ofNat

def renderLine (width : Int) (board : Board) (y : Int) : String :=
  String.join [renderCell board y x | for x in intRange width] ++ "\n"

def renderBoard (height width : Int) (board : Board) : String :=
  String.join [renderLine width board y | for y in intRange height]

def drawBoard (height width : Int) (board : Board) : IO Unit :=
   IO.println (renderBoard height width board)

def neighbors (height width x y : Int) : List Position :=
  [ (x - 1, y - 1)  --  ← ↑
  , (x,     y - 1)  --    ↑
  , (x + 1, y - 1)  --  → ↑
  , (x - 1, y)      --  ←
  , (x + 1, y)      --  →
  , (x - 1, y + 1)  --  ← ↓
  , (x,     y + 1)  --    ↓
  , (x + 1, y + 1)  --  → ↓
  ].map (λ (a, b) => (wrap a width, wrap b height))

def neighborCount (height width : Int) (board : Board) (x y : Int) : Int :=
  (neighbors height width x y).foldl
    (λ acc p => acc + cond ((board[p]?).getD false) 1 0)
    0

/--
Any live cell with fewer than two live neighbours dies, as if by underpopulation.
Any live cell with two or three live neighbours lives on to the next generation.
Any live cell with more than three live neighbours dies, as if by overpopulation.
Any dead cell with exactly three live neighbours becomes a live cell, as if by reproduction.
-/
def rule (height width : Int) (board : Board) (x y : Int) : Bool :=
  let n := neighborCount height width board x y
  let alive := (board[(x, y)]?).getD false
  (n == 3) || ((n == 2) && alive)

def step (height width : Int) (board : Board) : Board := HashMap.ofList newVals
  where
    evolve : Int → Int → Bool := rule height width board
    newVals : List (Position × Value) :=
      [ ( (x, y)
        , evolve x y
        )
      | for x in intRange width
      , for y in intRange height
      ]

def boardFromPattern : List (Int × Int) -> Board :=
  HashMap.ofList ∘ List.map (λ x => (x, true))

partial def go (delay : UInt32) (height width : Int) (board : Board) : IO Unit := do
  clearScreen
  drawBoard height width board
  IO.sleep delay
  go delay height width (step height width board)

def main := do
  let (height, width) ← getTerminalSize
  let board : Board := boardFromPattern pattern
  go delay height width board
  where
    pattern := rPentomino
    delay   := 25
