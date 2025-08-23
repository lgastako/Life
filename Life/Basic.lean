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

def renderLine (maxX : Int) (board : Board) (y : Int) : String :=
  String.join [renderCell board y x | for x in intRange maxX] ++ "\n"

def renderBoard (maxY maxX : Int) (board : Board) : String :=
  String.join [renderLine maxX board y | for y in intRange maxY]

def drawBoard (maxY maxX : Int) (board : Board) : IO Unit :=
   IO.println (renderBoard maxY maxX board)

def neighbors (maxY maxX x y : Int) : List Position :=
  [ (x - 1, y - 1)  --  ← ↑
  , (x,     y - 1)  --    ↑
  , (x + 1, y - 1)  --  → ↑
  , (x - 1, y)      --  ←
  , (x + 1, y)      --  →
  , (x - 1, y + 1)  --  ← ↓
  , (x,     y + 1)  --    ↓
  , (x + 1, y + 1)  --  → ↓
  ].map (λ (a, b) => (wrap a maxX, wrap b maxY))

def neighborCount (maxY maxX : Int) (board : Board) (x y : Int) : Int :=
  (neighbors maxY maxX x y).foldl
    (λ acc p => acc + cond ((board[p]?).getD false) 1 0)
    0

-- def neighborCount (maxY maxX : Int) (board : Board) (x y : Int) : Int :=
--   List.length ∘ List.filter id $
--     (neighbors maxY maxX x y).map (λ (p : Position) => (board[p]?).getD false)

/--
Any live cell with fewer than two live neighbours dies, as if by underpopulation.
Any live cell with two or three live neighbours lives on to the next generation.
Any live cell with more than three live neighbours dies, as if by overpopulation.
Any dead cell with exactly three live neighbours becomes a live cell, as if by reproduction.
-/
def rule (maxY maxX : Int) (board : Board) (x y : Int) : Bool :=
  let n := neighborCount maxY maxX board x y
  let alive := (board[(x, y)]?).getD false
  (n == 3) || ((n == 2) && alive)

def step (maxY maxX : Int) (board : Board) : Board := HashMap.ofList newVals
  where
    evolve : Int → Int → Bool := rule maxY maxX board
    newVals : List (Position × Value) :=
      [ ( (x, y)
        , evolve x y
        )
      | for x in intRange maxX
      , for y in intRange maxY
      ]

partial def go (delay : UInt32) (maxY maxX : Int) (board : Board) : IO Unit := do
  clearScreen
  drawBoard maxY maxX board
  IO.sleep delay
  go delay maxY maxX (step maxY maxX board)

def main := do
  let (maxY, maxX) ← getTerminalSize
  let board : Board := (HashMap.ofList ∘ List.map (λ x => (x, true))) pattern
  go delay maxY maxX board
  where
    pattern := rPentomino
    delay   := 25
