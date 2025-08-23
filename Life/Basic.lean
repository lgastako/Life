-- Conway's Game of Life

-- import Lean.Data.RBMap

import Std.Data.HashMap

import Life.ListComp

open Std

abbrev Position := Nat × Nat
abbrev Value := Bool
abbrev Board := HashMap Position Value

-- for now assume the terminal is 80 x 25
def maxY := 25
def maxX := 80

def clearScreen : IO Unit :=
  IO.print "\u001b[2J\u001b[H"

def glider : List (Nat × Nat) :=
  [ (0, 1)
  , (1, 2)
  , (2, 0)
  , (2, 1)
  , (2, 2)
  ]

-- #eval List.flatten [[1, 2], [3], [4, 5, 6]]
def renderCell (board : Board) (y : Nat) (x : Nat): String :=
  match board[(x, y)]? with
     | some true  => "*"
     | some false => " "
     | none       => " "

def renderLine (board : Board) (y : Nat) : String :=
  String.join [renderCell board y x | for x in List.range maxX] ++ "\n"

def renderBoard (board : Board) : String :=
  -- for now assume the terminal is 80 x 25
  -- String.join $ (List.range maxY).map (λ y => renderLine board y)
  String.join [renderLine board y | for y in List.range maxY]

def drawBoard := IO.println ∘ renderBoard

def neighbors (x : Nat) (y : Nat) : List Position :=
  [ (x - 1, y - 1) -- up, left
  , (x,     y - 1) -- up
  , (x + 1, y - 1) -- up, right
  , (x - 1, y)     -- same, left
  , (x + 1, y)     -- same, right
  , (x - 1, y + 1) -- down, left
  , (x,     y + 1) -- down
  , (x + 1, y + 1) -- down right
  ]

def neighborCount (board : Board) (x : Nat) (y : Nat) : Nat :=
  List.length ∘ List.filter id $
    (neighbors x y).map (λ (p : Position) => (board[p]?).getD false)

def nextVal (board : Board) (x : Nat) (y : Nat) : Bool :=
  match neighborCount board x y with
  | 0 => false
  | 1 => false
  | 2 => (board[(x, y)]?).getD False
  | 3 => True
  | _ => False
  -- Any live cell with fewer than two live neighbours dies, as if by underpopulation.
  -- Any live cell with two or three live neighbours lives on to the next generation.
  -- Any live cell with more than three live neighbours dies, as if by overpopulation.
  -- Any dead cell with exactly three live neighbours becomes a live cell, as if by reproduction.

def step (board : Board) : Board := HashMap.ofList newVals
  where
    newVals : List (Position × Value) :=
      [ ((x, y), nextVal board x y)
      | for x in List.range maxX
      , for y in List.range maxY
      ]

partial def go (board : Board) : IO Unit := do
  drawBoard board
  IO.sleep (1 : UInt32)
  go (step board)

def main := do
  let trues : List ((Nat × Nat) × Bool) := truify glider
  let board : Board := HashMap.ofList trues
  go board
  where
    truify : List (Nat × Nat) -> List ((Nat × Nat) × Bool) :=
      List.map (λ x => (x, true))

def exampleClear := do
  IO.print "hello "
  clearScreen
  IO.println "world"
