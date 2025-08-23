-- Conway's Game of Life

-- import Lean.Data.RBMap

import Std.Data.HashMap

import Life.ListComp

open Std

abbrev Position := Nat × Nat
abbrev Value := Bool
abbrev Board := HashMap Position Value

def clearScreen : IO Unit :=
  IO.print "\u001b[2J\u001b[H"

def glider : List (Nat × Nat) :=
  [ (0, 1)
  , (1, 2)
  , (2, 0)
  , (2, 1)
  , (2, 2)
  ]

def truify : List (Nat × Nat) -> List ((Nat × Nat) × Bool) :=
  List.map (λ x => (x, true))

-- def renderBoard (_board : Board) : String :=
--   -- Figure out what the bounds are
--   -- Map the bounds to the screen coords
--   -- render as string
--   "not impl"

-- #eval List.flatten [[1, 2], [3], [4, 5, 6]]
def renderCell (board : Board) (y : Nat) (x : Nat): String :=
  match board[(x, y)]? with
     | some true  => "*"
     | some false => " "
     | none       => " "

def renderLine (board : Board) (y : Nat) : String :=
  -- for now assume the terminal is 80 x 25
  let maxX := 80
  String.join [renderCell board y x | for x in List.range maxX] ++ "\n"

def renderBoard (board : Board) : String :=
  -- for now assume the terminal is 80 x 25
  let maxY := 25
  -- String.join $ (List.range maxY).map (λ y => renderLine board y)
  String.join [renderLine board y | for y in List.range maxY]

def drawBoard : Board -> IO Unit :=
  IO.println ∘ renderBoard

def main := do
  let trues : List ((Nat × Nat) × Bool) := truify glider
  let board : Board := HashMap.ofList trues
  drawBoard board

def exampleClear := do
  IO.print "hello "
  clearScreen
  IO.println "world"
