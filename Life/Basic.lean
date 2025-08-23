-- Conway's Game of Life

import Std.Data.HashMap

import Life.ListComp

open Std

abbrev Position := Int × Int
abbrev Value := Bool
abbrev Board := HashMap Position Value

-- for now assume the terminal is 80 x 25
def maxY := 25
def maxX := 80

def clearScreen : IO Unit :=
  IO.print "\u001b[2J\u001b[H"

@[inline] def wrap (a max : Int) : Int :=
  ((a % max) + max) % max

def glider : List (Int × Int) :=
  [ (0, 1)
  , (1, 2)
  , (2, 0)
  , (2, 1)
  , (2, 2)
  ]

def blinker : List (Int × Int) :=
  [ (1, 0)
  , (1, 1)
  , (1, 2)
  ]

def toad : List (Int × Int) :=
  [ (1, 1), (2, 1), (3, 1)
  , (0, 2), (1, 2), (2, 2)
  ]

def beacon : List (Int × Int) :=
  [ (0, 0), (1, 0)
  , (0, 1), (1, 1)
  , (2, 2), (3, 2)
  , (2, 3), (3, 3)
  ]

def diehard : List (Int × Int) :=
  [ (6, 0)
  , (0, 1), (1, 1)
  , (1, 2), (5, 2), (6, 2), (7, 2)
  ]

def renderCell (board : Board) (y x : Int) : String :=
  match board[(x, y)]? with
     | some true  => "*"
     | some false => " "
     | none       => " "

def intRange (n : Nat) : List Int :=
  (List.range n).map Int.ofNat

def renderLine (board : Board) (y : Int) : String :=
  String.join [renderCell board y x | for x in intRange maxX] ++ "\n"

def renderBoard (board : Board) : String :=
  String.join [renderLine board y | for y in intRange maxY]

def drawBoard := IO.println ∘ renderBoard

def neighbors (x y : Int) : List Position :=
  let raw : List Position :=
    [ (x - 1, y - 1) -- up, left
    , (x,     y - 1) -- up
    , (x + 1, y - 1) -- up, right
    , (x - 1, y)     -- same, left
    , (x + 1, y)     -- same, right
    , (x - 1, y + 1) -- down, left
    , (x,     y + 1) -- down
    , (x + 1, y + 1) -- down right
    ]
  raw.map (λ (a, b) => (wrap a maxX, wrap b maxY))

def neighborCount (board : Board) (x y : Int) : Int :=
  List.length ∘ List.filter id $
    (neighbors x y).map (λ (p : Position) => (board[p]?).getD false)

/--
Any live cell with fewer than two live neighbours dies, as if by underpopulation.
Any live cell with two or three live neighbours lives on to the next generation.
Any live cell with more than three live neighbours dies, as if by overpopulation.
Any dead cell with exactly three live neighbours becomes a live cell, as if by reproduction.
-/
def nextVal (board : Board) (x y : Int) : Bool :=
  match neighborCount board x y with
  | 0 => false
  | 1 => false
  | 2 => (board[(x, y)]?).getD False
  | 3 => True
  | _ => False

def step (board : Board) : Board := HashMap.ofList newVals
  where
    newVals : List (Position × Value) :=
      [ ((x, y), nextVal board x y)
      | for x in intRange maxX
      , for y in intRange maxY
      ]

partial def go (board : Board) : IO Unit := do
  clearScreen
  drawBoard board
  IO.sleep (30 : UInt32)
  go (step board)

def main := do
  let pattern := glider
  let board : Board := (HashMap.ofList ∘ truify) pattern
  go board
  where
    truify : List (Int × Int) -> List ((Int × Int) × Bool) :=
      List.map (λ x => (x, true))
