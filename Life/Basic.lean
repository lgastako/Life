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

def renderCell (board : Board) (onChar offChar : String) (y x : Int) : String :=
  match board[(x, y)]? with
  | some true  => onChar
  | some false => offChar
  | none       => offChar

def intRange (n : Int) : List Int :=
  (List.range ∘ Int.toNat $ n).map Int.ofNat

def renderLine (width : Int) (board : Board) (onChar offChar : String) (y : Int) : String :=
  String.join [renderCell board onChar offChar y x | for x in intRange width] ++ "\n"

def renderBoard (height width : Int) (board : Board) (onChar offChar : String) : String :=
  String.join [renderLine width board onChar offChar y | for y in intRange height]

def drawBoard (height width : Int) (board : Board) (onChar offChar : String) : IO Unit :=
   IO.println (renderBoard height width board onChar offChar)

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

partial def go (delay : UInt32) (height width : Int) (board : Board) (onChar offChar : String) : IO Unit := do
  clearScreen
  drawBoard height width board onChar offChar
  IO.sleep delay
  go delay height width (step height width board) onChar offChar

def getPatternByName (name : String) : Option (List (Int × Int)) :=
  match name with
  | "glider" => some glider
  | "blinker" => some blinker
  | "toad" => some toad
  | "beacon" => some beacon
  | "diehard" => some diehard
  | "acorn" => some acorn
  | "rPentomino" => some rPentomino
  | "pulsar" => some pulsar
  | "gosperGliderGun" => some gosperGliderGun
  | "pentadecathlonSeed" => some pentadecathlonSeed
  | "queenBeeShuttle" => some queenBeeShuttle
  | _ => none

structure Config where
  pattern : String := "rPentomino"
  delay : UInt32 := 25
  onChar : String := "□"
  offChar : String := " "

def printHelp : IO Unit := do
  IO.println "Conway's Game of Life"
  IO.println ""
  IO.println "Usage: Life [OPTIONS]"
  IO.println ""
  IO.println "Options:"
  IO.println "  --help              Show this help message and exit"
  IO.println "  --pattern PATTERN   Set the initial pattern (default: rPentomino)"
  IO.println "  --delay MILLISECS   Set delay between generations in milliseconds (default: 25)"
  IO.println "  --on CHAR           Character for live cells (default: □)"
  IO.println "  --off CHAR          Character for dead cells (default: space)"
  IO.println ""
  IO.println "Available patterns:"
  IO.println "  glider, blinker, toad, beacon, diehard, acorn, rPentomino,"
  IO.println "  pulsar, gosperGliderGun, pentadecathlonSeed, queenBeeShuttle"

partial def parseArgs (args : List String) (config : Config) : IO (Option Config) :=
  match args with
  | [] => pure (some config)
  | "--help" :: _ => do printHelp; pure none
  | "--pattern" :: pattern :: rest => 
    if getPatternByName pattern |>.isSome then
      parseArgs rest { config with pattern := pattern }
    else do
      IO.println s!"Error: Unknown pattern '{pattern}'"
      printHelp
      pure none
  | "--delay" :: delayStr :: rest =>
    match delayStr.toNat? with
    | some delay => parseArgs rest { config with delay := UInt32.ofNat delay }
    | none => do
      IO.println s!"Error: Invalid delay value '{delayStr}'"
      printHelp
      pure none
  | "--on" :: char :: rest => parseArgs rest { config with onChar := char }
  | "--off" :: char :: rest => parseArgs rest { config with offChar := char }
  | unknown :: _ => do
    IO.println s!"Error: Unknown option '{unknown}'"
    printHelp
    pure none

def main (args : List String) := do
  match ← parseArgs args {} with
  | none => pure ()
  | some config => do
    let pattern := getPatternByName config.pattern |>.getD rPentomino
    let (height, width) ← getTerminalSize
    let board : Board := boardFromPattern pattern
    go config.delay height width board config.onChar config.offChar
