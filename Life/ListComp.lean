declare_syntax_cat compClause
syntax "for " term " in " term : compClause
syntax "if " term : compClause

syntax "[" term " | " compClause,* "]" : term

macro_rules
    | `([$t:term |]) => `([$t])
    | `([$t:term | for $x in $xs]) => `(List.map (λ $x => $t) $xs)
    | `([$t:term | if $x]) => `(if $x then [$t] else [])
    | `([$t:term | $c, $cs,*]) => `(List.flatten [[$t | $cs,*] | $c])

#eval [x+1| for x in [1,2,3]]
-- [2, 3, 4]
#eval [4 | if 1 < 0]
-- []
#eval [4 | if 1 < 3]
-- [4]
#eval [(x, y) | for x in List.range 5, for y in List.range 5, if x + y <= 3]
-- [(0, 0), (0, 1), (0, 2), (0, 3), (1, 0), (1, 1), (1, 2), (2, 0), (2, 1), (3, 0)]
#eval [(x, y) | for x in List.range 5, for y in List.range 5, if x + y <= 3 && x != y]
-- [(0, 1), (0, 2), (0, 3), (1, 0), (1, 2), (2, 0), (2, 1), (3, 0)]
