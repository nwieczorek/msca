# Introduction to msca

MSCA is a 2-dimensional cellular automata where each cell can be in (3 * 255) states -- the state of the cell is directly translated into a 3-bit color in the display.

## Rules

The rule for the state value of a new cell is defined in four components in the rule file:

neighborhood-distance:  This determines the number of cells that will be examined to determine the new value.  

rule-red:
rule-green:
rule-blue:
Each of these rules should define an expression that will be used to create a function function taking 3 arguments [r g b], where each argument is a vector of the color components of the neighbor cells. The value returned is the new value of that color component of the new cell.

