# Rustcalc 3
_a command-line RPN calculator_

## Usage
This calculator is RPN, meaning operators are placed after, not between, operands.  For instance, 2\*3 would be written as `2 3*`, and (1+2)/(3\*4) would be `1 2+ 3 4*/`<br>
Ctrl+d can be used to exit

### Syntax
| Syntax | Behavior | Example |
|--------|----------|---------|
| `<number>` | Push to stack | `1` -> 1 is on the stack |
| `+` | Add last two stack items | `1 2+` -> 3 |
| `-` | Subtract last two stack items | `1 2-` -> -1 |
| `*` | Multiply last two stack items | `1 2*` -> 2 |
| `/` | Divide last two stack items | `1 2/` -> 0.5 |
| `^` | Wxponentiate last two stack items | `1 2^` -> 1 |
| `'<infix>'` | Infix notation | `'(1+2)/(3*4)'` -> 0.25 |
| `<decimal>e<integer>` | The decimal part times 10 to the power of the integer | `123.456e3` -> `123456` |
| `(<number>, <number>)`, `(<number> + <number>i)`, `<number> + <number>i`, `<number>i` | Complex number | `(2, 5)` -> 2 + 5i |
| ```<number>`<unit>` ``` | Unit | ```5`m/s` ``` -> 5 meter/second |
| ```to`<unit>` ``` | Convert units | ```5`m/s` to`in/hour` ``` -> 708661.(...) inch/hour |
| ``` `<unit>` ``` | Set units | ```5`m/s` `in/hour` ``` -> 5 inch/hour |
| `"comment"` | Comment | `"this is a comment"` |
| `=<var name>` | Set variable | `5 =aaa` -> 'aaa' is now equal to 5 |
| `<var name>` | Get variable | `aaa` -> 5 |
| `<function name>` | Invoke function | `pi cos` -> -1 |
| `r` | Remove | `rrr` -> removes the last 3 items from the stack |
| `d` | Duplicate | `5 d` -> adds 5 to the stack, then adds it again |
| `s` | Swap | `5 6 s` -> `6 5` on the stack |
| `pretty` | Pretty-print | `123e54 pretty` -> 123 septendecillion |
| `clear` | Clear stack | `1 2 3 4 5 clear` -> empty stack |
| `&#124;<unitname>&#124;` | Define unit | `&#124;person&#124;` -> you can now do `1\`person\`` |
| `&#124;<unitname> <unitvalue>&#124;` | Define unit | `&#124;town 100\`people\` * 10&#124;` |
| `[<dice>]` | Roll dice | `[1d8 + 2d10]` |
| `[P(<dice>)]` | Calculate dice probability distribution | `[P(1d8)]` shows a histogram and expected value for a d8 |

### Supported functions
sin, cos, tan, cot, asin, acos, atan, atan2 (coming soon), acot (coming soon), sqrt, ln, log10, log2, logb

## Building
`cargo build --release`

## Running
Once it's built, the executable will be at `target/release/rustcalc-4`<br>
Note that it looks for the `units.txt` file in the current directory