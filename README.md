![Yeah, this is bauble](/icon.png?s=200)
  
Dynamically typed imperative language with first-class functions.
**Created for educational purposes.**

## Brief overview

Implemented in haskell.    
`Makefile` is present for running and some dev shortcuts.
Building process is similar to usual haskell build process ()shortly described later)  
  
Basic features:
* supports simple types (numbers, strings, bools, arrays, null)
* some basic operators (`+`, `-`, `/`, `*`)
* builtin functions `print`, `isnull`, `length` 
* function as first class types as closures, with recursion support
* `if` and `while` control expressions
* possibility to run in AST-mode (code is not evaluated, just AST parsing -> for debug purposes)

Some examples could be found in the [examples](examples) directory.


## Building process and usage

> NOTE: no guarantee this will work out of the box, some googling around
> 'how to build haskell' could be required

1) Install [Stack](https://www.haskellstack.org/).
2) Run `stack build --fast` to check that build is working fine
3) Use `Makefile` commands to launch code

Makefile commands:
* `make repl` - starts REPL
* `make ast` - starts REPL with AST mode (no execution, just AST debug)
* `make file path=examples/curry.bbl` - executes file from given path
* `make file-ast path=examples/curry.bbl` - prints AST of given file
* `make tests` - launches tests
* `make fast` and `make build` - builds commands


## Language overview

### Supported types

**Numbers** are represented with 3 possible types: `integer`, `rational` and `double`
with support of `+, -, /, *`  operators between all of them. Integers and double
are parsed before evaluation, rationals are result of dividing coprime integers.  
There are basic coercion rule: `integers -> rational -> double`, and there is no way
for reverse convertion (hell yeah).
```
1, 2, 0, -3  // integers
1.1, 1.0, -1.1  // double

1 / 2  // coerced to rational during execution
1.1 / 2  // coerced to double during execution
(1/2) / 1.1  // coerced to double during execution  
```

**Strings, bools and null** are pretty straightforward:
```
"asdasd"  // This is string
true, false  // This is bools
null  // This is null
```

**Functions** are also a type, that can be passed as argument or assigned to variable.
As function remembers environment while it was created, all functions are closures.
(printing function in REPL clearly says so).
`return` is used to return value (did you expected something else?). 

Syntax for function creation is simple enough:
```
function() {}  // Simples function possible
print(function() {});  // prints "<Closure with args []>"

function(a, b) {return a+b;}  // Function with arguments
function(a, b) return a+b;  // ERROR, curly braces are required

function self(a, b) {return self;}  // Name is optinal and is used for recursion only
```
IMPORTANT: function name is used ONLY for recursion and does not assigns it to name
```
function hi() {return "Hi!";};
hi();  // ERROR -> variable hi is not defined

hi = function() {return "Hi!";};
hi();  // THIS is 100% correct
```
Function call syntax has nothing uncommon too.
Lambda calls are supported (but in this case parentheses should be present).
```
hi(1, 2, 1+2);  // function call
(function() {return 1;})()  // lambda call
function() {return 1;} ()  // ERROR, parentheses required for lambda call
```

There is **special type for builtin function**, there are 3 of them now:
`print`(takes n arguments and prints them),
`isnull` (takes 1 argument, returns bool)
and `length` (takes 1 argument, returns int, used for arrays only).


**Arrays** are defined via `[ ]` syntax and could contain any items.
Subscript operand is done via `[]` too. 
```
[1, 2/3, print, function() {}, null]  // This is legal
array[1+2]  // Subscript
[1, 2, 3][0]  // Array creation and subscript
[1, 2, 3] + [4, 5, 6]  // adding of arrays is supported
```  

### Control flow and other statements

There is some more well-known statements present:

**Assignment** - nothing unusual here
```
x = 1;
a = function() {};
```

**If expression**  
Conditional expression MUST evaluate to boolean.
Else block is optional. Body could be either single expression or block
(in this case braces are used)
```
if (true) a = 1;
if (true) a = 1; else a = 2;
if (true) {a = 1; b=2;} else a = 2;
```

**While loop**  
Conditional expression MUST evaluate to boolean.
Body could be either single expression or block (in this case braces are used)  
`continue` and `break` are present
```
while(true) print("You spin me right round, baby...");

while(true) {
  print("What is love");
  print("Baby dont hurt me");
}

while (something()) {
  if (true)
    break;
  else
    continue;
  print("Not a chance!")
}  
```

### These tricky semicolons

Rules for semicolons: all statements must end with semicolon,
but ones that ends with block (`function`, `if`, `while`) don't.
(However, semicolons still must be present in the blocks and after assignments).

**Examples:**
```
function() {}  // Ending with block - no colons

hi = function () {};  // Ending with block, but this is assignment, so semicolon there


while (true) a = 1;  // Semicolon required
while (true) {a=1;}  // Semicolon required in block, but not after block
if (true) a=1; else {b=2;}  // Same here 
```  

Follow to [examples](examples) folder for move examples of usage.


## How does it work under the hood

Running the program consists of two steps:
1) Parsing syntax to lexemes and AST code
2) Executing produces AST code

### How AST is generated

Haskell [Parsec combinators](http://hackage.haskell.org/package/parsec) library is used.
Given lexemes rules ([Lexer.hs](src/Lexer.hs)), library build simple lexemes parser, that
is used later to describe how syntax looks. Syntactic constructions are given in [Syntax.hs](src/Syntax.hs) file.

Rules for syntax are given in [Parser.hs](src/Parser.hs) and is also used to validate some
more complex syntax rules. For example, parsec combinator `flowExpr` parses syntax depending
on current state of parser and ensures `continue/break` are used only inside of `while`
(and `return` is used inside function definition only).

There is important distinction between `Expression` and `Statement` - expression could be
evaluated to some result (i.e. operator or function call), and statement does not return
anything and could be only **executed** (i.e. `while, if, return`)

Some high-level overview of combinators:
* `atomicExpr`  
parses expressions that could not be split anymore: numbers, strings, bool etc.
* `operatorExpr`  
uses library's `buildExpressionParser` function to generate parser of operators for
given table of operators (with associativity rules and priority)
* `withNext`  
allows chained calls and subscripts (i.e. `array[1][2](3, 4)(2)[2]`)
* `expression`  
parses expressions - statements that could be evaluated to some concrete value.
Contains values and function definitions, calls, operators, subscripts and assignments
(assignments return value of assignment to allow `a = b = 0` form)
* `flowExpr`  
parses statements depending on current position: whether inside of loop or
function definition
* `statement`  
Most general one - parses all statements and expressions of the language
* `ensureSemi`  
Ensures semicolons are present or not present depending on statements
* `contents`  
Handles whitespaces and newlines and parses without dependency on them  
* `parseToplevel`  
Finally, last one - retrieves string and parses it with `contents` and `statement` combinators

As a result, this parsing produces list of AST **STATEMENTS** that are executed later.
### How AST is executed

This one is split up to some steps too.  
Most of the logic about this is contained in [Evaluation.hs](src/Evaluation.hs) file.
In consists of two main functions:

**`runExpressions`**  
Executes expressions. Manages current execution context (loop / function call) and
environment (defined in [Environment.hs](src/Environment.hs)).  
Works in very simple way:
* `if` blocks - check condition and add `if` or `else` statements to list of ones
  to execute
* `while` - check conditions and either drop loop statements (if condition is false)
or launch another `runExpressions` and uses it's result env for future work
* for a function call, just new `runExpressions` is started and its result is used
* assignment just updates current flow
* `return`, `continue` and `break` work correct because `runExpression` stores current
call or loop so these statements are aware of execution context
* other statements are quite straightforward

**`evalExpression`**

This one is very easy. Either evaluates function call (by calling `runExpressions`), 
calculates operation based on [Operators.hs](src/Operators.hs) operators rules or
performs array creation - lookup. Builtit functions results and behavior are described in
[Builtins.hs](src/Builtins.hs).

### Final step

[Lib.hs](src/Lib.hs) defines two high-level functions for parsing and executing AST, and
[Main.hs](app/Main.hs) defines handling of commands (`repl`, `file-ast` etc) via Lib.hs functions.
