#### Simple programming languages

![Yeah, this is bubble](/icon.png?s=200)

Created for educational purposes.
Dynamically typed imperative language with first-class functions.

Supported values:
* number (integer, double, rational)
* bool (`true` and `false`)
* `null`
* closures (all functions are first-class and support clojuring)

Basic features:
* Recursion (via `self`)
* If expression
* While loop
* Lambdas and chained calls

Some more specific examples could be found in the [examples](examples) directory.


TODO:
* rational numbers
* fix `return` inside `if` and `while` blocks
* refactor error-handling
* Write down Backus-Naur form
* optimize closures
* change self, print etc as a predetermined functions (reserved names)




Thoughts:
* print and self can have `reserved functions` type in the Value datatype `Value`


#### Some notes
* All expressions are returning values (even while loops and ifs), they are
considered as flow-control expressions and does not allowed in 
assignment, return statements etc