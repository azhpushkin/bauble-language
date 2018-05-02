1;  // Integer
1 / 2;  // Rational

4.0;  // Irrational

foo = 100;  // Assign
{foo = 200; bar = 300;}  // Block, shadowing and destroying

display(123);  // Function call and 
// display(bar);  // This is not allowed because bar is destroyed over here

function() {return 123;}  // Function without env
function() {return foo;}  // Closure
foo = function() return foo;  // Clojure and assigning, foo now function

null;  // Null value

TODO:
* nonlocal keyword;
* calls like x() () should be allowed, or not?
* optional 'else' block
* continue and break