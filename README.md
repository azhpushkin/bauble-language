#### Simple programming languages

![Yeah, this is bauble](/icon.png?s=200)

Created for educational purposes.
Dynamically typed imperative language with first-class functions.

Supported values:
* number (integer, double, rational)
* bool (`true` and `false`)
* `null` (NOTE: no way to check if value is null now)

Basic features:
* If expressions
* While expressions
* Simple bool and numbers operators
* Double and rational numbers
* First-class functions (lambda support)

Some more specific examples could be found in the [examples](examples) directory.


TODO:
* Write down Backus-Naur form
  (who cares? but could be great for course-work documentation)
* optimize closures (PRIORITY)
* Write down details of Syntax analisys and evaluation rules
* Error-handling of evaluation
* executable options
* try to optimize detection of chained calls and subscriptions




### Imports behaviour (TODO: finish this)
Executable have option `--libs-path` that contains
list of paths to look for modules to import
By default, there are following paths in there:
* `.`  (executable directory)
* `./lib`  (default modules path) 

This could be overwritten or changed by two commands:
* `--add-libs-path <path>` - new path to look for libraries, 
could be applied several times  
Example:
```
bauble-exe --add-libs-path ../mymodules
           --add-libs-path /home/friend/libs 
```
* `--libs-path` - overrides paths 




https://hackage.haskell.org/package/optparse-applicative