all: repl


fast:
	stack build --fast --force-dirty
	cp .stack-work/dist/x86_64-linux-tinfo6-nopie/Cabal-2.0.1.0/build/bauble-exe/bauble-exe ./

build:
	stack build --force-dirty
	cp .stack-work/dist/x86_64-linux-tinfo6-nopie/Cabal-2.0.1.0/build/bauble-exe/bauble-exe ./

file:
	stack exec bauble-exe $(path)

repl:
	stack exec bauble-exe repl

ast:
	stack exec bauble-exe ast

file-ast:
	stack exec bauble-exe ast $(path)

tests:
	stack test --fast