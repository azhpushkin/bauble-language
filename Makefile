all: repl


fast:
	stack build --fast
	cp .stack-work/dist/x86_64-linux-tinfo6-nopie/Cabal-2.0.1.0/build/bauble-exe/bauble-exe ./

build:
	stack build
	cp .stack-work/dist/x86_64-linux-tinfo6-nopie/Cabal-2.0.1.0/build/bauble-exe/bauble-exe ./

file:
	stack build --fast && stack exec bauble-exe $(path)

repl:
	stack build --fast && stack exec bauble-exe repl

ast:
	stack build --fast && stack exec bauble-exe ast

tests:
	stack test --fast