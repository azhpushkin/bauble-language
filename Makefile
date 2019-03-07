all: repl


fast:
	stack build --fast

build:
	stack build --force-dirty

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