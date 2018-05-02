all: repl


fast:
	stack build --fast

build:
	stack build

file:
	stack build --fast && stack exec bauble-exe $(path)

repl:
	stack build --fast && stack exec bauble-exe