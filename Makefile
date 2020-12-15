
all:
	@dune build @all

clean:
	@dune clean

watch:
	@dune build @all -w
