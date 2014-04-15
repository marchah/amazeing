RESULT     = step4
SOURCES    =	\
		Functor.ml	\
		Aff.ml		\
		main.ml
LIBS       = bigarray sdl sdlloader
INCDIRS    = +sdl

include OCamlMakefile

all: byte-code

$(RESULT): all

fclean: clean
	rm -rf $(RESULT)

re: fclean byte-code
