RESULT     = step2
SOURCES    =	\
	Case.ml \
	Aff.ml	\
	Lab.ml	\
	main.ml
LIBS       = bigarray sdl sdlloader
INCDIRS    = +sdl

include OCamlMakefile

all: byte-code

$(RESULT): all

fclean: clean
	rm -rf $(RESULT)

re: fclean byte-code
