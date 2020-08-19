COLOR = color
IMAGE = image
ANIMATE = animate

export THREADS = y

all : bcl

bcl :
	make bcl -f Makefile-$(COLOR)
	make bcl -f Makefile-$(IMAGE)
	make bcl -f Makefile-$(ANIMATE)

ncl :
	make ncl -f Makefile-$(COLOR)
	make ncl -f Makefile-$(IMAGE)
	make ncl -f Makefile-$(ANIMATE)

clean :
	make clean -f Makefile-$(COLOR)
	make clean -f Makefile-$(IMAGE)
	make clean -f Makefile-$(ANIMATE)

install : bcl ncl
	ocamlfind install animate META $(wildcard *.cma *.mli *.cmxa *.cmi *.a)

uninstall :
	ocamlfind remove animate
