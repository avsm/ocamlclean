###########################################################################
##                                                                       ##
##                              OCamlClean                               ##
##                                                                       ##
##            Benoit Vaugon, Université Pierre et Marie Curie            ##
##                                                                       ##
##    Ce fichier est distribué sous les termes de la licence CeCILL-B    ##
##    décrite dans le fichier ../LICENCE.                                ##
##                                                                       ##
###########################################################################

include ../etc/Makefile.conf

TARG = $(BIN)/ocamlclean
SRCS = $(wildcard *.ml)
BUILD = main.native

ocamlclean: $(TARG)

$(TARG): _build/$(BUILD)
	cp $< $@

_build/$(BUILD): $(SRCS) config.ml
	$(OCAMLBUILD) $(BUILD)

config.ml: $(ETC)/config.ml
	cp $< $@

clean:
	@$(OCAMLBUILD) -clean
	@rm -f *~ config.ml $(TARG)

.PHONY: ocamlclean clean
