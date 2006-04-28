##
## DO NOT MODIFY ANYTHING BELOW THIS POINT
##

ifndef CONFIGDIR
CONFIGDIR = ../../config
endif

# Don't use the default dependency generator
NODEPEND = "true"

-include $(CONFIGDIR)/data.make
-include $(CONFIGDIR)/Makefile.os

clean:
	$(RM) *.cmi
	$(RM) *.mli
	$(RM) *.ml
	$(RM) *.cmo
	$(RM) *.cmx 
	$(RM) *.o
	$(RM) *~

libclean: 
	$(RM) *.cmi
	$(RM) *.cma *.cmxa

reallyclean: clean libclean
	$(RM) depend


