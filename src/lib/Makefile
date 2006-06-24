##
## DO NOT MODIFY ANYTHING BELOW THIS POINT
##

CONFIGDIR = ../../config

# Don't use the default dependency generator
NODEPEND = "true"

-include $(CONFIGDIR)/data.make
-include $(CONFIGDIR)/Makefile.os

clean:
	$(RM) *.mli *.ml
	$(RM) *.cmi *.cmo *.cmx *.o
	$(RM) *~

libclean: 
	$(RM) *.cmi *.cma *.cmxa *.a

reallyclean: clean libclean
	$(RM) depend


