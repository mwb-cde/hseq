##
## DO NOT MODIFY ANYTHING BELOW THIS POINT
##

CONFIGDIR = ../../config

# Don't use the default dependency generator
NODEPEND = "true"

-include $(CONFIGDIR)/data.make
include $(CONFIGDIR)/Makefile.os

clean:
	$(RM) *.data
	$(RM) *.html *.css
	$(RM) *.info
	$(RM) *.cmi
	$(RM) *.mli
	$(RM) *.cmo
	$(RM) *.cmx 
	$(RM) *.o
	$(RM) *~

libclean: clean
	$(RM) *.cmi
	$(RM) *.cma *.cmxa

reallyclean: clean libclean
	$(RM) depend


