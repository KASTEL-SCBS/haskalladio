# modify this to run queries on different model instances,
# or with a different linkModel.
INSTANCE=Instances/PaperExample/ExampleOne
# INSTANCE=Instances/FromPalladio
LINKMODEL=simpleLinkModel.P

# if INSTANCE==Instances/FromPalladio, specify the path to the exported facts file fromPalladio.P to be used:
FROMPALLADIO_FACTS=$(FROMPALLADIO_FACTS_EXAMPLE)

# path to the xsb binary, see http://xsb.sourceforge.net/
# use version 3.5
XSB=xsb
XSB_OPTIONS_QUIET=--noprompt --quietload --nobanner --nofeedback

# path to runghc, required for pretty-printed proofs
RUNGHC=runghc




# These shouldn't need to be changed
FROMPALLADIO_FACTS_MINIMAL=Instances/FromPalladio/edu.kit.kastel.scbs.minimalexample/src-gen/fromPalladio.P
FROMPALLADIO_FACTS_EXAMPLE=Instances/FromPalladio/edu.kit.kastel.scbs.paperexample/src-gen/fromPalladio.P
FROMPALLADIO_FACTS_TRAVELPLANNER=Instances/FromPalladio/edu.kit.kastel.scbs.travelplanner/src-gen/fromPalladio.P
FROMPALLADIO_FACTS_EMPTY=Instances/FromPalladio/empty.P
FROMPALLADIO_PREFIX=Instances/FromPalladio/fromPalladio.P.prefix
INSTANCE_SYMLINKS=componentRepositoryInstance.P palladioInstance.P securityInstance.P simpleLinkModelInstance.P
LINKMODEL_SYMLINK=linkModel.P
FROMPALLADIO=fromPalladio.P
SOURCES=$(INSTANCE_SYMLINKS) $(LINKMODEL_SYMLINK) $(FROMPALLADIO) abstractAnalysis.P  queries.P  security.P palladio.P
XWAMS=$(SOURCES:.P=.xwam)

.PHONY: clean clean-xwam test-queries-justify force queries.result default dist

default: queries-justify.result

dist: accessanalysis-prolog.zip

.force :

$(LINKMODEL_SYMLINK)  : .force
	ln -f -s $(LINKMODEL) $@

$(FROMPALLADIO) : .force  $(FROMPALLADIO_PREFIX) $(FROMPALLADIO_FACTS)
	cat $(filter-out $<,$^) > $@

$(INSTANCE_SYMLINKS) : .force
	ln -f -s $(INSTANCE)/$@ $@

%.xwam : %.P
	$(XSB) $(<:.P=)
%_jxm.P : %.P
	$(XSB) $(XSB_OPTIONS_QUIET) -e "[justify],[$(<:.P=)],jxm($(<:.P=)),halt."


listFacts.result: $(XWAMS)
	$(XSB) $(XSB_OPTIONS_QUIET) -e "[listFacts],listFacts,halt." > $@

queries.result: $(XWAMS)
	$(XSB) $(XSB_OPTIONS_QUIET) -e "[queries],nojustify6,halt." > $@

queries-justify.result.pretty: Prettyprint.hs queries-justify.result
	$(RUNGHC) $^ > $@

queries-justify.result: all_jxm.xwam all.xwam
	$(XSB) $(XSB_OPTIONS_QUIET) -e "[basics],[all_jxm],[all],justify6,halt." > $@

test-queries-justify: all_jxm.xwam all.xwam
	rlwrap $(XSB) -e "[basics],[all_jxm],[all]."

all.P : $(SOURCES)
	cat $^ | awk '/% BEGIN IMPORTS/{ignore=1}/% BEGIN EXPORTS/{ignore=1}{if (ignore==0) print }/% END IMPORTS/{ignore=0}/% END EXPORTS/{ignore=0};' > $@

all-with-imports.P : $(SOURCES)
	cat $^ > $@

clean: clean-xwam
	rm -f $(LINKMODEL_SYMLINK) $(INSTANCE_SYMLINKS) $(FROMPALLADIO)
	rm -f *_jxm.*
	rm -f *.result
	rm -f *.result.pretty
	rm -f all.P
	rm -f all-with-imports.P
	find . -name '*~' -delete

clean-xwam:
	rm -f *.xwam


accessanalysis-prolog.zip: abstractAnalysis.P  queries.P  security.P palladio.P $(LINKMODEL) $(wildcard $(INSTANCE)/*.P) $(wildcard Instances/FromPalladio/*.P)  Makefile README Prettyprint.hs $(FROMPALLADIO_FACTS_MINIMAL) $(FROMPALLADIO_FACTS_EXAMPLE) $(FROMPALLADIO_FACTS_TRAVELPLANNER) $(FROMPALLADIO_FACTS_EMPTY) $(FROMPALLADIO_PREFIX) $(FROMPALLADIO)
	rm -f $@
	zip $@ $^
