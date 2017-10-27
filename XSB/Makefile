# modify this to run queries on different model instances,
# or with a different linkModel.
# INSTANCE=Instances/PaperExample/ExampleOne
INSTANCE=Instances/FromPalladio
LINKMODEL=simpleLinkModel.P

# if INSTANCE==Instances/FromPalladio, specify the path to the exported facts file fromPalladio.P to be used:
FROMPALLADIO_FACTS=$(FROMPALLADIO_FACTS_CLOUDSCENARIO_MINIMIZED)

# path to the xsb binary, see http://xsb.sourceforge.net/
# use version 3.5
XSB=xsb
XSB_OPTIONS_QUIET=--noprompt --quietload --nobanner --nofeedback

# path to runghc, required for pretty-printed proofs
RUNGHC=runghc
# path to ghci, required for interactive proof exploration
GHCI=ghci




# These shouldn't need to be changed
FROMPALLADIO_FACTS_IFLOW_SIMON=Instances/FromPalladio/edu.kit.kastel.scbs.iflowexample-simonsdatasetsinterpretation/src-gen/fromPalladio.P
FROMPALLADIO_FACTS_CLOUDSCENARIO=Instances/FromPalladio/edu.kit.kastel.scbs.cloudscenario/src-gen/fromPalladio.P
FROMPALLADIO_FACTS_CLOUDSCENARIO_MINIMIZED=Instances/FromPalladio/edu.kit.kastel.scbs.cloudscenario-minimized/src-gen/fromPalladio.P
FROMPALLADIO_FACTS_MINIMAL=Instances/FromPalladio/edu.kit.kastel.scbs.minimalexample/src-gen/fromPalladio.P
FROMPALLADIO_FACTS_EXAMPLE=Instances/FromPalladio/edu.kit.kastel.scbs.paperexample/src-gen/fromPalladio.P
FROMPALLADIO_FACTS_TRAVELPLANNER=Instances/FromPalladio/edu.kit.kastel.scbs.travelplanner/src-gen/fromPalladio.P
FROMPALLADIO_FACTS_EMPTY=Instances/FromPalladio/empty.P
FROMPALLADIO_PREFIX=Instances/FromPalladio/fromPalladio.P.prefix
INSTANCE_SYMLINKS=componentRepositoryInstance.P palladioInstance.P securityInstance.P simpleLinkModelInstance.P descriptionInstance.P worldInstance.P
LINKMODEL_SYMLINK=linkModel.P
FROMPALLADIO=fromPalladio.P
SOURCES=$(INSTANCE_SYMLINKS) $(LINKMODEL_SYMLINK) $(FROMPALLADIO) abstractAnalysis.P  queries.P  security.P palladio.P
XWAMS=$(SOURCES:.P=.xwam)

.PHONY: clean clean-xwam test test-queries-justify force queries.result default dist symlinks

default: queries-justify.result

dist: accessanalysis-prolog.zip

.force :

symlinks : $(INSTANCE_SYMLINKS) $(LINKMODEL_SYMLINK) $(FROMPALLADIO)

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


descriptions.result: $(XWAMS)
	$(XSB) $(XSB_OPTIONS_QUIET) -e "[descriptionInstance], itemDescription(Item, Description, InstanceDescription), write_canonical(itemDescription(Item, Description, InstanceDescription)), nl, fail, true. halt." > $@

listFacts.result: $(XWAMS)
	$(XSB) $(XSB_OPTIONS_QUIET) -e "[listFacts],listFacts,halt." > $@

queries.result: $(XWAMS)
	time $(XSB) $(XSB_OPTIONS_QUIET) -e "[queries],((nojustify6, halt) ; halt)." > $@

queries-justify.result.pretty: Prettyprint.hs queries-justify.result descriptions.result
	time $(RUNGHC) $^ > $@

explore: Explore.hs Prettyprint.hs queries-justify.result descriptions.result
	@echo "\n\n\n\n"
	@echo "Use\n"
	@echo "    \"*Explore> main\"\n"
	@echo to get started!
	@echo "\n\n\n\n"
	$(GHCI) $<

queries-justify.result.pretty.simplified: Simplify.hs queries-justify.result
	time $(RUNGHC) $^ > $@


queries-justify.result: all_jxm.xwam all.xwam
	time $(XSB) $(XSB_OPTIONS_QUIET) -e "[basics],[all_jxm],[all],(justify6 ; halt)." > $@

test-queries-justify: all_jxm.xwam all.xwam
	rlwrap $(XSB) -e "[basics],[all_jxm],[all]."

test: clean symlinks
	rlwrap $(XSB) -e "[basics],[security],[securityInstance], [palladio], [componentRepositoryInstance], [abstractAnalysis],[descriptionInstance],[util],[fromPalladio],[worldInstance]."



all.P : $(SOURCES)
	cat $^ | awk '/% BEGIN IMPORTS/{ignore=1}/% BEGIN EXPORTS/{ignore=1}{if (ignore==0) print }/% END IMPORTS/{ignore=0}/% END EXPORTS/{ignore=0};' > $@

all-with-imports.P : $(SOURCES)
	cat $^ > $@

clean: clean-xwam
	rm -f $(LINKMODEL_SYMLINK) $(INSTANCE_SYMLINKS) $(FROMPALLADIO)
	rm -f *_jxm.*
	rm -f *.result
	rm -f *.result.pretty
	rm -f *.result.pretty.simplified
	rm -f all.P
	rm -f all-with-imports.P
	find . -name '*~' -delete

clean-xwam:
	rm -f *.xwam


accessanalysis-prolog.zip: abstractAnalysis.P  queries.P  security.P palladio.P $(LINKMODEL) $(wildcard $(INSTANCE)/*.P) $(wildcard Instances/FromPalladio/*.P)  Makefile README Prettyprint.hs $(FROMPALLADIO_FACTS_MINIMAL) $(FROMPALLADIO_FACTS_EXAMPLE) $(FROMPALLADIO_FACTS_TRAVELPLANNER) $(FROMPALLADIO_FACTS_EMPTY) $(FROMPALLADIO_PREFIX) $(FROMPALLADIO)
	rm -f $@
	zip $@ $^
