# modify this to run queries on different model instances,
# or with a different linkModel.
INSTANCE=Instances/PaperExample/ExampleOne
LINKMODEL=simpleLinkModel.P

# path to the xsb binary, see http://xsb.sourceforge.net/
XSB=xsb
XSB_OPTIONS_QUIET=--noprompt --quietload --nobanner --nofeedback



# These shouldn't need to be changed
INSTANCE_SYMLINKS=componentRepositoryInstance.P palladioInstance.P securityInstance.P simpleLinkModelInstance.P
LINKMODEL_SYMLINK=linkModel.P
SOURCES=$(INSTANCE_SYMLINKS) $(LINKMODEL_SYMLINK) abstractAnalysis.P  queries.P  security.P palladio.P
XWAMS=$(SOURCES:.P=.xwam)

.PHONY: clean clean-xwam test-queries-justify force queries.result default

default: queries-justify.result

.force :

$(LINKMODEL_SYMLINK)  : .force
	ln -f -s $(LINKMODEL) $@

$(INSTANCE_SYMLINKS) : .force
	ln -f -s $(INSTANCE)/$@ $@

%.xwam : %.P
	$(XSB) $(<:.P=)
%_jxm.P : %.P
	$(XSB) $(XSB_OPTIONS_QUIET) -e "[justify],[$(<:.P=)],jxm($(<:.P=)),halt."


queries.result: $(XWAMS)
	$(XSB) $(XSB_OPTIONS_QUIET) -e "[queries],nojustify6,halt." > $@

queries-justify.result: all_jxm.xwam all.xwam
	$(XSB) $(XSB_OPTIONS_QUIET) -e "[basics],[all_jxm],[all],justify6,halt." > $@

test-queries-justify: all_jxm.xwam all.xwam
	rlwrap $(XSB) -e "[basics],[all_jxm],[all]."

all.P : $(SOURCES)
	cat $^ | awk '/% BEGIN IMPORTS/{ignore=1}/% BEGIN EXPORTS/{ignore=1}{if (ignore==0) print }/% END IMPORTS/{ignore=0}/% END EXPORTS/{ignore=0};' > $@

all-with-imports.P : $(SOURCES)
	cat $^ > $@

clean: clean-xwam
	rm -f $(LINKMODEL_SYMLINK) $(INSTANCE_SYMLINKS)
	rm -f *_jxm.*
	rm -f *.result
	rm -f all.P
	rm -f all-with-imports.P
	find . -name '*~' -delete

clean-xwam:
	rm -f *.xwam


