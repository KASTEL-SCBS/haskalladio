

INSTANCE=Instances/SmartHome/ExampleOne
LINKMODEL=simpleLinkModel.P


XSB=xsb
XSB_OPTIONS_QUIET=--noprompt --quietload --nobanner --nofeedback




SOURCES=abstractAnalysis.P componentRepositoryInstance.P  linkModel.P palladioInstance.P queries.P  security.P palladio.P  securityInstance.P  simpleLinkModelInstance.P
XWAMS=$(SOURCES:.P=.xwam)

.PHONY: all clean clean-xwam test-queries-justify force


.force :

linkModel.P  : .force
	rm -f $@
	ln -s simpleLinkModel.P $@

componentRepositoryInstance.P palladioInstance.P securityInstance.P simpleLinkModelInstance.P : .force
	rm -f $@
	ln -s $(INSTANCE)/$@ $@



%.xwam : %.P
	$(XSB) $(<:.P=)
%_jxm.P : %.P
	$(XSB) $(XSB_OPTIONS_QUIET) -e "[justify],[$(<:.P=)],jxm($(<:.P=)),halt."


queries.result: $(XWAMS)
	$(XSB) $(XSB_OPTIONS_QUIET) -e "[queries],[$(<:.P=)],jxm($(<:.P=)),halt."


queries-justifiy.result: all_jxm.xwam all.xwam
	$(XSB) $(XSB_OPTIONS_QUIET) -e "[all_jxm],[justify],[all],justify6,halt." > $@

test-queries-justifiy: all_jxm.xwam all.xwam
	rlwrap $(XSB) -e "[all_jxm],[justify],[all],[lists]."


all.P : $(SOURCES)
	cat $^ | awk '/% BEGIN IMPORTS/{ignore=1}/% BEGIN EXPORTS/{ignore=1}{if (ignore==0) print }/% END IMPORTS/{ignore=0}/% END EXPORTS/{ignore=0};' > $@

all-with-imports.P : $(SOURCES)
	cat $^ > $@



clean: clean-xwam
	rm -f *_jxm.*
	rm -f *.result
	rm -f all.P
	rm -f all-with-imports.P

clean-xwam:
	rm -f *.xwam


